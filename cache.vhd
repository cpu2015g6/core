library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common.all;
use work.cache_pack.all;

entity cache is
	port(
		clk, rst : in std_logic;
		op : in cache_op_type;
		din : in std_logic_vector(31 downto 0);
		addr : in std_logic_vector(19 downto 0);
		sramifout : in sramif_out;
		sramifin : out sramif_in;
		id : out std_logic_vector(1 downto 0);
		out_port : out out_port_array_type;
		busy : out std_logic
	);
end cache;

architecture twoproc of cache is
	type load_hist_entry_type is record
		valid, obsolete : std_logic;
		addr : std_logic_vector(19 downto 0);
		port_waiting : std_logic_vector(2**port_width-1 downto 0);
	end record;
	constant load_hist_entry_zero : load_hist_entry_type := (
		'0', '0',
		(others => '0'),
		(others => '0')
	);
	type load_hist_type is array (0 to 4) of load_hist_entry_type;
	type reg_type is record
		busy : boolean;
		used_port : std_logic_vector(2**port_width-1 downto 0);
		prev_in : cache_in_type;
		victim : victim_type;
		victim_read_num : std_logic_vector(victim_width-1 downto 0);
		victim_read : victim_entry_type;
		id : id_type;
		load_hist : load_hist_type;
	end record;
	constant rzero : reg_type := (
		false,
		(others => '0'),
		cache_in_zero,
		victim_zero,
		(others => '0'),
		victim_entry_zero,
		id_zero,
		(others => load_hist_entry_zero)
	);
	signal r, r_in : reg_type := rzero;
	component bram_cache is
		generic(
			awidth : integer
		);
		port(
			clk : in std_logic;
			wea, web : in std_logic;
			addra, addrb : in std_logic_vector(awidth-1 downto 0);
			dina, dinb : in cache_entry_type;
			douta, doutb : out cache_entry_type
		);
	end component;
COMPONENT bram IS
  PORT (
    clka : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(43 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(43 DOWNTO 0);
    clkb : IN STD_LOGIC;
    enb : IN STD_LOGIC;
    web : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addrb : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dinb : IN STD_LOGIC_VECTOR(43 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(43 DOWNTO 0)
  );
END COMPONENT;
	signal douta, doutb : std_logic_vector(cache_entry_width-1 downto 0);
	function encode(e : cache_entry_type) return std_logic_vector is
	begin
		return e.valid & e.modified & e.tag & e.data;
	end encode;
	function decode(d : std_logic_vector(cache_entry_width-1 downto 0)) return cache_entry_type is
	begin
		return (
			valid => d(33+tag_width),
			modified => d(32+tag_width),
			tag => d(31+tag_width downto 32),
			data => d(31 downto 0)
		);
	end decode;
	type bram_in_type is record
		we, en : std_logic;
		addr : std_logic_vector(cache_width-1 downto 0);
		din : cache_entry_type;
	end record;
	constant bram_in_zero : bram_in_type := (
		'0', '0',
		(others => '0'),
		cache_entry_zero
	);
	signal ina, inb : bram_in_type := bram_in_zero;
	signal ina_we, inb_we : std_logic_vector(0 downto 0) := "0";
begin
	busy <= '1' when r.busy else '0';
	id <= r.id;
	ina_we(0) <= ina.we;
	inb_we(0) <= inb.we;
	blockram : bram
	PORT map (
		clka => clk,
		clkb => clk,
		ena => ina.en,
		enb => inb.en,
		wea => ina_we,
		web => inb_we,
		addra => ina.addr,
		addrb => inb.addr,
		dina => encode(ina.din),
		dinb => encode(inb.din),
		douta => douta,
		doutb => doutb
	);
	process(clk, rst) begin
		if rst = '1' then
			r <= rzero;
		elsif rising_edge(clk) then
			r <= r_in;
		end if;
	end process;
	comb: process(r, douta, doutb, op, din, addr, sramifout)
		variable v : reg_type;
		variable op_v : cache_op_type;
		variable ina_v, inb_v : bram_in_type;
		variable out_port_v : out_port_array_type;
		variable douta_decoded, doutb_decoded : cache_entry_type;
		variable douta_victim : victim_entry_type;
		variable victim_oldest : victim_entry_type;
		variable lh_ent, lh0 : load_hist_entry_type;
		variable load_hist_hit : integer;
		variable victim_oldest_i : integer;
		variable victim_read_num : integer;
		variable victim_i : integer;
		variable sramifin_v : sramif_in;
		variable port_waiting_v : std_logic_vector(victim_size-1 downto 0);
--		alias oldest_load_hist : load_hist_entry_type is r.load_hist(r.load_hist'length-1);
	begin
		ina_v := bram_in_zero;
		inb_v := bram_in_zero;
		v := r;
		op_v := op;
		douta_decoded := decode(douta);
		doutb_decoded := decode(doutb);
		out_port_v := (others => out_port_zero);
		sramifin_v := sramif_in_zero;
		if r.busy then
			op_v := NOP_cache_op;
		end if;
		v.prev_in := (
			id => r.id,
			op => op_v,
			data => din,
			addr => addr
		);
		victim_read_num := to_integer(unsigned(r.victim_read_num));
		-- search load history

		lh_ent := r.load_hist(r.load_hist'length-2);
		if lh_ent.valid = '1' then
			report "load completed" severity note;
			if lh_ent.obsolete = '0' then
				assert r.busy report "BUG";
				report "write to bram" severity note;
				ina_v := (
					we => '1',
					en => '1',
					addr => lh_ent.addr(cache_width-1 downto 0),
					din => (
						valid => '1',
						modified => '0',
						tag => lh_ent.addr(19 downto 20-cache_width),
						data => sramifout.rd
					)
				);
			end if;
			for i in lh_ent.port_waiting'range loop
				if lh_ent.port_waiting(i) = '1' then
					v.used_port(i) := '0';
					out_port_v(i) := (
						data => sramifout.rd,
						en => '1'
					);
				end if;
			end loop;
--			v.busy := false;
		end if;

		lh0 := load_hist_entry_zero;
		case r.prev_in.op is
		when NOP_cache_op =>
			lh_ent := r.load_hist(r.load_hist'length-1);
			douta_victim := to_victim_entry_type(douta_decoded, lh_ent.addr(cache_width-1 downto 0));
			if lh_ent.valid = '1' and lh_ent.obsolete = '0' then
				report "write to victim" severity note;
				victim_oldest_i := oldest_victim(r.victim.log);
				victim_oldest := r.victim.varray(victim_oldest_i);
				v.victim := replace(r.victim, victim_oldest_i, douta_victim);
				if victim_oldest.modified = '1' then
					assert victim_oldest.valid = '1' report "BUG";
					sramifin_v := (
						op => SRAM_STORE,
						addr => victim_oldest.addr,
						wd => victim_oldest.data
					);
				end if;
			end if;
		when READ_cache_op =>
			douta_victim := to_victim_entry_type(douta_decoded, r.prev_in.addr(cache_width-1 downto 0));
			if hit(r.prev_in.addr, douta_decoded) then
				report "previous read: bram hit" severity note;
				v.used_port(to_integer(unsigned(r.prev_in.id))) := '0';
				out_port_v(to_integer(unsigned(r.prev_in.id))) := (
					data => douta_decoded.data,
					en => '1'
				);
			elsif r.victim_read.valid = '1' then
				report "previous read: victim hit" severity note;
				v.victim := replace(r.victim, victim_read_num, douta_victim);
				v.used_port(to_integer(unsigned(r.prev_in.id))) := '0';
				out_port_v(to_integer(unsigned(r.prev_in.id))) := (
					data => r.victim_read.data,
					en => '1'
				);
			else
				report "previous read: miss" severity note;
				sramifin_v := (
					op => SRAM_LOAD,
					addr => r.prev_in.addr,
					wd => (others => '0')
				);
--				v.busy := true;
				port_waiting_v := (others => '0');
				port_waiting_v(to_integer(unsigned(r.prev_in.id))) := '1';
				lh0 := (
					valid => '1',
					obsolete => '0',
					addr => r.prev_in.addr,
					port_waiting => port_waiting_v
				);
			end if;
		when WRITE_cache_op =>
			douta_victim := to_victim_entry_type(douta_decoded, r.prev_in.addr(cache_width-1 downto 0));
			if hit(r.prev_in.addr, decode(douta)) then
				report "previous write: bram hit" severity note;
				-- do nothing
			elsif r.victim_read.valid = '1' then
				report "previous write: victim hit" severity note;
				v.victim := replace(r.victim, victim_read_num, douta_victim);
			else
				report "previous write: miss" severity note;
				victim_oldest_i := oldest_victim(r.victim.log);
				victim_oldest := r.victim.varray(victim_oldest_i);
				v.victim := replace(r.victim, victim_oldest_i, douta_victim);
				if victim_oldest.modified = '1' then
					report "previous write: write back" severity note;
					sramifin_v := (
						op => SRAM_STORE,
						addr => victim_oldest.addr,
						wd => victim_oldest.data
					);
				end if;
			end if;
		end case;

		v.load_hist(1 to v.load_hist'length-1) := v.load_hist(0 to v.load_hist'length-2);
		v.load_hist(0) := lh0;
		load_hist_hit := -1;
		for i in 0 to v.load_hist'length-2 loop
			if v.load_hist(i).addr = addr and v.load_hist(i).valid = '1' and v.load_hist(i).obsolete = '0' then
				load_hist_hit := i;
			end if;
		end loop;
		case op_v is
		when NOP_cache_op =>
			v.victim_read := victim_entry_zero;
			v.victim_read_num := (others => '0');
		when READ_cache_op =>
			v.used_port(to_integer(unsigned(r.id))) := '1';
			victim_i := search_victim(v.victim, addr);
			if load_hist_hit /= -1 then
				report "read: load history hit" severity note;
				v.load_hist(load_hist_hit).port_waiting(to_integer(unsigned(r.id))) := '1';
				v.prev_in := cache_in_zero;
			elsif victim_i = -1 then
				report "read: victim miss" severity note;
				v.victim_read := victim_entry_zero;
				v.victim_read_num := (others => '0');
				ina_v := (
					we => '0',
					en => '1',
					addr => addr(cache_width-1 downto 0),
					din => cache_entry_zero
				);
			else
				report "read: victim hit" severity note;
				v.victim_read := v.victim.varray(victim_i);
				v.victim.log := set_newest(v.victim.log, victim_i);
				v.victim_read_num := std_logic_vector(to_unsigned(victim_i, victim_width));
				ina_v := (
					we => '1',
					en => '1',
					addr => addr(cache_width-1 downto 0),
					din => to_cache_entry_type(v.victim.varray(victim_i))
				);
			end if;
		when WRITE_cache_op =>
			victim_i := search_victim(v.victim, addr);
			if load_hist_hit /= -1 then
				report "write: load history hit" severity note;
				v.load_hist(load_hist_hit).obsolete := '1';
			end if;
			if victim_i = -1 then
				report "write: victim miss" severity note;
				v.victim_read := victim_entry_zero;
				v.victim_read_num := (others => '0');
			else
				report "write: victim hit" severity note;
				v.victim_read := v.victim.varray(victim_i);
				v.victim_read_num := std_logic_vector(to_unsigned(victim_i, victim_width));
			end if;
			ina_v := (
				we => '1',
				en => '1',
				addr => addr(cache_width-1 downto 0),
				din => (
					valid => '1',
					modified => '1',
					tag => addr(19 downto 20-cache_width),
					data => din
				)
			);
		end case;
		if v.used_port = (2**port_width-1 downto 0 => '1') or (v.load_hist(v.load_hist'length-2).valid = '1' and v.load_hist(v.load_hist'length-2).obsolete = '0') then
			v.busy := true;
		else
			v.busy := false;
		end if;
		v.id := (others => '0');
		for i in v.used_port'range loop
			if v.used_port(i) = '0' then
				v.id := std_logic_vector(to_unsigned(i, port_width));
			end if;
		end loop;
		r_in <= v;
		ina <= ina_v;
		inb <= inb_v;
		sramifin <= sramifin_v;
		out_port <= out_port_v;
	end process;
end;
