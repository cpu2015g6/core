library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
package cache_pack is
	type op_type is (READ_op, WRITE_op, NOP_op);
	constant cache_width : integer := 10;
	constant port_width : integer := 2;
	subtype id_type is std_logic_vector(port_width-1 downto 0);
	constant id_zero : id_type := (others => '0');
	type in_type is record
		id : id_type;
		op : op_type;
		addr : std_logic_vector(19 downto 0);
		data : std_logic_vector(31 downto 0);
	end record;
	constant in_zero : in_type := (
		id_zero,
		NOP_op,
		(others => '0'),
		(others => '0')
	);
	type out_port_type is record
		data : std_logic_vector(31 downto 0);
		en : std_logic;
	end record;
	constant out_port_zero : out_port_type := (
		(others => '0'),
		'0'
	);
	type out_port_array_type is array (0 to 2**port_width-1) of out_port_type;
	type cache_entry_type is record
		modified, valid : std_logic;
		tag : std_logic_vector(20-1-cache_width downto 0);
		data : std_logic_vector(31 downto 0);
	end record;
	constant cache_entry_zero : cache_entry_type := (
		'0', '0',
		(others => '0'),
		(others => '0')
	);
	type victim_entry_type is record
		modified, valid : std_logic;
		addr : std_logic_vector(19 downto 0);
		data : std_logic_vector(31 downto 0);
	end record;
	constant victim_entry_zero : victim_entry_type := (
		'0', '0',
		(others => '0'),
		(others => '0')
	);
	constant victim_width : integer := 2;
	constant victim_size : integer := victim_width**2;
	subtype log_type is std_logic_vector((victim_size*(victim_size-1))/2 - 1 downto 0);
	type victim_array_type is array(0 to victim_size-1) of victim_entry_type;
	type victim_type is record
		varray : victim_array_type;
		log : log_type;
	end record;
	constant victim_zero : victim_type := (
		(others => victim_entry_zero),
		(others => '0')
	);
	function to_cache_entry_type(victim_entry : victim_entry_type) return cache_entry_type is
	begin
		return (
			valid => victim_entry.valid,
			modified => victim_entry.modified,
			tag => victim_entry.addr(19 downto 20-cache_width),
			data => victim_entry.data
		);
	end to_cache_entry_type;
	function to_victim_entry_type(cache_entry : cache_entry_type; loweraddr : std_logic_vector(cache_width-1 downto 0)) return victim_entry_type is
	begin
		return (
			valid => cache_entry.valid,
			modified => cache_entry.modified,
			addr => cache_entry.tag & loweraddr,
			data => cache_entry.data
		);
	end to_victim_entry_type;
	function oldest_victim(log : std_logic_vector(5 downto 0)) return integer is
	begin
		return 0;
	end oldest_victim;
--	function set_newest(log : log_type; i : integer) return log_type;
	function search_victim(victim : victim_type; addr: std_logic_vector(19 downto 0)) return integer is
		variable ret : integer;
	begin
		ret := -1;
		for i in victim.varray'range loop
			if victim.varray(i).addr = addr then
				ret := i;
			end if;
		end loop;
		return ret;
	end search_victim;
	function hit(addr : std_logic_vector(19 downto 0); cache_entry : cache_entry_type) return boolean is
	begin
		return (addr(19 downto 20-cache_width) = cache_entry.tag) and (cache_entry.valid = '1');
	end hit;
	function replace(victim : victim_type; i : integer; entry : victim_entry_type) return victim_type is
		variable v : victim_type;
	begin
		v := victim;
		v.varray(i) := entry;
		return v;
	end replace;
end cache_pack;

--library ieee;
--use ieee.std_logic_1164.all;
--use ieee.numeric_std.all;
--use work.cache_pack.all;
--
--entity bram_cache is
--	generic(
--		awidth : integer
--	);
--	port(
--		clk : in std_logic;
--		wea, web : in std_logic;
--		addra, addrb : in std_logic_vector(awidth-1 downto 0);
--		dina, dinb : in cache_entry_type;
--		douta, doutb : out cache_entry_type
--	);
--end entity;
--
--architecture beh of bram_cache is
--	type ram_type is array(0 to 2**awidth-1) of cache_entry_type;
--	signal ram : ram_type := (others => cache_entry_zero);
--begin
--	process(clk)
--	begin
--		if rising_edge(clk) then
--			douta <= ram(to_integer(unsigned(addra)));
--			doutb <= ram(to_integer(unsigned(addrb)));
--			if wea = '1' then
--				ram(to_integer(unsigned(addra))) <= dina;
--			end if;
--			if web = '1' then
--				ram(to_integer(unsigned(addrb))) <= dinb;
--			end if;
--		end if;
--	end process;
--end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common.all;
use work.cache_pack.all;

entity cache is
	port(
		clk, rst : in std_logic;
		op : in op_type;
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
	type load_hist_type is array (0 to 5) of load_hist_entry_type;
	type reg_type is record
		busy : std_logic;
		free_port : std_logic_vector(2**port_width-1 downto 0);
		prev_in : in_type;
		victim : victim_type;
		victim_read_num : std_logic_vector(victim_width-1 downto 0);
		victim_read : victim_entry_type;
		id : id_type;
		load_hist : load_hist_type;
	end record;
	constant rzero : reg_type := (
		'0',
		(others => '0'),
		in_zero,
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
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(43 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(43 DOWNTO 0);
    clkb : IN STD_LOGIC;
    web : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addrb : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dinb : IN STD_LOGIC_VECTOR(43 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(43 DOWNTO 0)
  );
END COMPONENT;
--	signal douta, doutb : cache_entry_type;
	signal douta, doutb : std_logic_vector(43 downto 0);
	function encode(e : cache_entry_type) return std_logic_vector is
	begin
		return e.valid & e.modified & e.tag & e.data;
	end encode;
	function decode(d : std_logic_vector(43 downto 0)) return cache_entry_type is
	begin
		return (
			valid => d(43),
			modified => d(42),
			tag => d(41 downto 32),
			data => d(31 downto 0)
		);
	end decode;
	type bram_in_type is record
		we : std_logic;
		addr : std_logic_vector(cache_width-1 downto 0);
		din : cache_entry_type;
	end record;
	constant bram_in_zero : bram_in_type := (
		'0',
		(others => '0'),
		cache_entry_zero
	);
	signal ina, inb : bram_in_type := bram_in_zero;
	signal ina_we, inb_we : std_logic_vector(0 downto 0) := "0";
begin
--	bram : bram_cache
--	generic map(awidth => cache_width)
--	port map(
--		clk => clk,
--		wea => wea,
--		web => web,
--		addra => addra,
--		addrb => addrb,
--		dina => dina,
--		dinb => dinb,
--		douta => douta,
--		doutb => doutb
--	);
	busy <= r.busy;
	ina_we(0) <= ina.we;
	inb_we(0) <= inb.we;
	blockram : bram
	PORT map (
		clka => clk,
		clkb => clk,
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
	process(r, douta, doutb, op, din, addr, sramifout)
		variable v : reg_type;
		variable op_v : op_type;
		variable ina_v, inb_v : bram_in_type;
		variable out_port_v : out_port_array_type;
		variable douta_decoded, doutb_decoded : cache_entry_type;
		variable douta_victim : victim_entry_type;
		variable victim_oldest : victim_entry_type;
		variable lh_ent : load_hist_entry_type;
		variable load_hist_hit : integer;
		variable victim_oldest_i : integer;
--		variable prev_in_bram_write : cache_entry_type;
		variable victim_read_num : integer;
		variable victim_i : integer;
		variable sramifin_v : sramif_in;
		variable port_waiting_v : std_logic_vector(victim_size-1 downto 0);
		alias oldest_load_hist : load_hist_entry_type is r.load_hist(r.load_hist'length-1);
	begin
		ina_v := bram_in_zero;
		inb_v := bram_in_zero;
		v := r;
		op_v := op;
		douta_decoded := decode(douta);
		doutb_decoded := decode(doutb);
--		prev_in_bram_write := cache_entry_zero;
		out_port_v := (others => out_port_zero);
		sramifin_v := sramif_in_zero;
		if r.busy = '1' then
			op_v := NOP_op;
		end if;
		v.prev_in := (
			id => r.id,
			op => op_v,
			data => din,
			addr => addr
		);
		v.load_hist(0) := load_hist_entry_zero;
		v.load_hist(1 to v.load_hist'length-1) := r.load_hist(0 to v.load_hist'length-2);
		victim_read_num := to_integer(unsigned(r.victim_read_num));
		-- search load history
		load_hist_hit := -1;
		for i in 0 to r.load_hist'length-2 loop
			if r.load_hist(i).addr = addr and r.load_hist(i).obsolete = '0' then
				load_hist_hit := i;
			end if;
		end loop;
		case r.prev_in.op is
		when NOP_op =>
			douta_victim := to_victim_entry_type(douta_decoded, lh_ent.addr(cache_width-1 downto 0));
			lh_ent := r.load_hist(r.load_hist'length-1);
			if lh_ent.valid = '1' then
				victim_oldest_i := oldest_victim(r.victim.log);
				victim_oldest := r.victim.varray(victim_oldest_i);
				v.victim := replace(r.victim, victim_oldest_i, douta_victim);
				if victim_oldest.modified = '1' then
					sramifin_v := (
						op => SRAM_STORE,
						addr => victim_oldest.addr,
						wd => victim_oldest.data
					);
				end if;
			end if;
		when READ_op =>
			douta_victim := to_victim_entry_type(douta_decoded, r.prev_in.addr(cache_width-1 downto 0));
			if hit(r.prev_in.addr, douta_decoded) then
				out_port_v(to_integer(unsigned(r.prev_in.id))) := (
					data => douta_decoded.data,
					en => '1'
				);
			elsif r.victim_read.valid = '1' then
				--prev_in_bram_write := to_cache_entry_type(r.victim_read);
				v.victim := replace(r.victim, victim_read_num, douta_victim);
				out_port_v(to_integer(unsigned(r.prev_in.id))) := (
					data => r.victim_read.data,
					en => '1'
				);
			else
				sramifin_v := (
					op => SRAM_LOAD,
					addr => r.prev_in.addr,
					wd => (others => '0')
				);
				port_waiting_v := (others => '0');
				port_waiting_v(to_integer(unsigned(r.prev_in.id))) := '1';
				v.load_hist(0) := (
					valid => '1',
					addr => r.prev_in.addr,
					port_waiting => port_waiting_v
				);
			end if;
		when WRITE_op =>
			douta_victim := to_victim_entry_type(douta_decoded, r.prev_in.addr(cache_width-1 downto 0));
			if hit(r.prev_in.addr, decode(douta)) then
				-- do nothing
			elsif r.victim_read.valid = '1' then
				v.victim := replace(r.victim, victim_read_num, douta_victim);
			else
				victim_oldest_i := oldest_victim(r.victim.log);
				victim_oldest := r.victim.varray(victim_oldest_i);
				v.victim := replace(r.victim, victim_oldest_i, douta_victim);
				if victim_oldest.modified = '1' then
					sramifin_v := (
						op => SRAM_STORE,
						addr => victim_oldest.addr,
						wd => victim_oldest.data
					);
				end if;
			end if;
		end case;
		case op_v is
		when NOP_op =>
			lh_ent := r.load_hist(r.load_hist'length-2);
			if lh_ent.valid = '1' then
				ina_v := (
					we => '1',
					addr => lh_ent.addr(cache_width-1 downto 0),
					din => (
						valid => '1',
						modified => '0',
						tag => lh_ent.addr(19 downto 20-cache_width),
						data => sramifout.rd
					)
				);
				for i in lh_ent.port_waiting'range loop
					if lh_ent.port_waiting(i) = '1' then
						out_port_v(i) := (
							data => sramifout.rd,
							en => '1'
						);
					end if;
				end loop;
			end if;
		when READ_op =>
			victim_i := search_victim(v.victim, addr);
			if victim_i = -1 then
				v.victim_read := victim_entry_zero;
				v.victim_read_num := (others => '0');
				ina_v := (
					we => '0',
					addr => addr(cache_width-1 downto 0),
					din => cache_entry_zero
				);
			else
				v.victim_read := v.victim.varray(victim_i);
				v.victim_read_num := std_logic_vector(to_unsigned(victim_i, victim_width));
				ina_v := (
					we => '1',
					addr => addr(cache_width-1 downto 0),
					din => to_cache_entry_type(v.victim.varray(victim_i))
				);
			end if;
		when WRITE_op =>
			victim_i := search_victim(v.victim, addr);
			if victim_i = -1 then
				v.victim_read := victim_entry_zero;
				v.victim_read_num := (others => '0');
			else
				v.victim_read := v.victim.varray(victim_i);
				v.victim_read_num := std_logic_vector(to_unsigned(victim_i, victim_width));
			end if;
			ina_v := (
				we => '1',
				addr => addr(cache_width-1 downto 0),
				din => (
					valid => '1',
					modified => '1',
					tag => addr(19 downto 20-cache_width),
					data => din
				)
			);
		end case;
		if v.free_port = (2**port_width-1 downto 0 => '0') or (v.load_hist(v.load_hist'length-2).valid = '1' and v.load_hist(v.load_hist'length-2).obsolete = '0') then
			v.busy := '1';
		else
			v.busy := '0';
		end if;
		r_in <= v;
		ina <= ina_v;
		inb <= inb_v;
		out_port <= out_port_v;
	end process;
end;
