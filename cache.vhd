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
	function set_newest(log : log_type; i : integer) return log_type;
	function to_cache_entry_type(victim_entry : victim_entry_type) return cache_entry_type;
	function to_victim_entry_type(cache_entry : cache_entry_type; loweraddr : std_logic_vector(cache_width-1 downto 0)) return victim_entry_type;
	function oldest_victim(log : std_logic_vector(5 downto 0)) return integer;
	function search_victim(victim : victim_type; addr: std_logic_vector(19 downto 0)) return integer;
	function hit(addr : std_logic_vector(19 downto 0); cache_entry : cache_entry_type) return boolean;
	function replace(victim : victim_type; i : integer; entry : victim_entry_type) return victim_type;
end cache_pack;

package body cache_pack is
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
--   newer
--  |0|1|2|3
-- 0| | | | 
-- 1|0| | | 
-- 2|1|3| | 
-- 3|2|4|5| 
	function oldest_victim(log : std_logic_vector(5 downto 0)) return integer is
	begin
		if log(0) = '0' and log(1) = '0' and log(2) = '0' then
			return 0;
		elsif log(0) = '1' and log(3) = '0' and log(4) = '0' then
			return 1;
		elsif log(1) = '1' and log(3) = '1' and log(5) = '0' then
			return 2;
		--elsif log(2) = '1' and log(4) = '1' and log(5) = '1' then
		--	return 3;
		else
			return 3;
		end if;
	end oldest_victim;
	function set_newest(log : log_type; i : integer) return log_type is
		variable ret : log_type;
	begin
		ret := log;
		if i = 0 then
			ret(0) := '1';
			ret(1) := '1';
			ret(2) := '1';
		elsif i = 1 then
			ret(0) := '0';
			ret(3) := '1';
			ret(4) := '1';
		elsif i = 2 then
			ret(1) := '0';
			ret(3) := '0';
			ret(5) := '1';
		elsif i = 3 then
			ret(2) := '0';
			ret(4) := '0';
			ret(5) := '0';
		end if;
		return ret;
	end set_newest;
	function search_victim(victim : victim_type; addr: std_logic_vector(19 downto 0)) return integer is
		variable ret : integer;
	begin
		ret := -1;
		for i in victim.varray'range loop
			if victim.varray(i).addr = addr and victim.varray(i).valid = '1' then
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
		if entry.valid = '1' then
			v.varray(i) := entry;
			v.log := set_newest(v.log, i);
		else
			v.varray(i) := victim_entry_zero;
		end if;
		return v;
	end replace;
end cache_pack;

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
		busy : boolean;
		used_port : std_logic_vector(2**port_width-1 downto 0);
		prev_in : in_type;
		victim : victim_type;
		victim_read_num : std_logic_vector(victim_width-1 downto 0);
		victim_read : victim_entry_type;
		id : id_type;
		load_hist : load_hist_type;
	end record;
	constant rzero : reg_type := (
		false,
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
	busy <= '1' when r.busy else '0';
	id <= r.id;
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
	comb: process(r, douta, doutb, op, din, addr, sramifout)
		variable v : reg_type;
		variable op_v : op_type;
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
		alias oldest_load_hist : load_hist_entry_type is r.load_hist(r.load_hist'length-1);
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
			op_v := NOP_op;
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
		end if;

		lh0 := load_hist_entry_zero;
		case r.prev_in.op is
		when NOP_op =>
			douta_victim := to_victim_entry_type(douta_decoded, lh_ent.addr(cache_width-1 downto 0));
			lh_ent := r.load_hist(r.load_hist'length-1);
			if lh_ent.valid = '1' and lh_ent.obsolete = '0' then
				report "write to victim" severity note;
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
				port_waiting_v := (others => '0');
				port_waiting_v(to_integer(unsigned(r.prev_in.id))) := '1';
				lh0 := (
					valid => '1',
					obsolete => '0',
					addr => r.prev_in.addr,
					port_waiting => port_waiting_v
				);
			end if;
		when WRITE_op =>
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
		when NOP_op =>
		when READ_op =>
			v.used_port(to_integer(unsigned(r.id))) := '1';
			victim_i := search_victim(v.victim, addr);
			if load_hist_hit /= -1 then
				report "read: load history hit" severity note;
				v.load_hist(load_hist_hit).port_waiting(to_integer(unsigned(r.id))) := '1';
				v.prev_in.op := NOP_op;
			elsif victim_i = -1 then
				report "read: victim miss" severity note;
				v.victim_read := victim_entry_zero;
				v.victim_read_num := (others => '0');
				ina_v := (
					we => '0',
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
					addr => addr(cache_width-1 downto 0),
					din => to_cache_entry_type(v.victim.varray(victim_i))
				);
			end if;
		when WRITE_op =>
			victim_i := search_victim(v.victim, addr);
			if load_hist_hit /= -1 then
				report "write: load history hit" severity note;
				v.load_hist(load_hist_hit).obsolete := '1';
			elsif victim_i = -1 then
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
		inb_v.addr := (others => '1');
		ina <= ina_v;
		inb <= inb_v;
		sramifin <= sramifin_v;
		out_port <= out_port_v;
	end process;
end;
