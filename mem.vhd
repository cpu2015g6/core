library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_pack.all;
use work.common.all;

entity mem is
	port(
		clk, rst : in std_logic;
		mem_in : in in_type;
		mem_out : out out_type);
end mem;

architecture twoproc of mem is
	type reg_type is record
		rs : rs_array_type;
		rs_youngest, rs_oldest, rs_exec : rs_num_type;
		rs_full : std_logic;
		cdb_out : cdb_type;
	end record;
	constant reg_zero : reg_type := (
		(others => rs_zero),
		(others => '0'), (others => '0'), (others => '0'),
		'0',
		cdb_zero
	);
	signal r, r_in : reg_type := reg_zero;
begin
	mem_out.cdb_out <= r.cdb_out;
	mem_out.rs_full <= '1' when std_logic_vector(unsigned(r.rs_youngest)+1) = r.rs_oldest else '0';
	process(clk, rst)
	begin
		if rst = '1' then
			r <= reg_zero;
		elsif rising_edge(clk) then
			r <= r_in;
		end if;
	end process;
	process(mem_in, r)
		variable v : reg_type;
		variable exec_complete : boolean;
	begin
		-- update rs
		for i in r.rs'range loop
			v.rs(i).common.ra := register_update(r.rs(i).common.ra, mem_in.cdb_in);
			v.rs(i).common.rb := register_update(r.rs(i).common.rb, mem_in.cdb_in);
		end loop;
		-- execute
		exec_complete := false;
		if rs_common_ready(v.rs(to_integer(unsigned(r.rs_exec))).common) then
			if exec_complete then
				v.rs_exec := std_logic_vector(unsigned(r.rs_exec) + 1);
			end if;
		end if;
		-- store new rs contents
		if r.rs_full = '0' and mem_in.rs_in.op /= NOP_op then
			v.rs(to_integer(unsigned(r.rs_youngest))) := mem_in.rs_in;
			v.rs_youngest := std_logic_vector(unsigned(r.rs_youngest) + 1);
		end if;
		if mem_in.cdb_next = '1' or r.cdb_out.tag.valid = '0' then
			-- clear rs
			if r.cdb_out.tag.valid = '1' then
				v.rs(to_integer(unsigned(r.rs_oldest))) := rs_zero;
				v.rs_oldest := std_logic_vector(unsigned(r.rs_oldest) + 1);
			end if;
			v.cdb_out := cdb_zero;
			if v.rs(to_integer(unsigned(v.rs_oldest))).common.state = RS_Done then
				v.cdb_out := make_cdb_out(v.rs(to_integer(unsigned(v.rs_oldest))).common);
			end if;
		end if;
		-- synchronous reset
		if mem_in.rst = '1' then
			r_in <= reg_zero;
		else
			r_in <= v;
		end if;
	end process;
end;
