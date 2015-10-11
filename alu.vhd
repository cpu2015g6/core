library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.alu_pack.all;
use work.common.all;

entity alu is
	port(
		clk, rst : in std_logic;
		alu_in : in in_type;
		alu_out : out out_type);
end alu;

architecture twoproc of alu is
	type rs_array_type is array (0 to 3) of rs_type;
	type reg_type is record
		rs : rs_array_type;
		rs_full : std_logic;
		free_rs_num : rs_num_type;
		cdb_out : cdb_type;
	end record;
	constant reg_zero : reg_type := (
		(others => rs_zero),
		'0',
		(others => '0'),
		cdb_zero
	);
	signal r, r_in : reg_type := reg_zero;
	procedure find_free_rs(
		rs : in rs_array_type;
		full : out std_logic;
		rs_num : out rs_num_type) is
	begin
	end find_free_rs;
begin
	process(clk, rst)
	begin
		if rst = '1' then
			r <= reg_zero;
		elsif rising_edge(clk) then
			r <= r_in;
		end if;
	end process;
	process(alu_in, r)
		variable v : reg_type;
		variable exec_done : boolean;
	begin
		v := r;
		exec_done := false;
		for i in r.rs'range loop
			if rs_common_ready(r.rs(i).common) and not exec_done then
				exec_done := true;
			end if;
		end loop;
		if r.rs_full = '0' and alu_in.rs_in.op /= NOP_op then
			v.rs(to_integer(unsigned(r.free_rs_num))) := alu_in.rs_in;
		end if;
		r_in <= v;
	end process;
end;
