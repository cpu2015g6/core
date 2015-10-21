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
		rs_full : std_logic;
		free_rs_num : rs_num_type;
		cdb_out : cdb_type;
		cdb_rs_num : rs_num_type;
	end record;
	constant reg_zero : reg_type := (
		(others => rs_zero),
		'0',
		(others => '0'),
		cdb_zero,
		(others => '0')
	);
	signal r, r_in : reg_type := reg_zero;
begin
	process(clk, rst)
	begin
		if rst = '1' then
			r <= reg_zero;
		elsif rising_edge(clk) then
			r <= r_in;
		end if;
	end process;
	process(mem_in, r)
	begin
	end process;
end;
