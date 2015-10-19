library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_pack.all;

entity mem is
	port(
		clk, rst : in std_logic;
		mem_in : in in_type;
		mem_out : out out_type);
end mem;

architecture twoproc of mem is
begin
end;
