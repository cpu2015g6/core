library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.fpu_pack.all;

entity fpu is
	port(
		clk, rst : in std_logic;
		fpu_in : in in_type;
		fpu_out : out out_type);
end fpu;

