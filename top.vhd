library ieee;
use ieee.std_logic_1164.all;

entity top is
	port(
		MCLK1 : in std_logic;
		RS_TX : out std_logic;
		RS_RX : in std_logic;
		XRST : in std_logic;
		ZD : inout std_logic_vector(31 downto 0);
		ZA : out std_logic_vector(19 downto 0);
		XWA, XE1, E2A, XE3 : out std_logic;
		XGA, XZCKE, ADVA, XLBO, ZZA, XFT : out std_logic;
		XZBE : out std_logic_vector(3 downto 0);
		ZCLKMA : out std_logic_vector(1 downto 0)
	);
end top;

architecture twoproc of top is
	component cpu_top is
		port(
			clk, rst : in std_logic
		);
	end component;
begin
	cpu_l : cpu_top
	port map(clk => MCLK1, rst => not XRST);
end;
