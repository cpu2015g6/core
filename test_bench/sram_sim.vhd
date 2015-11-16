library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sram_sim is
	port(
		clk : in std_logic;
		za : in std_logic_vector(19 downto 0);
		zd : inout std_logic_vector(31 downto 0);
		xwa : in std_logic
	);
end sram_sim;

architecture beh of sram_sim is
	type hist_type is record
		za : std_logic_vector(19 downto 0);
		xwa : std_logic;
	end record;
	constant hist_zero : hist_type := (
		(others => '0'),
		'1'
	);
	type hist_array is array(0 to 2) of hist_type;
	signal hist : hist_array := (others => hist_zero);
	type ram_t is array(0 to 1048575) of std_logic_vector(31 downto 0);
	signal ram : ram_t := (others => (others => '0'));
begin
	ZD <= (others => 'Z') when hist(1).xwa = '0' else ram(to_integer(unsigned(hist(1).za)));
	process(clk)
	begin
		if rising_edge(clk) then
			hist(1 to 2) <= hist(0 to 1);
			hist(0).za <= za;
			hist(0).xwa <= xwa;
			if hist(1).xwa = '0' then
				ram(to_integer(unsigned(hist(1).za))) <= zd;
			end if;
		end if;
	end process;
end;
