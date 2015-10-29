library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common.all;

entity program_loader is
	port(
		clk, rst : in std_logic;
		bram_we : out std_logic;
		bram_addr : out std_logic_vector(pc_width-1 downto 0);
		bram_din : out std_logic_vector(31 downto 0);
		recvifout : in recvif_out_type;
		recvifin : out recvif_in_type;
		sramifin : out sramif_in
	);
end program_loader;

architecture twoproc of program_loader is
	type state_type is (LOAD_HEADER, LOAD_BRAM, LOAD_SRAM, IDLE);
	type reg_type is record
		bram_size, sram_size : std_logic_vector(31 downto 0);
		state : state_type;
	end record;
	constant rzero : reg_type := (
		(others => '0'), (others => '0'),
		IDLE
	);
	signal r, r_in : reg_type := rzero;
begin
	process(clk, rst)
	begin
		if rst = '1' then
			r <= rzero;
		elsif rising_edge(clk) then
			r <= r_in;
		end if;
	end process;
	process(r, recvifout)
	begin
		case r.state is
		when IDLE =>
		when LOAD_HEADER =>
		when LOAD_BRAM =>
		when LOAD_SRAM =>
		end case;
	end process;
end;
