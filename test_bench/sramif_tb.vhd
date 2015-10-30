--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:   15:50:32 10/27/2015
-- Design Name:   
-- Module Name:   sramif_tb.vhd
-- Project Name:  cpu1
-- Target Device:  
-- Tool versions:  
-- Description:   
-- 
-- VHDL Test Bench Created by ISE for module: sramif
-- 
-- Dependencies:
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
-- Notes: 
-- This testbench has been automatically generated using types std_logic and
-- std_logic_vector for the ports of the unit under test.  Xilinx recommends
-- that these types always be used for the top-level I/O of a design in order
-- to guarantee that the testbench will bind correctly to the post-implementation 
-- simulation model.
--------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
 
use work.common.all;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--USE ieee.numeric_std.ALL;
 
ENTITY sramif_tb IS
END sramif_tb;
 
ARCHITECTURE behavior OF sramif_tb IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT sramif
	 generic(sim : boolean := false);
    PORT(
         clk : IN  std_logic;
         rst : IN  std_logic;
         ZD : INOUT  std_logic_vector(31 downto 0);
         ZA : OUT  std_logic_vector(19 downto 0);
         XWA : OUT  std_logic;
         XE1 : OUT  std_logic;
         E2A : OUT  std_logic;
         XE3 : OUT  std_logic;
         XGA : OUT  std_logic;
         XZCKE : OUT  std_logic;
         ADVA : OUT  std_logic;
         XLBO : OUT  std_logic;
         ZZA : OUT  std_logic;
         XFT : OUT  std_logic;
         XZBE : OUT  std_logic_vector(3 downto 0);
         ZCLKMA : OUT  std_logic_vector(1 downto 0);
         sramifin : IN  sramif_in;
         sramifout : OUT  sramif_out
        );
    END COMPONENT;
    

   --Inputs
   signal clk : std_logic := '0';
   signal rst : std_logic := '0';
   signal sramifin : sramif_in := sramif_in_zero;

	--BiDirs
   signal ZD : std_logic_vector(31 downto 0);

 	--Outputs
   signal ZA : std_logic_vector(19 downto 0);
   signal XWA : std_logic;
   signal XE1 : std_logic;
   signal E2A : std_logic;
   signal XE3 : std_logic;
   signal XGA : std_logic;
   signal XZCKE : std_logic;
   signal ADVA : std_logic;
   signal XLBO : std_logic;
   signal ZZA : std_logic;
   signal XFT : std_logic;
   signal XZBE : std_logic_vector(3 downto 0);
   signal ZCLKMA : std_logic_vector(1 downto 0);
   signal sramifout : sramif_out;

   -- Clock period definitions
   constant clk_period : time := 10 ns;
component sram_sim is
	port(
		clk : in std_logic;
		za : in std_logic_vector(19 downto 0);
		zd : inout std_logic_vector(31 downto 0);
		xwa : in std_logic
	);
end component;
BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
   uut: sramif
	generic map(sim => true)
	PORT MAP (
          clk => clk,
          rst => rst,
          ZD => ZD,
          ZA => ZA,
          XWA => XWA,
          XE1 => XE1,
          E2A => E2A,
          XE3 => XE3,
          XGA => XGA,
          XZCKE => XZCKE,
          ADVA => ADVA,
          XLBO => XLBO,
          ZZA => ZZA,
          XFT => XFT,
          XZBE => XZBE,
          ZCLKMA => ZCLKMA,
          sramifin => sramifin,
          sramifout => sramifout
        );
	sram : sram_sim
	port map(
		clk => clk,
		za => ZA,
		zd => ZD,
		xwa => XWA
	);
   -- Clock process definitions
   clk_process :process
   begin
		clk <= '0';
		wait for clk_period/2;
		clk <= '1';
		wait for clk_period/2;
   end process;
 

   -- Stimulus process
   stim_proc: process
   begin	
      wait for 10 ns;

      -- insert stimulus here 
		sramifin <= (
			op => SRAM_STORE,
			addr => (others => '0'),
			wd => x"cafecafe"
		);
      wait for clk_period;
		sramifin <= (
			op => SRAM_STORE,
			addr => x"00001",
			wd => x"deadbeef"
		);
      wait for clk_period;
		sramifin <= (
			op => SRAM_LOAD,
			addr => (others => '0'),
			wd => (others => '0')
		);
		wait for clk_period;
		sramifin <= (
			op => SRAM_LOAD,
			addr => x"00001",
			wd => (others => '0')
		);

      wait;
   end process;

END;
