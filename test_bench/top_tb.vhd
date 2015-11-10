--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:   10:33:44 10/28/2015
-- Design Name:   
-- Module Name:   top_tb.vhd
-- Project Name:  cpu1
-- Target Device:  
-- Tool versions:  
-- Description:   
-- 
-- VHDL Test Bench Created by ISE for module: top
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
 
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--USE ieee.numeric_std.ALL;
 
ENTITY top_tb IS
END top_tb;
 
ARCHITECTURE behavior OF top_tb IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT top
    PORT(
         MCLK1 : IN  std_logic;
         RS_TX : OUT  std_logic;
         RS_RX : IN  std_logic;
         XRST : IN  std_logic;
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
         ZCLKMA : OUT  std_logic_vector(1 downto 0)
        );
    END COMPONENT;
    

   --Inputs
   signal MCLK1 : std_logic := '0';
   signal RS_RX : std_logic := '0';
   signal XRST : std_logic := '1';

	--BiDirs
   signal ZD : std_logic_vector(31 downto 0);

 	--Outputs
   signal RS_TX : std_logic;
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
 
   constant MCLK1_period : time := 10 ns;
component sram_sim is
	port(
		clk : in std_logic;
		za : in std_logic_vector(19 downto 0);
		zd : inout std_logic_vector(31 downto 0);
		xwa : in std_logic
	);
end component;
	component transif is
		generic(w : std_logic_vector(15 downto 0));
		port(
			clk, rst : in std_logic;
			wr_en : in std_logic;
			tx : out std_logic;
			din : in std_logic_vector(7 downto 0);
			full : out std_logic
		);
	end component;
	component recvif is
		generic(w : std_logic_vector(15 downto 0));
		port(
	      clk, rst : in  std_logic;
	      rd_en : in  std_logic;
		  rx : in std_logic;
	      dout : out std_logic_vector(7 downto 0);
	      full : out std_logic;
	      empty : out std_logic
		);
	end component;
	signal rd_en : std_logic := '1';
	signal wr_en : std_logic := '0';
	signal din : std_logic_vector(7 downto 0) := (others => '0');
	signal dout : std_logic_vector(7 downto 0);
	signal empty : std_logic;
	signal r_full : std_logic;
	signal t_full : std_logic;
BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
   uut: top PORT MAP (
          MCLK1 => MCLK1,
          RS_TX => RS_TX,
          RS_RX => RS_RX,
          XRST => XRST,
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
          ZCLKMA => ZCLKMA
        );
	sram : sram_sim
	port map(
		clk => MCLK1,
		za => ZA,
		zd => ZD,
		xwa => XWA
	);
	trans : transif
	generic map(w => x"1ADB")
	port map(
		tx => RS_RX,
		clk => MCLK1,
		rst => '0',
		wr_en => wr_en,
		din => din,
		full => t_full
	);
	recv : recvif
	generic map(w => x"1ADB")
	port map(
		rd_en => rd_en,
		dout => dout,
		full => r_full,
		empty => empty,
		clk => MCLK1,
		rst => '0',
		rx => RS_TX
	);
   -- Clock process definitions
   MCLK1_process :process
   begin
		MCLK1 <= '0';
		wait for MCLK1_period/2;
		MCLK1 <= '1';
		wait for MCLK1_period/2;
   end process;
 

   -- Stimulus process
   stim_proc: process
   begin		
      -- hold reset state for 100 ns.
      wait for 100 ns;	

      wait for MCLK1_period*10;

      -- insert stimulus here 
		wr_en <= '1';
		din <= x"ca";
		wait for MCLK1_period;
		wr_en <= '0';

      wait;
   end process;

END;
