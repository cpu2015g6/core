--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:   13:56:01 10/20/2015
-- Design Name:   
-- Module Name:   cpu_top_tb.vhd
-- Project Name:  cpu1
-- Target Device:  
-- Tool versions:  
-- Description:   
-- 
-- VHDL Test Bench Created by ISE for module: cpu_top
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
 
ENTITY cpu_top_tb IS
END cpu_top_tb;
 
ARCHITECTURE behavior OF cpu_top_tb IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT cpu_top
    PORT(
         clk : IN  std_logic;
         rst : IN  std_logic;
		cpu_top_in : in cpu_top_in_type;
		cpu_top_out : out cpu_top_out_type
        );
    END COMPONENT;
    

   --Inputs
   signal clk : std_logic := '0';
   signal rst : std_logic := '0';
   signal cpu_top_in : cpu_top_in_type := (
   	sramifout => sramif_out_zero,
   	transifout => transif_out_zero,
   	recvifout => recvif_out_zero
   );
   --Outputs
   signal cpu_top_out : cpu_top_out_type;

   -- Clock period definitions
   constant clk_period : time := 10 ns;
 
BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
   uut: cpu_top PORT MAP (
          clk => clk,
          rst => rst,
		  cpu_top_in => cpu_top_in,
		  cpu_top_out => cpu_top_out
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
      -- hold reset state for 100 ns.
      wait for 100 ns;	

      wait for clk_period*10;

      -- insert stimulus here 

      wait;
   end process;

END;
