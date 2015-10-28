library ieee;
use ieee.std_logic_1164.all;
use work.common.all;

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
			clk, rst : in std_logic;
			cpu_top_in : in cpu_top_in_type;
			cpu_top_out : out cpu_top_out_type
		);
	end component;
	component sramif is
		generic(sim : boolean := false);
		port(
			clk, rst : in std_logic;
			ZD : inout std_logic_vector(31 downto 0);
			ZA : out std_logic_vector(19 downto 0);
			XWA, XE1, E2A, XE3 : out std_logic;
			XGA, XZCKE, ADVA, XLBO, ZZA, XFT : out std_logic;
			XZBE : out std_logic_vector(3 downto 0);
			ZCLKMA : out std_logic_vector(1 downto 0);
			sramifin : in sramif_in;
			sramifout : out sramif_out
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
	signal cpu_top_in : cpu_top_in_type;
	signal cpu_top_out : cpu_top_out_type;
begin
	cpu_l : cpu_top
	port map(
		clk => MCLK1,
		rst => not XRST,
		cpu_top_in => cpu_top_in,
		cpu_top_out => cpu_top_out
	);
	sramif_l : sramif
	port map(
		clk => MCLK1,
		rst => not XRST,
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
		sramifin => cpu_top_out.sramifin,
		sramifout => cpu_top_in.sramifout
	);
	trans : transif
	generic map(w => x"1ADB")
	port map(
		tx => RS_TX,
		clk => MCLK1,
		rst => not XRST,
		wr_en => cpu_top_out.transifin.wr_en,
		din => cpu_top_out.transifin.din,
		full => cpu_top_in.transifout.full
	);
	recv : recvif
	generic map(w => x"1ADB")
	port map(
		rd_en => cpu_top_out.recvifin.rd_en,
		dout => cpu_top_in.recvifout.dout,
		full => cpu_top_in.recvifout.full,
		empty => cpu_top_in.recvifout.empty,
		clk => MCLK1,
		rst => not XRST,
		rx => RS_RX
	);
end;
