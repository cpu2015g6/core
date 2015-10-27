library ieee;
use ieee.std_logic_1164.all;
use work.common.all;

entity sramif is
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
end sramif;

architecture beh of sramif is
	type cmd_type is record
		op : sramif_op;
		data : std_logic_vector(31 downto 0);
	end record;
	constant cmd_zero : cmd_type := (
		SRAM_NOP,
		(others => '0')
	);
	type cmd_hist is array(0 to 3) of cmd_type;
	type reg_type is record
		out_enable, out_enable1 : std_logic;
		store : std_logic;
		nop : std_logic;
		store_data : std_logic_vector(31 downto 0);
		hist : cmd_hist;
	end record;
	constant rzero : reg_type := (
		'0', '0',
		'0',
		'0',
		(others => '0'),
		(others => cmd_zero)
	);
	signal r, r_in : reg_type := rzero;
	signal zd_obuf : std_logic_vector(31 downto 0) := (others => '0');
	signal zd0 : std_logic_vector(31 downto 0) := (others => '0');
begin
	XE1 <= '0';
	E2A <= '1';
	XE3 <= '0';
	XGA <= r.nop;
	--XGA <= '0';
	XZCKE <= '0';
	ADVA <= '0';
	XLBO <= '1';
	ZZA <= '0';
	XFT <= '1';
	XZBE <= "0000";
	ZCLKMA <= (1 downto 0 => clk);
	ZA <= sramifin.addr;
	sramifout.data_enable <= r.out_enable;
	sramifout.rd <= ZD when r.out_enable = '1' else (others => '0');
	zd0 <= r.store_data when r.store = '1' else (others => 'Z');
	ZD <= zd_obuf when sim else zd0;
	XWA <= '0' when sramifin.op = SRAM_STORE else '1';
	process(r, sramifin)
		variable v : reg_type;
		variable nextcmd : cmd_type;
	begin
		v := r;
		v.hist(1 to 3) := r.hist(0 to 2);
		v.hist(0).op := sramifin.op;
		v.hist(0).data := sramifin.wd;
		nextcmd := r.hist(0);
		case nextcmd.op is
			when SRAM_NOP =>
				v.nop := '1';
				v.out_enable1 := '0';
				v.store := '0';
			when SRAM_LOAD =>
				v.nop := '0';
				v.out_enable1 := '1';
				v.store := '0';
			when SRAM_STORE =>
				v.nop := '0';
				v.out_enable1 := '0';
				v.store := '1';
				v.store_data := nextcmd.data;
		end case;
		v.out_enable := r.out_enable1;
		r_in <= v;
	end process;
	process(clk, rst)
	begin
		if rst = '1' then
			r <= rzero;
		elsif rising_edge(clk) then
			if sim then
				zd_obuf <= zd0;
			end if;
			r <= r_in;
		end if;
	end process;
end;
