library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common.all;

entity program_loader is
	port(
		clk, rst : in std_logic;
		go : in std_logic;
		active : out std_logic;
		bram_we : out std_logic;
		bram_addr : out std_logic_vector(pc_width-1 downto 0);
		bram_din : out std_logic_vector(31 downto 0);
		recvifout : in recvif_out_type;
		recvifin : out recvif_in_type;
		sramifin : out sramif_in
	);
end program_loader;

architecture twoproc of program_loader is
	type state_type is (LOAD_HEADER_B, LOAD_HEADER_S, LOAD_BRAM, LOAD_SRAM, IDLE);
	type reg_type is record
		bram_size, sram_size : std_logic_vector(31 downto 0);
		addr : std_logic_vector(31 downto 0);
		state : state_type;
		write_go : std_logic;
		buf : std_logic_vector(31 downto 0);
		read_count : std_logic_vector(1 downto 0);
	end record;
	constant rzero : reg_type := (
		(others => '0'), (others => '0'),
		(others => '0'),
		IDLE,
		'0',
		(others => '0'),
		(others => '0')
	);
	signal r, r_in : reg_type := rzero;
	constant sram_head_addr : std_logic_vector(19 downto 0) := x"00000";
begin
	active <= '0' when r.state = IDLE else '1';
	process(clk, rst)
	begin
		if rst = '1' then
			r <= rzero;
		elsif rising_edge(clk) then
			r <= r_in;
		end if;
	end process;
	process(r, recvifout, go)
		variable v : reg_type;
		variable bram_we_v : std_logic;
		variable bram_addr_v : std_logic_vector(pc_width-1 downto 0);
		variable bram_din_v : std_logic_vector(31 downto 0);
		variable sramifin_v : sramif_in;
		variable rd_en : std_logic;
	begin
		v := r;
		bram_we_v := '0';
		bram_addr_v := (others => '0');
		bram_din_v := (others => '0');
		sramifin_v := sramif_in_zero;
		rd_en := '0';
		if r.state /= IDLE then
			if recvifout.empty = '0' then
				rd_en := '1';
				v.buf := r.buf(23 downto 0) & recvifout.dout;
				v.read_count := std_logic_vector(unsigned(r.read_count) + 1);
				if v.read_count = "00" then
					v.write_go := '1';
				end if;
			else
				rd_en := '0';
			end if;
		end if;
		recvifin <= (rd_en => rd_en);
		case r.state is
		when IDLE =>
			if go = '1' then
				v.state := LOAD_HEADER_B;
			end if;
		when LOAD_HEADER_B =>
			if r.write_go = '1' then
				v.write_go := '0';
				v.bram_size := v.buf;
				v.state := LOAD_HEADER_S;
			end if;
		when LOAD_HEADER_S =>
			if r.write_go = '1' then
				v.write_go := '0';
				v.sram_size := v.buf;
				v.state := LOAD_BRAM;
				v.addr := (others => '0');
			end if;
		when LOAD_BRAM =>
			if r.write_go = '1' then
				v.write_go := '0';
				bram_we_v := '1';
				bram_addr_v := r.addr(pc_width-1 downto 0);
				v.addr := std_logic_vector(unsigned(r.addr) + 1);
				bram_din_v := v.buf;
			end if;
			if r.bram_size = v.addr then
				v.state := LOAD_SRAM;
				v.addr := (others => '0');
			end if;
		when LOAD_SRAM =>
			if r.write_go = '1' then
				v.write_go := '0';
				sramifin_v := (
					op => SRAM_STORE,
					addr => std_logic_vector(unsigned(sram_head_addr) + unsigned(r.addr(19 downto 0))),
					wd => v.buf
				);
				v.addr := std_logic_vector(unsigned(r.addr) + 1);
			end if;
			if r.sram_size = v.addr then
				v.state := IDLE;
				v.addr := (others => '0');
			end if;
		end case;
		bram_we <= bram_we_v;
		bram_addr <= bram_addr_v;
		bram_din <= bram_din_v;
		sramifin <= sramifin_v;
		r_in <= v;
	end process;
end;
