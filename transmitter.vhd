library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity transmitter is
	generic(w : std_logic_vector(15 downto 0));
	port(
		tx : out std_logic;
		go : in std_logic;
		clk, rst : in std_logic;
		data : in std_logic_vector(7 downto 0);
		full : out std_logic := '0');
end transmitter;

architecture beh of transmitter is
	signal state : std_logic_vector(3 downto 0) := (others => '0');
	signal buf : std_logic_vector(8 downto 0) := (0 => '1', others => '0');
	signal countdown : std_logic_vector(15 downto 0) := (others => '0');
begin
	tx <= buf(0);
	process(clk, rst) begin
		if rst = '1' then
			state <= (others => '0');
			buf <= (0 => '1', others => '0');
			countdown <= (others => '0');
		elsif rising_edge(clk) then
			if state = "0000" then
				if go = '1' then
					state <= "0001";
					countdown <= w;
					buf <= data & '0';
					full <= '1';
				end if;
			else
				if countdown = x"0000" then
					buf <= '1' & buf(8 downto 1);
					if state = "1010" then
						state <= "0000";
						full <= '0';
					else
						state <= std_logic_vector(unsigned(state) + 1);
						countdown <= w;
					end if;
				else
					countdown <= std_logic_vector(unsigned(countdown) - 1);
				end if;
			end if;
		end if;
	end process;
end;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity transif is
	generic(w : std_logic_vector(15 downto 0));
	port(
		clk, rst : in std_logic;
		wr_en : in std_logic;
		tx : out std_logic;
		din : in std_logic_vector(7 downto 0);
		full : out std_logic
	);
end entity;

architecture beh of transif is
-- FIFO
-- Standard FIFO
-- width: 8
  component fifo8
    port (
      clk   : in  std_logic;
      rst   : in  std_logic;
      wr_en : in  std_logic;
      rd_en : in  std_logic;
      din   : in  std_logic_vector(7 downto 0);
      dout  : out std_logic_vector(7 downto 0);
      full  : out std_logic;
      empty : out std_logic);
  end component;
	component transmitter is
		generic(w : std_logic_vector(15 downto 0));
		port(
			tx : out std_logic;
			go : in std_logic;
			clk, rst : in std_logic;
			data : in std_logic_vector(7 downto 0);
			full : out std_logic := '0');
	end component;
	type state_type is (WaitState, ReadFifoState, TransmitState);
	signal state : state_type := WaitState;
	signal rd_en : std_logic := '0';
	signal empty : std_logic := '1';
	signal dout : std_logic_vector(7 downto 0) := (others => '0');
	signal go : std_logic := '0';
	signal t_full : std_logic := '0';
begin
	f8 : fifo8
	port map(
		clk => clk,
		rst => rst,
		wr_en => wr_en,
		rd_en => rd_en,
		din => din,
		dout => dout,
		full => full,
		empty => empty
	);
	t : transmitter
	generic map(w => w)
	port map(
		tx => tx,
		go => go,
		clk => clk,
		rst => rst,
		data => dout,
		full => t_full
	);
	process(clk)
	begin
		if rising_edge(clk) then
			case state is
			when WaitState =>
				if empty = '0' then
					rd_en <= '1';
					state <= ReadFifoState;
				end if;
			when ReadFifoState =>
				rd_en <= '0';
				go <= '1';
				state <= TransmitState;
			when TransmitState =>
				if go = '1' then
					go <= '0';
				elsif t_full = '0' then
					state <= WaitState;
				end if;
			end case;
		end if;
	end process;
end;
