library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity receiver is
	generic(w : std_logic_vector(15 downto 0));
	port(
		data : out std_logic_vector(7 downto 0);
		de : out std_logic := '0';
		clk, rst : in std_logic;
		rx : in std_logic);
end receiver;

architecture beh of receiver is
	signal buf : std_logic_vector(8 downto 0) := (others => '0');
	signal state : std_logic_vector(3 downto 0) := (others => '0');
	signal countdown : std_logic_vector(15 downto 0) := (others => '0');
	signal rx0 : std_logic := '1';
begin
	data <= buf(7 downto 0);
	process(clk, rst)
	begin
		if rst = '1' then
			de <= '0';
			buf <= (others => '0');
			state <= "0000";
			countdown <= (others => '0');
		elsif rising_edge(clk) then
			rx0 <= rx;
			if state = "0000" then
				de <= '0';
				if rx0 = '0' then
					state <= "0001";
					countdown <= '0' & w(15 downto 1);
				end if;
			else
				if countdown = x"0000" then
					countdown <= w;
					buf <= rx0 & buf(8 downto 1);
					if state = "1010" then
						state <= "0000";
						de <= '1';
					else
						state <= std_logic_vector(unsigned(state) + 1);
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

entity recvif is
	generic(w : std_logic_vector(15 downto 0));
	port(
      clk, rst : in  std_logic;
      rd_en : in  std_logic;
	  rx : in std_logic;
      dout : out std_logic_vector(7 downto 0);
      full : out std_logic;
      empty : out std_logic
	);
end recvif;

architecture beh of recvif is
component receiver is
	generic(w : std_logic_vector(15 downto 0));
	port(
		data : out std_logic_vector(7 downto 0);
		de : out std_logic := '0';
		clk, rst : in std_logic;
		rx : in std_logic
	);
end component;
-- FIFO
-- First-Word Fall-Through FIFO
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
	signal data : std_logic_vector(7 downto 0) := (others => '0');
	signal wr_en : std_logic := '0';
begin
	f8 : fifo8
	port map(
		clk => clk,
		rst => rst,
		wr_en => wr_en,
		rd_en => rd_en,
		din => data,
		dout => dout,
		full => full,
		empty => empty
	);
	r : receiver
	generic map(w => w)
	port map(
		clk => clk,
		rst => rst,
		rx => rx,
		data => data,
		de => wr_en
	);
end;
