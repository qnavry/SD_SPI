library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity sd_controller is

Generic 
		( 	
			Div : natural := 62;							-- dzielnik zegara 50 MHz / 250 -> 400 kHz
			Div2 : natural := 1							-- dzielnik zegara 50 MHz / 250 -> 400 kHz
			
		 );

port (
	cs : out std_logic;
	mosi : out std_logic;
	miso : in std_logic;
	sclk : out std_logic;
	
	--seg : out std_logic;
	--an : out std_logic;
	test_tick_button : out std_logic;
	--level_button : out std_logic;
	--level_button : out std_logic;

	rd : in std_logic;
	wr : in std_logic;
	dm_in : in std_logic;	-- data mode, 0 = write continuously, 1 = write single block
	reset : in std_logic;
	din : in std_logic_vector(7 downto 0);
	dout : out std_logic_vector(7 downto 0);
	clk : in std_logic;	-- twice the SPI clk
	
	
	  zapis_fifo: out std_logic;
	  
	  clk1600: out std_logic;
	  clk400: out std_logic;
	  

	
		btn: in std_logic_vector(2 downto 0);
		wysw2_btn: in std_logic;
      rx: in std_logic;
      tx: out std_logic;
      led: out std_logic_vector(7 downto 0);
		
		an: out std_logic_vector(5 downto 0);  -- cyfra wyœwietlacza
      sseg: out std_logic_vector(7 downto 0) -- 
	
);

end sd_controller;

architecture rtl of sd_controller is
type states is (
	RST,
	INIT,
	CMD0,
	CMD58,
	CMD41,
	POLL_CMD,
	
	WAIT_BUTTON,
	WAIT_SEND,
	R3_0,
	R3_1,
	R3_2,
	R3_3,
	R3_4,
	R3_11,
	R3_22,
	R3_33,
	R3_44,
  
	IDLE,	-- wait for read or write pulse
	READ_BLOCK,
	READ_BLOCK_WAIT,
	READ_BLOCK_DATA,
	READ_BLOCK_CRC,
	SEND_CMD,
	RECEIVE_BYTE_WAIT,
	RECEIVE_BYTE,
	WRITE_BLOCK_CMD,
	WRITE_BLOCK_INIT,		-- initialise write command
	WRITE_BLOCK_DATA,		-- loop through all data bytes
	WRITE_BLOCK_BYTE,		-- send one byte
	WRITE_BLOCK_WAIT		-- wait until not busy
);


-- one start byte, plus 512 bytes of data, plus two FF end bytes (CRC)
constant WRITE_DATA_SIZE : integer := 515;

signal led5, led4, led3, led2, led1, led0: std_logic_vector(7 downto 0);

signal state, return_state : states;
signal sclk_sig : std_logic := '0';
signal cmd_out : std_logic_vector(55 downto 0);
signal recv_data : std_logic_vector(39 downto 0);
signal address : std_logic_vector(31 downto 0);
signal cmd_mode : std_logic := '1';
signal data_mode : std_logic := '1';
signal response_mode : std_logic_vector (1 downto 0) := "01";
signal data_sig : std_logic_vector(7 downto 0) := x"00";


signal to_fifo : std_logic_vector(7 downto 0) := x"00";


signal cnt: std_logic_vector (29 downto 0) := (others=>'0');
signal cnt2: std_logic_vector (29 downto 0) := (others=>'0');
signal clk1600kHz: std_logic :='0';
signal clk400kHz: std_logic :='0';

signal count : unsigned(9 downto 0); --will count from 0 to 2^10-1


	signal tx_full, rx_empty: std_logic;
   signal rec_data,rec_data1: std_logic_vector(7 downto 0);
   signal odpowiedzR3: std_logic_vector(39 downto 0);
   signal odpowiedzR1: std_logic_vector(7 downto 0);
   signal btn_tick: std_logic;
   signal write_uart_fifo_tx: std_logic :='0';
   signal wrr: std_logic :='0';
   signal button: std_logic :='1';
   signal level_button: std_logic :='1';
	signal odpowiedz : std_logic_vector (39 downto 0);
	signal dp_v : std_logic_vector (5 downto 0);
	
begin
  
  button <=not btn(0);
  --an <= '1';
  --seg <= level_button;
  test_tick_button <= btn_tick;
	
	--odpowiedz<=x"000000abcd";
	
   sseg_unit_0: entity work.hex_to_sseg
      port map(hex=>odpowiedz(39 downto 36), dp =>dp_v(0), sseg=>led0);
   -- instance for 4 MSBs of input
   sseg_unit_1: entity work.hex_to_sseg
      port map(hex=>odpowiedz(35 downto 32), dp =>dp_v(1), sseg=>led1);
   -- instance for 4 LSBs of incremented value
   sseg_unit_2: entity work.hex_to_sseg
      port map(hex=>odpowiedz(31 downto 28), dp =>dp_v(2), sseg=>led2);
   -- instance for 4 MSBs of incremented value
   sseg_unit_3: entity work.hex_to_sseg
      port map(hex=>odpowiedz(15 downto 12), dp =>dp_v(3), sseg=>led3);
	-- instance for 4 MSBs of incremented x2 value
   sseg_unit_4: entity work.hex_to_sseg
      port map(hex=>odpowiedz(7 downto 4), dp =>dp_v(4), sseg=>led4);
	-- instance for 4 MSBs of incremented x2 value
   sseg_unit_5: entity work.hex_to_sseg
      port map(hex=>odpowiedz(3 downto 0), dp =>dp_v(5), sseg=>led5);

   -- instantiate 7-seg LED display time-multiplexing module
   disp_unit: entity work.disp_mux
      port map(
         clk=>clk, reset=>'0',
         in0=>led0, in1=>led1, in2=>led2, in3=>led3, in4=>led4, in5=>led5,
         an=>an, sseg=>sseg);
  
   uart_unit: entity work.uart(str_arch)
      port map(clk=>clk, reset=>not reset, rd_uart=>btn_tick,
               wr_uart=> write_uart_fifo_tx,  --  impuls do zapisania wartoœci do w_data z rec_data1
               --wr_uart=> wrr,  --  impuls do zapisania wartoœci do w_data z rec_data1
					rx=>rx, 
					--w_data=> odpowiedz(7 downto 0), -- w_data -> dane wpisywane do kolejki FIFO_tx
					w_data=> to_fifo, -- w_data -> dane wpisywane do kolejki FIFO_tx
               tx_full=>tx_full, rx_empty=>rx_empty,
               r_data=>rec_data, tx=>tx);
  
   btn_db_unit: entity work.debounce(fsmd_arch)
      port map(clk=>clk400kHz, reset=> (not reset), sw=> button,
               db_level=>level_button, db_tick=>btn_tick);
  
  
  clock_divider: process (clk)
	begin
		if (rising_edge(clk)) then
			if (cnt < Div) then 
				cnt <= cnt+1;
			else
				cnt <= (others=>'0');
				clk400kHz <= not clk400kHz;
			end if;
		end if;
	end process clock_divider;
	--clk1600<=clk1600kHz;
	
--	clock_divider2: process (clk1600kHz)
--	begin
--		if (rising_edge(clk1600kHz)) then
--			if (cnt2 < Div2) then 
--				cnt2 <= cnt2+1;
--			else
--				cnt2 <= (others=>'0');
--				clk400kHz <= not clk400kHz;
--			end if;
--		end if;
--	end process clock_divider2;
		clk400<=clk400kHz;


  
	process(clk400kHz ,reset)
		variable byte_counter : integer range 0 to WRITE_DATA_SIZE;
		variable bit_counter : integer range 0 to 160;
		
	begin
		data_mode <= dm_in;

		if rising_edge(clk400kHz) then
			
			 count <= count + 1;

			
			if (reset='0') then
				state <= RST;
				sclk_sig <= '0';
			else
				case state is
				
				when RST =>
					sclk_sig <= '0';
					cmd_out <= (others => '1');
					address <= x"00000000";
					byte_counter := 0;
					cmd_mode <= '1'; -- 0=data, 1=command
					response_mode <= "01";	-- 0=data, 1=command
					bit_counter := 160;
					cs <= '1';
					state <= INIT;
										write_uart_fifo_tx <='0';

				
				when INIT =>		-- CS=1, send 80 clocks, CS=0
					if (bit_counter = 0) then
						cs <= '0';
						state <= CMD0;
					else
						bit_counter := bit_counter - 1;
						sclk_sig <= not sclk_sig;
					end if;
					write_uart_fifo_tx <='0';
					
				
				when CMD0 =>
					write_uart_fifo_tx <='0';
					cmd_out <= x"FF400000000095";
					bit_counter := 55;
					return_state <= CMD58;
					response_mode<="01";
					state <= SEND_CMD;

				when CMD58 =>
					write_uart_fifo_tx <='0';
					cmd_out <= x"FF7A0000000001";	-- 58d OR 40h = 7Ah
					bit_counter := 55;
					return_state <= CMD41;
					response_mode<="10";
					state <= SEND_CMD;
				
				when CMD41 =>
					write_uart_fifo_tx <='0';
					cmd_out <= x"FF690000000001";	-- 41d OR 40h = 69h
					bit_counter := 55;
					return_state <= POLL_CMD;
					response_mode<="01";
					state <= SEND_CMD;
			
				when POLL_CMD =>
					write_uart_fifo_tx <='0';
					if (recv_data(0) = '0') then
						state <= IDLE;
					else
						state <= CMD58; --CMD55
					end if;
        	
				when IDLE => 
					if (rd = '1') then
						state <= READ_BLOCK;
					elsif (wr='1') then
						state <= WRITE_BLOCK_CMD;
					else
						state <= IDLE;
					end if;
				
				when READ_BLOCK =>
					cmd_out <= x"FF" & x"51" & address & x"FF";
					bit_counter := 55;
					return_state <= READ_BLOCK_WAIT;
					state <= SEND_CMD;
				
				when READ_BLOCK_WAIT =>
					if (sclk_sig='1' and miso='0') then
						state <= READ_BLOCK_DATA;
						byte_counter := 511;
						bit_counter := 7;
						return_state <= READ_BLOCK_DATA;
						state <= RECEIVE_BYTE;
					end if;
					sclk_sig <= not sclk_sig;

				when READ_BLOCK_DATA =>
					if (byte_counter = 0) then
						bit_counter := 7;
						return_state <= READ_BLOCK_CRC;
						state <= RECEIVE_BYTE;
					else
						byte_counter := byte_counter - 1;
						return_state <= READ_BLOCK_DATA;
						bit_counter := 7;
						state <= RECEIVE_BYTE;
					end if;
			
				when READ_BLOCK_CRC =>
					bit_counter := 7;
					return_state <= IDLE;
					address <= std_logic_vector(unsigned(address) + x"200");
					state <= RECEIVE_BYTE;
        	
				when SEND_CMD =>
					if (sclk_sig = '1') then
						if (bit_counter = 0) then
							state <= RECEIVE_BYTE_WAIT;
						else
							bit_counter := bit_counter - 1;
							cmd_out <= cmd_out(54 downto 0) & '1';
						end if;
					end if;
					sclk_sig <= not sclk_sig;
				
				when RECEIVE_BYTE_WAIT =>
					if (sclk_sig = '1') then
						if (miso = '0') then
							recv_data <= (others => '0');
							if (response_mode="00") then
								bit_counter := 3; -- already read bits 7..4
							elsif (response_mode="01") then
								bit_counter := 6; -- already read bit 7
							elsif (response_mode="10") then
								bit_counter := 38; -- already read bit 39
							end if;
							state <= RECEIVE_BYTE;
						end if;
					end if;
					sclk_sig <= not sclk_sig;

				when RECEIVE_BYTE =>
					if (sclk_sig = '1') then
					
						
						--recv_data <= recv_data(6 downto 0) & miso;
						recv_data <= recv_data(38 downto 0) & miso;
						if (bit_counter = 0) then
							
							if (response_mode="00") then							
							--dout <= recv_data(6 downto 0) & miso;
								odpowiedzR1 <= recv_data(6 downto 0) & miso;
							elsif (response_mode="01") then
								odpowiedzR1 <= recv_data(6 downto 0) & miso;
							elsif (response_mode="10") then
								odpowiedzR3 <= recv_data(38 downto 0) & miso; -------------------------------------------------------
							end if;
								
							if(tx_full = '0') then
								rec_data1 <= std_logic_vector(unsigned(recv_data(7 downto 0)));
								--write_uart_fifo_tx <='1';
							end if;
							
							--led <= recv_data (15 downto 8);
							state <= WAIT_SEND;
						else
							bit_counter := bit_counter - 1;
							write_uart_fifo_tx <='0';
						end if;
					end if;
					sclk_sig <= not sclk_sig;
					
				when WAIT_SEND =>
					
					
						if (response_mode="01") then
							odpowiedz (7 downto 0) <= odpowiedzR1;
							odpowiedz (39 downto 8) <= x"00000000";
							dp_v <= "001111";
							
							to_fifo<=odpowiedzR1;
							write_uart_fifo_tx <='1';
							
							state <= WAIT_BUTTON;
							
							led <= "00000001";
						elsif (response_mode="10") then
							odpowiedz <= odpowiedzR3;
							dp_v <= "000000";
							
							to_fifo<=odpowiedzR3(7 downto 0);
							write_uart_fifo_tx <='1';
							state <= R3_0;
							led <= "00000011";

						end if;
					

						
					--led <= odpowiedz (15 downto 8);
				when R3_0 =>
						write_uart_fifo_tx <='0';
						--state <= R3_1;
						led <= "00000111";
						
						if count = 0 then
							state <= R3_1;
						else 
							state <= R3_0;
						end if;

				when R3_1 =>
						to_fifo<=odpowiedzR3(15 downto 8);
						write_uart_fifo_tx <='1';
						--state <= R3_11;
													led <= "00001111";
						if count = 0 then
							state <= R3_11;
						else 
							state <= R3_1;
						end if;

				when R3_11 =>
						write_uart_fifo_tx <='0';
						state <= R3_2;
													led <= "00011111";
						if count = 0 then
							state <= R3_2;
						else 
							state <= R3_11;
						end if;

				when R3_2 =>
						to_fifo<=odpowiedzR3(23 downto 16);
						write_uart_fifo_tx <='1';
						state <= R3_22;
					led <= "00111111";
					if count = 0 then
							state <= R3_22;
						else 
							state <= R3_2;
						end if;
				when R3_22 =>
						write_uart_fifo_tx <='0';
						state <= R3_3;
						led <= "01111111";
						if count = 0 then
							state <= R3_3;
						else 
							state <= R3_22;
						end if;
				when R3_3 =>
						to_fifo<=odpowiedzR3(31 downto 24);
						write_uart_fifo_tx <='1';
						state <= R3_33;
						if count = 0 then
							state <= R3_33;
						else 
							state <= R3_3;
						end if;
				when R3_33 =>
						write_uart_fifo_tx <='0';
						state <= R3_4;
						if count = 0 then
							state <= R3_4;
						else 
							state <= R3_33;
						end if;
				when R3_4 =>
						to_fifo<=odpowiedzR3(39 downto 32);
						write_uart_fifo_tx <='1';
						state <= WAIT_BUTTON;
						
					
				
				when WAIT_BUTTON =>
					write_uart_fifo_tx <='0';

					if (btn_tick = '1') then
						state <= return_state;
					else
						state <= WAIT_BUTTON;
					end if;
				
				when WRITE_BLOCK_CMD =>
					cmd_mode <= '1';
					if (data_mode = '0') then
						cmd_out <= x"FF" & x"59" & address & x"FF";	-- continuous
					else
						cmd_out <= x"FF" & x"58" & address & x"FF";	-- single block
					end if;
					bit_counter := 55;
					return_state <= WRITE_BLOCK_INIT;
					state <= SEND_CMD;
					
				when WRITE_BLOCK_INIT => 
					cmd_mode <= '0';
					byte_counter := WRITE_DATA_SIZE; 
					state <= WRITE_BLOCK_DATA;
					
				when WRITE_BLOCK_DATA => 
					if byte_counter = 0 then
						state <= RECEIVE_BYTE_WAIT;
						return_state <= WRITE_BLOCK_WAIT;
						response_mode <= "00";
					else 	
						if ((byte_counter = 2) or (byte_counter = 1)) then
							data_sig <= x"FF"; -- two CRC bytes
						elsif byte_counter = WRITE_DATA_SIZE then
							if (data_mode='0') then
								data_sig <= x"FC"; -- start byte, multiple blocks
							else
								data_sig <= x"FE"; -- start byte, single block
							end if;
						else
							-- just a counter, get real data here
							data_sig <= std_logic_vector(to_unsigned(byte_counter,8));
						end if;
						bit_counter := 7;
						state <= WRITE_BLOCK_BYTE;
						byte_counter := byte_counter - 1;
					end if;
				
				when WRITE_BLOCK_BYTE => 
					if (sclk_sig = '1') then
						if bit_counter=0 then
							state <= WRITE_BLOCK_DATA;
						else
							data_sig <= data_sig(6 downto 0) & '1';
							bit_counter := bit_counter - 1;
						end if;
					end if;
					sclk_sig <= not sclk_sig;
					
				when WRITE_BLOCK_WAIT =>
					response_mode <= "01";
					if sclk_sig='1' then
						if MISO='1' then
							if (data_mode='0') then
								state <= WRITE_BLOCK_INIT;
							else
								address <= std_logic_vector(unsigned(address) + x"200");
								state <= IDLE;
							end if;
						end if;
					end if;
					sclk_sig <= not sclk_sig;

				when others => state <= IDLE;
        end case;
      end if;
    end if;
  end process; 
 
  sclk <= sclk_sig;
  mosi <= cmd_out(55) when cmd_mode='1' else data_sig(7);
  
  dout <= odpowiedz (7 downto 0);
  
  zapis_fifo<=write_uart_fifo_tx;
  
--  process (clk1600kHz,write_uart_fifo_tx)
--  
--  begin
--		if (rising_edge(clk1600kHz)) then
--			if(rising_edge(write_uart_fifo_tx)) then
--				wrr<='1';
--			else
--				wrr<='0';
--			end if;
--		end if;
--					
--	
--  end process;
  
  
--  zapis_do_fifo : process (clk, wyslij, response_mode, odpowiedzR1, odpowiedzR3)
--	variable poczekaj : integer range 0 to 160;
-- 
--  begin
--  
--		if (wyslij = '1') then
--			poczekaj := 160;
--			if (response_mode="01") then
--			
--					to_fifo<=odpowiedzR1;
--					write_uart_fifo_tx <='1';
--			
--			elsif (response_mode="10") then
--					
--					if (poczekaj = 150) then 
--						to_fifo<=odpowiedzR3(7 downto 0);
--						write_uart_fifo_tx <='1';
--					elsif (poczekaj = 120) then 
--						to_fifo<=odpowiedzR3(15 downto 8);
--						write_uart_fifo_tx <='1';
--					elsif (poczekaj = 90) then 
--						to_fifo<=odpowiedzR3(23 downto 16);
--						write_uart_fifo_tx <='1';
--					elsif (poczekaj = 60) then 
--						to_fifo<=odpowiedzR3(31 downto 24);
--						write_uart_fifo_tx <='1';
--					elsif (poczekaj = 30) then 
--						to_fifo<=odpowiedzR3(39 downto 32);
--						write_uart_fifo_tx <='1';
--					else
--						write_uart_fifo_tx <='0';
--					end if;
--				
--			end if;
--			
--			poczekaj:= poczekaj - 1;
--
--		end if;
--	
--  end process;
  
end rtl;

