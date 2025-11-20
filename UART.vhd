
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity uart is
generic (
    CLK_FREQ    : integer := 40;        -- Main frequency (MHz)
    SER_FREQ    : integer := 110        -- Baud rate (bps)
);
port (
    -- Control
    clk         : in    std_logic;      -- Main clock
    rst         : in    std_logic;      -- Main reset

    -- External Interface
    rx          : in    std_logic;      -- RS232 received serial data
    tx          : out   std_logic;      -- RS232 transmitted serial data

    -- RS232/UART Configuration
    par_en      : in    std_logic;      -- Parity bit enable

    -- uPC Interface
    tx_req      : in    std_logic;                      -- Request SEND of data
    tx_end      : out   std_logic;                      -- Data SENDED
    tx_data     : in    std_logic_vector(7 downto 0);   -- Data to transmit
    rx_ready    : out   std_logic;                      -- Received data ready to uPC read
    rx_data     : out   std_logic_vector(7 downto 0)    -- Received data 
);
end uart;

architecture Behavioral of uart is

    -- Constants
    constant UART_IDLE  :   std_logic := '1';
    constant UART_START :   std_logic := '0';
    constant PARITY_EN  :   std_logic := '1';
    constant RST_LVL    :   std_logic := '0';

    -- Types
    type state is (idle,data,parity,stop1,stop2);

    -- RX Signals
    signal rx_fsm       :   state;
    signal rx_clk_en    :   std_logic;
    signal rx_rcv_init  :   std_logic;
    signal rx_par_bit   :   std_logic;
    signal rx_data_deb  :   std_logic;
    signal rx_data_tmp  :   std_logic_vector(7 downto 0);
    signal rx_data_cnt  :   std_logic_vector(2 downto 0);

    -- TX Signals
    signal tx_fsm       :   state;
    signal tx_clk_en    :   std_logic;
    signal tx_par_bit   :   std_logic;
    signal tx_data_tmp  :   std_logic_vector(7 downto 0);
    signal tx_data_cnt  :   std_logic_vector(2 downto 0);

begin

    -------------------------------------------------------------------------
    -- TX CLOCK GENERATOR
    -------------------------------------------------------------------------
    tx_clk_gen:process(clk)
        variable counter    :   integer range 0 to conv_integer((CLK_FREQ*1_000_000)/SER_FREQ-1);
    begin
        if rising_edge(clk) then
            if counter = (CLK_FREQ*1_000_000)/SER_FREQ-1 then
                tx_clk_en <= '1';
                counter   := 0;
            else
                tx_clk_en <= '0';
                counter   := counter + 1;
            end if;

            if rst = RST_LVL then
                tx_clk_en <= '0';
                counter   := 0;
            end if;
        end if;
    end process;

    -------------------------------------------------------------------------
    -- TX STATE MACHINE
    -------------------------------------------------------------------------
    tx_proc:process(clk)
        variable data_cnt  : std_logic_vector(2 downto 0);
    begin
        if rising_edge(clk) then
            if tx_clk_en = '1' then
                tx_end <= '0';
                tx     <= UART_IDLE;

                case tx_fsm is
                    when idle =>
                        if tx_req = '1' then
                            tx          <= UART_START;
                            tx_data_tmp <= tx_data;
                            tx_fsm      <= data;
                            tx_data_cnt <= (others=>'1');
                            tx_par_bit  <= '0';
                        end if;

                    when data =>
                        tx         <= tx_data_tmp(0);
                        tx_par_bit <= tx_par_bit xor tx_data_tmp(0);

                        if tx_data_cnt = 0 then
                            if par_en = PARITY_EN then
                                tx_fsm <= parity;
                            else
                                tx_fsm <= stop1;
                            end if;
                            tx_data_cnt <= (others=>'1');
                        else
                            tx_data_tmp <= '0' & tx_data_tmp(7 downto 1);
                            tx_data_cnt <= tx_data_cnt - 1;
                        end if;

                    when parity =>
                        tx     <= tx_par_bit;
                        tx_fsm <= stop1;

                    when stop1 =>
                        tx     <= UART_IDLE;
                        tx_fsm <= stop2;

                    when stop2 =>
                        tx_end <= '1';
                        tx     <= UART_IDLE;
                        tx_fsm <= idle;

                    when others => null;
                end case;

                if rst = RST_LVL then
                    tx_fsm      <= idle;
                    tx_par_bit  <= '0';
                    tx_data_tmp <= (others=>'0');
                    tx_data_cnt <= (others=>'0');
                end if;
            end if;
        end if;
    end process;

    -------------------------------------------------------------------------
    -- RX DEBOUNCER
    -------------------------------------------------------------------------
    rx_debounceer:process(clk)
        variable deb_buf : std_logic_vector(3 downto 0);
    begin
        if rising_edge(clk) then
            if deb_buf = "0000" then
                rx_data_deb <= '0';
            elsif deb_buf = "1111" then
                rx_data_deb <= '1';
            end if;

            deb_buf := deb_buf(2 downto 0) & rx;
        end if;
    end process;

    -------------------------------------------------------------------------
    -- RX START DETECT
    -------------------------------------------------------------------------
    rx_start_detect:process(clk)
        variable rx_data_old : std_logic;
    begin
        if rising_edge(clk) then
            if rx_data_old = '1' and rx_data_deb = '0' then
                rx_rcv_init <= '1';
            else
                rx_rcv_init <= '0';
            end if;

            rx_data_old := rx_data_deb;

            if rst = RST_LVL then
                rx_data_old := '0';
                rx_rcv_init <= '0';
            end if;
        end if;
    end process;

    -------------------------------------------------------------------------
    -- RX CLOCK GENERATOR
    -------------------------------------------------------------------------
    rx_clk_gen:process(clk)
        variable counter    : integer range 0 to conv_integer((CLK_FREQ*1_000_000)/SER_FREQ-1);
    begin
        if rising_edge(clk) then
            if counter = (CLK_FREQ*1_000_000)/SER_FREQ-1 or rx_rcv_init = '1' then
                rx_clk_en <= '1';
                counter   := 0;
            else
                rx_clk_en <= '0';
                counter   := counter + 1;
            end if;

            if rst = RST_LVL then
                rx_clk_en <= '0';
                counter   := 0;
            end if;
        end if;
    end process;

    -------------------------------------------------------------------------
    -- RX STATE MACHINE
    -------------------------------------------------------------------------
    rx_proc:process(clk)
    begin
        if rising_edge(clk) then
            rx_ready <= '0';

            if rx_clk_en = '1' then
                case rx_fsm is
                    when idle =>
                        if rx_data_deb = UART_START then
                            rx_fsm <= data;
                        end if;
                        rx_par_bit  <= '0';
                        rx_data_cnt <= (others=>'0');

                    when data =>
                        if par_en = PARITY_EN then
                            rx_par_bit <= rx_par_bit xor rx;
                        end if;

                        if rx_data_cnt = 7 then
                            rx_data(7) <= rx;
                            for i in 0 to 6 loop
                                rx_data(i) <= rx_data_tmp(6-i);
                            end loop;

                            if par_en = PARITY_EN then
                                rx_fsm <= parity;
                            else
                                rx_ready <= '1';
                                rx_fsm   <= idle;
                            end if;
                        else
                            rx_data_tmp <= rx_data_tmp(6 downto 0) & rx;
                            rx_data_cnt <= rx_data_cnt + 1;
                        end if;

                    when parity =>
                        rx_fsm <= idle;
                        if rx_par_bit = rx then
                            rx_ready <= '1';
                        end if;

                    when others =>
                        null;
                end case;

                if rst = RST_LVL then
                    rx_fsm      <= idle;
                    rx_ready    <= '0';
                    rx_data     <= (others=>'0');
                    rx_data_tmp <= (others=>'0');
                    rx_data_cnt <= (others=>'0');
                end if;
            end if;
        end if;
    end process;

end Behavioral;
