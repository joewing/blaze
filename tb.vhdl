
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb is
end tb;

architecture tb_arch of tb is

   procedure cycle(signal clk : out std_logic) is
   begin
      clk <= '1';
      wait for 10 ns;
      clk <= '0';
      wait for 10 ns;
   end cycle;

   subtype word_type is std_logic_vector(31 downto 0);
   type word_array_type is array(natural range <>) of word_type;

   signal clk  : std_logic;
   signal rst  : std_logic;

   signal dready  : std_logic;
   signal din     : std_logic_vector(31 downto 0);
   signal dout    : std_logic_vector(31 downto 0);
   signal daddr   : std_logic_vector(31 downto 0);
   signal dre     : std_logic;
   signal dwe     : std_logic;
   signal iready  : std_logic;
   signal iin     : std_logic_vector(31 downto 0);
   signal ire     : std_logic;
   signal iaddr   : std_logic_vector(31 downto 0);
   signal timer   : natural;
   signal dbuffer : std_logic_vector(31 downto 0);

   signal instr_ram  : word_array_type(0 to 255) := (

      -- LWI   r1, r0 + 1
      0        => "111010" & "00001" & "00000" & "0000000000000001",

      -- LWI   r2, r0 + 2
      1        => "111010" & "00010" & "00000" & "0000000000000010",

      -- ADD   r3, r1, r2
      2        => "000000" & "00011" & "00001" & "00010" & "00000000000",

      -- ADD   r3, r3, r1
      3        => "000000" & "00011" & "00011" & "00001" & "00000000000",

      -- ADD   r3, r3, r1
      4        => "000000" & "00011" & "00011" & "00001" & "00000000000",

      -- SWI   r3, r0 + 3
      5        => "111110" & "00011" & "00000" & "0000000000000011",

      -- SWI   r2, r0 + 4
      6        => "111110" & "00010" & "00000" & "0000000000000100",

      -- LWI   r1, r0 + 5
      7        => "111010" & "00001" & "00000" & "0000000000000101",

      -- AND   r2, r0, r0
      8        => "100001" & "00010" & "00000" & "00000" & "00000000000",

      -- ADD   r2, r1, r2
      9        => "000000" & "00010" & "00001" & "00010" & "00000000000",

      -- SWI   r2, r2, 6
      10       => "111110" & "00010" & "00010" & "0000000000000101",

      -- BRI   -8
      11       => "101110" & "00000" & "00000" & "1111111111111000",

      others   => (others => '0')
   );
   signal data_ram   : word_array_type(0 to 255) := (
      1        => std_logic_vector(to_unsigned(5, 32)),
      2        => std_logic_vector(to_unsigned(12, 32)),
      5        => std_logic_vector(to_unsigned(1, 32)),
      others   => (others => 'X')
   );

begin

   process
   begin

      rst <= '1';
      cycle(clk);
      rst <= '0';

      for i in 1 to 200 loop
         cycle(clk);
      end loop;

      wait;

   end process;

   -- Handle data reads/writes.
   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            timer    <= 0;
            dbuffer  <= (others => 'Z');
         elsif dre = '1' then
            timer    <= 10;
            report "READ: [" & integer'image(to_integer(unsigned(daddr))) &
               "] => " & integer'image(to_integer(
               unsigned(data_ram(to_integer(unsigned(daddr))))));
            dbuffer  <= data_ram(to_integer(unsigned(daddr)));
         elsif dwe = '1' then
            timer    <= 10;
            report "WRITE: [" & integer'image(to_integer(unsigned(daddr))) &
               "] <= " & integer'image(to_integer(unsigned(dout)));
            data_ram(to_integer(unsigned(daddr))) <= dout;
         elsif timer > 0 then
            timer <= timer - 1;
         end if;
      end if;
   end process;
   dready   <= '1' when timer = 0 else '0';
   din      <= dbuffer when timer = 0 else (others => 'Z');

   -- Handle instruction reads.
   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            iready <= '1';
         elsif ire = '1' then
            iin      <= instr_ram(to_integer(unsigned(iaddr(31 downto 2))));
            iready   <= '1';
         end if;
      end if;
   end process;

   cpu : entity work.blaze
      port map (
         clk      => clk,
         rst      => rst,
         dready   => dready,
         din      => din,
         dout     => dout,
         daddr    => daddr,
         dre      => dre,
         dwe      => dwe,
         iready   => iready,
         iin      => iin,
         ire      => ire,
         iaddr    => iaddr
      );

end tb_arch;
