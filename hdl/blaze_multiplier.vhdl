
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blaze_multiplier is
   generic (
      WIDTH    : natural := 32;
      SHIFT    : natural := 18
   );
   port (
      clk         : in  std_logic;
      start       : in  std_logic;
      ready       : out std_logic;
      ina         : in  unsigned(WIDTH - 1 downto 0);
      inb         : in  unsigned(WIDTH - 1 downto 0);
      a_signed    : in  std_logic;
      b_signed    : in  std_logic;
      result      : out unsigned(WIDTH * 2 - 1 downto 0)
   );
end blaze_multiplier;

architecture blaze_multiplier_arch of blaze_multiplier is

   signal a       : unsigned(WIDTH * 2 - 1 downto 0);
   signal b       : unsigned(WIDTH * 2 - 1 downto 0);
   signal temp_a  : unsigned(SHIFT - 1 downto 0);
   signal temp_b  : unsigned(SHIFT - 1 downto 0);
   signal sum     : unsigned(WIDTH * 2 - 1 downto 0);

begin

   process(clk)
   begin
      if clk'event and clk = '1' then
         if start = '1' then
            if a_signed = '1' then
               a <= unsigned(resize(signed(ina), WIDTH * 2));
            else
               a <= unsigned(resize(unsigned(ina), WIDTH * 2));
            end if;
            if b_signed = '1' then
               b <= unsigned(resize(signed(inb), WIDTH * 2));
            else
               b <= unsigned(resize(unsigned(inb), WIDTH * 2));
            end if;
            sum <= (others => '0');
         elsif a /= 0 then
            sum   <= sum + resize(temp_a * temp_b, WIDTH * 2);
            a     <= shift_right(a, SHIFT);
            b     <= shift_left(b, SHIFT);
         end if;
      end if;
   end process;

   temp_a   <= ina(SHIFT - 1 downto 0);
   temp_b   <= inb(SHIFT - 1 downto 0);

   ready    <= '1' when a = 0 else '0';
   result   <= sum;

end blaze_multiplier_arch;
