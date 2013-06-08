
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blaze_multiplier is
   generic (
      WIDTH    : natural := 32;
      SHIFT    : natural := 18
   );
   port (
      clk      : in  std_logic;
      start    : in  std_logic;
      ready    : out std_logic;
      ina      : in  unsigned(WIDTH - 1 downto 0);
      inb      : in  unsigned(WIDTH - 1 downto 0);
      result   : out unsigned(WIDTH - 1 downto 0)
   );
end blaze_multiplier;

architecture blaze_multiplier_arch of blaze_multiplier is

   signal a       : unsigned(WIDTH - 1 downto 0);
   signal b       : unsigned(WIDTH - 1 downto 0);
   signal temp_a  : unsigned(SHIFT - 1 downto 0);
   signal temp_b  : unsigned(SHIFT - 1 downto 0);
   signal sum     : unsigned(WIDTH - 1 downto 0);

begin

   process(clk)
   begin
      if start = '1' then
         a     <= ina;
         b     <= inb;
         sum   <= (others => '0');
      elsif a /= 0 then
         sum   <= sum + resize(temp_a * temp_b, WIDTH);
         a     <= shift_right(a, SHIFT);
         b     <= shift_left(b, SHIFT);
      end if;
   end process;

   temp_a   <= ina(SHIFT - 1 downto 0);
   temp_b   <= inb(SHIFT - 1 downto 0);

   ready    <= '1' when a = 0 else '0';
   result   <= sum;

end blaze_multiplier_arch;
