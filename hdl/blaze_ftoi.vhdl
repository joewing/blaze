
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blaze_ftoi is
   generic (
      WIDTH    : natural := 32;
      FRACTION : natural := 23;
      EXPONENT : natural := 8
   );
   port (
      clk      : in  std_logic;
      start    : in  std_logic;
      value    : in  unsigned(WIDTH - 1 downto 0);
      result   : out unsigned(WIDTH - 1 downto 0);
      ready    : out std_logic
   );
end blaze_ftoi;

architecture arch of blaze_ftoi is

   signal exp        : unsigned(EXPONENT - 1 downto 0);
   signal frac       : unsigned(WIDTH - 1 downto 0);
   signal sign       : std_logic;
   signal frac_shift : unsigned(WIDTH - 1 downto 0);
   signal shifted    : unsigned(WIDTH - 1 downto 0);

begin

   exp   <= value(WIDTH - 2 downto WIDTH - 1 - EXPONENT);
   frac  <= to_unsigned(0, EXPONENT) & "1" & value(FRACTION - 1 downto 0);
   sign  <= value(WIDTH - 1);

   frac_shift <= resize(exp, WIDTH) - 127;

   shift : entity work.blaze_shifter
      generic map (
         WIDTH       => WIDTH
      )
      port map (
         is_left     => '1',
         is_signed   => '0',
         value_in    => frac,
         shift_in    => frac_shift,
         result      => shifted
      );

   process(shifted, exp, sign, frac_shift)
   begin
      if exp < 127 then
         result <= (others => '0');
      elsif sign = '1' then
         result <= to_unsigned(0, WIDTH) - shifted;
      else
         result <= shifted;
      end if;
   end process;

   ready <= '1';

end arch;
