
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blaze_itof is
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
end blaze_itof;

architecture arch of blaze_itof is

   signal zeros         : unsigned(WIDTH - 1 downto 0);
   signal sign          : std_logic;
   signal exp           : unsigned(EXPONENT - 1 downto 0);
   signal frac          : unsigned(WIDTH - 1 downto 0);
   signal frac_shift    : unsigned(WIDTH - 1 downto 0);

begin

   sign        <= value(WIDTH - 1);
   frac_shift  <= to_unsigned(FRACTION + 1, WIDTH) + zeros -
                  to_unsigned(WIDTH, WIDTH);

   shift : entity work.blaze_shifter
      generic map (
         WIDTH       => WIDTH
      )
      port map (
         is_left     => '1',
         is_signed   => '0',
         value_in    => value,
         shift_in    => frac_shift,
         result      => frac
      );

   nlz : entity work.blaze_nlz
      generic map (
         WIDTH => WIDTH
      )
      port map (
         value    => value,
         result   => zeros
      );

   result   <= sign & exp & frac(FRACTION - 1 downto 0);
   ready    <= '1';

end arch;
