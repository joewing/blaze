
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blaze_nlz is
   generic (
      WIDTH    : natural := 32
   );
   port (
      value    : in  unsigned(WIDTH - 1 downto 0);
      result   : out unsigned(WIDTH - 1 downto 0)
   );
end blaze_nlz;

architecture arch of blaze_nlz is

begin

   process(value)
      variable i : natural;
   begin
      result <= to_unsigned(WIDTH, WIDTH);
      for i in WIDTH - 1 downto 0 loop
         if value(i) = '1' then
            result <= to_unsigned(i, WIDTH);
         end if;
      end loop;
   end process;

end arch;
