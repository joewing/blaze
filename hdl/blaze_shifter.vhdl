
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blaze_shifter is
   generic (
      WIDTH       : natural := 32
   );
   port (
      is_left     : in  std_logic;
      is_signed   : in  std_logic;
      value_in    : in  unsigned(WIDTH - 1 downto 0);
      shift_in    : in  unsigned(WIDTH - 1 downto 0);
      result      : out unsigned(WIDTH - 1 downto 0)
   );
end blaze_shifter;

architecture arch of blaze_shifter is

   function log2(n : natural) return natural is
      variable temp     : natural := n;
      variable result   : natural := 0;
   begin
      while temp > 1 loop
         result   := result + 1;
         temp     := temp / 2;
      end loop;
      return result;
   end log2;

   constant SHIFT_BITS : natural := log2(WIDTH);

   subtype word_type is unsigned(WIDTH - 1 downto 0);
   type word_array_type is array(0 to SHIFT_BITS) of word_type;

   signal stages : word_array_type;

begin

   process(is_left, is_signed, value_in, stages)
   begin
      stages(0) <= value_in;
      for i in 0 to SHIFT_BITS - 1 loop
         if shift_in(i) = '1' then
            if is_left = '1' then
               stages(i + 1) <= shift_left(stages(i), 2 ** i);
            elsif is_signed = '1' then
               stages(i + 1) <= unsigned(shift_right(signed(stages(i)),
                                                     2 ** i));
            else
               stages(i + 1) <= shift_right(stages(i), 2 ** i);
            end if;
         else
            stages(i + 1) <= stages(i);
         end if;
      end loop;
   end process;

   result <= stages(SHIFT_BITS);

end arch;
