
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blaze_divider is
   generic (
      WIDTH : natural := 32
   );
   port (
      clk         : in  std_logic;
      start       : in  std_logic;
      ready       : out std_logic;
      ina         : in  unsigned(WIDTH - 1 downto 0);
      inb         : in  unsigned(WIDTH - 1 downto 0);
      is_signed   : in  std_logic;
      result      : out unsigned(WIDTH - 1 downto 0)
   );
end blaze_divider;

architecture arch of blaze_divider is

   signal count   : natural;
   signal b       : unsigned(WIDTH - 1 downto 0);
   signal reg     : unsigned(2 * WIDTH - 1 downto 0);
   signal diff    : unsigned(WIDTH downto 0);
   signal negate  : std_logic;

begin

   diff     <= ("0" & reg(2 * WIDTH - 2 downto WIDTH - 1)) - ("0" & b);
   negate   <= (ina(WIDTH - 1) xor inb(WIDTH - 1)) and is_signed;

   process(clk)
   begin
      if clk'event and clk = '1' then
         if start = '1' then
            if is_signed = '1' then
               if ina(WIDTH - 1) = '1' then
                  reg <= resize((not ina) + 1, WIDTH * 2);
               else
                  reg <= resize(ina, WIDTH * 2);
               end if;
               if inb(WIDTH - 1) = '1' then
                  b <= (not inb) + 1;
               else
                  b <= inb;
               end if;
            else
               b <= inb;
               reg <= resize(ina, WIDTH * 2);
            end if;
            count <= WIDTH;
         elsif count /= 0 then
            if diff(WIDTH) = '1' then
               reg <= reg(2 * WIDTH - 2 downto 0) & "0";
            else
               reg <= diff(WIDTH - 1 downto 0) & reg(WIDTH - 2 downto 0) & "1";
            end if;
            count <= count - 1;
         end if;
      end if;
   end process;

   result   <= (not reg(WIDTH - 1 downto 0)) + 1 when negate = '1'
               else reg(WIDTH - 1 downto 0);
   ready    <= '1' when count = 0 else '0';

end arch;
