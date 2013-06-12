
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blaze_fsqrt is
   generic (
      WIDTH    : natural := 32;
      FRACTION : natural := 23;
      EXPONENT : natural := 8
   );
   port (
      clk      : in  std_logic;
      start    : in  std_logic;
      value_in : in  unsigned(31 downto 0);
      result   : out unsigned(31 downto 0);
      ready    : out std_logic
   );
end blaze_fsqrt;

architecture arch of blaze_fsqrt is

   signal count   : natural;
   signal q       : unsigned(WIDTH - 1 downto 0);
   signal s       : unsigned(WIDTH - 1 downto 0);
   signal r       : unsigned(WIDTH - 1 downto 0);
   signal exp     : unsigned(EXPONENT - 1 downto 0);

   signal expa    : unsigned(EXPONENT - 1 downto 0);
   signal fraca   : unsigned(FRACTION downto 0);
   signal c       : unsigned(EXPONENT - 1 downto 0);

begin

   expa  <= value_in(WIDTH - 2 downto WIDTH - 1 - EXPONENT);
   fraca <= to_unsigned(0, fraca'length) when expa = 0
            else "1" & value_in(FRACTION - 1 downto 0);
   c     <= expa - 9;

   process(clk)
      variable t : unsigned(WIDTH - 1 downto 0);
   begin
      if clk'event and clk = '1' then
         if start = '1' then
            q     <= to_unsigned(2 ** (FRACTION + 1), q'length);
            s     <= to_unsigned(2 ** FRACTION, s'length);
            exp   <= shift_right(expa + 125, 1);
            if c(0) = '1' then
               r <= shift_left(resize(fraca, r'length), 1) -
                    2 ** (FRACTION + 1);
            else
               r <= shift_left(resize(fraca, r'length), 2) -
                    2 ** (FRACTION + 1);
            end if;
            count <= FRACTION + 1;
         elsif count /= 0 then
            s <= shift_right(s, 1);
            t := shift_right(r, 1) - shift_right(q, 1);
            if t(WIDTH -  1) = '1' then
               r <= shift_left(r, 1);
            else
               q <= q + s;
               r <= t;
            end if;
            if expa = 0 then
               exp   <= to_unsigned(0, exp'length);
               q     <= to_unsigned(0, q'length);
               count <= 0;
            else
               count <= count - 1;
            end if;
         end if;
      end if;
   end process;

   result <= resize("0" & exp, WIDTH) + resize(q(FRACTION downto 0), WIDTH);
   ready  <= '1' when count = 0 else '0';

end arch;
