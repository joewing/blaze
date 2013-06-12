
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blaze_fpu is
   generic (
      WIDTH    : natural := 32;
      FRACTION : natural := 23;
      EXPONENT : natural := 8
   );
   port (
      clk      : in  std_logic;
      start    : in  std_logic;
      func     : in  std_logic_vector(10 downto 0);
      ina      : in  unsigned(WIDTH - 1 downto 0);
      inb      : in  unsigned(WIDTH - 1 downto 0);
      result   : out unsigned(WIDTH - 1 downto 0);
      ready    : out std_logic
   );
end blaze_fpu;

architecture arch of blaze_fpu is

   signal itof_ready    : std_logic;
   signal itof_result   : unsigned(WIDTH - 1 downto 0);

   signal ftoi_ready    : std_logic;
   signal ftoi_result   : unsigned(WIDTH - 1 downto 0);

   signal fsqrt_ready   : std_logic;
   signal fsqrt_result  : unsigned(WIDTH - 1 downto 0);

begin

   process(func,
           itof_ready, itof_result,
           fsqrt_ready, fsqrt_result)
   begin
      case func is
         when "00000000000" =>   -- fadd
         when "00010000000" =>   -- fsub
         when "00100000000" =>   -- fmul
         when "00110000000" =>   -- fdiv
         when "01000000000" =>   -- fcmp.un
         when "01000010000" =>   -- fcmp.lt
         when "01000100000" =>   -- fcmp.eq
         when "01000110000" =>   -- fcmp.le
         when "01001000000" =>   -- fcmp.gt
         when "01001010000" =>   -- fcmp.ne
         when "01001100000" =>   -- fcmp.ge
         when "01010000000" =>   -- flt
            result   <= itof_result;
            ready    <= itof_ready;
         when "01100000000" =>   -- fint
            result   <= ftoi_result;
            ready    <= ftoi_ready;
         when "01110000000" =>   -- fsqrt
            result   <= fsqrt_result;
            ready    <= fsqrt_ready;
         when others =>
            result <= (others => 'X');
      end case;
   end process;

   itof : entity work.blaze_itof
      generic map (
         WIDTH       => WIDTH,
         FRACTION    => FRACTION,
         EXPONENT    => EXPONENT
      )
      port map (
         clk         => clk,
         start       => start,
         value       => ina,
         ready       => itof_ready,
         result      => itof_result
      );

   ftoi : entity work.blaze_ftoi
      generic map (
         WIDTH       => WIDTH,
         FRACTION    => FRACTION,
         EXPONENT    => EXPONENT
      )
      port map (
         clk         => clk,
         start       => start,
         value       => ina,
         ready       => ftoi_ready,
         result      => ftoi_result
      );

   fsqrt : entity work.blaze_fsqrt
      generic map (
         WIDTH       => WIDTH,
         FRACTION    => FRACTION,
         EXPONENT    => EXPONENT
      )
      port map (
         clk         => clk,
         start       => start,
         value_in    => ina,
         ready       => fsqrt_ready,
         result      => fsqrt_result
      );

end arch;
