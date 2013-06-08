
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blaze_alu is
   port (
      clk      : in  std_logic;
      start    : in  std_logic;
      op       : in  std_logic_vector(5 downto 0);
      func     : in  std_logic_vector(10 downto 0);
      ina      : in  unsigned(31 downto 0);
      inb      : in  unsigned(31 downto 0);
      din      : in  std_logic_vector(31 downto 0);
      cin      : in  std_logic;
      ready    : out std_logic;
      result   : out unsigned(31 downto 0);
      cout     : out std_logic
   );
end blaze_alu;

architecture arch of blaze_alu is

   signal mult_a_signed : std_logic;
   signal mult_b_signed : std_logic;
   signal mult_ready    : std_logic;
   signal mult_result   : unsigned(63 downto 0);

begin

   process(op, func, ina, inb, cin, din, mult_result)
      variable carry       : unsigned(31 downto 0);
      variable ina_c       : unsigned(31 downto 0);
      variable not_ina_1   : unsigned(31 downto 0);
      variable not_ina_c   : unsigned(31 downto 0);
   begin
      carry       := to_unsigned(0, 31) & cin;
      ina_c       := ina + carry;
      not_ina_1   := (not ina) + 1;
      not_ina_c   := (not ina) + carry;
      cout        <= '0';
      ready       <= '1';
      case op is
         when "000000" | "001000" | "000100" | "001100" =>  -- add[ik]
            result   <= inb + ina;
            cout     <= inb(31) and ina(31);
         when "000101"  => -- rsubk, cmp, cmpu
            result <= inb + not_ina_1;
            if func = "00000000001" then
               -- cmp
               if signed(ina) > signed(inb) then
                  result(31) <= '1';
               else
                  result(31) <= '0';
               end if;
            elsif func = "00000000011" then
               -- cmpu
               if unsigned(ina) > unsigned(inb) then
                  result(31) <= '1';
               else
                  result(31) <= '0';
               end if;
            end if;
         when "000001" | "001001" | "001101" =>  -- rsub[ik]
            result <= inb + not_ina_1;
            cout   <= inb(31) and not_ina_1(31);
         when "000010" | "001010" | "000110" | "001110" =>  -- addc[ik]
            result  <= inb + ina_c;
            cout    <= inb(31) and ina_c(31);
         when "000011" | "001011" | "000111" | "001111" =>  -- rsubc[ik]
            result  <= inb + not_ina_c;
            cout    <= inb(31) and not_ina_c(31);
         when "100000" | "101000" =>   -- or[i]
            result <= ina or inb;
         when "100001" | "101001" =>   -- and[i]
            result <= ina and inb;
         when "100010" | "101010" =>   -- xor[i]
            result <= ina xor inb;
         when "100011" | "101011" =>   -- andn[i]
            result <= ina and not inb;
         when "010000" =>
            if func = "00000000000" then
               result <= mult_result(31 downto 0);
            else
               result <= mult_result(63 downto 32);
            end if;
            ready  <= mult_ready;
         when "011000" =>
            result <= mult_result(31 downto 0);
            ready  <= mult_ready;
         when "010001" | "011001" => -- bs, bsi
            for i in 0 to 31 loop
               if inb(4 downto 0) = i then
                  if func(10) = '1' then
                     -- Shift left
                     result <= shift_left(ina, i);
                  elsif func(9) = '1' then
                     -- Shift right arithmetic.
                     result <= unsigned(shift_right(signed(ina), i));
                  else
                     -- Shift right logical.
                     result <= shift_right(ina, i);
                  end if;
               end if;
            end loop;
         when "010010" =>
            result <= (others => 'X'); -- TODO
         when others =>
            result  <= unsigned(din);
      end case;
   end process;

   process(func)
   begin
      mult_a_signed  <= '0';
      mult_b_signed  <= '0';
      case func is
         when "00000000001" =>
            -- mulh
            mult_a_signed <= '1';
            mult_b_signed <= '1';
         when "00000000010" =>
            -- mulhsu
            mult_a_signed <= '1';
         when others =>
            -- mul, mulhu
            null;
      end case;
   end process;

   mult : entity work.blaze_multiplier
      generic map (
         WIDTH => 32
      )
      port map (
         clk      => clk,
         start    => start,
         ready    => mult_ready,
         ina      => ina,
         inb      => inb,
         a_signed => mult_a_signed,
         b_signed => mult_b_signed,
         result   => mult_result
      );

end arch;