
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity tb is
end tb;

architecture tb_arch of tb is

   procedure cycle(signal clk : out std_logic) is
   begin
      clk <= '1';
      wait for 10 ns;
      clk <= '0';
      wait for 10 ns;
   end cycle;

   function parse_number(ch : character) return integer is
   begin
      case ch is
         when '0'       => return 0;
         when '1'       => return 1;
         when '2'       => return 2;
         when '3'       => return 3;
         when '4'       => return 4;
         when '5'       => return 5;
         when '6'       => return 6;
         when '7'       => return 7;
         when '8'       => return 8;
         when '9'       => return 9;
         when 'a' | 'A' => return 10;
         when 'b' | 'B' => return 11;
         when 'c' | 'C' => return 12;
         when 'd' | 'D' => return 13;
         when 'e' | 'E' => return 14;
         when 'f' | 'F' => return 15;
         when others    => return -1;
      end case;
   end parse_number;

   subtype word_type is std_logic_vector(31 downto 0);
   type word_array_type is array(natural range <>) of word_type;

   signal clk        : std_logic;
   signal rst        : std_logic;
   signal dready     : std_logic;
   signal din        : std_logic_vector(31 downto 0);
   signal dout       : std_logic_vector(31 downto 0);
   signal daddr      : std_logic_vector(31 downto 2);
   signal dre        : std_logic;
   signal dwe        : std_logic;
   signal dmask      : std_logic_vector(3 downto 0);
   signal iready     : std_logic;
   signal iin        : std_logic_vector(31 downto 0);
   signal ire        : std_logic;
   signal iaddr      : std_logic_vector(31 downto 2);
   signal timer      : natural;
   signal dbuffer    : std_logic_vector(31 downto 0);
   signal init_addr  : integer;
   signal init_data  : std_logic_vector(31 downto 0);

   signal ram : word_array_type(0 to 1023) := (others => (others => 'X'));

begin

   process

      file     infile   : text is "input.hex";
      variable temp     : line;
      variable ch       : character;
      variable good     : boolean;
      variable word     : integer;
      variable nibble   : natural;
      variable index    : natural;

   begin

      -- Read the program into memory.
      rst      <= '1';
      index    := 0;
      word     := 0;
      nibble   := 0;
      while not endfile(infile) loop
         readline(infile, temp);
         loop
            read(temp, ch, good);
            exit when not good;
            if parse_number(ch) >= 0 then
               word   := word * 16 + parse_number(ch);
               nibble := nibble + 1;
               if nibble = 8 then
                  init_addr <= index;
                  init_data <= std_logic_vector(to_signed(word, 32));
                  cycle(clk);
                  index  := index + 1;
                  nibble := 0;
               end if;
            end if;
         end loop;
      end loop;
      report "Read " & integer'image(index) & " words";
      
      -- Run for a while.
      rst <= '0';
      for i in 1 to 1000 loop
         cycle(clk);
      end loop;

      wait;

   end process;

   -- Handle data reads/writes.
   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            timer    <= 0;
            dbuffer  <= (others => 'Z');
            ram(init_addr) <= init_data;
            report "INIT[" & integer'image(init_addr) &
                   "] <= " & integer'image(to_integer(signed(init_data)));
         elsif dre = '1' then
            timer    <= 10;
            report "READ[" & integer'image(to_integer(signed(daddr))) &
               "] => " & integer'image(to_integer(
               signed(ram(to_integer(unsigned(daddr))))));
            dbuffer  <= ram(to_integer(unsigned(daddr)));
         elsif dwe = '1' then
            timer    <= 10;
            report "WRITE[" & integer'image(to_integer(signed(daddr))) &
               "] <= " & integer'image(to_integer(signed(dout))) & " MASK: " &
               integer'image(to_integer(unsigned(dmask)));
            for b in 0 to 3 loop
               if dmask(b) = '1' then
                  ram(to_integer(unsigned(daddr)))(b * 8 + 7 downto b * 8)
                     <= dout(b * 8 + 7 downto b * 8);
               end if;
            end loop;
         elsif timer > 0 then
            timer <= timer - 1;
         end if;
      end if;
   end process;
   dready   <= '1' when timer = 0 else '0';
   din      <= dbuffer when timer = 0 else (others => 'Z');

   -- Handle instruction reads.
   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            iready <= '1';
         elsif ire = '1' then
            iin      <= ram(to_integer(unsigned(iaddr)));
            iready   <= '1';
         end if;
      end if;
   end process;

   cpu : entity work.blaze
      port map (
         clk      => clk,
         rst      => rst,
         dready   => dready,
         din      => din,
         dout     => dout,
         daddr    => daddr,
         dmask    => dmask,
         dre      => dre,
         dwe      => dwe,
         iready   => iready,
         iin      => iin,
         ire      => ire,
         iaddr    => iaddr
      );

end tb_arch;
