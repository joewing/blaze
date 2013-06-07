
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blaze is
   port (
      clk      : in  std_logic;
      rst      : in  std_logic;
      dready   : in  std_logic;
      din      : in  std_logic_vector(31 downto 0);
      dout     : out std_logic_vector(31 downto 0);
      daddr    : out std_logic_vector(31 downto 0);
      dre      : out std_logic;
      dwe      : out std_logic;
      dmask    : out std_logic_vector(3 downto 0);
      iready   : in  std_logic;
      iin      : in  std_logic_vector(31 downto 0);
      ire      : out std_logic;
      iaddr    : out std_logic_vector(31 downto 0)
   );
end blaze;

architecture blaze_arch of blaze is

   type exec_state_type is (
      EXEC_IDLE,
      EXEC_INIT_XFER,
      EXEC_WAIT_XFER
   );

   subtype register_type is std_logic_vector(31 downto 0);
   type register_array_type is array(natural range 0 to 31) of register_type;
   subtype instr_type is std_logic_vector(5 downto 0);

   signal fetch_valid   : std_logic;
   signal fetch_pc      : std_logic_vector(31 downto 0);
   signal fetch_instr   : std_logic_vector(31 downto 0);
   signal fetch_op      : std_logic_vector(5 downto 0);
   signal fetch_rd      : natural;
   signal fetch_ra      : natural;
   signal fetch_rb      : natural;
   signal fetch_func    : std_logic_vector(10 downto 0);
   signal fetch_imm16   : std_logic_vector(15 downto 0);

   signal decode_valid  : std_logic;
   signal decode_pc     : std_logic_vector(31 downto 0);
   signal decode_instr  : std_logic_vector(31 downto 0);
   signal decode_op     : std_logic_vector(5 downto 0);
   signal decode_rd     : natural;
   signal decode_ra     : natural;
   signal decode_rb     : natural;
   signal decode_func   : std_logic_vector(10 downto 0);
   signal decode_imm16  : std_logic_vector(15 downto 0);
   signal decode_va     : register_type;
   signal decode_vb     : register_type;
   signal decode_sum    : signed(31 downto 0);
   signal decode_sumi   : signed(31 downto 0);
   signal decode_addr   : std_logic_vector(31 downto 0);
   signal fd_bypass     : natural;

   signal exec_ready       : std_logic;
   signal exec_state       : exec_state_type;
   signal next_exec_state  : exec_state_type;
   signal exec_din         : register_type;
   signal exec_load        : std_logic;
   signal exec_store       : std_logic;
   signal exec_math        : std_logic;
   signal bad_branch       : std_logic;
   signal reading_instr    : std_logic;

   signal regs    : register_array_type;
   signal pc      : register_type;     -- Program Counter
   signal btr     : register_type;     -- Branch Target Register
   signal msr     : register_type;     -- Machine Status Register
   signal esr     : register_type;     -- Exception Status Register
   signal ear     : register_type;     -- Exception Address Register
   signal fsr     : register_type;     -- FPU Status Register

   signal next_pc : register_type;

   -- Bits of the MSR
   constant CARRY_COPY_BIT : natural   := 0;    -- Copy of bit 29
   constant VMS_BIT        : natural   := 17;   -- Virtual Mode Save
   constant VM_BIT         : natural   := 18;   -- Virtual Mode
   constant UMS_BIT        : natural   := 19;   -- User Mode Save
   constant UM_BIT         : natural   := 20;   -- User Mode
   constant PVR_BIT        : natural   := 21;   -- Proc Version Register
   constant EIP_BIT        : natural   := 22;   -- Exception In Progress
   constant EE_BIT         : natural   := 23;   -- Exception Enable
   constant DCE_BIT        : natural   := 24;   -- Data Cache Enable
   constant DZO_BIT        : natural   := 25;   -- Division Zero/Overflow
   constant ICE_BIT        : natural   := 26;   -- Instruction Cache Enable
   constant FSL_BIT        : natural   := 27;   -- Stream Error
   constant BIP_BIT        : natural   := 28;   -- Break In Progress
   constant CARRY_BIT      : natural   := 29;   -- Arithmetic Carry
   constant IE_BIT         : natural   := 30;   -- Interrupt Enable

   signal alu_ina          : unsigned(31 downto 0);
   signal alu_inb          : unsigned(31 downto 0);
   signal alu_cin          : unsigned(31 downto 0);
   signal alu_result       : unsigned(31 downto 0);

begin

   -- Stage 0: Fetch
   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            fetch_valid <= '0';
         elsif exec_ready = '1' then
            fetch_valid <= iready and not bad_branch;
            fetch_pc    <= pc;
            fetch_instr <= iin;
         end if;
      end if;
   end process;
   fetch_op    <= fetch_instr(31 downto 26);
   fetch_rd    <= to_integer(unsigned(fetch_instr(25 downto 21)));
   fetch_ra    <= to_integer(unsigned(fetch_instr(20 downto 16)));
   fetch_rb    <= to_integer(unsigned(fetch_instr(15 downto 11)));
   fetch_func  <= fetch_instr(10 downto 0);
   fetch_imm16 <= fetch_instr(15 downto 0);

   -- Stage 1: Decode
   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            decode_valid <= '0';
         elsif exec_ready = '1' then
            decode_valid   <= fetch_valid and not bad_branch;
            decode_pc      <= fetch_pc;
            decode_instr   <= fetch_instr;
            if fetch_ra = 0 then
               decode_va   <= (others => '0');
            elsif fetch_ra = fd_bypass then
               decode_va   <= std_logic_vector(alu_result);
            else
               decode_va   <= regs(fetch_ra);
            end if;
            if fetch_rb = 0 then
               decode_vb   <= (others => '0');
            elsif fetch_rb = fd_bypass then
               decode_vb   <= std_logic_vector(alu_result);
            else
               decode_vb   <= regs(fetch_rb);
            end if;
         end if;
      end if;
   end process;
   decode_op      <= decode_instr(31 downto 26);
   decode_rd      <= to_integer(unsigned(decode_instr(25 downto 21)));
   decode_ra      <= to_integer(unsigned(decode_instr(20 downto 16)));
   decode_rb      <= to_integer(unsigned(decode_instr(15 downto 11)));
   decode_func    <= decode_instr(10 downto 0);
   decode_imm16   <= decode_instr(15 downto 0);
   decode_sum     <= signed(decode_va) + signed(decode_vb);
   decode_sumi    <= signed(decode_va) + resize(signed(decode_imm16), 32);
   decode_addr    <= std_logic_vector(decode_sumi) when decode_op(3) = '1'
                     else std_logic_vector(decode_sum);
   daddr <= decode_addr;

   -- Stage 2: Execute
   process(decode_op, decode_valid)
   begin
      exec_load   <= '0';
      exec_store  <= '0';
      exec_math   <= '0';
      case decode_op is
         when "110000" | "110001" | "110010" 
            | "111000" | "111001" | "111010" =>
            exec_load <= decode_valid;
         when "110100" | "110101" | "110110"
            | "111100" | "111101" | "111110" =>
            exec_store <= decode_valid;
         when "000000" | "000001" | "000010" | "000011"
            | "000100" | "000101" | "000110" | "000111"
            | "001000" | "001001" | "001011" | "001100"
            | "001101" | "001110" | "001111"
            | "100000" | "101000" | "100001" | "101001"
            | "100010" | "101010" | "100011" | "101011"
            | "010000" | "010010" =>
            exec_math <= decode_valid;
         when others =>
            null;
      end case;
   end process;

   process(exec_state, decode_op, dready, decode_valid, exec_load, exec_store)
   begin
      next_exec_state <= exec_state;
      case exec_state is
         when EXEC_IDLE =>
            if decode_valid = '1' then
               if exec_load = '1' or exec_store = '1' then
                  next_exec_state <= EXEC_INIT_XFER;
               end if;
            end if;
         when EXEC_INIT_XFER =>
            next_exec_state <= EXEC_WAIT_XFER;
         when EXEC_WAIT_XFER =>
            if dready = '1' then
               next_exec_state <= EXEC_IDLE;
            end if;
      end case;
   end process;

   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            exec_state <= EXEC_IDLE;
         else
            exec_state <= next_exec_state;
         end if;
      end if;
   end process;

   process(exec_math, exec_load, decode_rd)
   begin
      if exec_math = '1' or exec_load = '1' then
         fd_bypass <= decode_rd;
      else
         fd_bypass <= 0;
      end if;
   end process;

   process(clk)
   begin
      if clk'event and clk = '1' then
         case exec_state is
            when EXEC_IDLE =>
               if decode_valid = '1' and exec_math = '1' then
                  regs(decode_rd) <= std_logic_vector(alu_result);
               end if;
            when EXEC_INIT_XFER =>
               null;
            when EXEC_WAIT_XFER =>
               if exec_load = '1' and dready = '1' then
                  regs(decode_rd) <= std_logic_vector(alu_result);
               end if;
         end case;
      end if;
   end process;
   exec_ready  <= '1' when next_exec_state = EXEC_IDLE else '0';

   -- Drive data memory ports.
   process(clk)
   begin
      if clk'event and clk = '1' then
         dre   <= '0';
         dwe   <= '0';
         dout  <= (others => 'Z');
         if next_exec_state = EXEC_INIT_XFER then
            dre <= exec_load;
            dwe <= exec_store;
            if decode_rd = 0 then
               dout <= (others => '0');
            else
               dout <= regs(decode_rd);
            end if;
         end if;
      end if;
   end process;

   -- Select input data.
   process(din, decode_op, decode_addr)
      variable byte_in  : register_type;
      variable half_in  : register_type;
   begin
      case decode_addr(1 downto 0) is
         when "00" =>
            byte_in := std_logic_vector(to_unsigned(0, 24)) & din(7 downto 0);
         when "01" =>
            byte_in := std_logic_vector(to_unsigned(0, 24)) & din(15 downto 8);
         when "10" =>
            byte_in := std_logic_vector(to_unsigned(0, 24)) & din(23 downto 16);
         when others =>
            byte_in := std_logic_vector(to_unsigned(0, 24)) & din(31 downto 24);
      end case;
      if decode_addr(1) = '0' then
         half_in := std_logic_vector(to_unsigned(0, 16)) & din(15 downto 0);
      else
         half_in := std_logic_vector(to_unsigned(0, 16)) & din(31 downto 16);
      end if;
      case decode_op is
         when "110000" | "111000" =>
            exec_din <= byte_in;
         when "110001" | "111001" =>
            exec_din <= half_in;
         when "110010" | "111010" =>
            exec_din <= din;
         when others =>
            exec_din <= (others => 'Z');
      end case;
   end process;

   -- Drive ALU inputs.
   process(decode_instr, decode_va, decode_vb, decode_imm16)
   begin
      if decode_instr(29) = '0' then
         alu_ina <= unsigned(decode_va);
      else
         alu_ina <= unsigned(resize(signed(decode_imm16), 32));
      end if;
      alu_inb <= unsigned(decode_vb);
   end process;

   -- ALU
   process(decode_op, alu_ina, alu_inb, exec_din)
   begin
      case decode_op is
         when "000000" | "001000" =>
            alu_result  <= alu_inb + alu_ina;
         when "000001" | "001001" =>
            alu_result <= alu_inb + (not alu_ina) + 1;
         when "000010" | "001010" =>
            alu_result <= alu_inb + alu_ina + alu_cin;
         when "000011" | "001011" =>
            alu_result <= alu_inb + (not alu_ina) + alu_cin;
         when "000100" | "001100" =>
            alu_result <= alu_inb + alu_ina;
         when "000101" | "001101" =>
            alu_result <= alu_inb + (not alu_ina) + 1;
         when "000110" | "001110" =>
            alu_result <= alu_inb + alu_ina + alu_cin;
         when "000111" | "001111" =>
            alu_result <= alu_inb + (not alu_ina) + alu_cin;
         when "100000" | "101000" =>
            alu_result <= alu_ina or alu_ina;
         when "100001" | "101001" =>
            alu_result <= alu_ina and alu_inb;
         when "100010" | "101010" =>
            alu_result <= alu_ina xor alu_inb;
         when "100011" | "101011" =>
            alu_result <= alu_ina and not alu_inb;
         when "010000" =>
            alu_result <= alu_ina * alu_inb;
         when "010010" =>
            alu_result <= alu_inb / alu_ina;
         when others =>
            alu_result <= unsigned(exec_din);
      end case;
   end process;
   alu_cin <= to_unsigned(0, 31) & msr(CARRY_BIT);

   -- Program counter
   process(pc, decode_valid, decode_op, decode_ra, decode_vb, decode_pc,
           decode_sumi)
      variable pc_plus_vb  : std_logic_vector(31 downto 0);
      variable pc_plus_4   : std_logic_vector(31 downto 0);
      variable pc_plus_imm : std_logic_vector(31 downto 0);
   begin
      pc_plus_vb  := std_logic_vector(signed(decode_pc) + signed(decode_vb));
      pc_plus_4   := std_logic_vector(signed(pc) + to_signed(4, 32));
      pc_plus_imm := std_logic_vector(signed(decode_pc) + decode_sumi +
                                      to_signed(-4, 32));
      bad_branch <= '0';
      if decode_valid = '1' and decode_op = "100110" then
         -- BR, BRD, BRLD, BRA, BRAD, BRALD, BRK
         case decode_ra is
            when 0 | 16 | 20 =>
               -- Relative branch.
               next_pc <= pc_plus_vb;
               bad_branch <= '1';
            when others    =>
               -- Absolute branch.
               next_pc <= decode_vb;
               bad_branch <= '1';
         end case;
      elsif decode_valid = '1' and decode_op = "101110" then
         -- BRI, BRID, BRLID, BRAI, BRAID, BRAIDR, BRKI
         next_pc <= pc_plus_imm;
         bad_branch <= '1';
      elsif decode_valid = '1' and decode_op = "101111" then
         -- BEQI, BNEI, BLTI, etc
         -- TODO
         bad_branch <= '1';
      else
         next_pc <= pc_plus_4;
      end if;
   end process;

   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            pc <= (others => '0');
            reading_instr <= '0';
         elsif iready = '1' then
            if exec_ready = '1' then
               pc <= next_pc;
               reading_instr <= '1';
            else
               reading_instr <= '0';
            end if;
         end if;
      end if;
   end process;
   iaddr <= pc;
   ire   <= exec_ready;

end blaze_arch;
