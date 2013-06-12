
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
      daddr    : out std_logic_vector(31 downto 2);
      dre      : out std_logic;
      dwe      : out std_logic;
      dmask    : out std_logic_vector(3 downto 0);
      iready   : in  std_logic;
      iin      : in  std_logic_vector(31 downto 0);
      ire      : out std_logic;
      iaddr    : out std_logic_vector(31 downto 2)
   );
end blaze;

architecture blaze_arch of blaze is

   type exec_state_type is (
      EXEC_IDLE,
      EXEC_INIT_XFER,
      EXEC_WAIT_XFER,
      EXEC_ALU
   );

   subtype register_type is std_logic_vector(31 downto 0);
   type register_array_type is array(natural range 0 to 31) of register_type;
   subtype instr_type is std_logic_vector(5 downto 0);

   signal fetch_valid   : std_logic;
   signal fetch_pc      : std_logic_vector(31 downto 0);
   signal fetch_instr   : std_logic_vector(31 downto 0);
   signal fetch_op      : std_logic_vector(5 downto 0);
   signal fetch_rd      : unsigned(4 downto 0);
   signal fetch_ra      : unsigned(4 downto 0);
   signal fetch_rb      : unsigned(4 downto 0);
   signal fetch_func    : std_logic_vector(10 downto 0);
   signal fetch_imm16   : std_logic_vector(15 downto 0);

   signal decode_valid  : std_logic;
   signal decode_pc     : std_logic_vector(31 downto 0);
   signal decode_instr  : std_logic_vector(31 downto 0);
   signal decode_op     : std_logic_vector(5 downto 0);
   signal decode_rd     : unsigned(4 downto 0);
   signal decode_ra     : unsigned(4 downto 0);
   signal decode_rb     : unsigned(4 downto 0);
   signal decode_func   : std_logic_vector(10 downto 0);
   signal decode_imm16  : std_logic_vector(15 downto 0);
   signal decode_va     : register_type;
   signal decode_vb     : register_type;
   signal decode_imm32  : signed(31 downto 0);
   signal decode_sum    : signed(31 downto 0);
   signal decode_sumi   : signed(31 downto 0);
   signal decode_addr   : std_logic_vector(31 downto 0);
   signal fd_bypass     : unsigned(4 downto 0);

   signal exec_ready       : std_logic;
   signal exec_state       : exec_state_type;
   signal next_exec_state  : exec_state_type;
   signal exec_din         : register_type;
   signal exec_load        : std_logic;
   signal exec_store       : std_logic;
   signal exec_math        : std_logic;
   signal exec_link        : std_logic;
   signal wait_alu         : std_logic;
   signal update_carry     : std_logic;
   signal flush_fetch      : std_logic;
   signal flush_decode     : std_logic;

   signal regs    : register_array_type;
   signal pc      : register_type;     -- Program Counter
   signal btr     : register_type;     -- Branch Target Register
   signal msr     : register_type;     -- Machine Status Register
   signal esr     : register_type;     -- Exception Status Register
   signal ear     : register_type;     -- Exception Address Register
   signal fsr     : register_type;     -- FPU Status Register

   signal ireg       : std_logic_vector(15 downto 0); -- Immediate register
   signal ireg_valid : std_logic;

   signal next_pc    : register_type;

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

   signal alu_start        : std_logic;
   signal alu_ready        : std_logic;
   signal alu_ina          : unsigned(31 downto 0);
   signal alu_inb          : unsigned(31 downto 0);
   signal alu_cin          : unsigned(31 downto 0);
   signal alu_result       : unsigned(31 downto 0);
   signal alu_cout         : std_logic;

begin

   -- Stage 0: Fetch
   process(clk)
   begin
      if clk'event and clk = '1' then
         if exec_ready = '1' then
            fetch_pc <= pc;
            fetch_valid <= iready and exec_ready and not flush_fetch;
         end if;
      end if;
   end process;
   fetch_instr <= iin;
   fetch_op    <= fetch_instr(31 downto 26);
   fetch_rd    <= unsigned(fetch_instr(25 downto 21));
   fetch_ra    <= unsigned(fetch_instr(20 downto 16));
   fetch_rb    <= unsigned(fetch_instr(15 downto 11));
   fetch_func  <= fetch_instr(10 downto 0);
   fetch_imm16 <= fetch_instr(15 downto 0);

   -- Stage 1: Decode
   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            decode_valid <= '0';
         elsif exec_ready = '1' then
            decode_valid   <= fetch_valid and not flush_decode;
            decode_pc      <= fetch_pc;
            decode_instr   <= fetch_instr;
            if fetch_ra = 0 then
               decode_va   <= (others => '0');
            elsif fetch_ra = fd_bypass then
               decode_va   <= std_logic_vector(alu_result);
            else
               decode_va   <= regs(to_integer(fetch_ra));
            end if;
            if fetch_rb = 0 then
               decode_vb   <= (others => '0');
            elsif fetch_rb = fd_bypass then
               decode_vb   <= std_logic_vector(alu_result);
            else
               decode_vb   <= regs(to_integer(fetch_rb));
            end if;
         end if;
      end if;
   end process;
   decode_op      <= decode_instr(31 downto 26);
   decode_rd      <= unsigned(decode_instr(25 downto 21));
   decode_ra      <= unsigned(decode_instr(20 downto 16));
   decode_rb      <= unsigned(decode_instr(15 downto 11));
   decode_func    <= decode_instr(10 downto 0);
   decode_imm16   <= decode_instr(15 downto 0);
   decode_sum     <= signed(decode_va) + signed(decode_vb);
   decode_sumi    <= signed(decode_va) + decode_imm32;
   decode_addr    <= std_logic_vector(decode_sumi) when decode_op(3) = '1'
                     else std_logic_vector(decode_sum);
   daddr <= decode_addr(31 downto 2);
   process(ireg, ireg_valid, decode_imm16)
   begin
      if ireg_valid = '1' then
         decode_imm32 <= signed(ireg) & signed(decode_imm16);
      else
         decode_imm32 <= resize(signed(decode_imm16), 32);
      end if;
   end process;

   -- Stage 2: Execute

   -- Determine the type of instruction.
   process(decode_op, decode_valid, decode_func, decode_ra)
   begin
      exec_load      <= '0';
      exec_store     <= '0';
      exec_math      <= '0';
      exec_link      <= '0';
      wait_alu       <= '0';
      update_carry   <= '0';
      case decode_op is
         when "110000" | "110001" | "110010" 
            | "111000" | "111001" | "111010" =>
            exec_load <= decode_valid;
         when "110100" | "110101" | "110110"
            | "111100" | "111101" | "111110" =>
            exec_store <= decode_valid;
         when "000000" | "000010"      -- add[c]
            | "000001" | "000011"      -- rsub[c]
            | "001000" | "001010"      -- addi[c]
            | "001001" | "001011"   => -- rsubi[c]
            update_carry   <= decode_valid;
            exec_math      <= decode_valid;
         when "000100" | "000110"   -- addk[c]
            | "000101" | "000111"   -- rsubk[c], cmp[u]
            | "001100" | "001110"   -- addik[ck]
            | "001101" | "001111"   -- rsubik[ck]
            | "100000"  -- or, pcmpbf
            | "101000"  -- ori
            | "100001"  -- and
            | "101001"  -- andi
            | "100010"  -- andn, pcmpeq
            | "101010"  -- andni
            | "100011"  -- xor, pcmpne
            | "101011"  -- xori
            | "100100"  -- sra, src, srl, sext, clz, swap, wdc
            | "010001" | "011001" => -- bs, bsi
            exec_math <= decode_valid;
         when "010010"              -- idiv, idivu
            | "010000" | "011000"   -- mul, muli
            | "010110" => -- float
            wait_alu    <= decode_valid;
         when "100110" | "101110" =>
            -- br, bri
            exec_link <= decode_valid and decode_ra(2);
         when others =>
            null;
      end case;
   end process;

   -- Determine the next execution state.
   process(exec_state, decode_op, dready, decode_valid,
           exec_load, exec_store, wait_alu, alu_ready)
   begin
      next_exec_state <= exec_state;
      case exec_state is
         when EXEC_IDLE =>
            if exec_load = '1' or exec_store = '1' then
               next_exec_state <= EXEC_INIT_XFER;
            elsif wait_alu = '1' then
               next_exec_state <= EXEC_ALU;
            end if;
         when EXEC_INIT_XFER =>
            next_exec_state <= EXEC_WAIT_XFER;
         when EXEC_WAIT_XFER =>
            if dready = '1' then
               next_exec_state <= EXEC_IDLE;
            end if;
         when EXEC_ALU =>
            if alu_ready = '1' then
               next_exec_state <= EXEC_IDLE;
            end if;
      end case;
   end process;

   -- Update the execution state.
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

   -- Drive the bypass signal.
   process(exec_math, exec_load, decode_rd, wait_alu, alu_ready)
   begin
      if exec_math = '1' or exec_load = '1' then
         fd_bypass <= decode_rd;
      elsif wait_alu = '1' and alu_ready = '1' then
         fd_bypass <= decode_rd;
      else
         fd_bypass <= (others => '0');
      end if;
   end process;

   -- Update registers.
   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            msr <= (others => '0');
         else
            case exec_state is
               when EXEC_IDLE =>
                  if exec_math = '1' then
                     regs(to_integer(decode_rd))
                        <= std_logic_vector(alu_result);
                  elsif exec_link = '1' then
                     regs(to_integer(decode_rd)) <= decode_pc;
                  end if;
                  if update_carry = '1' then
                     msr(CARRY_BIT) <= alu_cout;
                  end if;
               when EXEC_INIT_XFER =>
                  null;
               when EXEC_WAIT_XFER =>
                  if exec_load = '1' and dready = '1' then
                     regs(to_integer(decode_rd))
                        <= std_logic_vector(alu_result);
                  end if;
               when EXEC_ALU =>
                  if alu_ready = '1' then
                     regs(to_integer(decode_rd))
                        <= std_logic_vector(alu_result);
                  end if;
            end case;
         end if;
      end if;
   end process;

   -- Update immediate.
   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            ireg_valid <= '0';
         elsif decode_valid = '1' and exec_state = EXEC_IDLE then
            if decode_op = "101100" then
               ireg <= decode_imm16;
               ireg_valid <= '1';
            else
               ireg_valid <= '0';
            end if;
         end if;
      end if;
   end process;

   -- Drive the exec_ready signal.
   -- The pipeline is stalled when this is deasserted.
   exec_ready  <= '1' when next_exec_state = EXEC_IDLE else '0';

   -- Drive data memory ports.
   process(clk)
      variable sz_offset   : std_logic_vector(3 downto 0);
      variable value       : std_logic_vector(31 downto 0);
   begin
      if clk'event and clk = '1' then
         dre   <= '0';
         dwe   <= '0';
         dout  <= (others => 'Z');
         dmask <= (others => '0');
         if next_exec_state = EXEC_INIT_XFER then
            dre <= exec_load;
            dwe <= exec_store;
            if decode_rd = 0 then
               value := (others => '0');
            else
               value := regs(to_integer(decode_rd));
            end if;
            sz_offset := decode_op(1 downto 0) & decode_addr(1 downto 0);
            case sz_offset is
               when "00" & "11"  => -- byte 0
                  dmask <= "0001";
                  dout  <= value;
               when "00" & "10"  => -- byte 1
                  dmask <= "0010";
                  dout  <= value(23 downto 0) & x"00";
               when "00" & "01"  => -- byte 2
                  dmask <= "0100";
                  dout  <= value(15 downto 0) & x"0000";
               when "00" & "00"  => -- byte 3
                  dmask <= "1000";
                  dout  <= value(7 downto 0) & x"000000";
               when "01" & "00"  => -- half 0
                  dmask <= "0011";
                  dout  <= value;
               when "01" & "10"  => -- half 2
                  dmask <= "1100";
                  dout  <= value(15 downto 0) & x"0000";
               when others       => -- word
                  dmask <= "1111";  -- word
                  dout  <= value;
            end case;
         end if;
      end if;
   end process;

   -- Select input data.
   process(din, decode_op, decode_addr)
      variable byte_in  : register_type;
      variable half_in  : register_type;
   begin
      case decode_addr(1 downto 0) is
         when "11" =>
            byte_in := std_logic_vector(to_unsigned(0, 24)) & din(7 downto 0);
         when "10" =>
            byte_in := std_logic_vector(to_unsigned(0, 24)) & din(15 downto 8);
         when "01" =>
            byte_in := std_logic_vector(to_unsigned(0, 24)) & din(23 downto 16);
         when others =>
            byte_in := std_logic_vector(to_unsigned(0, 24)) & din(31 downto 24);
      end case;
      if decode_addr(1) = '1' then
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
   process(exec_state, decode_va, decode_vb, decode_imm32)
   begin
      if decode_op(3) = '0' then
         alu_inb <= unsigned(decode_vb);
      else
         alu_inb <= unsigned(decode_imm32);
      end if;
      if exec_state = EXEC_IDLE then
         alu_start <= '1';
      else
         alu_start <= '0';
      end if;
      alu_ina <= unsigned(decode_va);
   end process;

   alu : entity work.blaze_alu
      port map (
         clk      => clk,
         start    => alu_start,
         op       => decode_op,
         func     => decode_func,
         ina      => alu_ina,
         inb      => alu_inb,
         din      => exec_din,
         cin      => msr(CARRY_BIT),
         ready    => alu_ready,
         result   => alu_result,
         cout     => alu_cout
      );

   -- Program counter
   process(pc, decode_valid, decode_op, decode_ra, decode_rd,
           decode_va, decode_vb, decode_pc, decode_sumi, decode_imm32)
      variable pc_plus_vb  : std_logic_vector(31 downto 0);
      variable pc_plus_4   : std_logic_vector(31 downto 0);
      variable pc_plus_imm : std_logic_vector(31 downto 0);
      variable va_plus_imm : std_logic_vector(31 downto 0);
      variable take_branch : boolean;
   begin
      pc_plus_vb  := std_logic_vector(signed(decode_pc) + signed(decode_vb));
      pc_plus_4   := std_logic_vector(signed(pc) + to_signed(4, 32));
      pc_plus_imm := std_logic_vector(signed(decode_pc) + decode_imm32);
      va_plus_imm := std_logic_vector(signed(decode_va) + decode_imm32);
      flush_fetch    <= '0';
      flush_decode   <= '0';
      next_pc        <= pc_plus_4;
      case decode_rd(3 downto 0) is
         when "0000" => -- beq
            take_branch := unsigned(decode_va) = 0;
         when "0001" => -- bne
            take_branch := unsigned(decode_va) /= 0;
         when "0010" => -- blt
            take_branch := signed(decode_va) < 0;
         when "0011" => -- ble
            take_branch := signed(decode_va) <= 0;
         when "0100" => -- bgt
            take_branch := signed(decode_va) > 0;
         when "0101" => -- bge
            take_branch := signed(decode_va) >= 0;
         when others =>
            take_branch := false;
      end case;
      if decode_valid = '1' and decode_op = "100110" then
         -- br
         if decode_ra(3) = '1' then
            next_pc <= decode_vb;
         else
            next_pc <= pc_plus_vb;
         end if;
         flush_decode   <= not decode_ra(4);
         flush_fetch    <= '1';
      elsif decode_valid = '1' and decode_op = "101110" then
         -- bri
         if decode_ra(3) = '1' then
            next_pc <= std_logic_vector(decode_imm32);
         else
            next_pc <= pc_plus_imm;
         end if;
         flush_decode   <= not decode_ra(4);
         flush_fetch    <= '1';
      elsif decode_valid = '1' and decode_op = "101111" then
         -- bcci
         if take_branch then
            flush_decode   <= not decode_rd(4);
            flush_fetch    <= '1';
            next_pc        <= pc_plus_imm;
         end if;
      elsif decode_valid = '1' and decode_op = "100111" then
         -- bcc
         if take_branch then
            flush_decode   <= not decode_rd(4);
            flush_fetch    <= '1';
            next_pc        <= std_logic_vector(decode_imm32);
         end if;
      elsif decode_valid = '1' and decode_op = "101101" then
         -- rt
         next_pc     <= va_plus_imm;
         flush_fetch <= '1';
      end if;
   end process;

   process(clk)
   begin
      if clk'event and clk = '1' then
         if rst = '1' then
            pc <= (others => '0');
         elsif iready = '1' and exec_ready = '1' then
            pc <= next_pc;
         end if;
      end if;
   end process;
   iaddr <= pc(31 downto 2);
   ire   <= iready and exec_ready;

end blaze_arch;
