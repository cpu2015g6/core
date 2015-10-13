library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common.all;
use work.all;

entity cpu_top is
	port(
		clk, rst : in std_logic
	);
end cpu_top;

architecture twoproc of cpu_top is
	type program_memory_type is array (0 to 2**pc_width-1) of std_logic_vector(31 downto 0);
	type rob_array_type is array (0 to 2**rob_num_width-1) of rob_type;
	type rob_ring_buffer_type is record
		rob_array : rob_array_type;
		youngest, oldest : std_logic_vector(rob_num_width-1 downto 0);
	end record;
	constant rob_ring_buffer_zero : rob_ring_buffer_type := (
		(others => rob_zero),
		(others => '0'), (others => '0')
	);
	function rob_full(rob : rob_ring_buffer_type) return boolean is
	begin
		return rob.rob_array(to_integer(unsigned(rob.youngest))).state /= ROB_Invalid;
	end rob_full;
--	function rob_push(rob : rob_ring_buffer_type, x : rob_type) return rob_ring_buffer_type is
--		variable v : rob_ring_buffer_type;
--	begin
--		rob.rob_array(to_integer(unsigned(rob.youngest))) := x;
--	end rob_push;
	type CPU_state_type is (CPU_NORMAL, CPU_MISPREDICTION, CPU_LOADING);
	type reg_type is record
		decode_result : decode_result_type;
		cdb : cdb_type;
		registers : register_array_type;
		pc : pc_type;
		program_memory : program_memory_type;
		rob : rob_ring_buffer_type;
		state : CPU_state_type;
	end record;
	constant reg_zero : reg_type := (
		decode_result_zero,
		cdb_zero,
		register_array_zero,
		(others => '0'),
		(others => (others => '0')),
		rob_ring_buffer_zero,
		CPU_NORMAL
	);
	signal r, r_in : reg_type := reg_zero;
	component alu is
	port(
		clk, rst : in std_logic;
		alu_in : in alu_pack.in_type;
		alu_out : out alu_pack.out_type);
	end component;
	signal alu_in : alu_pack.in_type := alu_pack.in_zero;
	signal alu_out : alu_pack.out_type;
	component fpu is
		port(
			clk, rst : in std_logic;
			fpu_in : in fpu_pack.in_type;
			fpu_out : out fpu_pack.out_type);
	end component;
	signal fpu_in : fpu_pack.in_type;
	signal fpu_out : fpu_pack.out_type;
	component mem is
		port(
			clk, rst : in std_logic;
			mem_in : in mem_pack.in_type;
			mem_out : out mem_pack.out_type);
	end component;
	signal mem_in : mem_pack.in_type;
	signal mem_out : mem_pack.out_type;
	signal branch_in : branch_pack.in_type;
	signal branch_out : branch_pack.out_type;
	-- determines the next pc
	procedure branch_predictor(
		decode_result : in decode_result_type;
		pc : in pc_type;
		next_pc : out pc_type) is
	begin
		case decode_result.opc is
		when J_opc =>
			next_pc := std_logic_vector(signed(pc) + signed(decode_result.imm));
		when others =>
			next_pc := std_logic_vector(unsigned(pc) + 1);
		end case;
	end branch_predictor;
	procedure inst_decode(
		inst : in std_logic_vector(31 downto 0);
		decode_result : out decode_result_type) is
		alias opc_rev1 : std_logic_vector(7 downto 0) is inst(31 downto 24);
		alias rt_rev1 : std_logic_vector(7 downto 0) is inst(23 downto 16);
		alias ra_rev1 : std_logic_vector(7 downto 0) is inst(15 downto 8);
		alias rb_rev1 : std_logic_vector(7 downto 0) is inst(7 downto 0);
		alias imm_rev1 : std_logic_vector(15 downto 0) is inst(15 downto 0);
--		alias opc_rev2 : std_logic_vector(5 downto 0) is inst(31 downto 26);
	begin
		decode_result := decode_result_zero;
		case opc_rev1 is
		when x"D0" => --limm
			decode_result.opc := LIMM_opc;
			decode_result.rt := rt_rev1;
			decode_result.imm := imm_rev1;
		when x"D4" => --j
			decode_result.opc := J_opc;
		when x"D5" => --jz
			decode_result.opc := JZ_opc;
		when x"D6" => --jr
			decode_result.opc := JR_opc;
		when x"D8" => --stw
			decode_result.opc := STW_opc;
		when x"D9" => --ldw
			decode_result.opc := LDW_opc;
		when x"E0" => --add
			decode_result.opc := ADD_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"E1" => --sub
			decode_result.opc := SUB_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"E2" => --and
			decode_result.opc := AND_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"E3" => --or
			decode_result.opc := OR_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"E4" => --xor
			decode_result.opc := XOR_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"E5" => --not
			decode_result.opc := NOT_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"E6" => --shl
			decode_result.opc := SHL_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"E7" => --shr
			decode_result.opc := SHR_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"F0" => --eq
			decode_result.opc := EQ_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"F1" => --neq
			decode_result.opc := NEQ_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"F2" => --gt
			decode_result.opc := GT_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"F3" => --gte
			decode_result.opc := GTE_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"F8" => --fadd
			decode_result.opc := FADD_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"F9" => --fmul
			decode_result.opc := FMUL_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"FA" => --fdiv
		when x"FB" => --fsin
		when x"FC" => --fcos
		when x"FD" => --fatan
		when x"FE" => --fsqrt
		when others => --undefined instruction
		end case;
	end inst_decode;
	function read_reg(
		reg_num : std_logic_vector(7 downto 0);
		regs : register_array_type;
		rob_array : rob_array_type
	) return register_type is
		variable rob_i : integer;
		variable reg : register_type;
	begin
		reg := regs(to_integer(unsigned(reg_num)));
		rob_i := to_integer(unsigned(reg.tag.rob_num));
		-- What about ROB_Reset?
		if reg.tag.valid = '1' and rob_array(rob_i).state = ROB_Done then
			reg := (data => rob_array(rob_i).result, tag => rs_tag_zero);
		end if;
		if reg_num = x"FF" then
			return register_zero;
		else
			return reg;
		end if;
	end read_reg;
	procedure read_regs(
		decode_result : in decode_result_type;
		regs : in register_array_type;
		rob_array : in rob_array_type;
		rob_num : in rob_num_type;
		unit : out unit_type;
		alu_rs : out alu_pack.rs_type;
		fpu_rs : out fpu_pack.rs_type;
		mem_rs : out mem_pack.rs_type;
		branch_rs : out branch_pack.rs_type) is
	variable ra, rb : register_type;
	variable rs_common_3 : rs_common_type;
	variable zext_imm : std_logic_vector(31 downto 0);
	begin
		ra := read_reg(decode_result.ra, regs, rob_array);
		rb := read_reg(decode_result.rb, regs, rob_array);
		alu_rs := alu_pack.rs_zero;
		fpu_rs := fpu_pack.rs_zero;
		mem_rs := mem_pack.rs_zero;
		branch_rs := branch_pack.rs_zero;
		zext_imm := x"0000" & decode_result.imm;
		rs_common_3 := (
			ra => ra,
			rb => rb,
			state => RS_Waiting,
			result => (others => '0'),
			rt_num => decode_result.rt,
			rob_num => rob_num
		);
		case decode_result.opc is
		when LIMM_opc =>
			unit := ALU_UNIT;
			alu_rs.op := alu_pack.LIMM_op;
			alu_rs.common := (
				ra => (data => zext_imm, tag => rs_tag_zero),
				rb => register_zero,
				state => RS_Waiting,
				result => (others => '0'),
				rt_num => decode_result.rt,
				rob_num => rob_num
			);
		when ADD_opc =>
			unit := ALU_UNIT;
			alu_rs.op := alu_pack.ADD_op;
			alu_rs.common := rs_common_3;
		when SUB_opc =>
			unit := ALU_UNIT;
			alu_rs.op := alu_pack.SUB_op;
			alu_rs.common := rs_common_3;
		when AND_opc =>
		when OR_opc =>
		when XOR_opc =>
		when NOT_opc =>
		when FADD_opc =>
		when FMUL_opc =>
		when J_opc =>
		when JZ_opc =>
		when JR_opc =>
		when NOP_opc =>
			unit := NULL_UNIT;
		when others =>
		end case;
	end read_regs;
	procedure cdb_arbiter(
		alu_cdb_out, fpu_cdb_out, mem_cdb_out, branch_cdb_out : in cdb_type;
		alu_grant, fpu_grant, mem_grant, branch_grant : out std_logic;
		cdb : out cdb_type
	) is
	begin
		alu_grant := '0';
		fpu_grant := '0';
		mem_grant := '0';
		branch_grant := '0';
		if alu_cdb_out.tag.valid = '1' then
			alu_grant := '1';
			cdb := alu_cdb_out;
		elsif fpu_cdb_out.tag.valid = '1' then
			fpu_grant := '1';
			cdb := fpu_cdb_out;
		elsif mem_cdb_out.tag.valid = '1' then
			mem_grant := '1';
			cdb := mem_cdb_out;
		elsif branch_cdb_out.tag.valid = '1' then
			branch_grant := '1';
			cdb := branch_cdb_out;
		else
			cdb := cdb_zero;
		end if;
	end cdb_arbiter;
	function update_ROB(rob_array : rob_array_type;cdb : cdb_type) return rob_array_type is
		variable new_rob_array : rob_array_type;
		variable rob_i : integer;
	begin
		new_rob_array := rob_array;
		rob_i := to_integer(unsigned(cdb.tag.rob_num));
		if cdb.tag.valid = '1' then
			new_rob_array(rob_i).result := cdb.data;
			new_rob_array(rob_i).state := ROB_Done;
			if cdb.reset = '1' then
				new_rob_array(rob_i).state := ROB_RESET;
				new_rob_array(rob_i).pc_restart := cdb.pc_restart;
			end if;
		end if;
	end update_ROB;
begin
	alu_l : alu
	port map(clk => clk, rst => rst, alu_in => alu_in, alu_out => alu_out);
	fpu_l : fpu
	port map(clk => clk, rst => rst, fpu_in => fpu_in, fpu_out => fpu_out);
	mem_l : mem
	port map(clk => clk, rst => rst, mem_in => mem_in, mem_out => mem_out);
	process(clk, rst)
	begin
		if rst = '1' then
			r <= reg_zero;
		elsif rising_edge(clk) then
			r <= r_in;
		end if;
	end process;
	process(r, alu_out, fpu_out, mem_out, branch_out)
		variable v : reg_type;
		variable alu_in_v : alu_pack.in_type;
		variable fpu_in_v : fpu_pack.in_type;
		variable mem_in_v : mem_pack.in_type;
		variable branch_in_v : branch_pack.in_type;
		variable alu_rs_v : alu_pack.rs_type;
		variable fpu_rs_v : fpu_pack.rs_type;
		variable mem_rs_v : mem_pack.rs_type;
		variable branch_rs_v : branch_pack.rs_type;
		variable decode_result_v : decode_result_type;
		variable next_pc : pc_type;
		variable unit : unit_type;
		variable stall : boolean;
		variable oldest_rob : rob_type;
	begin
		v := r;
		alu_in_v := alu_pack.in_zero;
		-- update ROB
		if r.cdb.tag.valid = '1' then
			v.rob.rob_array(to_integer(unsigned(r.cdb.tag.rob_num))).result := r.cdb.data;
		end if;
		-- decode instruction
		inst_decode(
			inst => r.program_memory(to_integer(unsigned(r.pc))),
			decode_result => decode_result_v
		);
		-- branch prediction
		branch_predictor(
			decode_result => decode_result_v,
			pc => r.pc,
			next_pc => next_pc
		);
		decode_result_v.pc := r.pc;
		decode_result_v.pc_predicted := next_pc;
		-- read register and issue
		read_regs(
			decode_result => r.decode_result,
			regs => r.registers,
			rob_array => v.rob.rob_array,
			rob_num => r.rob.youngest,
			unit => unit,
			alu_rs => alu_rs_v,
			fpu_rs => fpu_rs_v,
			mem_rs => mem_rs_v,
			branch_rs => branch_rs_v
		);
		stall := rob_full(r.rob);
		case unit is
			when ALU_UNIT =>
				stall := stall or alu_out.rs_full = '1';
			when FPU_UNIT =>
				stall := stall or fpu_out.rs_full = '1';
			when MEM_UNIT =>
				stall := stall or mem_out.rs_full = '1';
			when BRANCH_UNIT =>
				stall := stall or branch_out.rs_full = '1';
			when NULL_UNIT =>
		end case;
		alu_in_v.cdb_in := r.cdb;
		fpu_in_v.cdb_in := r.cdb;
		mem_in_v.cdb_in := r.cdb;
		if not stall then
			v.decode_result := decode_result_v;
			v.pc := next_pc;
			alu_in_v.rs_in := alu_rs_v;
			fpu_in_v.rs_in := fpu_rs_v;
			mem_in_v.rs_in := mem_rs_v;
			v.rob.rob_array(to_integer(unsigned(r.rob.youngest))) := (
				state => ROB_Executing,
--				pc_inst => r.decode_result.pc,
				pc_restart => (others => '0'),
				result => (others => '0'),
				reg_num => r.decode_result.rt
			);
			v.rob.youngest := std_logic_vector(unsigned(r.rob.youngest) + 1);
		end if;
		cdb_arbiter(
			alu_cdb_out => alu_out.cdb_out,
			fpu_cdb_out => fpu_out.cdb_out,
			mem_cdb_out => mem_out.cdb_out,
			branch_cdb_out => branch_out.cdb_out,
			alu_grant => alu_in_v.cdb_next,
			fpu_grant => fpu_in_v.cdb_next,
			mem_grant => mem_in_v.cdb_next,
			branch_grant => branch_in_v.cdb_next,
			cdb => v.cdb
		);
		-- commit ROB
		oldest_rob := v.rob.rob_array(to_integer(unsigned(v.rob.oldest)));
		if oldest_rob.state = ROB_Done then
			v.registers(to_integer(unsigned(oldest_rob.reg_num))) := (
				data => oldest_rob.result,
				tag =>rs_tag_zero
			);
			v.rob.rob_array(to_integer(unsigned(v.rob.oldest))) := rob_zero;
			v.rob.oldest := std_logic_vector(unsigned(v.rob.oldest) + 1);
		elsif oldest_rob.state = ROB_Reset then
		end if;
		alu_in <= alu_in_v;
		fpu_in <= fpu_in_v;
		mem_in <= mem_in_v;
		r_in <= v;
	end process;
end;