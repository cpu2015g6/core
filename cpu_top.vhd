library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blockram is
	generic(
		dwidth : integer;
		awidth : integer
	);
	port(
		clk, we : in std_logic;
		addr : in std_logic_vector(awidth-1 downto 0);
		din : in std_logic_vector(dwidth-1 downto 0);
		dout : out std_logic_vector(dwidth-1 downto 0)
	);
end entity;

architecture beh of blockram is
	type ram_type is array(0 to 2**awidth-1) of std_logic_vector(dwidth-1 downto 0);
--	signal ram : ram_type := (others => (others => '0'));
	signal ram : ram_type := (
-- loopback
--		0 => x"d2010000",
--		1 => x"d3010000",
--		2 => x"d4fffffe",
-- infinite pohe
--		0 => x"d0010070",
--		1 => x"d002006f",
--		2 => x"d0030068",
--		3 => x"d0040065",
--		4 => x"d3010000",
--		5 => x"d3020000",
--		6 => x"d3030000",
--		7 => x"d3040000",
--		8 => x"d4fffff8",
-- fib_rec_infinite
0 => x"d0010000",
1 => x"e30801ff",
2 => x"d4020005",
3 => x"d3080000",
4 => x"d0080001",
5 => x"e0010108",
6 => x"d4fffffb",
7 => x"d0090002",
8 => x"d1090809",
9 => x"d01e0020",
10 => x"f4ff091e",
11 => x"d0090001",
12 => x"d80302ff",
13 => x"e0030309",
14 => x"d80308ff",
15 => x"e0030309",
16 => x"e1080809",
17 => x"d402fff6",
18 => x"d0090001",
19 => x"e1090309",
20 => x"d90a09ff",
21 => x"d80908ff",
22 => x"d0090002",
23 => x"e1080a09",
24 => x"d402ffef",
25 => x"d0090001",
26 => x"e1030309",
27 => x"d90a03ff",
28 => x"e008080a",
29 => x"e1030309",
30 => x"d90203ff",
31 => x"d5ff02ff",
32 => x"d0080001",
33 => x"d5ff02ff",
-- mem
--0 => x"d0020000",
--1 => x"d80202ff",
--2 => x"d90102ff",
--3 => x"d3010000",
--4 => x"d0010001",
--5 => x"e0020201",
--6 => x"d4fffffb",
-- fpu
--0 => x"d0013f80",
--1 => x"d0020010",
--2 => x"e6010102",
--3 => x"f8020101",
--4 => x"f9020101",
--5 => x"fa020101",
--6 => x"fe0201ff",
		others => (others => '0')
	);
	signal reg_addr : std_logic_vector(awidth-1 downto 0) := (others => '0');
begin
	process(clk)
	begin
		if rising_edge(clk) then
			if we = '1' then
				ram(to_integer(unsigned(addr))) <= din;
			end if;
			reg_addr <= addr;
		end if;
	end process;
	dout <= ram(to_integer(unsigned(reg_addr)));
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common.all;
use work.all;

entity cpu_top is
	port(
		clk, rst : in std_logic;
		cpu_top_in : in cpu_top_in_type;
		cpu_top_out : out cpu_top_out_type
	);
end cpu_top;

architecture twoproc of cpu_top is
	component blockram is
		generic(
			dwidth : integer;
			awidth : integer
		);
		port(
			clk, we : in std_logic;
			addr : in std_logic_vector(awidth-1 downto 0);
			din : in std_logic_vector(dwidth-1 downto 0);
			dout : out std_logic_vector(dwidth-1 downto 0)
		);
	end component;
	signal bram_we : std_logic := '0';
	signal bram_addr, bram_addr_cpu, bram_addr_pl : std_logic_vector(pc_width-1 downto 0) := (others => '0');
	signal bram_din : std_logic_vector(31 downto 0) := (others => '0');
	signal bram_dout : std_logic_vector(31 downto 0) := (others => '0');
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
		rob : rob_ring_buffer_type;
		state : CPU_state_type;
		inst_valid, state_sub : std_logic;
	end record;
	constant reg_zero : reg_type := (
		decode_result_zero,
		cdb_zero,
		register_array_zero,
		(others => '0'),
		rob_ring_buffer_zero,
		CPU_LOADING,
		'0', '0'
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
	signal fpu_in : fpu_pack.in_type := fpu_pack.in_zero;
	signal fpu_out : fpu_pack.out_type;
	component mem is
		port(
			clk, rst : in std_logic;
		rs_in_op : in mem_pack.op_type;
		rs_in_has_dummy : in std_logic;
		rs_in_common : in rs_common_type;
		cdb_in : in cdb_type;
		cdb_next : in std_logic;-- set cdb_next = 1 when cdb_out is broadcasted
		sync_rst : in std_logic;-- synchronous reset
		dummy_done : in std_logic;
		sramifout : in sramif_out;
		recvifout : in recvif_out_type;
		transifout : in transif_out_type;
			mem_out : out mem_pack.out_type);
	end component;
	component branch is
		port(
			clk, rst : in std_logic;
			branch_in : in branch_pack.in_type;
			branch_out : out branch_pack.out_type);
	end component;
	signal mem_in : mem_pack.in_type := mem_pack.in_zero;
	signal mem_out : mem_pack.out_type;
	signal branch_in : branch_pack.in_type := branch_pack.in_zero;
	signal branch_out : branch_pack.out_type;
	component program_loader is
		port(
			clk, rst : in std_logic;
			go : in std_logic;
			active : out std_logic;
			bram_we : out std_logic;
			bram_addr : out std_logic_vector(pc_width-1 downto 0);
			bram_din : out std_logic_vector(31 downto 0);
			recvifout : in recvif_out_type;
			recvifin : out recvif_in_type;
			sramifin : out sramif_in
		);
	end component;
	signal recvifin_pl : recvif_in_type;
	signal sramifin_pl : sramif_in;
	signal go_pl, active_pl : std_logic := '0';
	-- determines the next pc
	procedure branch_predictor(
		decode_result : in decode_result_type;
		pc : in pc_type;
		next_pc : out pc_type) is
	begin
		case decode_result.opc is
		when J_opc =>
			next_pc := std_logic_vector(signed(pc) + signed(decode_result.imm(pc_width-1 downto 0)));
		when others =>
			next_pc := std_logic_vector(unsigned(pc) + 1);
		end case;
	end branch_predictor;
	procedure inst_decode(
		inst : in std_logic_vector(31 downto 0);
		decode_result : out decode_result_type) is
		alias opc_rev1 : std_logic_vector(7 downto 0) is inst(31 downto 24);
--		alias rt_rev1 : std_logic_vector(7 downto 0) is inst(23 downto 16);
--		alias ra_rev1 : std_logic_vector(7 downto 0) is inst(15 downto 8);
--		alias rb_rev1 : std_logic_vector(7 downto 0) is inst(7 downto 0);
		alias rt_rev1 : std_logic_vector(reg_num_width-1 downto 0) is inst(15+reg_num_width downto 16);
		alias ra_rev1 : std_logic_vector(reg_num_width-1 downto 0) is inst(7+reg_num_width downto 8);
		alias rb_rev1 : std_logic_vector(reg_num_width-1 downto 0) is inst(reg_num_width-1 downto 0);
		alias imm_rev1 : std_logic_vector(15 downto 0) is inst(15 downto 0);
--		alias opc_rev2 : std_logic_vector(5 downto 0) is inst(31 downto 26);
	begin
		decode_result := decode_result_zero;
		case opc_rev1 is
		when x"D0" => --limm
			decode_result.opc := LIMM_opc;
			decode_result.rt := rt_rev1;
			decode_result.imm := imm_rev1;
		when x"D1" => --cmp
			decode_result.opc := CMP_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"D2" => --in
			decode_result.opc := IN_opc;
			decode_result.rt := rt_rev1;
			decode_result.need_dummy_rob_entry := '1';
		when x"D3" => --out
			decode_result.opc := OUT_opc;
			decode_result.ra := rt_rev1;
			decode_result.need_dummy_rob_entry := '1';
		when x"D4" => --j
			decode_result.opc := J_opc;
			decode_result.rt := rt_rev1;
			decode_result.imm := imm_rev1;
		when x"D5" => --jr
			decode_result.opc := JR_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
		when x"D8" => --stw
			decode_result.opc := STW_opc;
			decode_result.ra := ra_rev1;
			decode_result.rb := rt_rev1;
			decode_result.need_dummy_rob_entry := '1';
		when x"D9" => --ldw
			decode_result.opc := LDW_opc;
			decode_result.ra := ra_rev1;
			decode_result.rt := rt_rev1;
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
		when x"E6" => --sll
			decode_result.opc := SLL_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"E7" => --srl
			decode_result.opc := SRL_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"F0" => --jreq
			decode_result.opc := JREQ_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"F1" => --jrneq
			decode_result.opc := JRNEQ_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"F2" => --jrgt
			decode_result.opc := JRGT_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"F3" => --jrgte
			decode_result.opc := JRGTE_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"F4" => --jrlt
			decode_result.opc := JRLT_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"F5" => --jrlte
			decode_result.opc := JRLTE_opc;
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
		when x"FA" => --finv
			decode_result.opc := FINV_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when x"FE" => --fsqrt
			decode_result.opc := FSQRT_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
		when x"FF" => --fcmp
			decode_result.opc := FCMP_opc;
			decode_result.rt := rt_rev1;
			decode_result.ra := ra_rev1;
			decode_result.rb := rb_rev1;
		when others => --undefined instruction
			report "undefined instruction" severity warning;
		end case;
	end inst_decode;
	function read_reg(
		reg_num : reg_num_type;
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
		if reg_num = reg_num_zero then
			return register_zero;
		else
			return reg;
		end if;
	end read_reg;
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
			if rob_array(rob_i).reg_num /= reg_num_zero then
				new_rob_array(rob_i).result := cdb.data;
			end if;
			new_rob_array(rob_i).state := ROB_Done;
			if cdb.pc_next /= rob_array(rob_i).pc_next then
				new_rob_array(rob_i).state := ROB_RESET;
				new_rob_array(rob_i).pc_next := cdb.pc_next;
			end if;
		end if;
		return new_rob_array;
	end update_ROB;
begin
	bram_addr_cpu <= r_in.pc;
	bram_addr <= bram_addr_pl when r.state = CPU_LOADING else bram_addr_cpu;
	cpu_top_out.sramifin <= sramifin_pl when r.state = CPU_LOADING else mem_out.sramifin;
	cpu_top_out.recvifin <= recvifin_pl when r.state = CPU_LOADING else mem_out.recvifin;
	cpu_top_out.transifin <= mem_out.transifin;
	bram_l : blockram
	generic map(awidth => pc_width, dwidth => 32)
	port map(clk => clk, we => bram_we, din => bram_din, dout => bram_dout, addr => bram_addr);
	alu_l : alu
	port map(clk => clk, rst => rst, alu_in => alu_in, alu_out => alu_out);
	fpu_l : fpu
	port map(clk => clk, rst => rst, fpu_in => fpu_in, fpu_out => fpu_out);
	mem_l : mem
	port map(
		clk => clk,
		rst => rst,
		rs_in_op => mem_in.rs_in.op,
		rs_in_has_dummy => mem_in.rs_in.has_dummy,
		rs_in_common => mem_in.rs_in.common,
		cdb_in => mem_in.cdb_in,
		cdb_next => mem_in.cdb_next,
		sync_rst => mem_in.rst,
		dummy_done => mem_in.dummy_done,
		sramifout => mem_in.sramifout,
		recvifout => mem_in.recvifout,
		transifout => mem_in.transifout,
		mem_out => mem_out);
	branch_l : branch
	port map(clk => clk, rst => rst, branch_in => branch_in, branch_out => branch_out);
	program_loader_l : program_loader
	port map(
		clk => clk,
		rst => rst,
		go => go_pl,
		active => active_pl,
		bram_we => bram_we,
		bram_addr => bram_addr_pl,
		bram_din => bram_din,
		recvifout => cpu_top_in.recvifout,
		recvifin => recvifin_pl,
		sramifin => sramifin_pl
	);
	process(clk, rst)
	begin
		if rst = '1' then
			r <= reg_zero;
		elsif rising_edge(clk) then
			r <= r_in;
		end if;
	end process;
	comb : process(r, alu_out, fpu_out, mem_out, branch_out, bram_dout, cpu_top_in, active_pl)
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
		-- read_regs
		variable ra, rb : register_type;
		variable rs_common_3 : rs_common_type;
		variable zext_imm : std_logic_vector(31 downto 0);
		variable insert_dummy_rob_entry : boolean;
		variable go_pl_v : std_logic;
	begin
		v := r;
		alu_in_v := alu_pack.in_zero;
		fpu_in_v := fpu_pack.in_zero;
		mem_in_v := mem_pack.in_zero;
		branch_in_v := branch_pack.in_zero;
		go_pl_v := '0';
		case r.state is
		when CPU_NORMAL =>
		if r.inst_valid = '0' then
			v.pc := r.pc;
			v.inst_valid := '1';
		else
			-- update ROB
			v.rob.rob_array := update_ROB(r.rob.rob_array, r.cdb);
			-- decode instruction
			inst_decode(
				inst => bram_dout,
				decode_result => decode_result_v
			);
			if decode_result_v.need_dummy_rob_entry = '1' and r.decode_result.need_dummy_rob_entry = '1' then
				decode_result_v.need_dummy_rob_entry := '0';
			end if;
			-- branch prediction
			branch_predictor(
				decode_result => decode_result_v,
				pc => r.pc,
				next_pc => next_pc
			);
			if decode_result_v.need_dummy_rob_entry = '1' then
				next_pc := r.pc;
			end if;
			decode_result_v.pc := r.pc;
			decode_result_v.pc_predicted := next_pc;
			-- read registers and issue
			ra := read_reg(r.decode_result.ra, r.registers, v.rob.rob_array);
			rb := read_reg(r.decode_result.rb, r.registers, v.rob.rob_array);
			alu_rs_v := alu_pack.rs_zero;
			fpu_rs_v := fpu_pack.rs_zero;
			mem_rs_v := mem_pack.rs_zero;
			branch_rs_v := branch_pack.rs_zero;
			zext_imm := x"0000" & r.decode_result.imm;
			rs_common_3 := (
				ra => ra,
				rb => rb,
				state => RS_Waiting,
				result => (others => '0'),
				rob_num => r.rob.youngest,
				pc => r.decode_result.pc,
				pc_next => (others => '0')
			);
			unit := NULL_UNIT;
			case r.decode_result.opc is
			when LIMM_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.LIMM_op;
				alu_rs_v.common := (
					ra => (data => zext_imm, tag => rs_tag_zero),
					rb => register_zero,
					state => RS_Waiting,
					result => (others => '0'),
					rob_num => r.rob.youngest,
					pc => r.decode_result.pc,
					pc_next => (others => '0')
				);
			when CMP_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.CMP_op;
				alu_rs_v.common := rs_common_3;
			when ADD_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.ADD_op;
				alu_rs_v.common := rs_common_3;
			when SUB_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.SUB_op;
				alu_rs_v.common := rs_common_3;
			when AND_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.AND_op;
				alu_rs_v.common := rs_common_3;
			when OR_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.OR_op;
				alu_rs_v.common := rs_common_3;
			when XOR_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.XOR_op;
				alu_rs_v.common := rs_common_3;
			when NOT_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.NOT_op;
				alu_rs_v.common := rs_common_3;
			when SLL_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.SLL_op;
				alu_rs_v.common := rs_common_3;
			when SRL_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.SRL_op;
				alu_rs_v.common := rs_common_3;
			when FADD_opc =>
				unit := FPU_UNIT;
				fpu_rs_v.op := fpu_pack.FADD_op;
				fpu_rs_v.common := rs_common_3;
			when FMUL_opc =>
				unit := FPU_UNIT;
				fpu_rs_v.op := fpu_pack.FMUL_op;
				fpu_rs_v.common := rs_common_3;
			when FINV_opc =>
				unit := FPU_UNIT;
				fpu_rs_v.op := fpu_pack.FINV_op;
				fpu_rs_v.common := rs_common_3;
			when FSQRT_opc =>
				unit := FPU_UNIT;
				fpu_rs_v.op := fpu_pack.FSQRT_op;
				fpu_rs_v.common := rs_common_3;
			when FCMP_opc =>
				unit := FPU_UNIT;
				fpu_rs_v.op := fpu_pack.FCMP_op;
				fpu_rs_v.common := rs_common_3;
			when J_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.J_op;
				branch_rs_v.common := (
					ra => (data => zext_imm, tag => rs_tag_zero),
					rb => register_zero,
					state => RS_Waiting,
					result => (others => '0'),
					rob_num => r.rob.youngest,
					pc => r.decode_result.pc,
					pc_next => (others => '0')
				);
			when JR_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.JR_op;
				branch_rs_v.common := (
					ra => ra,
					rb => register_zero,
					state => RS_Waiting,
					result => (others => '0'),
					rob_num => r.rob.youngest,
					pc => r.decode_result.pc,
					pc_next => (others => '0')
				);
			when JREQ_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.JREQ_op;
				branch_rs_v.common := rs_common_3;
			when JRNEQ_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.JRNEQ_op;
				branch_rs_v.common := rs_common_3;
			when JRGT_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.JRGT_op;
				branch_rs_v.common := rs_common_3;
			when JRGTE_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.JRGTE_op;
				branch_rs_v.common := rs_common_3;
			when JRLT_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.JRLT_op;
				branch_rs_v.common := rs_common_3;
			when JRLTE_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.JRLTE_op;
				branch_rs_v.common := rs_common_3;
			when STW_opc =>
				unit := MEM_UNIT;
				mem_rs_v.op := mem_pack.STORE_op;
				mem_rs_v.has_dummy := '1';
				mem_rs_v.common := (
					ra => ra,
					rb => rb,
					state => RS_Waiting,
					result => (others => '0'),
					rob_num => r.rob.youngest,
					pc => r.decode_result.pc,
					pc_next => (others => '0')
				);
			when LDW_opc =>
				unit := MEM_UNIT;
				mem_rs_v.op := mem_pack.LOAD_op;
				mem_rs_v.common := (
					ra => ra,
					rb => register_zero,
					state => RS_Waiting,
					result => (others => '0'),
					rob_num => r.rob.youngest,
					pc => r.decode_result.pc,
					pc_next => (others => '0')
				);
			when NOP_opc =>
				unit := NULL_UNIT;
			when IN_opc =>
				unit := MEM_UNIT;
				mem_rs_v.op := mem_pack.IN_op;
				mem_rs_v.has_dummy := '1';
				mem_rs_v.common := (
					ra => register_zero,
					rb => register_zero,
					state => RS_Waiting,
					result => (others => '0'),
					rob_num => r.rob.youngest,
					pc => r.decode_result.pc,
					pc_next => (others => '0')
				);
			when OUT_opc =>
				unit := MEM_UNIT;
				mem_rs_v.op := mem_pack.OUT_op;
				mem_rs_v.has_dummy := '1';
				mem_rs_v.common := (
					ra => ra,
					rb => register_zero,
					state => RS_Waiting,
					result => (others => '0'),
					rob_num => r.rob.youngest,
					pc => r.decode_result.pc,
					pc_next => (others => '0')
				);
			when others =>
			end case;
			if r.decode_result.need_dummy_rob_entry = '1' then
				unit := NULL_UNIT;
				alu_rs_v := alu_pack.rs_zero;
				fpu_rs_v := fpu_pack.rs_zero;
				mem_rs_v := mem_pack.rs_zero;
				branch_rs_v := branch_pack.rs_zero;
			end if;
			stall := rob_full(r.rob);
			assert not stall report "rob full" severity note;
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
			branch_in_v.cdb_in := r.cdb;
			if not stall then
				v.decode_result := decode_result_v;
				v.pc := next_pc;
				alu_in_v.rs_in := alu_rs_v;
				fpu_in_v.rs_in := fpu_rs_v;
				mem_in_v.rs_in := mem_rs_v;
				branch_in_v.rs_in := branch_rs_v;
				if r.decode_result.opc /= NOP_opc then
					if r.decode_result.need_dummy_rob_entry = '1' then
						v.rob.rob_array(to_integer(unsigned(r.rob.youngest))) := (
							state => ROB_Dummy,
							pc_next => r.decode_result.pc_predicted,
							result => (others => '0'),
							reg_num => reg_num_zero
						);
					else
						v.rob.rob_array(to_integer(unsigned(r.rob.youngest))) := (
							state => ROB_Executing,
							pc_next => r.decode_result.pc_predicted,
							result => (others => '0'),
							reg_num => r.decode_result.rt
						);
					end if;
					if r.decode_result.rt /= reg_num_zero then
						v.registers(to_integer(unsigned(r.decode_result.rt))).tag := (
							valid => '1',
							rob_num => r.rob.youngest
						);
					end if;
					v.rob.youngest := std_logic_vector(unsigned(r.rob.youngest) + 1);
				end if;
			end if;
			assert not stall report "stall" severity note;
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
			if oldest_rob.state = ROB_Dummy then
				mem_in_v.dummy_done := '1';
				v.rob.rob_array(to_integer(unsigned(v.rob.oldest))) := rob_zero;
				v.rob.oldest := std_logic_vector(unsigned(v.rob.oldest) + 1);
			elsif oldest_rob.state = ROB_Done then
				if oldest_rob.reg_num /= reg_num_zero then
					assert v.registers(to_integer(unsigned(oldest_rob.reg_num))).tag.valid = '1' report "BUG @ register write reservation";
					v.registers(to_integer(unsigned(oldest_rob.reg_num))).data := oldest_rob.result;
					if v.registers(to_integer(unsigned(oldest_rob.reg_num))).tag.rob_num = v.rob.oldest then
						v.registers(to_integer(unsigned(oldest_rob.reg_num))).tag := rs_tag_zero;
					end if;
				end if;
				v.rob.rob_array(to_integer(unsigned(v.rob.oldest))) := rob_zero;
				v.rob.oldest := std_logic_vector(unsigned(v.rob.oldest) + 1);
			elsif oldest_rob.state = ROB_Reset then
				report "rob reset" severity note;
				if oldest_rob.reg_num /= reg_num_zero then
					assert v.registers(to_integer(unsigned(oldest_rob.reg_num))).tag.valid = '1' report "BUG @ register write reservation";
					v.registers(to_integer(unsigned(oldest_rob.reg_num))).data := oldest_rob.result;
				end if;
				for i in v.registers'range loop
					v.registers(i).tag := rs_tag_zero;
				end loop;
				v := (
					decode_result => decode_result_zero,
					cdb => cdb_zero,
					registers => v.registers,
					pc => oldest_rob.pc_next,
					rob => rob_ring_buffer_zero,
					state => CPU_NORMAL,
					inst_valid => '0',
					state_sub => '0'
				);
				-- reset other modules
				alu_in_v.rst := '1';
				fpu_in_v.rst := '1';
				mem_in_v.rst := '1';
				branch_in_v.rst := '1';
			end if;
		end if;
		when CPU_LOADING =>
			case r.state_sub is
			when '0' =>
				go_pl_v := '1';
				v.state_sub := '1';
			when '1' =>
				if active_pl = '0' then
					v.state := CPU_NORMAL;
				end if;
			when others =>
			end case;
		when others =>
		end case;
		go_pl <= go_pl_v;
		alu_in <= alu_in_v;
		fpu_in <= fpu_in_v;
		mem_in_v.sramifout := cpu_top_in.sramifout;
		mem_in_v.recvifout := cpu_top_in.recvifout;
		mem_in_v.transifout := cpu_top_in.transifout;
		mem_in <= mem_in_v;
		branch_in <= branch_in_v;
		r_in <= v;
	end process;
end;
