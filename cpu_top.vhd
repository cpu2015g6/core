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
--0 => x"d0010000",
--1 => x"e30801ff",
--2 => x"d4020005",
--3 => x"d3080000",
--4 => x"d0080001",
--5 => x"e0010108",
--6 => x"d4fffffb",
--7 => x"d0090002",
--8 => x"d1090809",
--9 => x"d01e0020",
--10 => x"f4ff091e",
--11 => x"d0090001",
--12 => x"d80302ff",
--13 => x"e0030309",
--14 => x"d80308ff",
--15 => x"e0030309",
--16 => x"e1080809",
--17 => x"d402fff6",
--18 => x"d0090001",
--19 => x"e1090309",
--20 => x"d90a09ff",
--21 => x"d80908ff",
--22 => x"d0090002",
--23 => x"e1080a09",
--24 => x"d402ffef",
--25 => x"d0090001",
--26 => x"e1030309",
--27 => x"d90a03ff",
--28 => x"e008080a",
--29 => x"e1030309",
--30 => x"d90203ff",
--31 => x"d5ff02ff",
--32 => x"d0080001",
--33 => x"d5ff02ff",
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
-- mem2
--0 => x"d0010000",
--1 => x"d0020000",
--2 => x"d0030001",
--3 => x"d80102ff",
--4 => x"d90401ff",
--5 => x"d3020000",
--6 => x"d3040000",
--7 => x"d80103ff",
--8 => x"d90501ff",
--9 => x"d3030000",
--10 => x"d3050000",
--11 => x"d0040002",
--12 => x"e0020204",
--13 => x"e0030304",
--14 => x"d0040001",
--15 => x"e0010104",
--16 => x"d4fffff3",
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
use work.gshare_pack.all;
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
		ghr : ghr_type;
		rob : rob_ring_buffer_type;
		state : CPU_state_type;
		inst_valid, state_sub : std_logic;
	end record;
	constant reg_zero : reg_type := (
		decode_result_zero,
		cdb_zero,
		register_array_zero,
		(others => '0'),
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
		rs_in_common : in rs_common_type;
		cdb_in : in cdb_type;
		cdb_next : in std_logic;-- set cdb_next = 1 when cdb_out is broadcasted
		sync_rst : in std_logic;-- synchronous reset
		store_commit, out_commit, in_commit : in std_logic;
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
		pht_entry : in pht_entry_type;
		btb_entry : in btb_entry_type;
		pc : in pc_type;
		ghr : in ghr_type;
		next_pc : out pc_type;
		next_ghr : out ghr_type;
		id_stall : out boolean) is
	variable taken : boolean;
	begin
		taken := pht_taken(pht_entry);
		id_stall := false;
		case decode_result.opc is
		when JIF_opc =>
			if taken then
				next_pc := decode_result.imm(pc_width-1 downto 0);
			else
				next_pc := std_logic_vector(signed(pc) + 1);
			end if;
		when JRF_opc =>
			if taken and btb_hit(btb_entry, pc) then
				next_pc := btb_entry.target;
			else
				next_pc := std_logic_vector(signed(pc) + 1);
			end if;
		when CI_opc =>
			next_pc := decode_result.imm(pc_width-1 downto 0);
			taken := false;
			next_ghr := ghr;
		when CR_opc =>
			if btb_hit(btb_entry, pc) then
				next_pc := btb_entry.target;
			else
				next_pc := std_logic_vector(signed(pc) + 1);
			end if;
			taken := false;
			next_ghr := ghr;
		when JIC_opc =>
			if taken then
				next_pc := decode_result.imm(pc_width-1 downto 0);
			else
				next_pc := std_logic_vector(signed(pc) + 1);
			end if;
		when JRC_opc =>
			if taken and btb_hit(btb_entry, pc) then
				next_pc := btb_entry.target;
			else
				next_pc := std_logic_vector(signed(pc) + 1);
			end if;
		when FJIC_opc =>
			if taken then
				next_pc := decode_result.imm(pc_width-1 downto 0);
			else
				next_pc := std_logic_vector(signed(pc) + 1);
			end if;
		when FJRC_opc =>
			if taken and btb_hit(btb_entry, pc) then
				next_pc := btb_entry.target;
			else
				next_pc := std_logic_vector(signed(pc) + 1);
			end if;
		when others =>
			next_pc := std_logic_vector(signed(pc) + 1);
			taken := false;
			next_ghr := ghr;
		end case;
		next_ghr := ghr_next(ghr, taken);
		case decode_result.opc is
		when JIF_opc =>
		when JRF_opc =>
		when CI_opc =>
		when CR_opc =>
		when JIC_opc =>
		when JRC_opc =>
		when FJIC_opc =>
		when FJRC_opc =>
--		when J_opc =>
--			next_pc := std_logic_vector(signed(pc) + signed(decode_result.imm(pc_width-1 downto 0)));
--			next_ghr := ghr;
--		when JR_opc =>
--			next_ghr := ghr;
--		when JREQ_opc =>
--		when JRNEQ_opc =>
--		when JRGT_opc =>
--		when JRGTE_opc =>
--		when JRLT_opc =>
--		when JRLTE_opc =>
		when others =>
			next_pc := std_logic_vector(unsigned(pc) + 1);
			next_ghr := ghr;
		end case;
	end branch_predictor;
	procedure inst_decode_try_1op(
		inst : in std_logic_vector(31 downto 0);
		decode_result : out decode_result_type) is
		alias opc : std_logic_vector(8 downto 0) is inst(31 downto 23);
		alias ra : std_logic_vector(4 downto 0) is inst(22 downto 18);
		alias imm : std_logic_vector(15 downto 0) is inst(17 downto 2);
	begin
		decode_result := decode_result_zero;
		case opc is
		when "000000001" =>
			decode_result.opc := LIMM_opc;
			decode_result.rt := ra;
			decode_result.imm := imm;
		when "000000010" =>
			decode_result.opc := IN_opc;
			decode_result.rt := ra;
		when "000000011" =>
			decode_result.opc := OUT_opc;
			decode_result.ra := ra;
		when others =>
		end case;
	end procedure;
	procedure inst_decode_try_2iop(
		inst : in std_logic_vector(31 downto 0);
		decode_result : out decode_result_type) is
		alias opc : std_logic_vector(5 downto 0) is inst(31 downto 26);
		alias ra : std_logic_vector(4 downto 0) is inst(25 downto 21);
		alias rb : std_logic_vector(4 downto 0) is inst(20 downto 16);
		alias imm : std_logic_vector(15 downto 0) is inst(15 downto 0);
	begin
		decode_result := decode_result_zero;
		case opc is
		when "000001" =>
			decode_result.opc := STWI_opc;
			decode_result.ra := ra;
			decode_result.rb := rb;
			decode_result.imm := imm;
		when "000010" =>
			decode_result.opc := LDWI_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.imm := imm;
		when "000011" =>
			decode_result.opc := JIF_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rc := ra;
			decode_result.imm := imm;
		when "000100" =>
			decode_result.opc := CI_opc;
			decode_result.rt := ra;
			decode_result.imm := imm;
		when "000101" =>
			decode_result.opc := ADDI_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.imm := imm;
		when "000110" =>
			decode_result.opc := SUBI_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.imm := imm;
		when others =>
		end case;
	end procedure;
	procedure inst_decode_try_2icop(
		inst : in std_logic_vector(31 downto 0);
		decode_result : out decode_result_type) is
		alias opc : std_logic_vector(2 downto 0) is inst(31 downto 29);
		alias cond : std_logic_vector(2 downto 0) is inst(28 downto 26);
		alias ra : std_logic_vector(4 downto 0) is inst(25 downto 21);
		alias rb : std_logic_vector(4 downto 0) is inst(20 downto 16);
		alias imm : std_logic_vector(15 downto 0) is inst(15 downto 0);
	begin
		decode_result := decode_result_zero;
		case opc is
		when "001" =>
			decode_result.opc := CMPIC_opc;
			decode_result.cond := cond;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.imm := imm;
		when "010" =>
			decode_result.opc := CMPAIC_opc;
			decode_result.cond := cond;
			decode_result.rt := ra;
			decode_result.ra := ra;
			decode_result.rb := rb;
			decode_result.imm := imm;
		when "011" =>
			decode_result.opc := JIC_opc;
			decode_result.cond := cond;
			decode_result.ra := ra;
			decode_result.rb := rb;
			decode_result.imm := imm;
		when "100" =>
			decode_result.opc := FJIC_opc;
			decode_result.cond := cond;
			decode_result.ra := ra;
			decode_result.rb := rb;
			decode_result.imm := imm;
		when others =>
		end case;
	end procedure;
	procedure inst_decode_try_3cop(
		inst : in std_logic_vector(31 downto 0);
		decode_result : out decode_result_type) is
		alias opc : std_logic_vector(11 downto 0) is inst(31 downto 20);
		alias cond : std_logic_vector(2 downto 0) is inst(19 downto 17);
		alias ra : std_logic_vector(4 downto 0) is inst(16 downto 12);
		alias rb : std_logic_vector(4 downto 0) is inst(11 downto 7);
		alias rc : std_logic_vector(4 downto 0) is inst(6 downto 2);
	begin
		decode_result := decode_result_zero;
		case opc is
		when "000000000001" =>
			decode_result.opc := CMPC_opc;
			decode_result.cond := cond;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
		when "000000000010" =>
			decode_result.opc := FCMPC_opc;
			decode_result.cond := cond;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
		when "000000000011" =>
			decode_result.opc := CMPAC_opc;
			decode_result.cond := cond;
			decode_result.rt := ra;
			decode_result.ra := ra;
			decode_result.rb := rb;
			decode_result.rc := rc;
		when "000000000100" =>
			decode_result.opc := FCMPAC_opc;
			decode_result.cond := cond;
			decode_result.rt := ra;
			decode_result.ra := ra;
			decode_result.rb := rb;
			decode_result.rc := rc;
		when "000000000101" =>
			decode_result.opc := JRC_opc;
			decode_result.cond := cond;
			decode_result.ra := ra;
			decode_result.rb := rb;
			decode_result.rc := rc;
		when "000000000110" =>
			decode_result.opc := FJRC_opc;
			decode_result.cond := cond;
			decode_result.ra := ra;
			decode_result.rb := rb;
			decode_result.rc := rc;
		when others =>
		end case;
	end procedure;
	procedure inst_decode_try_3op(
		inst : in std_logic_vector(31 downto 0);
		decode_result : out decode_result_type) is
		alias opc : std_logic_vector(16 downto 0) is inst(31 downto 15);
		alias ra : std_logic_vector(4 downto 0) is inst(14 downto 10);
		alias rb : std_logic_vector(4 downto 0) is inst(9 downto 5);
		alias rc : std_logic_vector(4 downto 0) is inst(4 downto 0);
	begin
		decode_result := decode_result_zero;
		case opc is
		when "00000000000000000" =>
			decode_result.opc := JRF_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
			decode_result.rc := ra;
		when "00000000000000001" =>
			decode_result.opc := CR_opc;
			decode_result.rt := ra;
			decode_result.ra := rc;
		when "00000000000000010" =>
			decode_result.opc := STW_opc;
			decode_result.ra := ra;
			decode_result.rb := rb;
			decode_result.rc := rc;
		when "00000000000000011" =>
			decode_result.opc := LDW_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
		when "00000000000000100" =>
			decode_result.opc := ADD_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
		when "00000000000000101" =>
			decode_result.opc := SUB_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
		when "00000000000000110" =>
			decode_result.opc := AND_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
		when "00000000000000111" =>
			decode_result.opc := OR_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
		when "00000000000001000" =>
			decode_result.opc := XOR_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
		when "00000000000001001" =>
			decode_result.opc := SLL_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
		when "00000000000001010" =>
			decode_result.opc := SRL_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
		when "00000000000001011" =>
			decode_result.opc := FADD_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
		when "00000000000001100" =>
			decode_result.opc := FSUB_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
		when "00000000000001101" =>
			decode_result.opc := FMUL_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
		when "00000000000001110" =>
			decode_result.opc := FINV_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
		when "00000000000001111" =>
			decode_result.opc := FABA_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
			decode_result.rb := rc;
		when "00000000000010000" =>
			decode_result.opc := FSQRT_opc;
			decode_result.rt := ra;
			decode_result.ra := rb;
		when others =>
		end case;
	end procedure;
	procedure inst_decode(
		inst : in std_logic_vector(31 downto 0);
		decode_result : out decode_result_type) is
		variable decode_result_1op : decode_result_type;
		variable decode_result_2iop : decode_result_type;
		variable decode_result_2icop : decode_result_type;
		variable decode_result_3cop : decode_result_type;
		variable decode_result_3op : decode_result_type;
	begin
		inst_decode_try_1op(inst, decode_result_1op);
		inst_decode_try_2iop(inst, decode_result_2iop);
		inst_decode_try_2icop(inst, decode_result_2icop);
		inst_decode_try_3cop(inst, decode_result_3cop);
		inst_decode_try_3op(inst, decode_result_3op);
		if decode_result_1op.opc /= NOP_opc then
			decode_result := decode_result_1op;
		elsif decode_result_2iop.opc /= NOP_opc then
			decode_result := decode_result_2iop;
		elsif decode_result_2icop.opc /= NOP_opc then
			decode_result := decode_result_2icop;
		elsif decode_result_3cop.opc /= NOP_opc then
			decode_result := decode_result_3cop;
		elsif decode_result_3op.opc /= NOP_opc then
			decode_result := decode_result_3op;
		else
			decode_result := decode_result_zero;
			report "undefined instruction" severity warning;
		end if;
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
	function cdb_arbiter_priority(cdb_out : cdb_type; rob_oldest : rob_num_type) return rob_num_type is
	begin
		if cdb_out.tag.valid = '1' then
			return std_logic_vector(unsigned(cdb_out.tag.rob_num) - unsigned(rob_oldest));
		else
			return (others => '1');
		end if;
	end cdb_arbiter_priority;
	procedure cdb_arbiter(
		rob_oldest : in rob_num_type;
		alu_cdb_out, fpu_cdb_out, mem_cdb_out, branch_cdb_out : in cdb_type;
		alu_grant, fpu_grant, mem_grant, branch_grant : out std_logic;
		cdb : out cdb_type
	) is
		variable unit : unit_type;
		variable priority_alu : rob_num_type;
		variable priority_fpu : rob_num_type;
		variable priority_mem : rob_num_type;
		variable priority_branch : rob_num_type;
		variable priority_alu_fpu : rob_num_type;
		variable priority_mem_branch : rob_num_type;
		variable alu_fpu, mem_branch : std_logic;
	begin
		alu_grant := '0';
		fpu_grant := '0';
		mem_grant := '0';
		branch_grant := '0';
		unit := NULL_UNIT;
		priority_alu := cdb_arbiter_priority(alu_cdb_out, rob_oldest);
		priority_fpu := cdb_arbiter_priority(fpu_cdb_out, rob_oldest);
		priority_mem := cdb_arbiter_priority(mem_cdb_out, rob_oldest);
		priority_branch := cdb_arbiter_priority(branch_cdb_out, rob_oldest);
		if unsigned(priority_alu) < unsigned(priority_fpu) then
			alu_fpu := '0';
			priority_alu_fpu := priority_alu;
		else
			alu_fpu := '1';
			priority_alu_fpu := priority_fpu;
		end if;
		if unsigned(priority_mem) < unsigned(priority_branch) then
			mem_branch := '0';
			priority_mem_branch := priority_mem;
		else
			mem_branch := '1';
			priority_mem_branch := priority_branch;
		end if;
		if unsigned(priority_alu_fpu) < unsigned(priority_mem_branch) then
			if alu_fpu = '0' then
				unit := ALU_UNIT;
			else
				unit := FPU_UNIT;
			end if;
		else
			if mem_branch = '0' then
				unit := MEM_UNIT;
			else
				unit := BRANCH_UNIT;
			end if;
		end if;
		case unit is
		when ALU_UNIT =>
			if alu_cdb_out.tag.valid = '0' then
				unit := NULL_UNIT;
			end if;
		when FPU_UNIT =>
			if fpu_cdb_out.tag.valid = '0' then
				unit := NULL_UNIT;
			end if;
		when MEM_UNIT =>
			if mem_cdb_out.tag.valid = '0' then
				unit := NULL_UNIT;
			end if;
		when BRANCH_UNIT =>
			if branch_cdb_out.tag.valid = '0' then
				unit := NULL_UNIT;
			end if;
		when NULL_UNIT =>
		end case;
		case unit is
		when ALU_UNIT =>
			alu_grant := '1';
			cdb := alu_cdb_out;
		when FPU_UNIT =>
			fpu_grant := '1';
			cdb := fpu_cdb_out;
		when MEM_UNIT =>
			mem_grant := '1';
			cdb := mem_cdb_out;
		when BRANCH_UNIT =>
			branch_grant := '1';
			cdb := branch_cdb_out;
		when NULL_UNIT =>
			cdb := cdb_zero;
		end case;
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
			new_rob_array(rob_i).taken := cdb.taken;
			new_rob_array(rob_i).state := ROB_Done;
			if cdb.pc_next /= rob_array(rob_i).pc_next then
				new_rob_array(rob_i).state := ROB_RESET;
				new_rob_array(rob_i).pc_next := cdb.pc_next;
			end if;
		end if;
		return new_rob_array;
	end update_ROB;
	procedure commit_branch_predictor(rob_entry : in rob_type; btb_we : out std_logic; btb_addr : out std_logic_vector(pht_array_width-1 downto 0); btb_din : out btb_type; pht_we : out std_logic; pht_addr : out std_logic_vector(pht_array_width-1 downto 0); pht_din : out pht_entry_type) is
		variable next_target : pc_type;
		variable btb_entry : btb_entry_type;
	begin
		btb_we := '0';
		pht_we := '0';
		btb_addr := rob_entry.pc(pht_array_width-1 downto 0);
		pht_addr := rob_entry.pc(rob_entry.ghr'length-1 downto 0) xor rob_entry.ghr;
		pht_din := pht_commit(rob_entry.taken, rob_entry.pht_entry);
		if rob_entry.taken then
			btb_entry := (
				valid => true,
				tag => rob_entry.pc(pc_width-1 downto pht_array_width),
				target => rob_entry.pc_next
			);
		else
			btb_entry := rob_entry.btb_entry;
		end if;
		btb_din := btb_encode(btb_entry);
		case rob_entry.opc is
		when JIF_opc =>
			pht_we := '1';
		when JRF_opc =>
			btb_we := '1';
			pht_we := '1';
		when CI_opc =>
		when CR_opc =>
			btb_we := '1';
			pht_we := '1';
		when JIC_opc =>
			pht_we := '1';
		when JRC_opc =>
			btb_we := '1';
			pht_we := '1';
		when FJIC_opc =>
			pht_we := '1';
		when FJRC_opc =>
			btb_we := '1';
			pht_we := '1';
		when others =>
		end case;
	end;
	function ghr_rollback(rob_entry : rob_type) return ghr_type is
	begin
		case rob_entry.opc is
		when JIF_opc =>
			return ghr_next(rob_entry.ghr, rob_entry.taken);
		when JRF_opc =>
			return ghr_next(rob_entry.ghr, rob_entry.taken);
		when JIC_opc =>
			return ghr_next(rob_entry.ghr, rob_entry.taken);
		when JRC_opc =>
			return ghr_next(rob_entry.ghr, rob_entry.taken);
		when FJIC_opc =>
			return ghr_next(rob_entry.ghr, rob_entry.taken);
		when FJRC_opc =>
			return ghr_next(rob_entry.ghr, rob_entry.taken);
		when others =>
			return rob_entry.ghr;
		end case;
	end ghr_rollback;
-- simple dual port ram
-- write_first
-- initialized by 0
-- width: btb_type(pc_width+pc_width-pht_array_width+1)
-- depth: 2^pht_array_width
component btb_ram IS
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(20 DOWNTO 0);
    clkb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(20 DOWNTO 0)
  );
END component;
-- simple dual port ram
-- write_first
-- initialized by 0
-- width: 2
-- depth: 2^pht_array_width
component pht_ram IS
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    clkb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(1 DOWNTO 0)
  );
END component;

	signal btb_wea : std_logic_vector(0 downto 0) := "0";
	signal btb_addra, btb_addrb : std_logic_vector(9 downto 0) := (others => '0');
	signal btb_din : btb_type := (others => '0');
	signal btb_dout : btb_type;
	signal pht_wea : std_logic_vector(0 downto 0) := "0";
	signal pht_addra, pht_addrb : std_logic_vector(9 downto 0) := (others => '0');
	signal pht_din : std_logic_vector(1 downto 0) := (others => '0');
	signal pht_dout : std_logic_vector(1 downto 0);
begin
	bram_addr_cpu <= r_in.pc;
	pht_addrb <= r_in.pc(r_in.ghr'length-1 downto 0) xor r_in.ghr;
	btb_addrb <= r_in.pc(pht_array_width-1 downto 0);
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
		rs_in_common => mem_in.rs_in.common,
		cdb_in => mem_in.cdb_in,
		cdb_next => mem_in.cdb_next,
		sync_rst => mem_in.rst,
		store_commit => mem_in.store_commit,
		out_commit => mem_in.out_commit,
		in_commit => mem_in.in_commit,
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
	btb_ram_l : btb_ram
	PORT map (
		clka => clk,
		clkb => clk,
		wea => btb_wea,
		addra => btb_addra,
		dina => btb_din,
		addrb => btb_addrb,
		doutb => btb_dout
	);
	pht_ram_l : pht_ram
	PORT map (
		clka => clk,
		clkb => clk,
		wea => pht_wea,
		addra => pht_addra,
		dina => pht_din,
		addrb => pht_addrb,
		doutb => pht_dout
	);
	process(clk, rst)
	begin
		if rst = '1' then
			r <= reg_zero;
		elsif rising_edge(clk) then
			r <= r_in;
		end if;
	end process;
	comb : process(r, alu_out, fpu_out, mem_out, branch_out, bram_dout, cpu_top_in, active_pl, btb_dout, pht_dout)
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
		variable ra, rb, rc : register_type;
		variable rs_common_3 : rs_common_type;
		variable zext_imm_data : std_logic_vector(31 downto 0);
		variable zext_imm : register_type;
		variable go_pl_v : std_logic;
		variable btb_we : std_logic;
		variable btb_addr_write : std_logic_vector(pht_array_width-1 downto 0);
		variable btb_din_v : btb_type;
		variable pht_we : std_logic;
		variable pht_addr_write : std_logic_vector(pht_array_width-1 downto 0);
		variable pht_din_v : pht_entry_type;
		variable btb_entry : btb_entry_type;
	begin
		v := r;
		alu_in_v := alu_pack.in_zero;
		fpu_in_v := fpu_pack.in_zero;
		mem_in_v := mem_pack.in_zero;
		branch_in_v := branch_pack.in_zero;
		btb_we := '0';
		btb_addr_write := (others => '0');
		btb_din_v := (others => '0');
		pht_we := '0';
		pht_addr_write := (others => '0');
		pht_din_v := (others => '0');
		btb_entry := btb_decode(btb_dout);
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
			-- branch prediction
			branch_predictor(
				decode_result => decode_result_v,
				pht_entry => pht_dout,
				btb_entry => btb_entry,
				pc => r.pc,
				ghr => r.ghr,
				next_pc => next_pc,
				next_ghr => v.ghr,
				id_stall => stall
			);
			decode_result_v.pc := r.pc;
			decode_result_v.pc_predicted := next_pc;
			decode_result_v.ghr := r.ghr;
			decode_result_v.pht_entry := pht_dout;
			decode_result_v.btb_entry := btb_entry;
			-- read registers and issue
			ra := read_reg(r.decode_result.ra, r.registers, v.rob.rob_array);
			rb := read_reg(r.decode_result.rb, r.registers, v.rob.rob_array);
			rc := read_reg(r.decode_result.rc, r.registers, v.rob.rob_array);
			zext_imm_data := x"0000" & r.decode_result.imm;
			zext_imm := (data => zext_imm_data, tag => rs_tag_zero);
			alu_rs_v := alu_pack.rs_zero;
			fpu_rs_v := fpu_pack.rs_zero;
			mem_rs_v := mem_pack.rs_zero;
			branch_rs_v := branch_pack.rs_zero;
			rs_common_3 := (
				ra => ra,
				rb => rb,
				rc => rc,
				cond => r.decode_result.cond,
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
				alu_rs_v.common := rs_common_3;
				alu_rs_v.common.ra := zext_imm;
			when OUT_opc =>
				unit := MEM_UNIT;
				mem_rs_v.op := mem_pack.OUT_op;
				mem_rs_v.common := rs_common_3;
			when IN_opc =>
				unit := MEM_UNIT;
				mem_rs_v.op := mem_pack.IN_op;
				mem_rs_v.common := rs_common_3;
			when STWI_opc =>
				unit := MEM_UNIT;
				mem_rs_v.op := mem_pack.STW_op;
				mem_rs_v.common := rs_common_3;
				mem_rs_v.common.rc := zext_imm;
			when LDWI_opc =>
				unit := MEM_UNIT;
				mem_rs_v.op := mem_pack.LDW_op;
				mem_rs_v.common := rs_common_3;
				mem_rs_v.common.rb := zext_imm;
			when JIF_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.JF_op;
				branch_rs_v.common := rs_common_3;
				branch_rs_v.common.rb := zext_imm;
			when CI_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.C_op;
				branch_rs_v.common := rs_common_3;
				branch_rs_v.common.ra := zext_imm;
			when ADDI_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.ADD_op;
				alu_rs_v.common := rs_common_3;
				alu_rs_v.common.rb := zext_imm;
			when SUBI_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.SUB_op;
				alu_rs_v.common := rs_common_3;
				alu_rs_v.common.rb := zext_imm;
			when CMPIC_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.CMPC_op;
				alu_rs_v.common := rs_common_3;
				alu_rs_v.common.rb := zext_imm;
			when CMPAIC_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.CMPAC_op;
				alu_rs_v.common := rs_common_3;
				alu_rs_v.common.rc := zext_imm;
			when JIC_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.JC_op;
				branch_rs_v.common := rs_common_3;
				branch_rs_v.common.rc := zext_imm;
			when FJIC_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.FJC_op;
				branch_rs_v.common := rs_common_3;
				branch_rs_v.common.rc := zext_imm;
			when CMPC_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.CMPC_op;
				alu_rs_v.common := rs_common_3;
			when FCMPC_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.FCMPC_op;
				alu_rs_v.common := rs_common_3;
			when CMPAC_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.CMPAC_op;
				alu_rs_v.common := rs_common_3;
			when FCMPAC_opc =>
				unit := ALU_UNIT;
				alu_rs_v.op := alu_pack.FCMPAC_op;
				alu_rs_v.common := rs_common_3;
			when JRC_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.JC_op;
				branch_rs_v.common := rs_common_3;
			when FJRC_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.FJC_op;
				branch_rs_v.common := rs_common_3;
			when JRF_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.JF_op;
				branch_rs_v.common := rs_common_3;
			when CR_opc =>
				unit := BRANCH_UNIT;
				branch_rs_v.op := branch_pack.C_op;
				branch_rs_v.common := rs_common_3;
			when STW_opc =>
				unit := MEM_UNIT;
				mem_rs_v.op := mem_pack.STW_op;
				mem_rs_v.common := rs_common_3;
			when LDW_opc =>
				unit := MEM_UNIT;
				mem_rs_v.op := mem_pack.LDW_op;
				mem_rs_v.common := rs_common_3;
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
			when FSUB_opc =>
				unit := FPU_UNIT;
				fpu_rs_v.op := fpu_pack.FSUB_op;
				fpu_rs_v.common := rs_common_3;
			when FMUL_opc =>
				unit := FPU_UNIT;
				fpu_rs_v.op := fpu_pack.FMUL_op;
				fpu_rs_v.common := rs_common_3;
			when FINV_opc =>
				unit := FPU_UNIT;
				fpu_rs_v.op := fpu_pack.FINV_op;
				fpu_rs_v.common := rs_common_3;
			when FABA_opc =>
				unit := FPU_UNIT;
				fpu_rs_v.op := fpu_pack.FABA_op;
				fpu_rs_v.common := rs_common_3;
			when FSQRT_opc =>
				unit := FPU_UNIT;
				fpu_rs_v.op := fpu_pack.FSQRT_op;
				fpu_rs_v.common := rs_common_3;
			when NOP_opc =>
			end case;
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
					v.rob.rob_array(to_integer(unsigned(r.rob.youngest))) := (
						state => ROB_Executing,
						taken => false,
						opc => r.decode_result.opc,
						pc => r.decode_result.pc,
						pc_next => r.decode_result.pc_predicted,
						ghr => r.decode_result.ghr,
						pht_entry => r.decode_result.pht_entry,
						btb_entry => r.decode_result.btb_entry,
						result => (others => '0'),
						reg_num => r.decode_result.rt
					);
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
				rob_oldest => v.rob.oldest,
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
			oldest_rob := r.rob.rob_array(to_integer(unsigned(v.rob.oldest)));
			if oldest_rob.state = ROB_Done then
				case oldest_rob.opc is
				when STWI_opc =>
					mem_in_v.store_commit := '1';
				when STW_opc =>
					mem_in_v.store_commit := '1';
				when IN_opc =>
					mem_in_v.in_commit := '1';
				when OUT_opc =>
					mem_in_v.out_commit := '1';
				when others =>
				end case;
				if oldest_rob.reg_num /= reg_num_zero then
					assert v.registers(to_integer(unsigned(oldest_rob.reg_num))).tag.valid = '1' report "BUG @ register write reservation";
					v.registers(to_integer(unsigned(oldest_rob.reg_num))).data := oldest_rob.result;
					if v.registers(to_integer(unsigned(oldest_rob.reg_num))).tag.rob_num = v.rob.oldest then
						v.registers(to_integer(unsigned(oldest_rob.reg_num))).tag := rs_tag_zero;
					end if;
				end if;
				v.rob.rob_array(to_integer(unsigned(v.rob.oldest))) := rob_zero;
				v.rob.oldest := std_logic_vector(unsigned(v.rob.oldest) + 1);
				commit_branch_predictor(oldest_rob, btb_we, btb_addr_write, btb_din_v, pht_we, pht_addr_write, pht_din_v);
			elsif oldest_rob.state = ROB_Reset then
				report "rob reset" severity note;
				if oldest_rob.reg_num /= reg_num_zero then
					assert v.registers(to_integer(unsigned(oldest_rob.reg_num))).tag.valid = '1' report "BUG @ register write reservation";
					v.registers(to_integer(unsigned(oldest_rob.reg_num))).data := oldest_rob.result;
				end if;
				commit_branch_predictor(oldest_rob, btb_we, btb_addr_write, btb_din_v, pht_we, pht_addr_write, pht_din_v);
				for i in v.registers'range loop
					v.registers(i).tag := rs_tag_zero;
				end loop;
				v := (
					decode_result => decode_result_zero,
					cdb => cdb_zero,
					registers => v.registers,
					pc => oldest_rob.pc_next,
					ghr => ghr_rollback(oldest_rob),
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
		btb_wea(0) <= btb_we;
		btb_addra <= btb_addr_write;
		btb_din <= btb_din_v;
		pht_wea(0) <= pht_we;
		pht_addra <= pht_addr_write;
		pht_din <= pht_din_v;
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
