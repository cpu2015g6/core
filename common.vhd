library ieee;
use ieee.std_logic_1164.all;
package common is
	constant rs_num_width : integer := 2;
	subtype rs_num_type is std_logic_vector(rs_num_width-1 downto 0);
	constant pc_width : integer := 14;
	subtype pc_type is std_logic_vector(pc_width-1 downto 0);
	constant rob_num_width : integer := 4;
	subtype rob_num_type is std_logic_vector(rob_num_width-1 downto 0);
	constant reg_num_width : integer := 6;
	subtype reg_num_type is std_logic_vector(reg_num_width-1 downto 0);
	constant reg_num_zero : reg_num_type := (others => '1');
	type opc_type is (NOP_opc, LIMM_opc, CMP_opc, IN_opc, OUT_opc, J_opc, JR_opc, JREQ_opc, JRNEQ_opc, JRGT_opc, JRGTE_opc, JRLT_opc, JRLTE_opc, STW_opc, LDW_opc, ADD_opc, SUB_opc, AND_opc, OR_opc, XOR_opc, NOT_opc, SLL_opc, SRL_opc, FADD_opc, FMUL_opc, FDIV_opc, FSIN_opc, FCOS_opc, FATAN_opc, FSQRT_opc, FCMP_opc);
	type unit_type is (ALU_UNIT, FPU_UNIT, MEM_UNIT, BRANCH_UNIT, NULL_UNIT);
	constant gt_const : std_logic_vector(31 downto 0) := x"00000002";
	constant eq_const : std_logic_vector(31 downto 0) := x"00000001";
	constant lt_const : std_logic_vector(31 downto 0) := x"00000000";
	-- reservation station
	type rs_tag_type is record
		valid : std_logic;
		rob_num : rob_num_type;
	end record;
	constant rs_tag_zero : rs_tag_type := (
		'0',
		(others => '0')
	);
	type decode_result_type is record
		opc : opc_type;
		rt, ra, rb : reg_num_type;
		imm : std_logic_vector(15 downto 0);
		pc, pc_predicted : pc_type;
	end record;
	constant decode_result_zero : decode_result_type := (
		NOP_opc,
		reg_num_zero, reg_num_zero, reg_num_zero,
		(others => '0'),
		(others => '0'), (others => '0')
	);
	type register_type is record
		data : std_logic_vector(31 downto 0);
		tag : rs_tag_type;
	end record;
	constant register_zero : register_type := (
		(others => '0'),
		rs_tag_zero
	);
	type register_array_type is array (0 to 2**reg_num_width-1) of register_type;
	constant register_array_zero : register_array_type := (others => register_zero);
	--common data bus
	type cdb_type is record
		data : std_logic_vector(31 downto 0);
		tag : rs_tag_type;
		pc_next : pc_type;
	end record;
	constant cdb_zero : cdb_type := (
		(others => '0'),
		rs_tag_zero,
		(others => '0')
	);
	type rs_state_type is (RS_Invalid, RS_Waiting, RS_Executing, RS_Done, RS_Reserved);
	type rs_common_type is record
		state : rs_state_type;
		ra, rb : register_type;
		result : std_logic_vector(31 downto 0);
		rob_num : rob_num_type;
		pc, pc_next : pc_type;
	end record;
	constant rs_common_zero : rs_common_type := (
		RS_Invalid,
		register_zero, register_zero,
		(others => '0'),
		(others => '0'),
		(others => '0'), (others => '0')
	);
	function rs_common_ready(r : rs_common_type) return boolean;
	-- ROB_Executing : ROB is valid but the result has not yet been obtained. pc_next has a value of predicted pc
	-- ROB_Done : The result is available
	-- ROB_Reset : CPU enters rollback mode due to a mispredicted branch and restarts execution from pc_next
	-- Distinguishing faults, traps and aborts may be needed.
	type rob_state_type is (ROB_Invalid, ROB_Executing, ROB_Done, ROB_Reset);
	type rob_type is record
		state : rob_state_type;
		pc_next : pc_type;
		result : std_logic_vector(31 downto 0);
		reg_num : reg_num_type;
	end record;
	constant rob_zero : rob_type := (
		ROB_Invalid,
		(others => '0'),
		(others => '0'),
		(others => '0')
	);
	function register_update(reg : register_type; cdb : cdb_type) return register_type;
	function make_cdb_out(rs_common : rs_common_type) return cdb_type;
end common;

package body common is
	function rs_common_ready(r : rs_common_type) return boolean is
	begin
		return r.state = RS_Waiting and r.ra.tag.valid = '0' and r.rb.tag.valid = '0';
	end rs_common_ready;
	function register_update(reg : register_type; cdb : cdb_type) return register_type is
		variable v : register_type;
	begin
		if cdb.tag.valid = '1' and cdb.tag = reg.tag then
			v := (tag => rs_tag_zero, data => cdb.data);
		else
			v := reg;
		end if;
		return v;
	end register_update;
	function make_cdb_out(rs_common : rs_common_type) return cdb_type is
	begin
		return (
			tag => (
				valid => '1',
				rob_num => rs_common.rob_num
			),
			data => rs_common.result,
			pc_next => rs_common.pc_next
		);
	end make_cdb_out;
end common;

library ieee;
use ieee.std_logic_1164.all;
use work.common.all;
package alu_pack is
	type op_type is (LIMM_op, CMP_op, ADD_op, SUB_op, AND_op, OR_op, XOR_op, NOT_op, SLL_op, SRL_op, NOP_op);
	type rs_type is record
		op : op_type;
		common : rs_common_type;
	end record;
	constant rs_zero : rs_type := (
		NOP_op,
		rs_common_zero
	);
	type rs_array_type is array (0 to 2**rs_num_width-1) of rs_type;
	type in_type is record
		rs_in : rs_type;
		cdb_in : cdb_type;-- invalid -> cdb_in.tag.unit = NULL_UNIT, valid -> ALU_UNIT
		cdb_next : std_logic;-- set cdb_next = 1 when cdb_out is broadcasted
		rst : std_logic;-- synchronous reset
	end record;
	constant in_zero : in_type := (
		rs_zero,
		cdb_zero,
		'0',
		'0'
	);
	type out_type is record
		rs_full : std_logic;
		cdb_out : cdb_type;
	end record;
	constant out_zero : out_type := (
		'0',
		cdb_zero
	);
end alu_pack;

library ieee;
use ieee.std_logic_1164.all;
use work.common.all;
package fpu_pack is
	type op_type is (FADD_op, FMUL_op, FDIV_op, FSIN_op, FCOS_op, FATAN_op, FSQRT_op, FCMP_op, NOP_op);
	type rs_type is record
		op : op_type;
		countdown : std_logic_vector(2 downto 0);
		common : rs_common_type;
	end record;
	constant rs_zero : rs_type := (
		NOP_op,
		(others => '0'),
		rs_common_zero
	);
	type rs_array_type is array (0 to 2**rs_num_width-1) of rs_type;
	type in_type is record
		rs_in : rs_type;
		cdb_in : cdb_type;
		cdb_next : std_logic;-- set cdb_next = 1 when cdb_out is broadcasted
		rst : std_logic;-- synchronous reset
	end record;
	constant in_zero : in_type := (
		rs_zero,
		cdb_zero,
		'0',
		'0'
	);
	type out_type is record
		rs_full : std_logic;
		cdb_out : cdb_type;
	end record;
	constant out_zero : out_type := (
		'0',
		cdb_zero
	);
end fpu_pack;

library ieee;
use ieee.std_logic_1164.all;
use work.common.all;
package mem_pack is
	type op_type is (LOAD_op, STORE_op, NOP_op);
	type rs_type is record
		op : op_type;
		common : rs_common_type;
	end record;
	constant rs_zero : rs_type := (
		NOP_op,
		rs_common_zero
	);
	type rs_array_type is array (0 to 2**rs_num_width-1) of rs_type;
	type in_type is record
		rs_in : rs_type;
		cdb_in : cdb_type;
		cdb_next : std_logic;-- set cdb_next = 1 when cdb_out is broadcasted
		rst : std_logic;-- synchronous reset
	end record;
	constant in_zero : in_type := (
		rs_zero,
		cdb_zero,
		'0',
		'0'
	);
	type out_type is record
		rs_full : std_logic;
		cdb_out : cdb_type;
	end record;
	constant out_zero : out_type := (
		'0',
		cdb_zero
	);
end mem_pack;

library ieee;
use ieee.std_logic_1164.all;
use work.common.all;
package branch_pack is
	type op_type is (J_op, JR_op, JREQ_op, JRNEQ_op, JRGT_op, JRGTE_op, JRLT_op, JRLTE_op, NOP_op);
	type rs_type is record
		op : op_type;
		common : rs_common_type;
	end record;
	constant rs_zero : rs_type := (
		NOP_op,
		rs_common_zero
	);
	type rs_array_type is array (0 to 2**rs_num_width-1) of rs_type;
	type in_type is record
		rs_in : rs_type;
		cdb_in : cdb_type;
		cdb_next : std_logic;-- set cdb_next = 1 when cdb_out is broadcasted
		rst : std_logic;-- synchronous reset
	end record;
	constant in_zero : in_type := (
		rs_zero,
		cdb_zero,
		'0',
		'0'
	);
	type out_type is record
		rs_full : std_logic;
		cdb_out : cdb_type;
	end record;
	constant out_zero : out_type := (
		'0',
		cdb_zero
	);
end branch_pack;

