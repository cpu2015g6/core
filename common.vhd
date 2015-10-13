library ieee;
use ieee.std_logic_1164.all;
package common is
	constant rs_num_width : integer := 3;
	subtype rs_num_type is std_logic_vector(rs_num_width-1 downto 0);
	constant pc_width : integer := 14;
	subtype pc_type is std_logic_vector(pc_width-1 downto 0);
	constant rob_num_width : integer := 5;
	subtype rob_num_type is std_logic_vector(rob_num_width-1 downto 0);
	type opc_type is (NOP_opc, LIMM_opc, J_opc, JZ_opc, JR_opc, STW_opc, LDW_opc, ADD_opc, SUB_opc, AND_opc, OR_opc, XOR_opc, NOT_opc, SHL_opc, SHR_opc, EQ_opc, NEQ_opc, GT_opc, GTE_opc, LT_opc, LTE_opc, FADD_opc, FMUL_opc, FDIV_opc, FSIN_opc, FCOS_opc, FATAN_opc, FSQRT_opc);
	type unit_type is (ALU_UNIT, FPU_UNIT, MEM_UNIT, BRANCH_UNIT, NULL_UNIT);
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
		rt, ra, rb : std_logic_vector(7 downto 0);
		imm : std_logic_vector(15 downto 0);
		pc, pc_predicted : pc_type;
	end record;
	constant decode_result_zero : decode_result_type := (
		NOP_opc,
		(others => '1'), (others => '1'), (others => '1'),
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
	type register_array_type is array (0 to 255) of register_type;
	constant register_array_zero : register_array_type := (others => register_zero);
	--common data bus
	type cdb_type is record
		data : std_logic_vector(31 downto 0);
		tag : rs_tag_type;
		reg_num : std_logic_vector(7 downto 0);
		reset : std_logic;
		pc_restart : pc_type;
	end record;
	constant cdb_zero : cdb_type := (
		(others => '0'),
		rs_tag_zero,
		(others => '0'),
		'0',
		(others => '0')
	);
--	type cdb_branch_type is record
--		rob_num : rob_num_type;
--		reset : std_logic;
--		pc_restart : pc_type;
--	end record;
--	constant cdb_branch_zero : cdb_branch_type := (
--		(others => '0'),
--		'0',
--		(others => '0')
--	);
	type rs_state_type is (RS_Invalid, RS_Waiting, RS_Executing, RS_Done, RS_Reserved);
	type rs_common_type is record
		state : rs_state_type;
		ra, rb : register_type;
		result : std_logic_vector(31 downto 0);
		rt_num : std_logic_vector(7 downto 0);
		rob_num : rob_num_type;
	end record;
	constant rs_common_zero : rs_common_type := (
		RS_Invalid,
		register_zero, register_zero,
		(others => '0'),
		(others => '0'),
		(others => '0')
	);
	function rs_common_ready(r : rs_common_type) return boolean;
	-- ROB_Executing : ROB is valid but the result has not yet been obtained
	-- ROB_Done : The result is available
	-- ROB_Reset : CPU enters rollback mode due to a mispredicted branch and restarts execution from pc_next
	type rob_state_type is (ROB_Invalid, ROB_Executing, ROB_Done, ROB_Reset);
	type rob_type is record
		state : rob_state_type;
		pc_restart : pc_type;
		result : std_logic_vector(31 downto 0);
		reg_num : std_logic_vector(7 downto 0);
	end record;
	constant rob_zero : rob_type := (
		ROB_Invalid,
		(others => '0'),
		(others => '0'),
		(others => '0')
	);
	function register_update(reg : register_type; cdb : cdb_type) return register_type;
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
end common;

library ieee;
use ieee.std_logic_1164.all;
use work.common.all;
package alu_pack is
	type op_type is (LIMM_op, ADD_op, SUB_op, AND_op, OR_op, XOR_op, NOT_op, SHL_op, SHR_op, NOP_op);
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
	end record;
	constant in_zero : in_type := (
		rs_zero,
		cdb_zero,
		'0'
	);
	type out_type is record
		rs_full : std_logic;
--		free_rs_num : rs_num_type;
		cdb_out : cdb_type;
	end record;
	constant out_zero : out_type := (
		'0',
--		(others => '0'),
		cdb_zero
	);
end alu_pack;

library ieee;
use ieee.std_logic_1164.all;
use work.common.all;
package fpu_pack is
	type op_type is (FADD_op, FMUL_op, NOP_op);
	type rs_type is record
		op : op_type;
		common : rs_common_type;
	end record;
	constant rs_zero : rs_type := (
		NOP_op,
		rs_common_zero
	);
	type in_type is record
		rs_in : rs_type;
		cdb_in : cdb_type;
		cdb_next : std_logic;-- set cdb_next = 1 when cdb_out is broadcasted
	end record;
	type out_type is record
		rs_full : std_logic;
--		free_rs_num : rs_num_type;
		cdb_out : cdb_type;
	end record;
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
	type in_type is record
		rs_in : rs_type;
		cdb_in : cdb_type;
		cdb_next : std_logic;-- set cdb_next = 1 when cdb_out is broadcasted
	end record;
	type out_type is record
		rs_full : std_logic;
--		free_rs_num : rs_num_type;
		cdb_out : cdb_type;
	end record;
end mem_pack;

library ieee;
use ieee.std_logic_1164.all;
use work.common.all;
package branch_pack is
	type op_type is (J_op, JZ_op, JR_opc, NOP_op);
	type rs_type is record
		rs_state : rs_state_type;
		pc, predicted_pc : pc_type;
	end record;
	constant rs_zero : rs_type := (
		RS_Invalid,
		(others => '0'), (others => '0')
	);
	type in_type is record
		rs_branch : rs_type;
		cdb_in : cdb_type;
		cdb_next : std_logic;-- set cdb_next = 1 when cdb_out is broadcasted
	end record;
	type out_type is record
		rs_full : std_logic;
		cdb_out : cdb_type;
	end record;
end branch_pack;

