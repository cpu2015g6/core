library ieee;
use ieee.std_logic_1164.all;
package common is
	constant pht_array_width : integer := 10;
	subtype ghr_type is std_logic_vector(pht_array_width-1 downto 0);
	subtype pht_entry_type is std_logic_vector(1 downto 0);
	constant pht_entry_zero : pht_entry_type := "00";
	constant rs_num_width : integer := 2;
	subtype rs_num_type is std_logic_vector(rs_num_width-1 downto 0);
	constant pc_width : integer := 15;
	subtype pc_type is std_logic_vector(pc_width-1 downto 0);
	constant rob_num_width : integer := 4;
	subtype rob_num_type is std_logic_vector(rob_num_width-1 downto 0);
	constant reg_num_width : integer := 5;
	subtype reg_num_type is std_logic_vector(reg_num_width-1 downto 0);
	constant reg_num_zero : reg_num_type := (others => '1');
	type opc_type is (NOP_opc, LIMM_opc, CMP_opc, IN_opc, OUT_opc, J_opc, JR_opc, JREQ_opc, JRNEQ_opc, JRGT_opc, JRGTE_opc, JRLT_opc, JRLTE_opc, STW_opc, LDW_opc, ADD_opc, SUB_opc, AND_opc, OR_opc, XOR_opc, NOT_opc, SLL_opc, SRL_opc, FADD_opc, FMUL_opc, FINV_opc, FSQRT_opc, FCMP_opc);
	type unit_type is (ALU_UNIT, FPU_UNIT, MEM_UNIT, BRANCH_UNIT, NULL_UNIT);
	constant gt_const : std_logic_vector(31 downto 0) := x"00000002";
	constant eq_const : std_logic_vector(31 downto 0) := x"00000001";
	constant lt_const : std_logic_vector(31 downto 0) := x"00000000";
	subtype gshare_entry_type is std_logic_vector(pc_width+2-1 downto 0);
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
		ghr : ghr_type;
		pht_entry : pht_entry_type;
		branch_target : pc_type;
		need_dummy_rob_entry : std_logic;
	end record;
	constant decode_result_zero : decode_result_type := (
		NOP_opc,
		reg_num_zero, reg_num_zero, reg_num_zero,
		(others => '0'),
		(others => '0'), (others => '0'),
		(others => '0'),
		pht_entry_zero,
		(others => '0'),
		'0'
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
		taken : boolean;
	end record;
	constant cdb_zero : cdb_type := (
		(others => '0'),
		rs_tag_zero,
		(others => '0'),
		false
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
	type rob_state_type is (ROB_Invalid, ROB_Executing, ROB_Done, ROB_Reset, ROB_Dummy);
	type rob_type is record
		state : rob_state_type;
		taken : boolean;
		opc : opc_type;
		pc, pc_next : pc_type;
		ghr : ghr_type;
		pht_entry : pht_entry_type;
		branch_target : pc_type;
		result : std_logic_vector(31 downto 0);
		reg_num : reg_num_type;
	end record;
	constant rob_zero : rob_type := (
		ROB_Invalid,
		false,
		NOP_opc,
		(others => '0'), (others => '0'),
		(others => '0'),
		pht_entry_zero,
		(others => '0'),
		(others => '0'),
		(others => '0')
	);
	type sramif_op is (SRAM_NOP, SRAM_LOAD, SRAM_STORE);
	type sramif_in is record
		op : sramif_op;
		addr : std_logic_vector(19 downto 0);
		wd : std_logic_vector(31 downto 0);
	end record;
	constant sramif_in_zero : sramif_in := (
		op => SRAM_NOP,
		addr => (others => '0'),
		wd => (others => '0')
	);
	type sramif_out is record
		data_enable : std_logic;
		rd : std_logic_vector(31 downto 0);
	end record;
	constant sramif_out_zero : sramif_out := (
		'0',
		(others => '0')
	);
	type recvif_in_type is record
		rd_en : std_logic;
	end record;
	constant recvif_in_zero : recvif_in_type := (
		rd_en => '0'
	);
	type recvif_out_type is record
		dout : std_logic_vector(7 downto 0);
		full, empty : std_logic;
	end record;
	constant recvif_out_zero : recvif_out_type := (
		dout => (others => '0'),
		full => '0',
		empty => '0'
	);
	type transif_in_type is record
		wr_en : std_logic;
		din : std_logic_vector(7 downto 0);
	end record;
	constant transif_in_zero : transif_in_type := (
		wr_en => '0',
		din => (others => '0')
	);
	type transif_out_type is record
		full : std_logic;
	end record;
	constant transif_out_zero : transif_out_type := (
		full => '0'
	);
	type cpu_top_in_type is record
		sramifout : sramif_out;
		recvifout : recvif_out_type;
		transifout : transif_out_type;
	end record;
	type cpu_top_out_type is record
		sramifin : sramif_in;
		recvifin : recvif_in_type;
		transifin : transif_in_type;
	end record;
	function register_update(reg : register_type; cdb : cdb_type) return register_type;
	function make_cdb_out(rs_common : rs_common_type;taken : boolean) return cdb_type;
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
	function make_cdb_out(rs_common : rs_common_type;taken : boolean) return cdb_type is
	begin
		return (
			tag => (
				valid => '1',
				rob_num => rs_common.rob_num
			),
			data => rs_common.result,
			pc_next => rs_common.pc_next,
			taken => taken
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
	type op_type is (FADD_op, FMUL_op, FINV_op, FSQRT_op, FCMP_op, NOP_op);
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
use ieee.numeric_std.all;
package cache_pack is
	type cache_op_type is (READ_cache_op, WRITE_cache_op, NOP_cache_op);
	constant cache_width : integer := 10;
	constant tag_width : integer := 20 - cache_width;
	constant cache_entry_width : integer := tag_width + 32 + 2;
	constant port_width : integer := 2;
	subtype id_type is std_logic_vector(port_width-1 downto 0);
	constant id_zero : id_type := (others => '0');
	type cache_in_type is record
		id : id_type;
		op : cache_op_type;
		addr : std_logic_vector(19 downto 0);
		data : std_logic_vector(31 downto 0);
	end record;
	constant cache_in_zero : cache_in_type := (
		id_zero,
		NOP_cache_op,
		(others => '0'),
		(others => '0')
	);
	type out_port_type is record
		data : std_logic_vector(31 downto 0);
		en : std_logic;
	end record;
	constant out_port_zero : out_port_type := (
		(others => '0'),
		'0'
	);
	type out_port_array_type is array (0 to 2**port_width-1) of out_port_type;
	type cache_entry_type is record
		modified, valid : std_logic;
		tag : std_logic_vector(20-1-cache_width downto 0);
		data : std_logic_vector(31 downto 0);
	end record;
	constant cache_entry_zero : cache_entry_type := (
		'0', '0',
		(others => '0'),
		(others => '0')
	);
	type victim_entry_type is record
		modified, valid : std_logic;
		addr : std_logic_vector(19 downto 0);
		data : std_logic_vector(31 downto 0);
	end record;
	constant victim_entry_zero : victim_entry_type := (
		'0', '0',
		(others => '0'),
		(others => '0')
	);
	constant victim_width : integer := 2;
	constant victim_size : integer := victim_width**2;
	subtype log_type is std_logic_vector((victim_size*(victim_size-1))/2 - 1 downto 0);
	type victim_array_type is array(0 to victim_size-1) of victim_entry_type;
	type victim_type is record
		varray : victim_array_type;
		log : log_type;
	end record;
	constant victim_zero : victim_type := (
		(others => victim_entry_zero),
		(others => '0')
	);
	function set_newest(log : log_type; i : integer) return log_type;
	function to_cache_entry_type(victim_entry : victim_entry_type) return cache_entry_type;
	function to_victim_entry_type(cache_entry : cache_entry_type; loweraddr : std_logic_vector(cache_width-1 downto 0)) return victim_entry_type;
	function oldest_victim(log : std_logic_vector(5 downto 0)) return integer;
	function search_victim(victim : victim_type; addr: std_logic_vector(19 downto 0)) return integer;
	function hit(addr : std_logic_vector(19 downto 0); cache_entry : cache_entry_type) return boolean;
	function replace(victim : victim_type; i : integer; entry : victim_entry_type) return victim_type;
end cache_pack;

package body cache_pack is
	function to_cache_entry_type(victim_entry : victim_entry_type) return cache_entry_type is
	begin
		return (
			valid => victim_entry.valid,
			modified => victim_entry.modified,
			tag => victim_entry.addr(19 downto 20-cache_width),
			data => victim_entry.data
		);
	end to_cache_entry_type;
	function to_victim_entry_type(cache_entry : cache_entry_type; loweraddr : std_logic_vector(cache_width-1 downto 0)) return victim_entry_type is
	begin
		return (
			valid => cache_entry.valid,
			modified => cache_entry.modified,
			addr => cache_entry.tag & loweraddr,
			data => cache_entry.data
		);
	end to_victim_entry_type;
--   newer
--  |0|1|2|3
-- 0| | | | 
-- 1|0| | | 
-- 2|1|3| | 
-- 3|2|4|5| 
	function oldest_victim(log : std_logic_vector(5 downto 0)) return integer is
	begin
		if log(0) = '0' and log(1) = '0' and log(2) = '0' then
			return 0;
		elsif log(0) = '1' and log(3) = '0' and log(4) = '0' then
			return 1;
		elsif log(1) = '1' and log(3) = '1' and log(5) = '0' then
			return 2;
		--elsif log(2) = '1' and log(4) = '1' and log(5) = '1' then
		--	return 3;
		else
			return 3;
		end if;
	end oldest_victim;
	function set_newest(log : log_type; i : integer) return log_type is
		variable ret : log_type;
	begin
		ret := log;
		if i = 0 then
			ret(0) := '1';
			ret(1) := '1';
			ret(2) := '1';
		elsif i = 1 then
			ret(0) := '0';
			ret(3) := '1';
			ret(4) := '1';
		elsif i = 2 then
			ret(1) := '0';
			ret(3) := '0';
			ret(5) := '1';
		elsif i = 3 then
			ret(2) := '0';
			ret(4) := '0';
			ret(5) := '0';
		end if;
		return ret;
	end set_newest;
	function search_victim(victim : victim_type; addr: std_logic_vector(19 downto 0)) return integer is
		variable ret : integer;
	begin
		ret := -1;
		for i in victim.varray'range loop
			if victim.varray(i).addr = addr and victim.varray(i).valid = '1' then
				ret := i;
			end if;
		end loop;
		return ret;
	end search_victim;
	function hit(addr : std_logic_vector(19 downto 0); cache_entry : cache_entry_type) return boolean is
	begin
		return (addr(19 downto 20-cache_width) = cache_entry.tag) and (cache_entry.valid = '1');
	end hit;
	function replace(victim : victim_type; i : integer; entry : victim_entry_type) return victim_type is
		variable v : victim_type;
	begin
		v := victim;
		if entry.valid = '1' then
			v.varray(i) := entry;
			v.log := set_newest(v.log, i);
		else
			v.varray(i) := victim_entry_zero;
		end if;
		return v;
	end replace;
end cache_pack;

library ieee;
use ieee.std_logic_1164.all;
use work.common.all;
use work.cache_pack.all;
package mem_pack is
--	type op_type is (IN_op, OUT_op, LOAD_op, STORE_op, NOP_op);
	subtype op_type is std_logic_vector(2 downto 0);
	constant LOAD_op : op_type := "001";
	constant STORE_op : op_type := "010";
	constant IN_op : op_type := "011";
	constant OUT_op : op_type := "100";
	constant NOP_op : op_type := "000";
	type rs_type is record
		op : op_type;
		has_dummy : std_logic;
		common : rs_common_type;
		id : id_type;
	end record;
	constant rs_zero : rs_type := (
		NOP_op,
		'0',
		rs_common_zero,
		(others => '0')
	);
	type rs_array_type is array (0 to 2**rs_num_width-1) of rs_type;
	type in_type is record
		rs_in : rs_type;
		cdb_in : cdb_type;
		cdb_next : std_logic;-- set cdb_next = 1 when cdb_out is broadcasted
		rst : std_logic;-- synchronous reset
		dummy_done : std_logic;
		sramifout : sramif_out;
		recvifout : recvif_out_type;
		transifout : transif_out_type;
	end record;
	constant in_zero : in_type := (
		rs_zero,
		cdb_zero,
		'0',
		'0',
		'0',
		sramif_out_zero,
		recvif_out_zero,
		transif_out_zero
	);
	type out_type is record
		rs_full : std_logic;
		cdb_out : cdb_type;
		sramifin : sramif_in;
		recvifin : recvif_in_type;
		transifin : transif_in_type;
	end record;
	constant out_zero : out_type := (
		'0',
		cdb_zero,
		sramif_in_zero,
		recvif_in_zero,
		transif_in_zero
	);
end mem_pack;

library ieee;
use ieee.std_logic_1164.all;
use work.common.all;
package branch_pack is
	type op_type is (J_op, JR_op, JREQ_op, JRNEQ_op, JRGT_op, JRGTE_op, JRLT_op, JRLTE_op, NOP_op);
	type rs_type is record
		op : op_type;
		taken : boolean;
		common : rs_common_type;
	end record;
	constant rs_zero : rs_type := (
		NOP_op,
		false,
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

