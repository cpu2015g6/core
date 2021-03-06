library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.branch_pack.all;
use work.common.all;

entity branch is
	port(
		clk, rst : in std_logic;
		branch_in : in in_type;
		branch_out : out out_type);
end branch;

architecture twoproc of branch is
	type reg_type is record
		rs : rs_array_type;
		rs_full : std_logic;
		free_rs_num : rs_num_type;
		cdb_out : cdb_type;
		cdb_rs_num : rs_num_type;
	end record;
	constant reg_zero : reg_type := (
		(others => rs_zero),
		'0',
		(others => '0'),
		cdb_zero,
		(others => '0')
	);
	signal r, r_in : reg_type := reg_zero;
begin
	branch_out <= (
		rs_full => r.rs_full,
		cdb_out => r.cdb_out
	);
	process(clk, rst)
	begin
		if rst = '1' then
			r <= reg_zero;
		elsif rising_edge(clk) then
			r <= r_in;
		end if;
	end process;
	process(branch_in, r)
		variable v : reg_type;
		variable exec_done : boolean;
		variable ret_pc32 : std_logic_vector(31 downto 0);
		variable ra_data : std_logic_vector(31 downto 0);
		variable rb_data : std_logic_vector(31 downto 0);
		variable rc_data : std_logic_vector(31 downto 0);
	begin
		v := r;
		-- update rs
		for i in r.rs'range loop
			v.rs(i).common.ra := register_update(r.rs(i).common.ra, branch_in.cdb_in);
			v.rs(i).common.rb := register_update(r.rs(i).common.rb, branch_in.cdb_in);
			v.rs(i).common.rc := register_update(r.rs(i).common.rc, branch_in.cdb_in);
		end loop;
		-- execute
		exec_done := false;
		for i in v.rs'range loop
			ra_data := v.rs(i).common.ra.data;
			rb_data := v.rs(i).common.rb.data;
			rc_data := v.rs(i).common.rc.data;
			if rs_common_ready(v.rs(i).common) and not exec_done then
				ret_pc32 := std_logic_vector(unsigned((31-pc_width downto 0 => '0') & v.rs(i).common.pc) + 1);
				case v.rs(i).op is
					when JF_op =>
						if ra_data = (31 downto 0 => '0') then
							v.rs(i).common.pc_next := rb_data(pc_width-1 downto 0);
							v.rs(i).common.result := (others => '0');
							v.rs(i).taken := true;
						else
							v.rs(i).common.pc_next := std_logic_vector(unsigned(v.rs(i).common.pc) + 1);
							v.rs(i).common.result := rc_data;
							v.rs(i).taken := false;
						end if;
					when C_op =>
						v.rs(i).common.pc_next := ra_data(pc_width-1 downto 0);
						v.rs(i).common.result := ret_pc32;
						v.rs(i).taken := true;
					when JC_op =>
						if cmpc(v.rs(i).common.cond, ra_data, rb_data) then
							v.rs(i).common.pc_next := rc_data(pc_width-1 downto 0);
							v.rs(i).taken := true;
						else
							v.rs(i).common.pc_next := std_logic_vector(unsigned(v.rs(i).common.pc) + 1);
							v.rs(i).taken := false;
						end if;
					when FJC_op =>
						if fcmpc(v.rs(i).common.cond, ra_data, rb_data) then
							v.rs(i).common.pc_next := rc_data(pc_width-1 downto 0);
							v.rs(i).taken := true;
						else
							v.rs(i).common.pc_next := std_logic_vector(unsigned(v.rs(i).common.pc) + 1);
							v.rs(i).taken := false;
						end if;
					when NOP_op =>
				end case;
				v.rs(i).common.state := RS_Done;
				exec_done := true;
			end if;
		end loop;
		-- store new rs contents
		if r.rs_full = '0' and branch_in.rs_in.op /= NOP_op then
			v.rs(to_integer(unsigned(r.free_rs_num))) := branch_in.rs_in;
		end if;
		-- update output (cdb)
		if branch_in.cdb_next = '1' or r.cdb_out.tag.valid = '0' then
			-- clear rs
			if r.cdb_out.tag.valid = '1' then
				v.rs(to_integer(unsigned(r.cdb_rs_num))) := rs_zero;
			end if;
			-- select next rs for cdb
			v.cdb_out := cdb_zero;
			for i in v.rs'range loop
				if v.rs(i).common.state = RS_Done then
					v.cdb_out := make_cdb_out(v.rs(i).common, v.rs(i).taken);
					v.cdb_rs_num := std_logic_vector(to_unsigned(i, rs_num_width));
				end if;
			end loop;
		end if;
		-- update output (rs_full/free_rs_num)
		v.rs_full := '1';
		for i in v.rs'range loop
			if v.rs(i).common.state = RS_Invalid then
				v.free_rs_num := std_logic_vector(to_unsigned(i, rs_num_width));
				v.rs_full := '0';
			end if;
		end loop;
		if branch_in.rst = '1' then
			r_in <= reg_zero;
		else
			r_in <= v;
		end if;
	end process;
end;
