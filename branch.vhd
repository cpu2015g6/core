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
	begin
		v := r;
		-- update rs
		for i in r.rs'range loop
			v.rs(i).common.ra := register_update(r.rs(i).common.ra, branch_in.cdb_in);
			v.rs(i).common.rb := register_update(r.rs(i).common.rb, branch_in.cdb_in);
		end loop;
		-- execute
		exec_done := false;
		for i in v.rs'range loop
			if rs_common_ready(v.rs(i).common) and not exec_done then
				case v.rs(i).op is
					when J_op =>
						v.rs(i).common.pc_next := std_logic_vector(signed(v.rs(i).common.pc) + signed(v.rs(i).common.ra.data));
					when JZ_op =>
						if v.rs(i).common.ra.data = (31 downto 0 => '0') then
							v.rs(i).common.pc_next := std_logic_vector(unsigned(v.rs(i).common.pc) + unsigned(v.rs(i).common.rb.data));
						else
							v.rs(i).common.pc_next := std_logic_vector(unsigned(v.rs(i).common.pc) + 1);
						end if;
					when JR_op =>
					when others =>
				end case;
				exec_done := true;
			end if;
		end loop;
		-- store new rs contents
		if r.rs_full = '0' and branch_in.rs_in.op /= NOP_op then
			v.rs(to_integer(unsigned(r.free_rs_num))) := branch_in.rs_in;
		end if;
		-- update output (cdb)
		-- the second condition is usually not necessary
		if branch_in.cdb_next = '1' and r.cdb_out.tag.valid = '1' then
			-- clear rs
			v.rs(to_integer(unsigned(r.cdb_rs_num))) := rs_zero;
			-- select next rs for cdb
			v.cdb_out := cdb_zero;
			for i in v.rs'range loop
				if v.rs(i).common.state = RS_Done then
					v.cdb_out := (
						tag => (
							valid => '1',
							rob_num => v.rs(i).common.rob_num
						),
						reg_num => v.rs(i).common.rt_num,
						data => v.rs(i).common.result,
						pc_next => v.rs(i).common.pc_next
					);
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
		r_in <= v;
	end process;
end;
