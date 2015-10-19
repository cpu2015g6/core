library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.alu_pack.all;
use work.common.all;

entity alu is
	port(
		clk, rst : in std_logic;
		alu_in : in in_type;
		alu_out : out out_type);
end alu;

architecture twoproc of alu is
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
	procedure find_free_rs(
		rs : in rs_array_type;
		full : out std_logic;
		rs_num : out rs_num_type) is
	begin
	end find_free_rs;
begin
	alu_out.rs_full <= r.rs_full;
	process(clk, rst)
	begin
		if rst = '1' then
			r <= reg_zero;
		elsif rising_edge(clk) then
			r <= r_in;
		end if;
	end process;
	process(alu_in, r)
		variable v : reg_type;
		variable exec_done : boolean;
		variable ra_data : std_logic_vector(31 downto 0);
		variable rb_data : std_logic_vector(31 downto 0);
	begin
		v := r;
		-- update rs
		for i in r.rs'range loop
			v.rs(i).common.ra := register_update(r.rs(i).common.ra, alu_in.cdb_in);
			v.rs(i).common.rb := register_update(r.rs(i).common.rb, alu_in.cdb_in);
		end loop;
		-- execute
		exec_done := false;
		for i in v.rs'range loop
			if rs_common_ready(v.rs(i).common) and not exec_done then
				ra_data := v.rs(i).common.ra.data;
				rb_data := v.rs(i).common.rb.data;
				case v.rs(i).op is
					when LIMM_op =>
						v.rs(i).common.result := ra_data;
					when ADD_op =>
						v.rs(i).common.result := std_logic_vector(unsigned(ra_data) + unsigned(rb_data));
					when SUB_op =>
						v.rs(i).common.result := std_logic_vector(unsigned(ra_data) - unsigned(rb_data));
					when AND_op =>
						v.rs(i).common.result := ra_data and rb_data;
					when OR_op =>
						v.rs(i).common.result := ra_data or rb_data;
					when XOR_op =>
						v.rs(i).common.result := ra_data xor rb_data;
					when NOT_op =>
						v.rs(i).common.result := not ra_data;
					when EQ_op =>
						if ra_data = rb_data then
							v.rs(i).common.result := (others => '0');
						else
							v.rs(i).common.result := (others => '1');
						end if;
					when NEQ_op =>
						if ra_data /= rb_data then
							v.rs(i).common.result := (others => '0');
						else
							v.rs(i).common.result := (others => '1');
						end if;
					when GT_op =>
						if signed(ra_data) < signed(rb_data) then
							v.rs(i).common.result := (others => '0');
						else
							v.rs(i).common.result := (others => '1');
						end if;
					when GTE_op =>
						if signed(ra_data) <= signed(rb_data) then
							v.rs(i).common.result := (others => '0');
						else
							v.rs(i).common.result := (others => '1');
						end if;
					when others =>
					-- TODO
				end case;
				v.rs(i).common.state := RS_Done;
				v.rs(i).common.pc_next := std_logic_vector(unsigned(v.rs(i).common.pc) + 1);
				exec_done := true;
			end if;
		end loop;
		-- store new rs contents
		if r.rs_full = '0' and alu_in.rs_in.op /= NOP_op then
			v.rs(to_integer(unsigned(r.free_rs_num))) := alu_in.rs_in;
		end if;
		-- update output (cdb)
		-- the second condition is usually not necessary
		if alu_in.cdb_next = '1' and r.cdb_out.tag.valid = '1' then
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
