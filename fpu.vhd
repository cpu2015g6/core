library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.fpu_pack.all;
use work.common.all;

entity fpu is
	port(
		clk, rst : in std_logic;
		fpu_in : in in_type;
		fpu_out : out out_type);
end fpu;

architecture twoproc of fpu is
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
	process(fpu_in, r)
		variable v : reg_type;
		variable exec_done : boolean;
		variable ra_data : std_logic_vector(31 downto 0);
		variable rb_data : std_logic_vector(31 downto 0);
	begin
		v := r;
		-- update rs
		for i in r.rs'range loop
			v.rs(i).common.ra := register_update(r.rs(i).common.ra, fpu_in.cdb_in);
			v.rs(i).common.rb := register_update(r.rs(i).common.rb, fpu_in.cdb_in);
		end loop;
		-- execute
		exec_done := false;
		for i in v.rs'range loop
			if rs_common_ready(v.rs(i).common) and not exec_done then
				ra_data := v.rs(i).common.ra.data;
				rb_data := v.rs(i).common.rb.data;
				case v.rs(i).op is
					when FADD_op =>
					when FMUL_op =>
					when FDIV_op =>
					when FSIN_op =>
					when FCOS_op =>
					when FATAN_op =>
					when FSQRT_op =>
					when FCMP_op =>
					when NOP_op =>
--					when others =>
				end case;
--				v.rs(i).common.state := RS_Done;
				v.rs(i).common.pc_next := std_logic_vector(unsigned(v.rs(i).common.pc) + 1);
				exec_done := true;
			end if;
		end loop;
		-- store new rs contents
		if r.rs_full = '0' and fpu_in.rs_in.op /= NOP_op then
			v.rs(to_integer(unsigned(r.free_rs_num))) := fpu_in.rs_in;
		end if;
		if fpu_in.cdb_next = '1' or r.cdb_out.tag.valid = '0' then
			-- clear rs
			if r.cdb_out.tag.valid = '1' then
				v.rs(to_integer(unsigned(r.cdb_rs_num))) := rs_zero;
			end if;
			-- select next rs for cdb
			v.cdb_out := cdb_zero;
			for i in v.rs'range loop
				if v.rs(i).common.state = RS_Done then
					v.cdb_out := make_cdb_out(v.rs(i).common);
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
		if fpu_in.rst = '1' then
			r_in <= reg_zero;
		else
			r_in <= v;
		end if;
	end process;
end;
