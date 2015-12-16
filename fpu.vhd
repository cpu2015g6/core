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
	component fadd is
	  port(clk:       in std_logic;
	       op1, op2:  in std_logic_vector(31 downto 0);
	       ans:       out std_logic_vector(31 downto 0) := x"00000000"
	       );
	end component;
	signal fadd_op1, fadd_op2 : std_logic_vector(31 downto 0) := (others => '0');
	signal fadd_ans : std_logic_vector(31 downto 0);
	component fmul is
	  port(clk:       in std_logic;
	       op1, op2:  in std_logic_vector(31 downto 0);
	       ans:       out std_logic_vector(31 downto 0) := x"00000000"
	       );
	end component;
	signal fmul_op1, fmul_op2 : std_logic_vector(31 downto 0) := (others => '0');
	signal fmul_ans : std_logic_vector(31 downto 0);
	component fsqrt is
	  port(clk:       in std_logic;
	       op:        in std_logic_vector(31 downto 0);
	       ans:       out std_logic_vector(31 downto 0) := x"00000000"
	       );
	end component;
	signal fsqrt_op1 : std_logic_vector(31 downto 0) := (others => '0');
	signal fsqrt_ans : std_logic_vector(31 downto 0);
	component finv is
	  port(clk:  in std_logic;
	       op:  in std_logic_vector(31 downto 0);
	       ans:  out std_logic_vector(31 downto 0) := x"00000000"
	       );
	end component;
	signal finv_op1 : std_logic_vector(31 downto 0) := (others => '0');
	signal finv_ans : std_logic_vector(31 downto 0);
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
	fpu_out <= (
		rs_full => r.rs_full,
		cdb_out => r.cdb_out
	);
	fadd_l : fadd
	port map(
		clk => clk,
		op1 => fadd_op1,
		op2 => fadd_op2,
		ans => fadd_ans
	);
	fmul_l : fmul
	port map(
		clk => clk,
		op1 => fmul_op1,
		op2 => fmul_op2,
		ans => fmul_ans
	);
	fsqrt_l : fsqrt
	port map(
		clk => clk,
		op => fsqrt_op1,
		ans => fsqrt_ans
	);
	finv_l : finv
	port map(
		clk => clk,
		op => finv_op1,
		ans => finv_ans
	);
	process(clk, rst)
	begin
		if rst = '1' then
			r <= reg_zero;
		elsif rising_edge(clk) then
			r <= r_in;
		end if;
	end process;
	process(fpu_in, r, fadd_ans, fmul_ans, fsqrt_ans, finv_ans)
		variable v : reg_type;
		variable ra_data : std_logic_vector(31 downto 0);
		variable rb_data : std_logic_vector(31 downto 0);
		variable fadd_used : boolean;
		variable fadd_op1_v, fadd_op2_v : std_logic_vector(31 downto 0);
		variable fmul_used : boolean;
		variable fmul_op1_v, fmul_op2_v : std_logic_vector(31 downto 0);
		variable fsqrt_used : boolean;
		variable fsqrt_op1_v : std_logic_vector(31 downto 0);
		variable finv_used : boolean;
		variable finv_op1_v : std_logic_vector(31 downto 0);
	begin
		v := r;
		-- update rs
		for i in r.rs'range loop
			v.rs(i).common.ra := register_update(r.rs(i).common.ra, fpu_in.cdb_in);
			v.rs(i).common.rb := register_update(r.rs(i).common.rb, fpu_in.cdb_in);
		end loop;
		-- execute
		fadd_used := false;
		fmul_used := false;
		fsqrt_used := false;
		finv_used := false;
		fadd_op1_v := (others => '0');
		fadd_op2_v := (others => '0');
		fmul_op1_v := (others => '0');
		fmul_op2_v := (others => '0');
		fsqrt_op1_v := (others => '0');
		finv_op1_v := (others => '0');
		for i in r.rs'range loop
			if rs_common_ready(r.rs(i).common) then
				ra_data := r.rs(i).common.ra.data;
				rb_data := r.rs(i).common.rb.data;
				case r.rs(i).op is
					when FADD_op =>
						if not fadd_used then
							fadd_op1_v := ra_data;
							fadd_op2_v := rb_data;
							fadd_used := true;
							v.rs(i).common.state := RS_Executing;
							v.rs(i).countdown := "000";
						end if;
					when FMUL_op =>
						if not fmul_used then
							fmul_op1_v := ra_data;
							fmul_op2_v := rb_data;
							fmul_used := true;
							v.rs(i).common.state := RS_Executing;
							v.rs(i).countdown := "001";
						end if;
					when FINV_op =>
						if not finv_used then
							finv_op1_v := ra_data;
							finv_used := true;
							v.rs(i).common.state := RS_Executing;
							v.rs(i).countdown := "001";
						end if;
					when FSQRT_op =>
						if not fsqrt_used then
							fsqrt_op1_v := ra_data;
							fsqrt_used := true;
							v.rs(i).common.state := RS_Executing;
							v.rs(i).countdown := "001";
						end if;
					when FCMP_op =>
						if ra_data(30 downto 0) = (30 downto 0 => '0') and rb_data(30 downto 0) = (30 downto 0 => '0') then
							v.rs(i).common.result := eq_const;
						elsif ra_data(31) = '0' and rb_data(31) = '1' then
							v.rs(i).common.result := gt_const;
						elsif ra_data(31) = '1' and rb_data(31) = '0' then
							v.rs(i).common.result := lt_const;
						else
							if ra_data = rb_data then
								v.rs(i).common.result := eq_const;
							elsif (ra_data(31) = '1') xor (unsigned(ra_data(30 downto 0)) < unsigned(rb_data(30 downto 0))) then
								v.rs(i).common.result := lt_const;
							else
								v.rs(i).common.result := gt_const;
							end if;
						end if;
						v.rs(i).common.state := RS_Done;
					when NOP_op =>
--					when others =>
				end case;
				v.rs(i).common.pc_next := std_logic_vector(unsigned(r.rs(i).common.pc) + 1);
			elsif r.rs(i).common.state = RS_Executing then
				if r.rs(i).countdown = "000" then
				case r.rs(i).op is
					when FADD_op =>
						v.rs(i).common.result := fadd_ans;
						v.rs(i).common.state := RS_Done;
					when FMUL_op =>
						v.rs(i).common.result := fmul_ans;
						v.rs(i).common.state := RS_Done;
					when FINV_op =>
						v.rs(i).common.result := finv_ans;
						v.rs(i).common.state := RS_Done;
					when FSQRT_op =>
						v.rs(i).common.result := fsqrt_ans;
						v.rs(i).common.state := RS_Done;
					when others =>
				end case;
				else
					v.rs(i).countdown := std_logic_vector(unsigned(r.rs(i).countdown) - 1);
				end if;
			end if;
		end loop;
		fadd_op1 <= fadd_op1_v;
		fadd_op2 <= fadd_op2_v;
		fmul_op1 <= fmul_op1_v;
		fmul_op2 <= fmul_op2_v;
		fsqrt_op1 <= fsqrt_op1_v;
		finv_op1 <= finv_op1_v;
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
					v.cdb_out := make_cdb_out(v.rs(i).common, false);
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
