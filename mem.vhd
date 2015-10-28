library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_pack.all;
use work.common.all;

entity mem is
	port(
		clk, rst : in std_logic;
		mem_in : in in_type;
		mem_out : out out_type);
end mem;

architecture twoproc of mem is
	type reg_type is record
		rs : rs_array_type;
		rs_youngest, rs_oldest, rs_exec : rs_num_type;
		rs_full : std_logic;
		cdb_out : cdb_type;
	end record;
	constant reg_zero : reg_type := (
		(others => rs_zero),
		(others => '0'), (others => '0'), (others => '0'),
		'0',
		cdb_zero
	);
	signal r, r_in : reg_type := reg_zero;
begin
	mem_out.cdb_out <= r.cdb_out;
	mem_out.rs_full <= '1' when std_logic_vector(unsigned(r.rs_youngest)+1) = r.rs_oldest else '0';
	process(clk, rst)
	begin
		if rst = '1' then
			r <= reg_zero;
		elsif rising_edge(clk) then
			r <= r_in;
		end if;
	end process;
	process(mem_in, r)
		variable v : reg_type;
		variable exec_complete : boolean;
		variable t : rs_common_type;
		variable sramifin_v : sramif_in;
		variable transifin_v : transif_in_type;
		variable recvifin_v : recvif_in_type;
	begin
		v := r;
		-- update rs
		for i in r.rs'range loop
			v.rs(i).common.ra := register_update(r.rs(i).common.ra, mem_in.cdb_in);
			v.rs(i).common.rb := register_update(r.rs(i).common.rb, mem_in.cdb_in);
		end loop;
		-- execute
		for i in v.rs'range loop
			if r.rs(i).common.state = RS_Executing then
-- ERROR:Xst:1706, 1847
--				if r.rs(i).countdown = "000" then
				if false then
					case r.rs(i).op is
						when LOAD_op =>
							v.rs(i).common.result := mem_in.sramifout.rd;
						when IN_op =>
							v.rs(i).common.result := x"000000" & mem_in.recvifout.dout;
						when others =>
					end case;
					v.rs(i).common.state := RS_Done;
					v.rs(i).common.pc_next := std_logic_vector(unsigned(v.rs(i).common.pc) + 1);
				else
					v.rs(i).countdown := std_logic_vector(unsigned(r.rs(i).countdown) - 1);
				end if;
			end if;
		end loop;
		exec_complete := false;
		sramifin_v := sramif_in_zero;
		recvifin_v := recvif_in_zero;
		transifin_v := transif_in_zero;
		t := v.rs(to_integer(unsigned(r.rs_exec))).common;
		if rs_common_ready(t) then
			if v.rs(to_integer(unsigned(r.rs_exec))).has_dummy = '1' then
				exec_complete := mem_in.dummy_done = '1';
			else
				exec_complete := true;
			end if;
			if exec_complete then
				case v.rs(to_integer(unsigned(r.rs_exec))).op is
				when LOAD_op =>
					v.rs(to_integer(unsigned(r.rs_exec))).common.state := RS_Executing;
					v.rs(to_integer(unsigned(r.rs_exec))).countdown := "010";
					sramifin_v := (
						op => SRAM_LOAD,
						addr => t.ra.data(19 downto 0),
						wd => (others => '0')
					);
				when STORE_op =>
					v.rs(to_integer(unsigned(r.rs_exec))).common.state := RS_Done;
					v.rs(to_integer(unsigned(r.rs_exec))).common.pc_next := std_logic_vector(unsigned(v.rs(to_integer(unsigned(r.rs_exec))).common.pc) + 1);
					sramifin_v := (
						op => SRAM_STORE,
						addr => t.rb.data(19 downto 0),
						wd => t.ra.data
					);
				when IN_op =>
					if mem_in.recvifout.empty = '0' then
						v.rs(to_integer(unsigned(r.rs_exec))).common.state := RS_Executing;
						v.rs(to_integer(unsigned(r.rs_exec))).countdown := "000";
						recvifin_v := (rd_en => '1');
					else
						exec_complete := false;
					end if;
				when OUT_op =>
					if mem_in.transifout.full = '0' then
						v.rs(to_integer(unsigned(r.rs_exec))).common.state := RS_Done;
						v.rs(to_integer(unsigned(r.rs_exec))).common.pc_next := std_logic_vector(unsigned(v.rs(to_integer(unsigned(r.rs_exec))).common.pc) + 1);
						transifin_v := (
							wr_en => '1',
							din => t.ra.data(7 downto 0)
						);
					else
						exec_complete := false;
					end if;
				when others =>
				end case;
			end if;
			if exec_complete then
				v.rs_exec := std_logic_vector(unsigned(r.rs_exec) + 1);
			end if;
		end if;
		mem_out.sramifin <= sramifin_v;
		mem_out.recvifin <= recvifin_v;
		mem_out.transifin <= transifin_v;
		-- store new rs contents
		if r.rs_full = '0' and mem_in.rs_in.op /= NOP_op then
			v.rs(to_integer(unsigned(r.rs_youngest))) := mem_in.rs_in;
			v.rs_youngest := std_logic_vector(unsigned(r.rs_youngest) + 1);
		end if;
		if mem_in.cdb_next = '1' or r.cdb_out.tag.valid = '0' then
			-- clear rs
			if r.cdb_out.tag.valid = '1' then
				v.rs(to_integer(unsigned(r.rs_oldest))) := rs_zero;
				v.rs_oldest := std_logic_vector(unsigned(r.rs_oldest) + 1);
			end if;
			v.cdb_out := cdb_zero;
			if v.rs(to_integer(unsigned(v.rs_oldest))).common.state = RS_Done then
				t := v.rs(to_integer(unsigned(v.rs_oldest))).common;
				v.cdb_out := make_cdb_out(t);
			end if;
		end if;
		-- synchronous reset
		if mem_in.rst = '1' then
			r_in <= reg_zero;
		else
			r_in <= v;
		end if;
	end process;
end;
