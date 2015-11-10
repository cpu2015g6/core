library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_pack.all;
use work.common.all;

entity mem is
	port(
		clk, rst : in std_logic;
		rs_in_op : in op_type;
		rs_in_has_dummy : in std_logic;
		rs_in_common : in rs_common_type;
		cdb_in : in cdb_type;
		cdb_next : in std_logic;-- set cdb_next = 1 when cdb_out is broadcasted
		sync_rst : in std_logic;-- synchronous reset
		dummy_done : in std_logic;
		sramifout : in sramif_out;
		recvifout : in recvif_out_type;
		transifout : in transif_out_type;
		mem_out : out out_type);
end mem;

architecture twoproc of mem is
	type countdown_type is array (0 to 2**rs_num_width-1) of std_logic_vector(2 downto 0);
	type reg_type is record
		rs : rs_array_type;
		countdown : countdown_type;
		rs_youngest, rs_oldest, rs_exec : rs_num_type;
		rs_full : std_logic;
		cdb_out : cdb_type;
		dummy_done : std_logic;
	end record;
	constant reg_zero : reg_type := (
		(others => rs_zero),
		(others => (others => '0')),
		(others => '0'), (others => '0'), (others => '0'),
		'0',
		cdb_zero,
		'0'
	);
	signal r, r_in : reg_type := reg_zero;
	function rs_executing_update(rs : rs_type;load_in : std_logic_vector(31 downto 0);serial_in : std_logic_vector(7 downto 0)) return rs_type is
		variable new_rs : rs_type;
	begin
		new_rs := rs;
		case rs.op is
			when LOAD_op =>
				new_rs.common.result := load_in;
			when IN_op =>
				new_rs.common.result := x"000000" & serial_in;
			when others =>
		end case;
		new_rs.common.state := RS_Done;
		new_rs.common.pc_next := std_logic_vector(unsigned(rs.common.pc) + 1);
		return new_rs;
	end rs_executing_update;
	signal mem_in : in_type;
begin
	mem_in.rs_in <= (
		op => rs_in_op,
		has_dummy => rs_in_has_dummy,
		common => rs_in_common
	);
	mem_in.cdb_in <= cdb_in;
	mem_in.cdb_next <= cdb_next;
	mem_in.rst <= sync_rst;
	mem_in.dummy_done <= dummy_done;
	mem_in.sramifout <= sramifout;
	mem_in.recvifout <= recvifout;
	mem_in.transifout <= transifout;
	mem_out.cdb_out <= r.cdb_out;
	mem_out.rs_full <= r.rs_full;
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
		variable exec_i : integer;
	begin
		v := r;
		if mem_in.dummy_done = '1' then
			v.dummy_done := '1';
		end if;
		-- update rs
		for i in r.rs'range loop
			v.rs(i).common.ra := register_update(r.rs(i).common.ra, mem_in.cdb_in);
			v.rs(i).common.rb := register_update(r.rs(i).common.rb, mem_in.cdb_in);
			if r.rs(i).common.state = RS_Executing then
				if v.countdown(i) = "000" then
					v.rs(i) := rs_executing_update(r.rs(i), mem_in.sramifout.rd, mem_in.recvifout.dout);
				else
					v.countdown(i) := std_logic_vector(unsigned(v.countdown(i)) - 1);
				end if;
			end if;
		end loop;
		exec_complete := false;
		sramifin_v := sramif_in_zero;
		recvifin_v := recvif_in_zero;
		transifin_v := transif_in_zero;
		exec_i := to_integer(unsigned(r.rs_exec));
		t := v.rs(exec_i).common;
		if rs_common_ready(t) then
			if v.rs(exec_i).has_dummy = '0' or v.dummy_done = '1' then
				exec_complete := true;
			else
				exec_complete := false;
			end if;
			if exec_complete then
				case v.rs(exec_i).op is
				when LOAD_op =>
					v.rs(exec_i).common.state := RS_Executing;
					v.countdown(exec_i) := "010";
					sramifin_v := (
						op => SRAM_LOAD,
						addr => t.ra.data(19 downto 0),
						wd => (others => '0')
					);
				when STORE_op =>
					v.rs(exec_i).common.state := RS_Done;
					v.rs(exec_i).common.pc_next := std_logic_vector(unsigned(t.pc) + 1);
					sramifin_v := (
						op => SRAM_STORE,
						addr => t.rb.data(19 downto 0),
						wd => t.ra.data
					);
				when IN_op =>
					if mem_in.recvifout.empty = '0' then
						v.rs(exec_i).common.state := RS_Executing;
						v.countdown(exec_i) := "000";
						recvifin_v := (rd_en => '1');
					else
						exec_complete := false;
					end if;
				when OUT_op =>
					if mem_in.transifout.full = '0' then
						v.rs(exec_i).common.state := RS_Done;
						v.rs(exec_i).common.pc_next := std_logic_vector(unsigned(t.pc) + 1);
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
				v.dummy_done := '0';
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
				v.countdown(to_integer(unsigned(r.rs_oldest))) := (others => '0');
				v.rs_oldest := std_logic_vector(unsigned(r.rs_oldest) + 1);
			end if;
			v.cdb_out := cdb_zero;
			if v.rs(to_integer(unsigned(v.rs_oldest))).common.state = RS_Done then
				t := v.rs(to_integer(unsigned(v.rs_oldest))).common;
				v.cdb_out := make_cdb_out(t);
			end if;
		end if;
		if std_logic_vector(unsigned(v.rs_youngest)+1) = v.rs_oldest then
			v.rs_full := '1';
		else
			v.rs_full := '0';
		end if;
		-- synchronous reset
		if mem_in.rst = '1' then
			r_in <= reg_zero;
		else
			r_in <= v;
		end if;
	end process;
end;
