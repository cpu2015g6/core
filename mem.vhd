library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_pack.all;
use work.cache_pack.all;
use work.common.all;

entity mem is
	port(
		clk, rst : in std_logic;
		rs_in_op : in op_type;
		rs_in_common : in rs_common_type;
		cdb_in : in cdb_type;
		cdb_next : in std_logic;-- set cdb_next = 1 when cdb_out is broadcasted
		sync_rst : in std_logic;-- synchronous reset
		store_commit, out_commit, in_commit : in std_logic;
		sramifout : in sramif_out;
		recvifout : in recvif_out_type;
		transifout : in transif_out_type;
		mem_out : out out_type);
end mem;

architecture twoproc of mem is
	type store_buffer_entry_type is record
		valid, commit_ready : boolean;
		addr : std_logic_vector(19 downto 0);
		wd : std_logic_vector(31 downto 0);
	end record;
	constant store_buffer_entry_zero : store_buffer_entry_type := (
		false, false,
		(others => '0'),
		(others => '0')
	);
	type store_buffer_type is array(0 to 3) of store_buffer_entry_type;
	function is_store_buffer_full(sb : store_buffer_type) return boolean is
	begin
		return sb(sb'length-1).valid;
	end is_store_buffer_full;
	function search_store_buffer(sb : store_buffer_type; addr : std_logic_vector(19 downto 0)) return integer is
	variable ind : integer;
	begin
		ind := -1;
		for i in 0 to sb'length-1 loop
			if sb(i).valid and sb(i).addr = addr then
				ind := i;
			end if;
		end loop;
		return ind;
	end search_store_buffer;
	function push_store_buffer(sb : store_buffer_type; addr : std_logic_vector(19 downto 0); wd : std_logic_vector(31 downto 0)) return store_buffer_type is
		variable done : boolean;
		variable ret_sb : store_buffer_type;
	begin
		done := false;
		ret_sb := sb;
		for i in 0 to sb'length-1 loop
			if not done and not sb(i).valid then
				done := true;
				ret_sb(i) := (
					valid => true,
					commit_ready => false,
					addr => addr,
					wd => wd
				);
			end if;
		end loop;
		return ret_sb;
	end push_store_buffer;
	function clear_store_buffer(sb : store_buffer_type) return store_buffer_type is
		variable ret_sb : store_buffer_type;
	begin
		for i in 0 to sb'length-1 loop
			if sb(i).commit_ready then
				ret_sb(i) := sb(i);
			else
				ret_sb(i) := store_buffer_entry_zero;
			end if;
		end loop;
		return ret_sb;
	end clear_store_buffer;
	function set_commit_ready_store_buffer(sb : store_buffer_type) return store_buffer_type is
		variable done : boolean;
		variable ret_sb : store_buffer_type;
	begin
		done := false;
		ret_sb := sb;
		for i in 0 to sb'length-1 loop
			if not done and (not sb(i).commit_ready and sb(i).valid) then
				done := true;
				ret_sb(i).commit_ready := true;
			end if;
		end loop;
		return ret_sb;
	end;
	type out_buffer_entry_type is record
		valid, commit_ready : boolean;
		data : std_logic_vector(7 downto 0);
	end record;
	constant out_buffer_entry_zero : out_buffer_entry_type := (
		false, false,
		(others => '0')
	);
	type out_buffer_type is array(0 to 3) of out_buffer_entry_type;
	function is_out_buffer_full(ob : out_buffer_type) return boolean is
	begin
		return ob(ob'length-1).valid;
	end is_out_buffer_full;
	function push_out_buffer(ob : out_buffer_type;data : std_logic_vector(7 downto 0)) return out_buffer_type is
		variable done : boolean;
		variable ret_ob : out_buffer_type;
	begin
		done := false;
		ret_ob := ob;
		for i in 0 to ob'length-1 loop
			if not done and not ob(i).valid then
				done := true;
				ret_ob(i).valid := true;
				ret_ob(i).data := data;
			end if;
		end loop;
		return ret_ob;
	end push_out_buffer;
	function clear_out_buffer(ob : out_buffer_type) return out_buffer_type is
		variable ret_ob : out_buffer_type;
	begin
		for i in 0 to ob'length-1 loop
			if ob(i).commit_ready then
				ret_ob(i) := ob(i);
			else
				ret_ob(i) := out_buffer_entry_zero;
			end if;
		end loop;
		return ret_ob;
	end clear_out_buffer;
	function set_commit_ready_out_buffer(ob : out_buffer_type) return out_buffer_type is
		variable done : boolean;
		variable ret_ob : out_buffer_type;
	begin
		done := false;
		ret_ob := ob;
		for i in 0 to ob'length-1 loop
			if not done and (not ob(i).commit_ready and ob(i).valid) then
				done := true;
				ret_ob(i).commit_ready := true;
			end if;
		end loop;
		return ret_ob;
	end set_commit_ready_out_buffer;
	type countdown_type is array (0 to 2**rs_num_width-1) of std_logic_vector(2 downto 0);
	type reg_type is record
		rs : rs_array_type;
		store_buffer : store_buffer_type;
		out_buffer : out_buffer_type;
		in_valid : boolean;
		countdown : countdown_type;
		rs_youngest, rs_oldest, rs_exec : rs_num_type;
		rs_full : std_logic;
		cdb_out : cdb_type;
	end record;
	constant reg_zero : reg_type := (
		(others => rs_zero),
		(others => store_buffer_entry_zero),
		(others => out_buffer_entry_zero),
		true,
		(others => (others => '0')),
		(others => '0'), (others => '0'), (others => '0'),
		'0',
		cdb_zero
	);
	signal r, r_in : reg_type := reg_zero;
	function rs_executing_update(
		rs : rs_type;
		out_port : out_port_array_type
	) return rs_type is
		variable new_rs : rs_type;
		variable out_port_entry : out_port_type;
	begin
		new_rs := rs;
		out_port_entry := out_port(to_integer(unsigned(rs.id)));
		case rs.op is
			when LDW_op =>
				if out_port_entry.en = '1' then
					new_rs.common.result := out_port_entry.data;
					new_rs.common.state := RS_Done;
					new_rs.common.pc_next := std_logic_vector(unsigned(rs.common.pc) + 1);
				end if;
			when others =>
		end case;
		return new_rs;
	end rs_executing_update;
	signal mem_in : in_type;
	component cache is
		port(
			clk, rst : in std_logic;
			op : in cache_op_type;
			din : in std_logic_vector(31 downto 0);
			addr : in std_logic_vector(19 downto 0);
			sramifout : in sramif_out;
			sramifin : out sramif_in;
			id : out std_logic_vector(1 downto 0);
			out_port : out out_port_array_type;
			busy : out std_logic
		);
	end component;
	signal cache_op : cache_op_type := NOP_cache_op;
	signal cache_din : std_logic_vector(31 downto 0) := (others => '0');
	signal cache_addr : std_logic_vector(19 downto 0) := (others => '0');
	signal id : id_type;
	signal out_port : out_port_array_type;
	signal cache_busy : std_logic;
begin
	mem_in.rs_in <= (
		op => rs_in_op,
		common => rs_in_common,
		id => id_zero
	);
	mem_in.cdb_in <= cdb_in;
	mem_in.cdb_next <= cdb_next;
	mem_in.rst <= sync_rst;
	mem_in.store_commit <= store_commit;
	mem_in.in_commit <= in_commit;
	mem_in.out_commit <= out_commit;
	mem_in.sramifout <= sramifout;
	mem_in.recvifout <= recvifout;
	mem_in.transifout <= transifout;
	mem_out.cdb_out <= r.cdb_out;
	mem_out.rs_full <= r.rs_full;
	cache_l : cache port map(
		clk => clk,
		rst => rst,
		op => cache_op,
		din => cache_din,
		addr => cache_addr,
		sramifout => mem_in.sramifout,
		sramifin => mem_out.sramifin,
		id => id,
		out_port => out_port,
		busy => cache_busy
	);
	process(clk, rst)
	begin
		if rst = '1' then
			r <= reg_zero;
		elsif rising_edge(clk) then
			r <= r_in;
		end if;
	end process;
	process(mem_in, r, id, out_port, cache_busy)
		variable v, v_reset : reg_type;
		variable exec_complete : boolean;
		variable cache_used : boolean;
		variable t : rs_common_type;
--		variable sramifin_v : sramif_in;
		variable transifin_v : transif_in_type;
		variable recvifin_v : recvif_in_type;
		variable exec_i : integer;
		variable cache_op_v : cache_op_type;
		variable cache_din_v : std_logic_vector(31 downto 0);
		variable cache_addr_v : std_logic_vector(19 downto 0);
		variable addr : std_logic_vector(19 downto 0);
		variable ind : integer;
	begin
		v := r;
		v_reset := reg_zero;
		cache_op_v := NOP_cache_op;
		cache_din_v := (others => '0');
		cache_addr_v := (others => '0');
		-- update rs
		for i in r.rs'range loop
			v.rs(i).common.ra := register_update(r.rs(i).common.ra, mem_in.cdb_in);
			v.rs(i).common.rb := register_update(r.rs(i).common.rb, mem_in.cdb_in);
			v.rs(i).common.rc := register_update(r.rs(i).common.rc, mem_in.cdb_in);
			if r.rs(i).common.state = RS_Executing then
				if v.countdown(i) = "000" then
					v.rs(i) := rs_executing_update(r.rs(i), out_port);
				else
					v.countdown(i) := std_logic_vector(unsigned(v.countdown(i)) - 1);
				end if;
			end if;
		end loop;
		cache_used := false;
--		sramifin_v := sramif_in_zero;
		recvifin_v := recvif_in_zero;
		transifin_v := transif_in_zero;
		exec_i := to_integer(unsigned(r.rs_exec));
		t := r.rs(exec_i).common;
		if rs_common_ready(t) then
			exec_complete := true;
			case r.rs(exec_i).op is
			when LDW_op =>
				addr := std_logic_vector(unsigned(t.ra.data(19 downto 0)) + unsigned(t.rb.data(19 downto 0)));
				ind := search_store_buffer(r.store_buffer, addr);
				if ind /= -1 then
					v.rs(exec_i).common.state := RS_Done;
					v.rs(exec_i).common.result := r.store_buffer(ind).wd;
					v.rs(exec_i).common.pc_next := std_logic_vector(unsigned(t.pc) + 1);
				elsif cache_busy = '0' then
					v.rs(exec_i).common.state := RS_Executing;
					v.rs(exec_i).id := id;
					v.countdown(exec_i) := "000";
					cache_op_v := READ_cache_op;
					cache_addr_v := addr;
					cache_used := true;
				else
					exec_complete := false;
				end if;
			when STW_op =>
				if is_store_buffer_full(r.store_buffer) then
					exec_complete := false;
				else
					v.rs(exec_i).common.state := RS_Done;
					v.rs(exec_i).common.pc_next := std_logic_vector(unsigned(t.pc) + 1);
					v.store_buffer := push_store_buffer(v.store_buffer, std_logic_vector(unsigned(t.ra.data(19 downto 0)) + unsigned(t.rc.data(19 downto 0))), t.rb.data);
				end if;
			when IN_op =>
				if r.in_valid and mem_in.recvifout.empty = '0' then
					v.rs(exec_i).common.state := RS_Done;
					v.rs(exec_i).common.pc_next := std_logic_vector(unsigned(t.pc) + 1);
					v.rs(exec_i).common.result := x"000000" & mem_in.recvifout.dout;
					v.in_valid := false;
				else
					exec_complete := false;
				end if;
			when OUT_op =>
				if is_out_buffer_full(v.out_buffer) then
					exec_complete := false;
				else
					v.rs(exec_i).common.state := RS_Done;
					v.rs(exec_i).common.pc_next := std_logic_vector(unsigned(t.pc) + 1);
					v.out_buffer := push_out_buffer(v.out_buffer, t.ra.data(7 downto 0));
				end if;
			when NOP_op =>
--			when others =>
			end case;
			if exec_complete then
				v.rs_exec := std_logic_vector(unsigned(r.rs_exec) + 1);
			end if;
		end if;
		if not cache_used and cache_busy = '0' then
			if v.store_buffer(0).valid and v.store_buffer(0).commit_ready then
				cache_op_v := WRITE_cache_op;
				cache_addr_v := v.store_buffer(0).addr;
				cache_din_v := v.store_buffer(0).wd;
				v.store_buffer(0 to 2) := v.store_buffer(1 to 3);
				v.store_buffer(3) := store_buffer_entry_zero;
			end if;
		end if;
		if mem_in.transifout.full = '0' then
			if v.out_buffer(0).valid and v.out_buffer(0).commit_ready then
				transifin_v := (
					wr_en => '1',
					din => v.out_buffer(0).data
				);
				v.out_buffer(0 to 2) := v.out_buffer(1 to 3);
				v.out_buffer(3) := out_buffer_entry_zero;
			end if;
		end if;
		if mem_in.store_commit = '1' then
			v.store_buffer := set_commit_ready_store_buffer(v.store_buffer);
		end if;
		if mem_in.out_commit = '1' then
			v.out_buffer := set_commit_ready_out_buffer(v.out_buffer);
		end if;
		if mem_in.in_commit = '1' then
--			assert not r.in_valid report "BUG @ r.in_valid";
			v.in_valid := true;
			recvifin_v := (rd_en => '1');
		end if;
		cache_op <= cache_op_v;
		cache_din <= cache_din_v;
		cache_addr <= cache_addr_v;
--		mem_out.sramifin <= sramifin_v;
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
				v.cdb_out := make_cdb_out(t, false);
			end if;
		end if;
		if std_logic_vector(unsigned(v.rs_youngest)+1) = v.rs_oldest then
			v.rs_full := '1';
		else
			v.rs_full := '0';
		end if;
		-- synchronous reset
		if mem_in.rst = '1' then
			v_reset.out_buffer := clear_out_buffer(v.out_buffer);
			v_reset.store_buffer := clear_store_buffer(v.store_buffer);
			r_in <= v_reset;
		else
			r_in <= v;
		end if;
	end process;
end;
