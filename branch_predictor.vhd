library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common.all;

package gshare_pack is
--type pht_array_type is array (0 to 2**pht_array_width-1) of pht_entry_type;
--type btb_array_type is array (0 to 2**pht_array_width-1) of btb_entry_type;
--type gshare_type is record
--	pht_array : pht_array_type;
--	ghr : std_logic_vector(2**(rob_num_width+1)-1 downto 0);
--	ghr_i : std_logic_vector(rob_num_width-1 downto 0);
----	btb : btb_array_type;
--end record;
--constant gshare_zero : gshare_type := (
--	(others => pht_entry_zero),
--	(others => '0'),
--	(others => '0'),
----	(others => btb_entry_zero)
--);

function saturated_increment(pht_entry : pht_entry_type) return pht_entry_type;
function saturated_decrement(pht_entry : pht_entry_type) return pht_entry_type;
function pht_taken(pht_entry : pht_entry_type) return boolean;
--function predict(g : gshare_type;pc : std_logic_vector) return boolean;
--function gshare_reset(g : gshare_type) return gshare_type;
--function ghr_push(g : gshare_type;taken : boolean) return gshare_type;
--function ghr_commit(g : gshare_type;taken : boolean) return gshare_type;
--function btb_lookup(g : gshare_type;pc : pc_type) return btb_entry_type;
function btb_hit(b : btb_entry_type;pc : pc_type) return boolean;
function btb_decode(b : btb_type) return btb_entry_type;
function btb_encode(b : btb_entry_type) return btb_type;
--procedure btb_register(g : in gshare_type;pc : in pc_type;target : in pc_type; btb_we : out std_logic; btb_addr : out std_logic_vector(9 downto 0); btb_entry : out btb_entry_type);
--procedure gshare_commit(g_in : in gshare_type;taken : in boolean;pc : in pc_type;target : in pc_type; btb_we : out std_logic; btb_addr : out std_logic_vector(pht_array_width-1 downto 0); btb_entry : out btb_entry_type; g_out : out gshare_type);
--procedure gshare_entry_decode(g : in gshare_entry_type; target : out pc_type; pht : out pht_entry_type);
--procedure gshare_entry_encode(target : in pc_type; pht : in pht_entry_type; g : out gshare_entry_type);
function ghr_next(ghr : ghr_type; taken : boolean) return ghr_type;
function pht_commit(taken : boolean; pht_entry: pht_entry_type) return pht_entry_type;
end gshare_pack;

package body gshare_pack is
function saturated_increment(pht_entry : pht_entry_type) return pht_entry_type is
	variable v : pht_entry_type;
begin
	if pht_entry = "11" then
		v := pht_entry;
	else
		v := std_logic_vector(unsigned(pht_entry) + 1);
	end if;
	return v;
end saturated_increment;
function saturated_decrement(pht_entry : pht_entry_type) return pht_entry_type is
	variable v : pht_entry_type;
begin
	if pht_entry = "00" then
		v := pht_entry;
	else
		v := std_logic_vector(unsigned(pht_entry) - 1);
	end if;
	return v;
end saturated_decrement;
function pht_taken(pht_entry : pht_entry_type) return boolean is
begin
	return pht_entry(1) = '1';
end pht_taken;
--function predict(g : gshare_type;pc : std_logic_vector) return boolean is
--	variable index : std_logic_vector(pht_array_width-1 downto 0);
--begin
--	index := pc(pht_array_width-1 downto 0) xor g.ghr(pht_array_width-1 downto 0);
--	return pht_taken(g.pht_array(to_integer(unsigned(index))));
--end predict;
--function gshare_reset(g : gshare_type) return gshare_type is
--	variable v : gshare_type;
--begin
--	v := g;
--	v.ghr := std_logic_vector(unsigned(v.ghr) srl to_integer(unsigned(v.ghr_i)));
--	v.ghr_i := (others => '0');
--	return v;
--end gshare_reset;
--function ghr_push(g : gshare_type;taken : boolean) return gshare_type is
--	variable v : gshare_type;
--	variable t : std_logic;
--begin
--	v := g;
--	if taken then
--		t := '1';
--	else
--		t := '0';
--	end if;
--	v.ghr := v.ghr(v.ghr'length-2 downto 0) & t;
--	v.ghr_i := std_logic_vector(unsigned(v.ghr_i) + 1);
--	return v;
--end ghr_push;
--function ghr_commit(g : gshare_type;taken : boolean) return gshare_type is
--	variable v : gshare_type;
--begin
--	v := g;
--	if v.ghr_i /= (v.ghr_i'range => '0') then
--		v.ghr_i := std_logic_vector(unsigned(v.ghr_i) - 1);
--		if taken then
--			v.ghr(to_integer(unsigned(v.ghr_i))) := '1';
--		else
--			v.ghr(to_integer(unsigned(v.ghr_i))) := '0';
--		end if;
--	end if;
--	return v;
--end ghr_commit;
--function btb_lookup(g : gshare_type;pc : pc_type) return btb_entry_type is
--	variable btb_entry : btb_entry_type;
--begin
--	btb_entry := g.btb(to_integer(unsigned(pc(pht_array_width-1 downto 0))));
--	if btb_entry.tag /= pc(pc_width-1 downto pht_array_width) then
--		btb_entry := btb_entry_zero;
--	end if;
--	return btb_entry;
--end btb_lookup;
function btb_hit(b : btb_entry_type;pc : pc_type) return boolean is
begin
	return b.valid and b.tag = pc(pc_width-1 downto pht_array_width);
end btb_hit;
function btb_decode(b : btb_type) return btb_entry_type is
begin
	return (
		valid => b(pc_width+pc_width-pht_array_width) = '1',
		tag => b(pc_width+pc_width-pht_array_width-1 downto pc_width),
		target => b(pc_width-1 downto 0)
	);
end btb_decode;
function btb_encode(b : btb_entry_type) return btb_type is
	variable valid : std_logic;
begin
	if b.valid then
		valid := '1';
	else
		valid := '0';
	end if;
	return valid & b.tag & b.target;
end btb_encode;
--procedure btb_register(g : in gshare_type;pc : in pc_type;target : in pc_type; btb_we : out std_logic; btb_addr : out std_logic_vector(9 downto 0); btb_entry : out btb_entry_type) is
--begin
--	btb_we := '1';
--	btb_addr := pc(pht_array_width-1 downto 0);
--	btb_entry := (
--		valid => true,
--		tag => pc(pc_width-1 downto pht_array_width),
--		target => target
--	);
--end btb_register;
--procedure gshare_commit(g_in : in gshare_type;taken : in boolean;pc : in pc_type;target : in pc_type; btb_we : out std_logic; btb_addr : out std_logic_vector(pht_array_width-1 downto 0); btb_entry : out btb_entry_type; g_out : out gshare_type) is
--	variable v : gshare_type;
--	variable ind : integer;
--begin
--	v := g_in;
--	ind := to_integer(unsigned(pc(pht_array_width-1 downto 0)));
--	btb_we := '0';
--	btb_addr := (others => '0');
--	btb_entry := btb_entry_zero;
--	v := ghr_commit(v, taken);
--	if taken then
--		v.pht_array(ind) := saturated_increment(v.pht_array(ind));
--		btb_register(v, pc, target, btb_we, btb_addr, btb_entry);
--	else
--		v.pht_array(ind) := saturated_decrement(v.pht_array(ind));
--	end if;
--	g_out := v;
--end gshare_commit;
--procedure gshare_entry_decode(g : in gshare_entry_type; target : out pc_type; pht : out pht_entry_type) is
--begin
--	target := g(pc_width-1 downto 0);
--	pht := g(pc_width+1 downto pc_width);
--end gshare_entry_decode;
--procedure gshare_entry_encode(target : in pc_type; pht : in pht_entry_type; g : out gshare_entry_type) is
--begin
--	g := pht & target;
--end gshare_entry_encode;
function ghr_next(ghr : ghr_type; taken : boolean) return ghr_type is
begin
	if taken then
		return ghr(ghr'length-2 downto 0) & '1';
	else
		return ghr(ghr'length-2 downto 0) & '0';
	end if;
end ghr_next;
function pht_commit(taken : boolean; pht_entry: pht_entry_type) return pht_entry_type is
begin
	if taken then
		return saturated_increment(pht_entry);
	else
		return saturated_decrement(pht_entry);
	end if;
end pht_commit;
end gshare_pack;
