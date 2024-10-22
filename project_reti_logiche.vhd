---------------------------------------------------------------------------------
--
-- Prova Finale (Progetto di Reti Logiche)
-- Prof. William Fornaciari - Anno 2021/2022
--
-- Daniele Cicala (Codice Persona 10630561 Matricola 910392)
--
--------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_unsigned.all;
use IEEE.numeric_std.all;

entity project_reti_logiche is
    Port (
	    i_clk     : in std_logic;
        i_start   : in std_logic;
        i_rst     : in std_logic;
        i_data    : in std_logic_vector(7 downto 0);
        o_address : out std_logic_vector(15 downto 0);
        o_done    : out std_logic;
        o_en      : out std_logic;
        o_we      : out std_logic;
        o_data    : out std_logic_vector (7 downto 0)
);
end project_reti_logiche;

architecture Behavioral of project_reti_logiche is
component datapath is
    Port(
        i_clk     : in std_logic;
        i_start   : in std_logic;
        i_rst     : in std_logic;
        i_data    : in std_logic_vector(7 downto 0);
        o_address : out std_logic_vector(15 downto 0);
        o_done    : out std_logic;
        o_en      : out std_logic;
        o_we      : out std_logic;
        o_data    : out std_logic_vector (7 downto 0);
        rcol_load : in std_logic;
        rrow_load : in std_logic;
        rmin_load : in std_logic;
        rshift_load : in std_logic;
        rdata_load : in std_logic;
        dim_read : in std_logic;
        c1_sum : in std_logic;
        c1_reset : in std_logic;
        c2_reset : in std_logic;
        csel_1 : in std_logic;
        csel_2 : in std_logic;
        sub_delta : in std_logic;
        w_sel : in std_logic
        );
end component;

signal rcol_load : STD_LOGIC;
signal rrow_load : STD_LOGIC;
signal rmin_load : STD_LOGIC;
signal rmax_load : STD_LOGIC;
signal rdata_load : STD_LOGIC;
signal rshift_load : std_logic;
signal o_rcol : std_logic_vector (7 downto 0);
signal o_rrow : std_logic_vector (7 downto 0);
signal o_rmin : std_logic_vector (7 downto 0);
signal o_rmax : std_logic_vector (7 downto 0);
signal o_rshift : std_logic_vector (3 downto 0);
signal o_rnewdata : std_logic_vector (7 downto 0);
signal c1 : std_logic_vector (7 downto 0);
signal c2 : std_logic_vector (7 downto 0);
signal c1_sum : std_logic;
signal c1_reset : std_logic;
signal c2_reset : std_logic;
signal csel_1 : std_logic;
signal csel_2 : std_logic;
signal dim_read : std_logic;
signal delta : std_logic_vector (7 downto 0);
signal sub_delta : std_logic;
signal read_value : STD_LOGIC_VECTOR (15 downto 0);
signal r_sum : std_logic;
signal r_reset : std_logic;
signal write_value : STD_LOGIC_VECTOR (15 downto 0);
signal w_sum : std_logic;
signal w_read : std_logic;
signal w_sel : std_logic;

type STATE is (S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14);
signal current_state, next_state : STATE;
    
function calc_shift_level(delta: std_logic_vector(7 downto 0)) return std_logic_vector is 
    begin
    if(delta = "00000000") then
	   return "1000";
	elsif(delta < "000000011") then
	   return "0111";
	elsif(delta < "00000111") then
	   return "0110";
	elsif(delta < "00001111") then
	   return "0101";
	elsif(delta < "00011111") then
	   return "0100";
	elsif(delta < "00111111") then
	   return "0011";
	elsif(delta < "01111111") then
	   return "0010";
    elsif(delta < "11111111") then
	   return "0001";
	else
	   return "0000";
    end if;   
end function;  

function shift_level(value: std_logic_vector(7 downto 0) ; shift_value: std_logic_vector(3 downto 0)) return std_logic_vector is 
    begin
    if(value = "00000000") then
        return value;
    end if;    
    case shift_value is
	when "0000" =>
	   return value;
	when "0001" =>
	   if(value(7) = '1') then
	       return "11111111";  
	   end if; 	
	   return (value(6 downto 0) & "0"); 
	when "0010" =>
	   if(value(7 downto 6) >= "01") then
	       return "11111111";   
	   end if;
	   return (value(5 downto 0) & "00"); 
	when "0011" =>
	   if(value(7 downto 5) >= "001") then
	       return "11111111";   
	   end if;
	   return (value(4 downto 0) & "000"); 
	when "0100" =>
	   if(value(7 downto 4) >= "0001") then
	       return "11111111";   
	   end if;
	   return (value(3 downto 0) & "0000"); 
	when "0101" =>
	   if(value(7 downto 3) >= "00001") then
	       return "11111111";   
	   end if;
	   return (value(2 downto 0) & "00000");
	when "0110" =>
	   if(value(7 downto 2) >= "000001") then
	       return "11111111";   
	   end if;
	   return (value(1 downto 0) & "000000");
	when "0111" =>
	   if(value(7 downto 1) >= "0000001") then
	       return "11111111";   
	   end if;
	   return (value(0) & "0000000");
	when "1000" =>
	   return "11111111";
	when others =>
	   return "00000000";   
    end case;
end function;      
    
begin      
  process(i_clk, i_rst)
  begin  
    if(i_rst = '1') then
        current_state <= S0;
    elsif i_clk'event and i_clk = '1' then
        current_state <= next_state;
    end if;
  end process;
  
  process(current_state, i_rst, i_start, c1_reset, c2_reset, o_rcol, o_rrow, read_value, write_value)
  begin
    next_state <= current_state;
    case current_state is
        when S0 =>
            if i_start = '1' then
                next_state <= S1;
            end if;
        when S1 =>
            if i_rst = '1' then
                next_state <= S0;
            else
                next_state <= S2;
            end if;
        when S2 =>
            if i_rst = '1' then
                next_state <= S0;
            else
                next_state <= S3;
            end if;
        when S3 =>
            if i_rst = '1' then
                next_state <= S0;
            else
                next_state <= S4;
            end if;
        when S4 =>
            if i_rst = '1' then
                next_state <= S0;
            else
                if(o_rcol = "00000000" or o_rrow = "00000000") then
                  next_state <= S14;  
                elsif (o_rcol = "00000001" and o_rrow = "00000001") then
                  next_state <= S7;
		        else 
		          next_state <= S5; 
		        end if;
            end if; 
        when S5 =>
            if i_rst = '1' then
                next_state <= S0;
            else
                if (c2_reset = '1') then
                  next_state <= S7;
		        else 
		          next_state <= S6; 
		        end if;
            end if;
        when S6 =>
            if i_rst = '1' then
                next_state <= S0;
            else
                if (c2_reset = '1') then
                  next_state <= S7;
		        else 
		          next_state <= S5; 
		        end if;
            end if;
        when S7 =>
            if i_rst = '1' then
                next_state <= S0;
            else
                next_state <= S8;
            end if;
        when S8 =>
            if i_rst = '1' then
                next_state <= S0;
            else
                next_state <= S9;
            end if;            
        when S9 =>
            if i_rst = '1' then
                next_state <= S0;
            else
                next_state <= S10;
            end if;
        when S10 =>
            if i_rst = '1' then
                next_state <= S0;
    		else
		        next_state <= S11;
            end if;
        when S11 =>
            if i_rst = '1' then
                next_state <= S0;
            else
                next_state <= S12;
            end if;
        when S12 =>
            if i_rst = '1' then
                next_state <= S0;
            else
                next_state <= S13;
            end if; 
        when S13 =>
            if i_rst = '1' then
                next_state <= S0;
            else
                if (c2_reset = '1') then
		          next_state <= S14;
    		    else
		          next_state <= S10;
		        end if;
            end if;  
        when S14 =>
            next_state <= S0;
    end case;
  end process;
 
  process(current_state)
  begin
  
  c1_sum <= '0';
  csel_1 <= '0';
  csel_2 <= '0';
  dim_read <= '0';
  rcol_load <= '0';
  rrow_load <= '0';
  rmin_load <= '0';
  rmax_load <= '0';
  rshift_load <= '0';
  rdata_load <= '0';
  o_done <= '0';
  o_en <= '0';
  o_we <= '0';
  r_sum <= '0';
  r_reset <= '0';
  w_sum <= '0';
  w_read <= '0';
  w_sel <= '0';
  sub_delta <= '0'
  ;
    case current_state is
        when S0 =>
        when S1 =>
            o_en <= '1';
            r_sum <= '1';
        when S2 =>
            o_en <= '1';
	        rcol_load <= '1';
        when S3 =>
            r_sum <= '1';
            o_en <= '1';
            c1_sum <= '1';
	        rrow_load <= '1';
	    when S4 =>
            o_en <= '1';
            dim_read <= '1'; 
        when S5 =>
            o_en <= '1';
            c1_sum <= '1';
            rmin_load <= '1';
            rmax_load <= '1';
            dim_read <= '1';
            r_sum <= '1';  
        when S6 =>
            o_en <= '1'; 
            csel_2 <= '1';	
            rmin_load <= '1';
            rmax_load <= '1';
            dim_read <= '1';  
        when S7 =>
            o_en <= '1';
            w_read <= '1';
            r_reset <= '1';
            csel_1 <= '1';
            sub_delta <= '1';	
            rmin_load <= '1';
            rmax_load <= '1';
            dim_read <= '1';  
        when S8 =>
            o_en <= '1';    
            rshift_load <= '1';
            dim_read <= '1';
        when S9 => 
            o_en <= '1';
	        rdata_load <= '1';
            dim_read <= '1';
        when S10 =>
            r_sum <= '1';
            w_sum <= '1';
            o_en <= '1';
            dim_read <= '1';
        when S11 =>
            o_en <= '1';  
            w_sel <= '1';
            c1_sum<= '1';
	        o_we <= '1';
            dim_read <= '1';
	    when S12 =>
            o_en <= '1'; 
            dim_read <= '1';   
	    when S13 =>
            o_en <= '1';    
	        rdata_load <= '1';
            dim_read <= '1';
        when S14 => 
            o_done <= '1';
        end case;
    end process;   
      
    process(i_clk, i_rst, next_state)
    begin
        if(i_rst = '1'or next_state = S0) then
            o_rcol <= "00000000";
        elsif i_clk'event and i_clk = '1' then
            if(rcol_load = '1') then
                o_rcol <= i_data;  
            end if;
        end if;
    end process;

    process(i_clk, i_rst, next_state)
    begin
        if(i_rst = '1' or next_state = S0) then
            c1 <= "00000000";
        elsif i_clk'event and i_clk = '1' then
            if(c1_reset = '1'or c2_reset = '1') then
                c1 <= "00000000";
	        elsif (c1_sum = '1') then
		        c1 <= c1 + "00000001";    
            end if;
        end if;
    end process;
    
    c1_reset <= '1' when ((c1 = o_rcol and dim_read = '1') or csel_1 = '1') else '0';
    
    process(i_clk, i_rst, next_state)
    begin
        if(i_rst = '1' or next_state = S0) then
            o_rrow <= "00000000";
        elsif i_clk'event and i_clk = '1' then
            if(rrow_load = '1') then
                o_rrow <= i_data;
            end if;
        end if;
    end process;
    
    process(i_clk, i_rst, next_state)
    begin
        if(i_rst = '1' or next_state = S0) then
            c2 <= "00000000";
        elsif i_clk'event and i_clk = '1' then
            if(c2_reset = '1') then
                c2 <= "00000000";
	        elsif (c1_reset = '1') then
		        c2 <= c2 + "00000001";
            end if;
        end if;
    end process;
    
    c2_reset <= '1' when ((c2 = (o_rrow -"0000000000000001") and c1 = o_rcol and dim_read = '1' and csel_2 = '1') or (c2 = o_rrow and dim_read = '1') or csel_1 = '1') else '0';
    
    process(i_clk, i_rst, next_state)
    begin
        if(i_rst = '1' or next_state = S0) then
            o_rmin <= "11111111";
        elsif i_clk'event and i_clk = '1' then
            if(rmin_load = '1' and i_data < o_rmin) then
            	o_rmin <= i_data;
            end if;
        end if;
    end process;

    process(i_clk, i_rst, next_state)
    begin
        if(i_rst = '1' or next_state = S0) then
            o_rmax <= "00000000";
        elsif i_clk'event and i_clk = '1' then
            if(rmax_load = '1' and i_data > o_rmax) then
            	o_rmax <= i_data;
            end if;
        end if;
    end process;

    process(i_clk, i_rst, next_state)
    begin
        if(i_rst = '1' or next_state <= S0) then
            o_rshift <= "0000";
        elsif i_clk'event and i_clk = '1' then
            if(rshift_load = '1') then
                o_rshift <= calc_shift_level(delta);   
            end if;
        end if;
    end process;
    
    process(i_clk, i_rst, next_state)
    begin
        if(i_rst = '1' or next_state = S0) then
            delta <= "11111111";
        elsif i_clk'event and i_clk = '1' then
            if(sub_delta = '1') then
                delta <= o_rmax - o_rmin;
            end if;
        end if;
    end process;
    
    process(i_clk, i_rst, next_state)
    begin
        if(i_rst = '1' or next_state = S0) then
            o_rnewdata <= "00000000";
        elsif i_clk'event and i_clk = '1' then
            if(rdata_load = '1') then
                o_rnewdata <= shift_level(i_data - o_rmin, o_rshift);
            end if;
        end if;
    end process;
    
    process(i_clk, i_rst, next_state)
    begin
        if(i_rst = '1' or next_state = S0) then
            read_value <= "0000000000000000";
        elsif i_clk'event and i_clk = '1' then
            if(r_reset = '1') then
                read_value <= "0000000000000010";
            elsif(r_sum = '1') then
                read_value <= read_value + "0000000000000001";    
            end if;
        end if;
    end process;
    
    process(i_clk, i_rst, next_state)
    begin
        if(i_rst = '1' or next_state = S0) then
            write_value <= "0000000000000000";
        elsif i_clk'event and i_clk = '1' then
            if(w_read = '1') then
                write_value <= read_value;
            elsif(w_sum = '1') then
                write_value <= write_value + "0000000000000001";       
            end if;
        end if;
    end process;
    
    o_address <= read_value when w_sel = '0' else write_value;

    o_data <= o_rnewdata;
        
end Behavioral;