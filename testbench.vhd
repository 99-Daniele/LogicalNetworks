--------------------------------------------------------------------------------
--
-- Test Bench Prova Finale (Progetto di Reti Logiche)
-- Prof. William Fornaciari - Anno 2021/2022
--
-- Daniele Cicala (Codice Persona 10630561 Matricola 910392)
--
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_1 is
end project_tb_1;

architecture projecttb of project_tb_1 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  2  , 8)), 
                         1 => std_logic_vector(to_unsigned(  2  , 8)), 
                         2 => std_logic_vector(to_unsigned(  46  , 8)),  
                         3 => std_logic_vector(to_unsigned(  131  , 8)), 
                         4 => std_logic_vector(to_unsigned(  62  , 8)), 
                         5 => std_logic_vector(to_unsigned(  89  , 8)), 
                         others => (others =>'0'));         
			 -- Expected Output  6 -> 0                         
			 -- Expected Output  7 -> 255                         
			 -- Expected Output  8 -> 64                         
			 -- Expected Output  9 -> 172                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [46, 131, 62, 89]  
    -- Immagine di output =  [0, 255, 64, 172]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 46 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  46  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 131 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  131  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 62 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  62  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 89 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  89  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 64 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  64  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 172 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  172  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;

    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_1_bis is
end project_tb_1_bis;

architecture projecttb of project_tb_1_bis is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  2  , 8)), 
                         1 => std_logic_vector(to_unsigned(  2  , 8)), 
                         2 => std_logic_vector(to_unsigned(  46  , 8)),  
                         3 => std_logic_vector(to_unsigned(  131  , 8)), 
                         4 => std_logic_vector(to_unsigned(  62  , 8)), 
                         5 => std_logic_vector(to_unsigned(  89  , 8)), 
                         others => (others =>'0'));         
			 -- Expected Output  6 -> 0                         
			 -- Expected Output  7 -> 255                         
			 -- Expected Output  8 -> 64                         
			 -- Expected Output  9 -> 172                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [46, 131, 62, 89]  
    -- Immagine di output =  [0, 255, 64, 172]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 46 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  46  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 131 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  131  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 62 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  62  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 89 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  89  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 64 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  64  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 172 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  172  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;

    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_1_reset is
end project_tb_1_reset;

architecture projecttb of project_tb_1_reset is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  2  , 8)), 
                         1 => std_logic_vector(to_unsigned(  2  , 8)), 
                         2 => std_logic_vector(to_unsigned(  46  , 8)),  
                         3 => std_logic_vector(to_unsigned(  131  , 8)), 
                         4 => std_logic_vector(to_unsigned(  62  , 8)), 
                         5 => std_logic_vector(to_unsigned(  89  , 8)), 
                         others => (others =>'0'));         
			 -- Expected Output  6 -> 0                         
			 -- Expected Output  7 -> 255                         
			 -- Expected Output  8 -> 64                         
			 -- Expected Output  9 -> 172                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD * 10;
    tb_rst <= '1';
    tb_start <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [46, 131, 62, 89]  
    -- Immagine di output =  [0, 255, 64, 172]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 46 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  46  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 131 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  131  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 62 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  62  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 89 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  89  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 64 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  64  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 172 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  172  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;

    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_2 is
end project_tb_2;

architecture projecttb of project_tb_2 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  0  , 8)), 
                         1 => std_logic_vector(to_unsigned(  0  , 8)), 
                         others => (others =>'0'));                    

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;
    
    assert RAM(0) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    
    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_3 is
end project_tb_3;

architecture projecttb of project_tb_3 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  1  , 8)), 
                         1 => std_logic_vector(to_unsigned(  1  , 8)), 
                         2 => std_logic_vector(to_unsigned(  46  , 8)),
                         others => (others =>'0'));         
			 -- Expected Output  3 -> 0                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [46]  
    -- Immagine di output =  [0]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 1 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  1  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 1 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  1  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 46 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  46  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
  
    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_4 is
end project_tb_4;

architecture projecttb of project_tb_4 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  3  , 8)), 
                         1 => std_logic_vector(to_unsigned(  1  , 8)), 
                         2 => std_logic_vector(to_unsigned(  12  , 8)),  
                         3 => std_logic_vector(to_unsigned(  12  , 8)),  
                         4 => std_logic_vector(to_unsigned(  12  , 8)),
                         others => (others =>'0'));         
			 -- Expected Output  5 -> 0               
			 -- Expected Output  6 -> 0       
			 -- Expected Output  7 -> 0                

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [12, 12, 12]  
    -- Immagine di output =  [0, 0, 0]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 3 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  3  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 1 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  1  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 12 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  12  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 12 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  12  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 12 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  12  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
   
    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_5 is
end project_tb_5;

architecture projecttb of project_tb_5 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  1  , 8)), 
                         1 => std_logic_vector(to_unsigned(  5  , 8)), 
                         2 => std_logic_vector(to_unsigned(  46  , 8)),
                         3 => std_logic_vector(to_unsigned(  45  , 8)), 
                         4 => std_logic_vector(to_unsigned(  44  , 8)), 
                         5 => std_logic_vector(to_unsigned(  43  , 8)),  
                         6 => std_logic_vector(to_unsigned(  42  , 8)),  
                         others => (others =>'0'));         
			 -- Expected Output  7 -> 255               
			 -- Expected Output  3 -> 192         
			 -- Expected Output  3 -> 128         
			 -- Expected Output  3 -> 64         
			 -- Expected Output  3 -> 0                 

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [46, 45, 44, 43, 42]  
    -- Immagine di output =  [255, 192, 128, 64, 0]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 1 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  1  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 5 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  5  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 46 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  46  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 45 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  45  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 44 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  44  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 43 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  43  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 42 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  42  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 192 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  192  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 128 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  128  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;
    assert RAM(10) = std_logic_vector(to_unsigned( 64 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  64  found " & integer'image(to_integer(unsigned(RAM(10))))  severity failure;
    assert RAM(11) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(11))))  severity failure;
  
    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_6 is
end project_tb_6;

architecture projecttb of project_tb_6 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  2  , 8)), 
                         1 => std_logic_vector(to_unsigned(  2  , 8)), 
                         2 => std_logic_vector(to_unsigned(  0  , 8)),  
                         3 => std_logic_vector(to_unsigned(  100  , 8)), 
                         4 => std_logic_vector(to_unsigned(  200  , 8)), 
                         5 => std_logic_vector(to_unsigned(  255  , 8)), 
                         others => (others =>'0'));         
			 -- Expected Output  6 -> 0                         
			 -- Expected Output  7 -> 100                         
			 -- Expected Output  8 -> 200                         
			 -- Expected Output  9 -> 255                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [0, 100, 200, 255]  
    -- Immagine di output =  [0, 100, 200, 255]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 100 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  100  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 200 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  200  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 100 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  100  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 200 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  200  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;

    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_7 is
end project_tb_7;

architecture projecttb of project_tb_7 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  2  , 8)), 
                         1 => std_logic_vector(to_unsigned(  2  , 8)), 
                         2 => std_logic_vector(to_unsigned(  131  , 8)),  
                         3 => std_logic_vector(to_unsigned(  86  , 8)), 
                         4 => std_logic_vector(to_unsigned(  34  , 8)), 
                         5 => std_logic_vector(to_unsigned(  12  , 8)), 
                         others => (others =>'0'));         
			 -- Expected Output  6 -> 255                         
			 -- Expected Output  7 -> 255                         
			 -- Expected Output  8 -> 88                         
			 -- Expected Output  9 -> 0                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [131, 86, 34, 12]  
    -- Immagine di output =  [255, 255, 88, 0]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 131 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  131  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 86 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  86  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 34 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  34  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 12 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  12  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 88 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  88  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;

    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_8 is
end project_tb_8;

architecture projecttb of project_tb_8 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  2  , 8)), 
                         1 => std_logic_vector(to_unsigned(  2  , 8)), 
                         2 => std_logic_vector(to_unsigned(  21  , 8)),  
                         3 => std_logic_vector(to_unsigned(  72  , 8)), 
                         4 => std_logic_vector(to_unsigned(  21  , 8)), 
                         5 => std_logic_vector(to_unsigned(  205  , 8)), 
                         others => (others =>'0'));         
			 -- Expected Output  6 -> 0                         
			 -- Expected Output  7 -> 102                         
			 -- Expected Output  8 -> 0                         
			 -- Expected Output  9 -> 255                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [21, 72, 21, 205]  
    -- Immagine di output =  [0, 102, 0, 255]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 21 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  21  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 72 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  72  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 21 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  21  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 205 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  205  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 102 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  102  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;

    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_9 is
end project_tb_9;

architecture projecttb of project_tb_9 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  4  , 8)), 
                         1 => std_logic_vector(to_unsigned(  3  , 8)), 
                         2 => std_logic_vector(to_unsigned(  76  , 8)),  
                         3 => std_logic_vector(to_unsigned(  131  , 8)), 
                         4 => std_logic_vector(to_unsigned(  109  , 8)), 
                         5 => std_logic_vector(to_unsigned(  89  , 8)),
                         6 => std_logic_vector(to_unsigned(  46  , 8)),
                         7 => std_logic_vector(to_unsigned(  121  , 8)),
                         8 => std_logic_vector(to_unsigned(  62  , 8)),
                         9 => std_logic_vector(to_unsigned(  59  , 8)),
                         10 => std_logic_vector(to_unsigned(  46  , 8)),
                         11 => std_logic_vector(to_unsigned(  77  , 8)),
                         12 => std_logic_vector(to_unsigned(  68  , 8)),
                         13 => std_logic_vector(to_unsigned(  94  , 8)),
                         others => (others =>'0'));         
			 -- Expected Output  14 -> 120                         
			 -- Expected Output  15 -> 255                         
			 -- Expected Output  16 -> 252                         
			 -- Expected Output  17 -> 172
			 -- Expected Output  18 -> 0                         
			 -- Expected Output  19 -> 255                         
			 -- Expected Output  20 -> 64                         
			 -- Expected Output  21 -> 52
			 -- Expected Output  22 -> 0                         
			 -- Expected Output  23 -> 124                         
			 -- Expected Output  24 -> 88                         
			 -- Expected Output  25 -> 192                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [76, 131, 109, 89, 46, 121, 62, 59, 46, 77, 68, 94]  
    -- Immagine di output =  [94, 120, 255, 252, 172, 0, 255, 64, 52, 0, 124, 88, 192]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 4 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  4  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 3 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  3  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 76 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  76  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 131 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  131  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 109 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  109  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 89 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  89  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 46 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  46  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 121 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  121  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 62 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  62  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 59 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  59  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;
    assert RAM(10) = std_logic_vector(to_unsigned( 46 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  46  found " & integer'image(to_integer(unsigned(RAM(10))))  severity failure;
    assert RAM(11) = std_logic_vector(to_unsigned( 77 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  77  found " & integer'image(to_integer(unsigned(RAM(11))))  severity failure;
    assert RAM(12) = std_logic_vector(to_unsigned( 68 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  68  found " & integer'image(to_integer(unsigned(RAM(12))))  severity failure;
    assert RAM(13) = std_logic_vector(to_unsigned( 94 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  94  found " & integer'image(to_integer(unsigned(RAM(13))))  severity failure;
    assert RAM(14) = std_logic_vector(to_unsigned( 120 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  120  found " & integer'image(to_integer(unsigned(RAM(14))))  severity failure;
    assert RAM(15) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(15))))  severity failure;
    assert RAM(16) = std_logic_vector(to_unsigned( 252 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  252  found " & integer'image(to_integer(unsigned(RAM(16))))  severity failure;
    assert RAM(17) = std_logic_vector(to_unsigned( 172 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  172  found " & integer'image(to_integer(unsigned(RAM(17))))  severity failure;
    assert RAM(18) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(18))))  severity failure;
    assert RAM(19) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(19))))  severity failure;
    assert RAM(20) = std_logic_vector(to_unsigned( 64 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  64  found " & integer'image(to_integer(unsigned(RAM(20))))  severity failure;
    assert RAM(21) = std_logic_vector(to_unsigned( 52 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  52  found " & integer'image(to_integer(unsigned(RAM(21))))  severity failure;
    assert RAM(22) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(22))))  severity failure;
    assert RAM(23) = std_logic_vector(to_unsigned( 124 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  124  found " & integer'image(to_integer(unsigned(RAM(23))))  severity failure;
    assert RAM(24) = std_logic_vector(to_unsigned( 88 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  88  found " & integer'image(to_integer(unsigned(RAM(24))))  severity failure;
    assert RAM(25) = std_logic_vector(to_unsigned( 192 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  192  found " & integer'image(to_integer(unsigned(RAM(25))))  severity failure;
    
    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_10 is
end project_tb_10;

architecture projecttb of project_tb_10 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  4  , 8)), 
                         1 => std_logic_vector(to_unsigned(  3  , 8)), 
                         2 => std_logic_vector(to_unsigned(  0  , 8)),  
                         3 => std_logic_vector(to_unsigned(  10  , 8)), 
                         4 => std_logic_vector(to_unsigned(  20  , 8)), 
                         5 => std_logic_vector(to_unsigned(  30  , 8)),
                         6 => std_logic_vector(to_unsigned(  40  , 8)),
                         7 => std_logic_vector(to_unsigned(  50  , 8)),
                         8 => std_logic_vector(to_unsigned(  60  , 8)),
                         9 => std_logic_vector(to_unsigned(  70  , 8)),
                         10 => std_logic_vector(to_unsigned(  80  , 8)),
                         11 => std_logic_vector(to_unsigned(  90  , 8)),
                         12 => std_logic_vector(to_unsigned(  100  , 8)),
                         13 => std_logic_vector(to_unsigned(  120  , 8)),
                         others => (others =>'0'));         
			 -- Expected Output  14 -> 0                         
			 -- Expected Output  15 -> 40                         
			 -- Expected Output  16 -> 80                         
			 -- Expected Output  17 -> 120
			 -- Expected Output  18 -> 160                         
			 -- Expected Output  19 -> 200                         
			 -- Expected Output  20 -> 240                         
			 -- Expected Output  21 -> 255
			 -- Expected Output  22 -> 255                         
			 -- Expected Output  23 -> 255                         
			 -- Expected Output  24 -> 255                         
			 -- Expected Output  25 -> 255                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 120]  
    -- Immagine di output =  [0, 40, 80, 120, 160, 200, 240, 255, 255, 255, 255, 255]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 4 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  4  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 3 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  3  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 10 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  10  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 20 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  20  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 30 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  30  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 40 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  40  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 50 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  50  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 60 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  60  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 70 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  70  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;
    assert RAM(10) = std_logic_vector(to_unsigned( 80 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  80  found " & integer'image(to_integer(unsigned(RAM(10))))  severity failure;
    assert RAM(11) = std_logic_vector(to_unsigned( 90 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  90  found " & integer'image(to_integer(unsigned(RAM(11))))  severity failure;
    assert RAM(12) = std_logic_vector(to_unsigned( 100 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  100  found " & integer'image(to_integer(unsigned(RAM(12))))  severity failure;
    assert RAM(13) = std_logic_vector(to_unsigned( 120 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  120  found " & integer'image(to_integer(unsigned(RAM(13))))  severity failure;
    assert RAM(14) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(14))))  severity failure;
    assert RAM(15) = std_logic_vector(to_unsigned( 40 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  40  found " & integer'image(to_integer(unsigned(RAM(15))))  severity failure;
    assert RAM(16) = std_logic_vector(to_unsigned( 80 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  80  found " & integer'image(to_integer(unsigned(RAM(16))))  severity failure;
    assert RAM(17) = std_logic_vector(to_unsigned( 120 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  120  found " & integer'image(to_integer(unsigned(RAM(17))))  severity failure;
    assert RAM(18) = std_logic_vector(to_unsigned( 160 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  160  found " & integer'image(to_integer(unsigned(RAM(18))))  severity failure;
    assert RAM(19) = std_logic_vector(to_unsigned( 200 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  200  found " & integer'image(to_integer(unsigned(RAM(19))))  severity failure;
    assert RAM(20) = std_logic_vector(to_unsigned( 240 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  240  found " & integer'image(to_integer(unsigned(RAM(20))))  severity failure;
    assert RAM(21) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(21))))  severity failure;
    assert RAM(22) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(22))))  severity failure;
    assert RAM(23) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(23))))  severity failure;
    assert RAM(24) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(24))))  severity failure;
    assert RAM(25) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(25))))  severity failure;
    
    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_11 is
end project_tb_11;

architecture projecttb of project_tb_11 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  4  , 8)), 
                         1 => std_logic_vector(to_unsigned(  3  , 8)), 
                         2 => std_logic_vector(to_unsigned(  122  , 8)),  
                         3 => std_logic_vector(to_unsigned(  123  , 8)), 
                         4 => std_logic_vector(to_unsigned(  124  , 8)), 
                         5 => std_logic_vector(to_unsigned(  125  , 8)),
                         6 => std_logic_vector(to_unsigned(  126  , 8)),
                         7 => std_logic_vector(to_unsigned(  127  , 8)),
                         8 => std_logic_vector(to_unsigned(  128  , 8)),
                         9 => std_logic_vector(to_unsigned(  129  , 8)),
                         10 => std_logic_vector(to_unsigned(  130  , 8)),
                         11 => std_logic_vector(to_unsigned(  131  , 8)),
                         12 => std_logic_vector(to_unsigned(  132  , 8)),
                         13 => std_logic_vector(to_unsigned(  133  , 8)),
                         others => (others =>'0'));         
			 -- Expected Output  14 -> 0                         
			 -- Expected Output  15 -> 32                         
			 -- Expected Output  16 -> 64                         
			 -- Expected Output  17 -> 96
			 -- Expected Output  18 -> 128                        
			 -- Expected Output  19 -> 160                         
			 -- Expected Output  20 -> 192                         
			 -- Expected Output  21 -> 224
			 -- Expected Output  22 -> 255                         
			 -- Expected Output  23 -> 255                         
			 -- Expected Output  24 -> 255                         
			 -- Expected Output  25 -> 255                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133]  
    -- Immagine di output =  [0, 32, 64, 96, 128, 160, 192, 224, 255, 255, 255, 255, 255]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 4 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  4  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 3 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  3  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 122 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  122  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 123 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  123  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 124 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  124  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 125 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  125  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 126 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  126  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 127 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  127  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 128 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  128  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 129 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  129  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;
    assert RAM(10) = std_logic_vector(to_unsigned( 130 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  130  found " & integer'image(to_integer(unsigned(RAM(10))))  severity failure;
    assert RAM(11) = std_logic_vector(to_unsigned( 131 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  131  found " & integer'image(to_integer(unsigned(RAM(11))))  severity failure;
    assert RAM(12) = std_logic_vector(to_unsigned( 132 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  132  found " & integer'image(to_integer(unsigned(RAM(12))))  severity failure;
    assert RAM(13) = std_logic_vector(to_unsigned( 133 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  133  found " & integer'image(to_integer(unsigned(RAM(13))))  severity failure;
    assert RAM(14) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(14))))  severity failure;
    assert RAM(15) = std_logic_vector(to_unsigned( 32 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  32  found " & integer'image(to_integer(unsigned(RAM(15))))  severity failure;
    assert RAM(16) = std_logic_vector(to_unsigned( 64 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  64  found " & integer'image(to_integer(unsigned(RAM(16))))  severity failure;
    assert RAM(17) = std_logic_vector(to_unsigned( 96 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  96  found " & integer'image(to_integer(unsigned(RAM(17))))  severity failure;
    assert RAM(18) = std_logic_vector(to_unsigned( 128 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  128  found " & integer'image(to_integer(unsigned(RAM(18))))  severity failure;
    assert RAM(19) = std_logic_vector(to_unsigned( 160 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  160  found " & integer'image(to_integer(unsigned(RAM(19))))  severity failure;
    assert RAM(20) = std_logic_vector(to_unsigned( 192 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  192  found " & integer'image(to_integer(unsigned(RAM(20))))  severity failure;
    assert RAM(21) = std_logic_vector(to_unsigned( 224 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  224  found " & integer'image(to_integer(unsigned(RAM(21))))  severity failure;
    assert RAM(22) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(22))))  severity failure;
    assert RAM(23) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(23))))  severity failure;
    assert RAM(24) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(24))))  severity failure;
    assert RAM(25) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(25))))  severity failure;
    
    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_12 is
end project_tb_12;

architecture projecttb of project_tb_12 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  4  , 8)), 
                         1 => std_logic_vector(to_unsigned(  3  , 8)), 
                         2 => std_logic_vector(to_unsigned(  0  , 8)),  
                         3 => std_logic_vector(to_unsigned(  0  , 8)), 
                         4 => std_logic_vector(to_unsigned(  0  , 8)), 
                         5 => std_logic_vector(to_unsigned(  0  , 8)),
                         6 => std_logic_vector(to_unsigned(  128  , 8)),
                         7 => std_logic_vector(to_unsigned(  128  , 8)),
                         8 => std_logic_vector(to_unsigned(  128  , 8)),
                         9 => std_logic_vector(to_unsigned(  128  , 8)),
                         10 => std_logic_vector(to_unsigned(  255  , 8)),
                         11 => std_logic_vector(to_unsigned(  255  , 8)),
                         12 => std_logic_vector(to_unsigned(  255  , 8)),
                         13 => std_logic_vector(to_unsigned(  255  , 8)),
                         others => (others =>'0'));         
			 -- Expected Output  14 -> 0                         
			 -- Expected Output  15 -> 0                         
			 -- Expected Output  16 -> 0                         
			 -- Expected Output  17 -> 0
			 -- Expected Output  18 -> 128                         
			 -- Expected Output  19 -> 128                         
			 -- Expected Output  20 -> 128                         
			 -- Expected Output  21 -> 128
			 -- Expected Output  22 -> 255                         
			 -- Expected Output  23 -> 255                         
			 -- Expected Output  24 -> 255                         
			 -- Expected Output  25 -> 255                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [0, 0, 0, 0, 128, 128, 128, 128, 255, 255, 255, 255]  
    -- Immagine di output =  [0, 0, 0, 0, 128, 128, 128, 128, 255, 255, 255, 255, 255]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 4 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  4  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 3 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  3  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 128 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  128  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 128 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  128  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 128 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  128  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 128 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  128  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;
    assert RAM(10) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(10))))  severity failure;
    assert RAM(11) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(11))))  severity failure;
    assert RAM(12) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(12))))  severity failure;
    assert RAM(13) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(13))))  severity failure;
    assert RAM(14) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(14))))  severity failure;
    assert RAM(15) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(15))))  severity failure;
    assert RAM(16) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(16))))  severity failure;
    assert RAM(17) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(17))))  severity failure;
    assert RAM(18) = std_logic_vector(to_unsigned( 128 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  128  found " & integer'image(to_integer(unsigned(RAM(18))))  severity failure;
    assert RAM(19) = std_logic_vector(to_unsigned( 128 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  128  found " & integer'image(to_integer(unsigned(RAM(19))))  severity failure;
    assert RAM(20) = std_logic_vector(to_unsigned( 128 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  128  found " & integer'image(to_integer(unsigned(RAM(20))))  severity failure;
    assert RAM(21) = std_logic_vector(to_unsigned( 128 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  128  found " & integer'image(to_integer(unsigned(RAM(21))))  severity failure;
    assert RAM(22) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(22))))  severity failure;
    assert RAM(23) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(23))))  severity failure;
    assert RAM(24) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(24))))  severity failure;
    assert RAM(25) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(25))))  severity failure;
    
    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_13 is
end project_tb_13;

architecture projecttb of project_tb_13 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  4  , 8)), 
                         1 => std_logic_vector(to_unsigned(  3  , 8)), 
                         2 => std_logic_vector(to_unsigned(  196  , 8)),  
                         3 => std_logic_vector(to_unsigned(  182  , 8)), 
                         4 => std_logic_vector(to_unsigned(  174  , 8)), 
                         5 => std_logic_vector(to_unsigned(  173  , 8)),
                         6 => std_logic_vector(to_unsigned(  112  , 8)),
                         7 => std_logic_vector(to_unsigned(  99  , 8)),
                         8 => std_logic_vector(to_unsigned(  89  , 8)),
                         9 => std_logic_vector(to_unsigned(  64  , 8)),
                         10 => std_logic_vector(to_unsigned(  46  , 8)),
                         11 => std_logic_vector(to_unsigned(  32  , 8)),
                         12 => std_logic_vector(to_unsigned(  20  , 8)),
                         13 => std_logic_vector(to_unsigned(  1  , 8)),
                         others => (others =>'0'));         
			 -- Expected Output  14 -> 255                         
			 -- Expected Output  15 -> 255                         
			 -- Expected Output  16 -> 255                         
			 -- Expected Output  17 -> 255
			 -- Expected Output  18 -> 222                         
			 -- Expected Output  19 -> 196                         
			 -- Expected Output  20 -> 176                         
			 -- Expected Output  21 -> 126
			 -- Expected Output  22 -> 90                         
			 -- Expected Output  23 -> 62                         
			 -- Expected Output  24 -> 38                         
			 -- Expected Output  25 -> 0                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [196, 182, 174, 173, 112, 99, 89, 64, 46, 32, 20, 1]  
    -- Immagine di output =  [255, 255, 255, 255, 222, 196, 176, 126, 90, 62, 38, 0]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 4 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  4  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 3 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  3  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 196 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  196  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 182 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  182  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 174 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  174  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 173 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  173  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 112 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  112  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 99 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  99  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 89 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  89  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 64 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  64  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;
    assert RAM(10) = std_logic_vector(to_unsigned( 46 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  46  found " & integer'image(to_integer(unsigned(RAM(10))))  severity failure;
    assert RAM(11) = std_logic_vector(to_unsigned( 32 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  32  found " & integer'image(to_integer(unsigned(RAM(11))))  severity failure;
    assert RAM(12) = std_logic_vector(to_unsigned( 20 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  20  found " & integer'image(to_integer(unsigned(RAM(12))))  severity failure;
    assert RAM(13) = std_logic_vector(to_unsigned( 1 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  1  found " & integer'image(to_integer(unsigned(RAM(13))))  severity failure;
    assert RAM(14) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(14))))  severity failure;
    assert RAM(15) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(15))))  severity failure;
    assert RAM(16) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(16))))  severity failure;
    assert RAM(17) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(17))))  severity failure;
    assert RAM(18) = std_logic_vector(to_unsigned( 222 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  222  found " & integer'image(to_integer(unsigned(RAM(18))))  severity failure;
    assert RAM(19) = std_logic_vector(to_unsigned( 196 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  196  found " & integer'image(to_integer(unsigned(RAM(19))))  severity failure;
    assert RAM(20) = std_logic_vector(to_unsigned( 176 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  176  found " & integer'image(to_integer(unsigned(RAM(20))))  severity failure;
    assert RAM(21) = std_logic_vector(to_unsigned( 126 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  126  found " & integer'image(to_integer(unsigned(RAM(21))))  severity failure;
    assert RAM(22) = std_logic_vector(to_unsigned( 90 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  90  found " & integer'image(to_integer(unsigned(RAM(22))))  severity failure;
    assert RAM(23) = std_logic_vector(to_unsigned( 62 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  62  found " & integer'image(to_integer(unsigned(RAM(23))))  severity failure;
    assert RAM(24) = std_logic_vector(to_unsigned( 38 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  28  found " & integer'image(to_integer(unsigned(RAM(24))))  severity failure;
    assert RAM(25) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(25))))  severity failure;
    
    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_14 is
end project_tb_14;

architecture projecttb of project_tb_14 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  3  , 8)), 
                         1 => std_logic_vector(to_unsigned(  4  , 8)), 
                         2 => std_logic_vector(to_unsigned(  116  , 8)),  
                         3 => std_logic_vector(to_unsigned(  127  , 8)), 
                         4 => std_logic_vector(to_unsigned(  185  , 8)), 
                         5 => std_logic_vector(to_unsigned(  214  , 8)),
                         6 => std_logic_vector(to_unsigned(  139  , 8)),
                         7 => std_logic_vector(to_unsigned(  51  , 8)),
                         8 => std_logic_vector(to_unsigned(  143  , 8)),
                         9 => std_logic_vector(to_unsigned(  106  , 8)),
                         10 => std_logic_vector(to_unsigned(  168  , 8)),
                         11 => std_logic_vector(to_unsigned(  21  , 8)),
                         12 => std_logic_vector(to_unsigned(  73  , 8)),
                         13 => std_logic_vector(to_unsigned(  16  , 8)),
                         others => (others =>'0'));         
			 -- Expected Output  14 -> 200                         
			 -- Expected Output  15 -> 222                         
			 -- Expected Output  16 -> 255                         
			 -- Expected Output  17 -> 255
			 -- Expected Output  18 -> 246                         
			 -- Expected Output  19 -> 70                         
			 -- Expected Output  20 -> 254                         
			 -- Expected Output  21 -> 180
			 -- Expected Output  22 -> 255                         
			 -- Expected Output  23 -> 10                         
			 -- Expected Output  24 -> 114                         
			 -- Expected Output  25 -> 0                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [116, 127, 185, 214, 139, 51, 143, 106, 168, 21, 73, 16]  
    -- Immagine di output =  [200, 222, 255, 255, 246, 70, 254, 180, 255, 10, 114, 0]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 3 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  3  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 4 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  4  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 116 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  116  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 127 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  127  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 185 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  185  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 214 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  214  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 139 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  139  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 51 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  51  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 143 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  143  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 106 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  106  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;
    assert RAM(10) = std_logic_vector(to_unsigned( 168 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  168  found " & integer'image(to_integer(unsigned(RAM(10))))  severity failure;
    assert RAM(11) = std_logic_vector(to_unsigned( 21 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  21  found " & integer'image(to_integer(unsigned(RAM(11))))  severity failure;
    assert RAM(12) = std_logic_vector(to_unsigned( 73 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  73  found " & integer'image(to_integer(unsigned(RAM(12))))  severity failure;
    assert RAM(13) = std_logic_vector(to_unsigned( 16 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  16  found " & integer'image(to_integer(unsigned(RAM(13))))  severity failure;
    assert RAM(14) = std_logic_vector(to_unsigned( 200 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  200  found " & integer'image(to_integer(unsigned(RAM(14))))  severity failure;
    assert RAM(15) = std_logic_vector(to_unsigned( 222 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  222  found " & integer'image(to_integer(unsigned(RAM(15))))  severity failure;
    assert RAM(16) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(16))))  severity failure;
    assert RAM(17) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(17))))  severity failure;
    assert RAM(18) = std_logic_vector(to_unsigned( 246 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  246  found " & integer'image(to_integer(unsigned(RAM(18))))  severity failure;
    assert RAM(19) = std_logic_vector(to_unsigned( 70 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  70  found " & integer'image(to_integer(unsigned(RAM(19))))  severity failure;
    assert RAM(20) = std_logic_vector(to_unsigned( 254 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  254  found " & integer'image(to_integer(unsigned(RAM(20))))  severity failure;
    assert RAM(21) = std_logic_vector(to_unsigned( 180 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  180  found " & integer'image(to_integer(unsigned(RAM(21))))  severity failure;
    assert RAM(22) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(22))))  severity failure;
    assert RAM(23) = std_logic_vector(to_unsigned( 10 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  10  found " & integer'image(to_integer(unsigned(RAM(23))))  severity failure;
    assert RAM(24) = std_logic_vector(to_unsigned( 114 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  114  found " & integer'image(to_integer(unsigned(RAM(24))))  severity failure;
    assert RAM(25) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(25))))  severity failure;
    
    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_15 is
end project_tb_15;

architecture projecttb of project_tb_15 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  2  , 8)), 
                         1 => std_logic_vector(to_unsigned(  2  , 8)), 
                         2 => std_logic_vector(to_unsigned(  10  , 8)),  
                         3 => std_logic_vector(to_unsigned(  20  , 8)), 
                         4 => std_logic_vector(to_unsigned(  30  , 8)), 
                         5 => std_logic_vector(to_unsigned(  40  , 8)), 
                         others => (others =>'0'));         
			 -- Expected Output  6 -> 0                         
			 -- Expected Output  7 -> 160                         
			 -- Expected Output  8 -> 255                         
			 -- Expected Output  9 -> 255                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [10, 20, 30, 40]  
    -- Immagine di output =  [0, 160, 255, 255]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 10 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  10  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 20 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  20  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 30 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  30  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 40 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  40  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 160 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  160  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;

    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity project_tb_16 is
end project_tb_16;

architecture projecttb of project_tb_16 is
constant c_CLOCK_PERIOD         : time := 15 ns;
signal   tb_done                : std_logic;
signal   mem_address            : std_logic_vector (15 downto 0) := (others => '0');
signal   tb_rst                 : std_logic := '0';
signal   tb_start               : std_logic := '0';
signal   tb_clk                 : std_logic := '0';
signal   mem_o_data,mem_i_data  : std_logic_vector (7 downto 0);
signal   enable_wire            : std_logic;
signal   mem_we                 : std_logic;

type ram_type is array (65535 downto 0) of std_logic_vector(7 downto 0);


signal RAM: ram_type := (0 => std_logic_vector(to_unsigned(  2  , 8)), 
                         1 => std_logic_vector(to_unsigned(  2  , 8)), 
                         2 => std_logic_vector(to_unsigned(  100  , 8)),  
                         3 => std_logic_vector(to_unsigned(  110  , 8)), 
                         4 => std_logic_vector(to_unsigned(  125  , 8)), 
                         5 => std_logic_vector(to_unsigned(  140  , 8)), 
                         others => (others =>'0'));         
			 -- Expected Output  6 -> 0                         
			 -- Expected Output  7 -> 80                         
			 -- Expected Output  8 -> 200                         
			 -- Expected Output  9 -> 255                         

component project_reti_logiche is
port (
      i_clk         : in  std_logic;
      i_rst         : in  std_logic;
      i_start       : in  std_logic;
      i_data        : in  std_logic_vector(7 downto 0);
      o_address     : out std_logic_vector(15 downto 0);
      o_done        : out std_logic;
      o_en          : out std_logic;
      o_we          : out std_logic;
      o_data        : out std_logic_vector (7 downto 0)
      );
end component project_reti_logiche;


begin
UUT: project_reti_logiche
port map (
          i_clk      	=> tb_clk,
          i_rst      	=> tb_rst,
          i_start       => tb_start,
          i_data    	=> mem_o_data,
          o_address  	=> mem_address,
          o_done      	=> tb_done,
          o_en   	=> enable_wire,
          o_we 		=> mem_we,
          o_data    	=> mem_i_data
          );

p_CLK_GEN : process is
begin
    wait for c_CLOCK_PERIOD/2;
    tb_clk <= not tb_clk;
end process p_CLK_GEN;


MEM : process(tb_clk)
begin
    if tb_clk'event and tb_clk = '1' then
        if enable_wire = '1' then
            if mem_we = '1' then
                RAM(conv_integer(mem_address))  <= mem_i_data;
                mem_o_data                      <= mem_i_data after 1 ns;
            else
                mem_o_data <= RAM(conv_integer(mem_address)) after 1 ns;
            end if;
        end if;
    end if;
end process;


test : process is
begin 
    wait for 100 ns;
    wait for c_CLOCK_PERIOD;
    tb_rst <= '1';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_rst <= '0';
    wait for c_CLOCK_PERIOD;
    wait for 100 ns;
    tb_start <= '1';
    wait for c_CLOCK_PERIOD;
    wait until tb_done = '1';
    wait for c_CLOCK_PERIOD;
    tb_start <= '0';
    wait until tb_done = '0';
    wait for 100 ns;

    -- Immagine originale =  [100, 110, 125, 140]  
    -- Immagine di output =  [0, 80, 200, 255]  
    
    assert RAM(0) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(0))))  severity failure;
    assert RAM(1) = std_logic_vector(to_unsigned( 2 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  2  found " & integer'image(to_integer(unsigned(RAM(1))))  severity failure;
    assert RAM(2) = std_logic_vector(to_unsigned( 100 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  100  found " & integer'image(to_integer(unsigned(RAM(2))))  severity failure;
    assert RAM(3) = std_logic_vector(to_unsigned( 110 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  110  found " & integer'image(to_integer(unsigned(RAM(3))))  severity failure;
    assert RAM(4) = std_logic_vector(to_unsigned( 125 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  125  found " & integer'image(to_integer(unsigned(RAM(4))))  severity failure;
    assert RAM(5) = std_logic_vector(to_unsigned( 140 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  140  found " & integer'image(to_integer(unsigned(RAM(5))))  severity failure;
    assert RAM(6) = std_logic_vector(to_unsigned( 0 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  0  found " & integer'image(to_integer(unsigned(RAM(6))))  severity failure;
    assert RAM(7) = std_logic_vector(to_unsigned( 80 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  80  found " & integer'image(to_integer(unsigned(RAM(7))))  severity failure;
    assert RAM(8) = std_logic_vector(to_unsigned( 200 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  200  found " & integer'image(to_integer(unsigned(RAM(8))))  severity failure;
    assert RAM(9) = std_logic_vector(to_unsigned( 255 , 8)) report "TEST FALLITO (WORKING ZONE). Expected  255  found " & integer'image(to_integer(unsigned(RAM(9))))  severity failure;

    assert false report "Simulation Ended! TEST PASSATO" severity failure;
end process test;

end projecttb;
