/**
 * VHDL Code Base Generator
 * Generates boilerplate VHDL code
 */

function generateVhdlCodeBase() {
    return `\n-- Basic VHDL program\n\nlibrary IEEE;\nuse IEEE.STD_LOGIC_1164.ALL;\nuse IEEE.NUMERIC_STD.ALL;\n\n-- Entity declaration\nentity HelloWorld is\n    Port (\n        clk : in STD_LOGIC;\n        reset : in STD_LOGIC;\n        output : out STD_LOGIC_VECTOR(7 downto 0)\n    );\nend HelloWorld;\n\n-- Architecture implementation\narchitecture Behavioral of HelloWorld is\n    signal counter : unsigned(7 downto 0) := (others => '0');\nbegin\n    -- Main process\n    process(clk, reset)\n    begin\n        if reset = '1' then\n            counter <= (others => '0');\n        elsif rising_edge(clk) then\n            counter <= counter + 1;\n        end if;\n    end process;\n    \n    -- Output assignment\n    output <= std_logic_vector(counter);\n    \n    -- Alternative combinational logic\n    -- output <= \"10101010\" when reset = '1' else\n    --           std_logic_vector(counter);\n    \nend Behavioral;\n\n-- Testbench (uncomment to use)\n-- library IEEE;\n-- use IEEE.STD_LOGIC_1164.ALL;\n-- \n-- entity HelloWorld_TB is\n-- end HelloWorld_TB;\n-- \n-- architecture Testbench of HelloWorld_TB is\n--     component HelloWorld\n--         Port (\n--             clk : in STD_LOGIC;\n--             reset : in STD_LOGIC;\n--             output : out STD_LOGIC_VECTOR(7 downto 0)\n--         );\n--     end component;\n--     \n--     signal clk : STD_LOGIC := '0';\n--     signal reset : STD_LOGIC := '0';\n--     signal output : STD_LOGIC_VECTOR(7 downto 0);\n--     \n-- begin\n--     uut: HelloWorld Port Map (\n--         clk => clk,\n--         reset => reset,\n--         output => output\n--     );\n--     \n--     clk_process: process\n--     begin\n--         clk <= '0';\n--         wait for 10 ns;\n--         clk <= '1';\n--         wait for 10 ns;\n--     end process;\n--     \n-- end Testbench;\n`;
}

module.exports = {
    generateVhdlCodeBase
};