/**
 * Verilog Code Base Generator
 * Generates boilerplate Verilog code
 */

function generateVerilogCodeBase() {
    return `\n// Basic Verilog module\n\nmodule hello_world(\n    input wire clk,\n    input wire reset,\n    output reg [7:0] data_out\n);\n\n// Internal registers\nreg [31:0] counter;\n\n// Main logic\nalways @(posedge clk or posedge reset) begin\n    if (reset) begin\n        counter <= 32'h0;\n        data_out <= 8'h0;\n    end else begin\n        counter <= counter + 1;\n        data_out <= counter[7:0];\n    end\nend\n\n// Example combinational logic\nalways @(*) begin\n    // Add combinational logic here\n    // data_out = some_function(inputs);\nend\n\n// Example initial block for simulation\ninitial begin\n    $display("Hello, World!");\n    $display("This is a basic Verilog module.");\nend\n\nendmodule\n\n// Testbench (uncomment for simulation)\n/*\nmodule hello_world_tb;\n\nreg clk, reset;\nwire [7:0] data_out;\n\n// Instantiate the module\nhello_world uut (\n    .clk(clk),\n    .reset(reset),\n    .data_out(data_out)\n);\n\n// Clock generation\nalways #5 clk = ~clk;\n\n// Test sequence\ninitial begin\n    clk = 0;\n    reset = 1;\n    #10 reset = 0;\n    #100 $finish;\nend\n\ninitial begin\n    $monitor("Time=%0t, data_out=%h", $time, data_out);\nend\n\nendmodule\n*/\n`;
}

module.exports = {
    generateVerilogCodeBase
};