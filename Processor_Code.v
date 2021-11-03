`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    12:49:01 01/10/2021 
// Design Name: 
// Module Name:    Basic RISC-V single cycle implementation on Verilog 
// Project Name: 
// Target Devices: 
// Tool versions: 
// Description: 
//
// Dependencies: 
//
// Revision: 
// Revision 0.01 - File Created
// Additional Comments: 
//
//////////////////////////////////////////////////////////////////////////////////
module Lab5_pipeline(rst, clk, Anode_Activate, LED_out,SelNum,SelDigit,num,done);
input rst, clk;						// 1 button to reset, clock signal as input
//output alu_z;						// An LED turned ON if result is zero
output reg[7:0] Anode_Activate;		// Anodes to control 7-segments
output reg [6:0] LED_out;			// output result to be sent on 7-segments
input SelNum,SelDigit,done;
input [3:0] num;
wire [31:0] GCD;
reg [7:0] num1,num2;
wire [31:0] s1_pc_in,s1_pc_out;


	// ALL modules will be called in this file. Code will be executed and results will be shown on 7-segment display
// Code segment for BCD to 7-segment Decoder. Keep this code as it is
reg [31:0] counter;		// A 32 bit flashing counter
reg toggle = 0;			// A variable to toggle between two 7-segments 

wire [6:0] opcode,func7;
wire [4:0] rs1,rs2,s2_rd_in,s3_rd_in,s4_rd_in,s4_rd_out;
wire [2:0] func3,instr_type;
wire [1:0] immsrc;
wire [31:0] s1_instruction_in,s1_instruction_out,s2_Port_A_in, s2_Port_B_in,
s2_immext_in,A,B,Din,s4_Data_Out_in,s3_ALU_Out_in,PCPlus4,PCNext,s3_pcplus4_out,s3_Port_B_out,
s3_ALU_Out_out,s4_ALU_Out_out,s4_Data_Out_out,s4_pcplus4_out;
wire [31:0] s2_pcplus4_in;
wire [31:0] s2_Port_A_out,s2_Port_B_out,s2_pc_out,s2_immext_out,s2_pcplus4_out;
wire [31:7] instr;
wire s2_RegWrite_in,s2_RegWrite_out,s2_MemWrite_in,s2_MemWrite_out,s2_MemRead_in,s2_MemRead_out,
s2_ALUSrc_in,s2_ALUSrc_out,CarryOut,ZeroOut,s2_PCSrc_in,s2_PCSrc_out;
wire [1:0] s2_ResultSrc_in,s2_ResultSrc_out,s3_ResultSrc_out,s4_ResultSrc_out;
wire [3:0] s2_ALU_Sel_in,s2_ALU_Sel_out;
wire s3_RegWrite_out,s3_MemWrite_out,s4_RegWrite_out;
reg clock_50Mhz = 0;
always @(posedge clk)
begin
    clock_50Mhz <= ~clock_50Mhz;
end


Control_Unit CU_ins(.opcode(opcode),.func3(func3),.func7(func7),.RegWrite(s2_RegWrite_in),.MemWrite(s2_MemWrite_in),
.MemRead(s2_MemRead_in),.ALUSrc(s2_ALUSrc_in),.ResultSrc(s2_ResultSrc_in),.ALU_Sel(s2_ALU_Sel_in),
.immsrc(immsrc),.instr_type(instr_type));

Instruction_Memory I_M_ins(.pc(s1_pc_in),.instruction(s1_instruction_in));

Instruction_fetch I_f_ins(.instruction(s1_instruction_out),.rs1(rs1),.rs2(rs2),.rd(s2_rd_in),.instr(instr),.func3(func3),.func7(func7)
,.opcode(opcode));

mux2x1 rf_alu(.o(B),.a(s2_immext_out),.b(s2_Port_B_out),.sel(s2_ALUSrc_out));

mux3x1 dm_rf(.o(Din),.sel(s4_ResultSrc_out),.a(s4_ALU_Out_out),.b(s4_Data_Out_out),.c(s4_pcplus4_out));

Data_Memory D_M_ins(.Data_Out(s4_Data_Out_in),.Data_In(s3_Port_B_out),.D_Addr(s3_ALU_Out_out[7:0]),.MemWrite(s3_MemWrite_out),
.MemRead(s2_MemRead_out),.clk(clock_50Mhz),.rst(rst),.num1(num1),.num2(num2),.done(done));

register_file rf_ins(.Port_A(s2_Port_A_in), .Port_B(s2_Port_B_in), .Din(Din), .Addr_A(rs1), .Addr_B(rs2), .Addr_Wr(s4_rd_out),
 .RegWrite(s4_RegWrite_out),.clk(clock_50Mhz),.rst(rst),.GCD(GCD));
 
adress_generator a_g_ins(.pc(s1_pc_in),.clk(clock_50Mhz),.PCNext(PCNext),.rst(rst));
 
alu alu_ins(.A(A),.B(B),.ALU_Sel(s2_ALU_Sel_out),.ALU_Out(s3_ALU_Out_in),.CarryOut(CarryOut),.ZeroOut(ZeroOut));

extend ext_ins(.immext(s2_immext_in),.instr(instr),.immsrc(immsrc));

adder pcplus4_ins(.a(s1_pc_in),.b(32'h4),.y(PCPlus4));

mux2x1 pc_pcNext_ins(.sel(s2_PCSrc_out),.a(s3_ALU_Out_out),.b(PCPlus4),.o(PCNext));

mux2x1 rf_alu_pc(.sel(s2_PCSrc_out),.a(s2_pc_out),.b(s2_Port_A_out),.o(A));

CompareAndBranch cab_ins(.Port_A(s2_Port_A_in),.Port_B(s2_Port_B_in),.instr_type(instr_type),
.func3(func3),.PCSrc(s2_PCSrc_in));

stage1 s1_ins(.rst(rst),.clk(clock_50Mhz),.instruction_in(s1_instruction_in),.pc_in(s1_pc_in),
.instruction_out(s1_instruction_out),.pc_out(s1_pc_out),.PCPlus4_out(s2_pcplus4_in),.PCPlus4_in(PCPlus4));

stage2 s2_ins(.rst(rst),.clk(clock_50Mhz),.Port_A_in(s2_Port_A_in),.Port_B_in(s2_Port_B_in),.pc_in(s1_pc_out),
.immext_in(s2_immext_in),.PCPlus4_in(s2_pcplus4_in),.Port_A_out(s2_Port_A_out),.Port_B_out(s2_Port_B_out),
.pc_out(s2_pc_out),.immext_out(s2_immext_out),.PCPlus4_out(s2_pcplus4_out),.rd_in(s2_rd_in),.rd_out(s3_rd_in),
.RegWrite_in(s2_RegWrite_in),.ALUSrc_in(s2_ALUSrc_in),.MemWrite_in(s2_MemWrite_in),
.MemRead_in(s2_MemRead_in),.PCSrc_in(s2_PCSrc_in),.ALU_Sel_in(s2_ALU_Sel_in), .ResultSrc_in(s2_ResultSrc_in),
.RegWrite_out(s2_RegWrite_out),.ALUSrc_out(s2_ALUSrc_out),.MemWrite_out(s2_MemWrite_out),
.MemRead_out(s2_MemRead_out),.PCSrc_out(s2_PCSrc_out),.ALU_Sel_out(s2_ALU_Sel_out),
.ResultSrc_out(s2_ResultSrc_out));

stage3 s3_ins(.rst(rst),.clk(clock_50Mhz),.ALU_Out_in(s3_ALU_Out_in),.PCPlus4_in(s2_pcplus4_out),
.Port_B_in(s2_Port_B_in),.PCPlus4_out(s3_pcplus4_out),.Port_B_out(s3_Port_B_out),
.ALU_Out_out(s3_ALU_Out_out),.rd_in(s3_rd_in),.rd_out(s4_rd_in), .RegWrite_in(s2_RegWrite_out),
.ResultSrc_in(s2_ResultSrc_out),.MemWrite_in(s2_MemWrite_out),.RegWrite_out(s3_RegWrite_out),
.MemWrite_out(s3_MemWrite_out),.ResultSrc_out(s3_ResultSrc_out));

stage4 s4_ins(.rst(rst),.ALU_Out_in(s3_ALU_Out_out),.PCPlus4_in(s3_pcplus4_out),.Data_Out_in(s4_Data_Out_in),
.ALU_Out_out(s4_ALU_Out_out),.PCPlus4_out(s4_pcplus4_out),.clk(clock_50Mhz),
.Data_Out_out(s4_Data_Out_out),.rd_in(s4_rd_in),.rd_out(s4_rd_out), .RegWrite_in(s3_RegWrite_out),
.ResultSrc_in(s3_ResultSrc_out),.ResultSrc_out(s4_ResultSrc_out),.RegWrite_out(s4_RegWrite_out));
//taking input from user
always @(*)
begin
    if(SelNum && ~done)
    begin
        if(SelDigit)
            num2[7:4]=num;
        else
            num2[3:0]=num;
    end
    if(~SelNum && ~done)
    begin
        if(SelDigit)
            num1[7:4]=num;
        else
            num1[3:0]=num;
    end
end
reg [4:0] states;
initial begin
	states = 0;
end
always @(posedge clk)
    begin
            if(counter>=75000) begin
                 counter <= 0;
				 toggle <= ~toggle;
				  end
            else begin
                counter <= counter + 1;
				end
    end 
    // anode activating signals for 8 segments, digit period of 1ms
    // decoder to generate anode signals 
    always @(posedge toggle)
    begin
		if(states == 5'h5)
			states <= 0;
		else
			states <= states + 1;
    end
    always @(*)
    begin
        case(states)
        5'h0: begin
            Anode_Activate = 8'b01111111; 
              end
        5'h1: begin
            Anode_Activate = 8'b10111111; 
               end
		5'h2: begin
            Anode_Activate = 8'b11101111; 
              end
        5'h3: begin
            Anode_Activate = 8'b11110111; 
               end
		5'h4: begin
            Anode_Activate = 8'b11111101; 
              end
        5'h5: begin
            Anode_Activate = 8'b11111110; 
               end
        endcase
    end
    // Cathode patterns of the 7-segment 1 LED display 
    always @(*)
    begin
	if (states == 5'h0) begin
        case(num1[7:4])				// First 4 bits of Result from ALU will be displayed on 1st segment
        4'b0000: LED_out = 7'b0000001; // "0"     
        4'b0001: LED_out = 7'b1001111; // "1" 
        4'b0010: LED_out = 7'b0010010; // "2" 
        4'b0011: LED_out = 7'b0000110; // "3" 
        4'b0100: LED_out = 7'b1001100; // "4" 
        4'b0101: LED_out = 7'b0100100; // "5" 
        4'b0110: LED_out = 7'b0100000; // "6" 
        4'b0111: LED_out = 7'b0001111; // "7" 
        4'b1000: LED_out = 7'b0000000; // "8"     
        4'b1001: LED_out = 7'b0000100; // "9"
		4'b1010: LED_out = 7'b0001000; // "A"     
        4'b1011: LED_out = 7'b1100000; // "b"     
        4'b1100: LED_out = 7'b0110001; // "C"     
        4'b1101: LED_out = 7'b1000010; // "d"     
        4'b1110: LED_out = 7'b0110000; // "E"     
        4'b1111: LED_out = 7'b0111000; // "F"     
        
        default: LED_out = 7'b0000001; // "0"
        endcase
		end
    

	// Cathode patterns of the 7-segment 2 LED display
	if(states == 5'h1) begin	
        case(num1[3:0])			// Next 4 bits of Result from ALU will be displayed on 2nd segment
        4'b0000: LED_out = 7'b0000001; // "0"     
        4'b0001: LED_out = 7'b1001111; // "1" 
        4'b0010: LED_out = 7'b0010010; // "2" 
        4'b0011: LED_out = 7'b0000110; // "3" 
        4'b0100: LED_out = 7'b1001100; // "4" 
        4'b0101: LED_out = 7'b0100100; // "5" 
        4'b0110: LED_out = 7'b0100000; // "6" 
        4'b0111: LED_out = 7'b0001111; // "7" 
        4'b1000: LED_out = 7'b0000000; // "8"     
        4'b1001: LED_out = 7'b0000100; // "9"
		4'b1010: LED_out = 7'b0001000; // "A"     
        4'b1011: LED_out = 7'b1100000; // "b"     
        4'b1100: LED_out = 7'b0110001; // "C"     
        4'b1101: LED_out = 7'b1000010; // "d"     
        4'b1110: LED_out = 7'b0110000; // "E"     
        4'b1111: LED_out = 7'b0111000; // "F"     
        
        default: LED_out = 7'b0000001; // "0"
        endcase
    end
	if(states == 5'h2) begin	
        case(num2[7:4])			// Next 4 bits of Result from ALU will be displayed on 2nd segment
        4'b0000: LED_out = 7'b0000001; // "0"     
        4'b0001: LED_out = 7'b1001111; // "1" 
        4'b0010: LED_out = 7'b0010010; // "2" 
        4'b0011: LED_out = 7'b0000110; // "3" 
        4'b0100: LED_out = 7'b1001100; // "4" 
        4'b0101: LED_out = 7'b0100100; // "5" 
        4'b0110: LED_out = 7'b0100000; // "6" 
        4'b0111: LED_out = 7'b0001111; // "7" 
        4'b1000: LED_out = 7'b0000000; // "8"     
        4'b1001: LED_out = 7'b0000100; // "9"
		4'b1010: LED_out = 7'b0001000; // "A"     
        4'b1011: LED_out = 7'b1100000; // "b"     
        4'b1100: LED_out = 7'b0110001; // "C"     
        4'b1101: LED_out = 7'b1000010; // "d"     
        4'b1110: LED_out = 7'b0110000; // "E"     
        4'b1111: LED_out = 7'b0111000; // "F"     
        
        default: LED_out = 7'b0000001; // "0"
        endcase
    end
	if(states == 5'h3) begin	
        case(num2[3:0])			// Next 4 bits of Result from ALU will be displayed on 2nd segment
        4'b0000: LED_out = 7'b0000001; // "0"     
        4'b0001: LED_out = 7'b1001111; // "1" 
        4'b0010: LED_out = 7'b0010010; // "2" 
        4'b0011: LED_out = 7'b0000110; // "3" 
        4'b0100: LED_out = 7'b1001100; // "4" 
        4'b0101: LED_out = 7'b0100100; // "5" 
        4'b0110: LED_out = 7'b0100000; // "6" 
        4'b0111: LED_out = 7'b0001111; // "7" 
        4'b1000: LED_out = 7'b0000000; // "8"     
        4'b1001: LED_out = 7'b0000100; // "9"
		4'b1010: LED_out = 7'b0001000; // "A"     
        4'b1011: LED_out = 7'b1100000; // "b"     
        4'b1100: LED_out = 7'b0110001; // "C"     
        4'b1101: LED_out = 7'b1000010; // "d"     
        4'b1110: LED_out = 7'b0110000; // "E"     
        4'b1111: LED_out = 7'b0111000; // "F"     
        
        default: LED_out = 7'b0000001; // "0"
        endcase
    end
	if(states == 5'h4) begin	
        case(GCD[7:4])			// Next 4 bits of Result from ALU will be displayed on 2nd segment
        4'b0000: LED_out = 7'b0000001; // "0"     
        4'b0001: LED_out = 7'b1001111; // "1" 
        4'b0010: LED_out = 7'b0010010; // "2" 
        4'b0011: LED_out = 7'b0000110; // "3" 
        4'b0100: LED_out = 7'b1001100; // "4" 
        4'b0101: LED_out = 7'b0100100; // "5" 
        4'b0110: LED_out = 7'b0100000; // "6" 
        4'b0111: LED_out = 7'b0001111; // "7" 
        4'b1000: LED_out = 7'b0000000; // "8"     
        4'b1001: LED_out = 7'b0000100; // "9"
		4'b1010: LED_out = 7'b0001000; // "A"     
        4'b1011: LED_out = 7'b1100000; // "b"     
        4'b1100: LED_out = 7'b0110001; // "C"     
        4'b1101: LED_out = 7'b1000010; // "d"     
        4'b1110: LED_out = 7'b0110000; // "E"     
        4'b1111: LED_out = 7'b0111000; // "F"     
        
        default: LED_out = 7'b0000001; // "0"
        endcase
    end
	if(states == 5'h5) begin	
        case(GCD[3:0])			// Next 4 bits of Result from ALU will be displayed on 2nd segment
        4'b0000: LED_out = 7'b0000001; // "0"     
        4'b0001: LED_out = 7'b1001111; // "1" 
        4'b0010: LED_out = 7'b0010010; // "2" 
        4'b0011: LED_out = 7'b0000110; // "3" 
        4'b0100: LED_out = 7'b1001100; // "4" 
        4'b0101: LED_out = 7'b0100100; // "5" 
        4'b0110: LED_out = 7'b0100000; // "6" 
        4'b0111: LED_out = 7'b0001111; // "7" 
        4'b1000: LED_out = 7'b0000000; // "8"     
        4'b1001: LED_out = 7'b0000100; // "9"
		4'b1010: LED_out = 7'b0001000; // "A"     
        4'b1011: LED_out = 7'b1100000; // "b"     
        4'b1100: LED_out = 7'b0110001; // "C"     
        4'b1101: LED_out = 7'b1000010; // "d"     
        4'b1110: LED_out = 7'b0110000; // "E"     
        4'b1111: LED_out = 7'b0111000; // "F"     
        
        default: LED_out = 7'b0000001; // "0"
        endcase
    end
	
end

	// Keep writing your code (calling lower module) here!


endmodule

module stage1(input clk,rst,input [31:0] instruction_in,PCPlus4_in,pc_in,output reg [31:0] instruction_out,pc_out,PCPlus4_out);
    always @(posedge clk or posedge rst)
    begin
        instruction_out <= instruction_in;
		if(rst)
        begin
            PCPlus4_out <= 0;
            pc_out <= 0;
        end
        else
        begin
            PCPlus4_out <= PCPlus4_in;
            pc_out <= pc_in;
        end
		
    end
endmodule

module stage2(input [31:0] Port_A_in,Port_B_in,pc_in,immext_in,PCPlus4_in,input clk,rst,
output reg [31:0] Port_A_out,Port_B_out,pc_out,immext_out,PCPlus4_out,input [4:0] rd_in,output reg [4:0] rd_out,
input RegWrite_in,ALUSrc_in,MemWrite_in,MemRead_in,PCSrc_in,input [3:0] ALU_Sel_in,input [1:0] ResultSrc_in,
output reg RegWrite_out,ALUSrc_out,MemWrite_out,MemRead_out,PCSrc_out,output reg [3:0] ALU_Sel_out,output reg [1:0] ResultSrc_out);
    always @(posedge clk or posedge rst)
    begin
        Port_A_out <= Port_A_in;
        Port_B_out <= Port_B_in;
        immext_out <= immext_in;
        rd_out <= rd_in;
        RegWrite_out <= RegWrite_in;
        ALUSrc_out <= ALUSrc_in;
        MemWrite_out <= MemWrite_in;
        MemRead_out <= MemRead_in;
        ALU_Sel_out <= ALU_Sel_in;
        ResultSrc_out <= ResultSrc_in;
        if(rst)
        begin
            PCPlus4_out <= 0;
            pc_out <= 0;
            PCSrc_out <= 0;
        end
        else
        begin
            PCPlus4_out <= PCPlus4_in;
            pc_out <= pc_in;
            PCSrc_out <= PCSrc_in;
        end
    end
endmodule

module stage3(input clk,rst,input [31:0] ALU_Out_in,PCPlus4_in,Port_B_in,output reg [31:0] PCPlus4_out,Port_B_out,
ALU_Out_out,input [4:0] rd_in,output reg [4:0] rd_out,input RegWrite_in,input [1:0] ResultSrc_in,input MemWrite_in,
output reg RegWrite_out,MemWrite_out,output reg [1:0] ResultSrc_out);
    always @(posedge clk or posedge rst)
    begin
        Port_B_out <= Port_B_in;
        ALU_Out_out <= ALU_Out_in;
        rd_out <= rd_in;
        MemWrite_out <= MemWrite_in;
        ResultSrc_out <= ResultSrc_in;
        RegWrite_out <= RegWrite_in;
        if(rst)
            PCPlus4_out <= 0;
        else
            PCPlus4_out <= PCPlus4_in;
    end
    
endmodule

module stage4(input clk,rst,input [31:0] ALU_Out_in,PCPlus4_in,Data_Out_in,output reg [31:0] ALU_Out_out,
PCPlus4_out,Data_Out_out,input [4:0] rd_in,output reg [4:0] rd_out,input RegWrite_in,input [1:0] ResultSrc_in,
output reg [1:0] ResultSrc_out,output reg RegWrite_out);
    always @(posedge clk or posedge rst)
    begin
        ALU_Out_out <= ALU_Out_in;
        Data_Out_out <= Data_Out_in;
        rd_out <= rd_in;
        RegWrite_out <= RegWrite_in;
        ResultSrc_out <= ResultSrc_in;
        if(rst)
            PCPlus4_out <= 0;
        else
            PCPlus4_out <= PCPlus4_in;
    end

endmodule




// A module to generate the address of next instruction
// LOGIC: 	Add 1 in program counter if its a normal instruction
//			Add address of label in PC if its a branch instruction			
// other parameters can also be added based on Datapath and Controller Inputs/Outputs
module adress_generator (output reg [31:0] pc, input [31:0] PCNext,input clk,rst);

	// Write your code here. This is not the part of Phase-I
	always @(posedge clk or posedge rst)
	begin
	   if(rst)
	       pc <= 32'b0;
	   else
	       pc <= PCNext;
    end
endmodule


// A module that will carry the all instructions. PC value will be provided to this module
// and it will return the instuction other parameters can also be added based on Datapath 
//and Controller Inputs/Outputs
module Instruction_Memory (input [31:0] pc,output reg [31:0] instruction);

	// Write your code here
	always @(pc)
	begin
	   case(pc)
	   32'h00: instruction = 32'h00002503;
	   32'h04: instruction = 32'h00000033;
	   32'h08: instruction = 32'h00000033;
	   32'h0c: instruction = 32'h00000033;
	   32'h10: instruction = 32'h00000033;
	   32'h14: instruction = 32'h00000033;
	   32'h18: instruction = 32'hfe0504e3;
	   32'h1c: instruction = 32'h00102403;
	   32'h20: instruction = 32'h00202483;
	   32'h24: instruction = 32'h00000033;
	   32'h28: instruction = 32'h00000033;
	   32'h2c: instruction = 32'h00000033;
	   32'h30: instruction = 32'h00000033;
	   32'h34: instruction = 32'h00000033;
	   32'h38: instruction = 32'h04940a63;
	   32'h3c: instruction = 32'h00000033;
	   32'h40: instruction = 32'h00000033;
	   32'h44: instruction = 32'h00000033;
	   32'h48: instruction = 32'h00000033;
	   32'h4c: instruction = 32'h00000033;
	   32'h50: instruction = 32'h02944a63;
	   32'h54: instruction = 32'h00000033;
	   32'h58: instruction = 32'h00000033;
	   32'h5c: instruction = 32'h00000033;
	   32'h60: instruction = 32'h00000033;
	   32'h64: instruction = 32'h00000033;
	   32'h68: instruction = 32'h40940433;
	   32'h6c: instruction = 32'hfc0006e3;
	   32'h70: instruction = 32'h00000033;
	   32'h74: instruction = 32'h00000033;
       32'h78: instruction = 32'h00000033;
       32'h7c: instruction = 32'h00000033;
       32'h80: instruction = 32'h00000033;
	   32'h84: instruction = 32'h408484b3;
	   32'h88: instruction = 32'hfa0008e3;
	   32'h8c: instruction = 32'h00002503;
	   32'h90: instruction = 32'h00000033;
	   32'h94: instruction = 32'h00000033;
       32'h98: instruction = 32'h00000033;
       32'h9c: instruction = 32'h00000033;
       32'ha0: instruction = 32'h00000033;
	   32'ha4: instruction = 32'hf4050ee3;
	   32'ha8: instruction = 32'h00000033;
	   32'hac: instruction = 32'h00000033;
	   32'hb0: instruction = 32'h00000033;
	   32'hb4: instruction = 32'h00000033;
       32'hb8: instruction = 32'h00000033;
       32'hbc: instruction = 32'hfc0008e3;
	   default: instruction = 32'h00000000;
	   endcase
	end
endmodule


// This module will take a 32-bit instruction, and find its op-code, operands, and functions.
// based on the op-code and functions, the controller will be operated.
// other parameters can also be added based on Datapath and Controller Inputs/Outputs
module Instruction_fetch (input [31:0] instruction, output reg [6:0] opcode,
output reg [4:0] rs1,rs2,rd,output reg [2:0] func3,output reg [6:0] func7,
output reg [31:7] instr);

	// Write your code here
	always @(instruction)
	begin
	   opcode = instruction[6:0];
	   rs1 = instruction[19:15];
	   rs2 = instruction[24:20];
	   rd = instruction[11:7];
	   func3 = instruction[14:12];
	   func7 = instruction[31:25];
	   instr = instruction[31:7];
	end
endmodule


// This module is called Data_Memory. It will consists of 256 byte memory unit. It will have 
// one input as 8-bit address, 1 input flag wr that will decide if you want to read memory or write memory
module Data_Memory(output [31:0] Data_Out, input [31:0] Data_In,
 input [7:0] D_Addr, input MemWrite,MemRead,clk,rst,input [7:0] num1,num2,input done);
		reg [31:0] Mem [255:0];			// Data Memory

	// Write your code here
	integer i;
	assign Data_Out = MemRead ? Mem[D_Addr] : 32'bx;
	always @(posedge clk or posedge rst)
	begin
		if(rst) 
		begin
			for (i=0; i<256; i=i+1)
			begin
				Mem[i] = 32'b0;
			end
		end
		else if(MemWrite)
		begin
			Mem[D_Addr] = Data_In;
		end
		Mem[0][7:0] = done;
		Mem[1][7:0] = num1;
		Mem[2][7:0] = num2;
	end
endmodule



// This module is called Register_Memory. It will consists of 32 registers each of 32-bits. It will have 
// one input flag wr that will decide if you want to read any register or write or update any value in register
// This module will 2 addresses and provide the data in 2 different outputs
module register_file(Port_A, Port_B, Din, Addr_A, Addr_B, Addr_Wr, RegWrite,clk,rst,GCD);
			output [31:0] Port_A, Port_B;			// Data to be provided from register to execute the instruction
			input [31:0] Din;						// Data to be loaded in the register
			input [4:0] Addr_A, Addr_B, Addr_Wr;	// Address (or number) of register to be written or to be read
			input RegWrite,clk,rst;								// input RegWrite flag input
			output [31:0] GCD;
			reg [31:0] Reg_File [31:0];				// Register file

	// Write your code here 
	initial begin
	   Reg_File[0]=32'b0;
	end
	integer i;
	assign Port_A = Reg_File[Addr_A];
	assign Port_B = Reg_File[Addr_B];
	assign GCD = Reg_File[8];
	always @(negedge clk or posedge rst)
	begin
	   if(rst)
	   begin
	      for (i=1; i<32; i=i+1) 
          begin
            Reg_File[i] = 32'b0;
          end
	   end
	   else if(RegWrite==1 && Addr_Wr!=0)
	   begin
	       Reg_File[Addr_Wr] = Din;
	   end
	end
endmodule


module Control_Unit(input [6:0] opcode,func7,input [2:0] func3,output RegWrite,ALUSrc,MemWrite,
MemRead,output reg [3:0] ALU_Sel,output [1:0] immsrc,output reg [1:0] ResultSrc,output reg [2:0] instr_type);			
	// This is the part of Phase-II
    parameter RTYPE=5, ITYPE =0, BTYPE=2,STYPE=1,UTYPE=4,JTYPE=3;
    always @(opcode)
    begin
        case(opcode)
            7'b0110011: instr_type = RTYPE;
            7'b0010011: instr_type = ITYPE;
            7'b1100011: instr_type = BTYPE;
            7'b0000011: instr_type = ITYPE;
            7'b0100011: instr_type = STYPE;
            7'b1101111: instr_type = JTYPE;
            7'b0110111: instr_type = UTYPE;
            7'b0010111: instr_type = UTYPE;
            default: instr_type = RTYPE;
        endcase
    end
    assign immsrc = instr_type[1:0];
    assign MemRead = 1;
    assign MemWrite = (instr_type == STYPE);
    assign RegWrite = (instr_type == RTYPE || instr_type ==ITYPE );
    assign ALUSrc = (instr_type == BTYPE || instr_type == JTYPE || instr_type ==ITYPE || instr_type == STYPE);
    always @(*)
    begin
        case(opcode)
            7'b0000011: ResultSrc = 1;
            7'b1101111: ResultSrc = 2;
            7'b1100111: ResultSrc = 2;
            default: ResultSrc = 0;
        endcase
    end
    always @(opcode or func3 or func7)
    begin
        case({opcode,func3,func7})
            17'b01100110000000000: ALU_Sel = 4'b0000; // ADD
            17'b0010011000xxxxxxx: ALU_Sel = 4'b0000; // ADDI
            17'b01100110000100000: ALU_Sel = 4'b0001; // SUB
            17'b01100111110000000,17'b0010011111xxxxxxx: ALU_Sel = 4'b1000; // AND and ANDI
            17'b01100111100000000,17'b0010011110xxxxxxx: ALU_Sel = 4'b1001; // OR and ORI
            17'b01100111000000000,17'b0010011100xxxxxxx: ALU_Sel = 4'b1010; // XOR and XORI
            17'b01100110100000000,17'b0010011010xxxxxxx: ALU_Sel = 4'b1110; // SLT and SLTI
            17'b01100110110000000,17'b0010011011xxxxxxx: ALU_Sel = 4'b1110; // SLTU and SLTIU
            17'b0000011xxxxxxxxxx: ALU_Sel = 4'b0000; // Load
            17'b0100011xxxxxxxxxx: ALU_Sel = 4'b0000; // Store
            default:  ALU_Sel = 4'b0000; // ADD
        endcase
    end
    
endmodule

module CompareAndBranch(input [31:0] Port_A,input [31:0] Port_B,input [2:0] instr_type,input [2:0] func3,output reg PCSrc);
    parameter RTYPE=5, ITYPE =0, BTYPE=2,STYPE=1,UTYPE=4,JTYPE=3;
    wire zero_flag,neg_flag;
    wire [31:0] temp;
    assign temp = (Port_A - Port_B);
    assign zero_flag = (temp == 0);
    assign neg_flag = temp[31];
    always @(*)
    begin
        case({instr_type,func3})
            6'b011xxx: PCSrc = 1;   //J type instruction
            6'b010000: PCSrc = zero_flag; //BEQ
            6'b010001: PCSrc = ~zero_flag; //BNE
            6'b010100: PCSrc = neg_flag; //BLT
            6'b010101: PCSrc = (~neg_flag) || zero_flag; //BGE
            6'b000xxx,6'b101xxx,6'b100xxx,6'b001xxx: PCSrc = 0;
            default: PCSrc = 0;
        endcase
    end
    
endmodule

// General Module of two input (5 bit) multiplexer. This multiplexer will be connected with ALU control signals
module mux(o,a,b, sel);
    input [4:0] a,b;			// 5 bit inputs
	input sel;					// selection signal
    output [4:0] o;			// 5 bit output

	// write your code here!
	assign o = sel ? a:b;
	
endmodule

// A two by one 32 bit multiplexer (to select the branch instruction)
module mux2x1(output [31:0]o,		// 32 bit output
					input[31:0]a,b,		// 32 bit inputs
					input sel			// Selection Signal
			);
			
	// Write your code here!
	assign o = sel ? a:b;
	
endmodule

module mux3x1(output [31:0]o,		// 32 bit output
					input[31:0]a,b,c,		// 32 bit inputs
					input [1:0] sel			// Selection Signal
			);
			
	// Write your code here!
	assign o = (sel==0) ? a: (sel==1)? b:c; 
	
endmodule

module adder(input   [31:0] a, b,
             output  [31:0] y);
     assign y = a + b;
endmodule

// The final module ALU which will accept the signal (Function) from Control Unit
// and two operands (either from register file or from memory (data or address),
// will perform the desired operarion and proivde the output in Result and Zero flag.

/* ALU Arithmetic and Logic Operations
----------------------------------------------------------------------
|ALU_Sel|   ALU Operation
----------------------------------------------------------------------
| 0000  |   ALU_Out = A + B;
----------------------------------------------------------------------
| 0001  |   ALU_Out = A - B;
----------------------------------------------------------------------
| 0010  |   ALU_Out = A * B;
----------------------------------------------------------------------
| 0011  |   ALU_Out = A / B;
----------------------------------------------------------------------
| 0100  |   ALU_Out = A << B;
----------------------------------------------------------------------
| 0101  |   ALU_Out = A >> A;
----------------------------------------------------------------------
| 0110  |   ALU_Out = A rotated left by 1;
----------------------------------------------------------------------
| 0111  |   ALU_Out = A rotated right by 1;
----------------------------------------------------------------------
| 1000  |   ALU_Out = A and B;
----------------------------------------------------------------------
| 1001  |   ALU_Out = A or B;
----------------------------------------------------------------------
| 1010  |   ALU_Out = A xor B;
----------------------------------------------------------------------
| 1011  |   ALU_Out = A nor B;
----------------------------------------------------------------------
| 1100  |   ALU_Out = A nand B;
----------------------------------------------------------------------
| 1101  |   ALU_Out = A xnor B;
----------------------------------------------------------------------
| 1110  |   ALU_Out = 1 if A<B else 0;
----------------------------------------------------------------------
| 1111  |   ALU_Out = 1 if A=B else 0;
----------------------------------------------------------------------*/
module alu(
           input [31:0] A,B,  // ALU 8-bit Inputs
           input [3:0] ALU_Sel,// ALU Selection
           output [31:0] ALU_Out, // ALU 8-bit Output
           output CarryOut, // Carry Out Flag
		   output ZeroOut	// Zero Flag
    );
    reg [31:0] ALU_Result;
    wire [32:0] tmp;
    assign ALU_Out = ALU_Result; // ALU out
    assign tmp = {1'b0,A} + {1'b0,B};
    assign CarryOut = tmp[32]; // Carryout flag
	assign ZeroOut = (ALU_Result == 0); // Zero Flag
    always @(*)
    begin
        case(ALU_Sel)
        4'b0000: // Addition
           ALU_Result = A + B ;
        4'b0001: // Subtraction
           ALU_Result = A - B ;
        4'b0010: // Multiplication
           ALU_Result = A * B;
        4'b0011: // Division
           ALU_Result = A/B;
        4'b0100: // Logical shift left
           ALU_Result = A<<B;
         4'b0101: // Logical shift right
           ALU_Result = A>>B;
         4'b0110: // Rotate left
           ALU_Result = {A[30:0],A[31]};
         4'b0111: // Rotate right
           ALU_Result = {A[0],A[31:1]};
          4'b1000: //  Logical and
           ALU_Result = A & B;
          4'b1001: //  Logical or
           ALU_Result = A | B;
          4'b1010: //  Logical xor
           ALU_Result = A ^ B;
          4'b1011: //  Logical nor
           ALU_Result = ~(A | B);
          4'b1100: // Logical nand
           ALU_Result = ~(A & B);
          4'b1101: // Logical xnor
           ALU_Result = ~(A ^ B);
          4'b1110: // Less comparison
           ALU_Result = (A<B)?32'd1:32'd0 ;
          4'b1111: // Equal comparison
            ALU_Result = (A==B)?32'd1:32'd0 ;
          default: ALU_Result = A + B ;
        endcase
    end

endmodule


module extend(input  		[31:7] instr,
              input  		[1:0]  immsrc,
              output reg 	[31:0] immext);
    always @(*)
    case(immsrc)
         // I-type
    2'b00:     immext = {{20{instr[31]}}, instr[31:20]};
		 // S-type (stores)
    2'b01:     immext = {{20{instr[31]}}, instr[31:25], instr[11:7]};
         // B-type (branches)
    2'b10:      immext = {{20{instr[31]}}, instr[7],  instr[30:25], instr[11:8], 1'b0};                          // J-type (jal)
		// J-type (branches)
	2'b11:      immext = {{12{instr[31]}}, instr[19:12],  instr[20], instr[30:21], 1'b0};
           
	default: 	immext = 32'bx; // undefined
    endcase
endmodule
