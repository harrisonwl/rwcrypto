module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  input logic [7:0] __in1,
  output logic [0:0] __out0,
  output logic [7:0] __out1);
  logic [46:0] zll_pure_dispatch5_in;
  logic [8:0] main_refold1_in;
  logic [35:0] main_conn1_in;
  logic [26:0] main_conn1_out;
  logic [26:0] zll_main_thrice2_in;
  logic [26:0] zll_main_thrice2_out;
  logic [26:0] zll_main_out3_in;
  logic [8:0] zll_main_out3_out;
  logic [46:0] zll_pure_dispatch4_in;
  logic [44:0] zll_pure_dispatch_in;
  logic [44:0] zll_main_refold_in;
  logic [35:0] main_conn1_inR1;
  logic [26:0] main_conn1_outR1;
  logic [26:0] zll_main_thrice2_inR1;
  logic [26:0] zll_main_thrice2_outR1;
  logic [35:0] main_refold_in;
  logic [46:0] main_refold_out;
  logic [46:0] zll_pure_dispatch1_in;
  logic [17:0] zll_pure_dispatch3_in;
  logic [17:0] zll_main_refold1_in;
  logic [35:0] main_conn1_inR2;
  logic [26:0] main_conn1_outR2;
  logic [26:0] zll_main_thrice2_inR2;
  logic [26:0] zll_main_thrice2_outR2;
  logic [35:0] main_refold_inR1;
  logic [46:0] main_refold_outR1;
  logic [0:0] __continue;
  logic [37:0] __resumption_tag;
  logic [37:0] __resumption_tag_next;
  assign zll_pure_dispatch5_in = {{__in0, __in1}, __resumption_tag};
  assign main_refold1_in = zll_pure_dispatch5_in[46:38];
  assign main_conn1_in = {27'h0, main_refold1_in[8:0]};
  Main_conn1  inst (main_conn1_in[35:9], main_conn1_in[8:0], main_conn1_out);
  assign zll_main_thrice2_in = main_conn1_out;
  ZLL_Main_thrice2  instR1 (zll_main_thrice2_in[26:0], zll_main_thrice2_out);
  assign zll_main_out3_in = zll_main_thrice2_out;
  ZLL_Main_out3  instR2 (zll_main_out3_in[26:0], zll_main_out3_out);
  assign zll_pure_dispatch4_in = {{__in0, __in1}, __resumption_tag};
  assign zll_pure_dispatch_in = {zll_pure_dispatch4_in[46:38], zll_pure_dispatch4_in[35:27], zll_pure_dispatch4_in[26:0]};
  assign zll_main_refold_in = {zll_pure_dispatch_in[35:27], zll_pure_dispatch_in[26:0], zll_pure_dispatch_in[44:36]};
  assign main_conn1_inR1 = {zll_main_refold_in[35:9], zll_main_refold_in[44:36]};
  Main_conn1  instR3 (main_conn1_inR1[35:9], main_conn1_inR1[8:0], main_conn1_outR1);
  assign zll_main_thrice2_inR1 = main_conn1_outR1;
  ZLL_Main_thrice2  instR4 (zll_main_thrice2_inR1[26:0], zll_main_thrice2_outR1);
  assign main_refold_in = {zll_main_thrice2_outR1, zll_main_refold_in[8:0]};
  Main_refold  instR5 (main_refold_in[35:9], main_refold_in[8:0], main_refold_out);
  assign zll_pure_dispatch1_in = {{__in0, __in1}, __resumption_tag};
  assign zll_pure_dispatch3_in = {zll_pure_dispatch1_in[46:38], zll_pure_dispatch1_in[8:0]};
  assign zll_main_refold1_in = {zll_pure_dispatch3_in[8:0], zll_pure_dispatch3_in[17:9]};
  assign main_conn1_inR2 = {27'h0, zll_main_refold1_in[17:9]};
  Main_conn1  instR6 (main_conn1_inR2[35:9], main_conn1_inR2[8:0], main_conn1_outR2);
  assign zll_main_thrice2_inR2 = main_conn1_outR2;
  ZLL_Main_thrice2  instR7 (zll_main_thrice2_inR2[26:0], zll_main_thrice2_outR2);
  assign main_refold_inR1 = {zll_main_thrice2_outR2, zll_main_refold1_in[8:0]};
  Main_refold  instR8 (main_refold_inR1[35:9], main_refold_inR1[8:0], main_refold_outR1);
  assign {__continue, __out0, __out1, __resumption_tag_next} = (zll_pure_dispatch1_in[37:36] == 2'h1) ? main_refold_outR1 : ((zll_pure_dispatch4_in[37:36] == 2'h2) ? main_refold_out : {zll_main_out3_out, 29'h8000000, main_refold1_in[8:0]});
  initial __resumption_tag <= {6'h26{1'h0}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= {6'h26{1'h0}};
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module Main_conn1 (input logic [26:0] arg0,
  input logic [8:0] arg1,
  output logic [26:0] res);
  logic [71:0] zll_main_conn15_in;
  logic [71:0] zll_main_conn11_in;
  logic [71:0] zll_main_conn12_in;
  logic [35:0] zll_main_conn8_in;
  logic [35:0] zll_main_conn9_in;
  logic [25:0] zll_main_conn5_in;
  logic [16:0] zll_main_conn2_in;
  logic [35:0] zll_main_conn17_in;
  logic [25:0] zll_main_conn4_in;
  logic [16:0] zll_main_conn10_in;
  logic [35:0] zll_main_conn13_in;
  logic [33:0] zll_main_conn7_in;
  logic [33:0] zll_main_conn1_in;
  logic [24:0] zll_main_conn14_in;
  logic [35:0] zll_main_conn16_in;
  logic [8:0] zll_main_conn6_in;
  assign zll_main_conn15_in = {arg1, arg0, arg0, arg1};
  assign zll_main_conn11_in = {zll_main_conn15_in[71:63], zll_main_conn15_in[62:36], zll_main_conn15_in[62:36], zll_main_conn15_in[71:63]};
  assign zll_main_conn12_in = {zll_main_conn11_in[71:63], zll_main_conn11_in[62:36], zll_main_conn11_in[62:36], zll_main_conn11_in[71:63]};
  assign zll_main_conn8_in = {zll_main_conn12_in[62:36], zll_main_conn12_in[71:63]};
  assign zll_main_conn9_in = zll_main_conn8_in[35:0];
  assign zll_main_conn5_in = {zll_main_conn9_in[25:18], zll_main_conn9_in[17:9], zll_main_conn9_in[8:0]};
  assign zll_main_conn2_in = {zll_main_conn5_in[25:18], zll_main_conn5_in[8:0]};
  assign zll_main_conn17_in = zll_main_conn12_in[35:0];
  assign zll_main_conn4_in = {zll_main_conn17_in[34:27], zll_main_conn17_in[17:9], zll_main_conn17_in[8:0]};
  assign zll_main_conn10_in = {zll_main_conn4_in[25:18], zll_main_conn4_in[8:0]};
  assign zll_main_conn13_in = zll_main_conn11_in[35:0];
  assign zll_main_conn7_in = {zll_main_conn13_in[34:27], zll_main_conn13_in[25:18], zll_main_conn13_in[17:9], zll_main_conn13_in[8:0]};
  assign zll_main_conn1_in = {zll_main_conn7_in[25:18], zll_main_conn7_in[33:26], zll_main_conn7_in[17:9], zll_main_conn7_in[8:0]};
  assign zll_main_conn14_in = {zll_main_conn1_in[33:26], zll_main_conn1_in[25:18], zll_main_conn1_in[8:0]};
  assign zll_main_conn16_in = zll_main_conn15_in[35:0];
  assign zll_main_conn6_in = zll_main_conn16_in[8:0];
  assign res = ((zll_main_conn16_in[35] == 1'h0) && (zll_main_conn16_in[26] == 1'h0)) ? {zll_main_conn6_in[8:0], 18'h0} : (((zll_main_conn13_in[35] == 1'h1) && (zll_main_conn13_in[26] == 1'h1)) ? {zll_main_conn14_in[8:0], 1'h1, zll_main_conn14_in[16:9], 1'h1, zll_main_conn14_in[24:17]} : (((zll_main_conn17_in[35] == 1'h1) && (zll_main_conn17_in[26] == 1'h0)) ? {zll_main_conn10_in[8:0], 1'h1, zll_main_conn10_in[16:9], 9'h0} : {zll_main_conn2_in[8:0], 10'h1, zll_main_conn2_in[16:9]}));
endmodule

module ZLL_Main_thrice2 (input logic [26:0] arg0,
  output logic [26:0] res);
  logic [26:0] zll_main_thrice_in;
  logic [8:0] main_io_one_in;
  logic [17:0] zll_main_io_one1_in;
  logic [8:0] zll_main_io_one2_in;
  logic [8:0] zll_main_io_one_in;
  logic [7:0] zll_main_one_in;
  logic [15:0] binop_in;
  logic [8:0] lit_in;
  logic [8:0] main_io_two_in;
  logic [17:0] zll_main_io_two1_in;
  logic [8:0] zll_main_io_two_in;
  logic [8:0] zll_main_io_two3_in;
  logic [7:0] zll_main_two_in;
  logic [15:0] binop_inR1;
  logic [8:0] lit_inR1;
  logic [8:0] main_io_three_in;
  logic [17:0] zll_main_io_three1_in;
  logic [8:0] zll_main_io_three_in;
  logic [8:0] zll_main_io_three3_in;
  logic [7:0] zll_main_three1_in;
  logic [15:0] binop_inR2;
  logic [8:0] lit_inR2;
  assign zll_main_thrice_in = arg0;
  assign main_io_one_in = zll_main_thrice_in[26:18];
  assign zll_main_io_one1_in = {main_io_one_in[8:0], main_io_one_in[8:0]};
  assign zll_main_io_one2_in = zll_main_io_one1_in[17:9];
  assign zll_main_io_one_in = zll_main_io_one2_in[8:0];
  assign zll_main_one_in = zll_main_io_one_in[7:0];
  assign binop_in = {zll_main_one_in[7:0], 8'h1};
  assign lit_in = zll_main_io_one1_in[8:0];
  assign main_io_two_in = zll_main_thrice_in[17:9];
  assign zll_main_io_two1_in = {main_io_two_in[8:0], main_io_two_in[8:0]};
  assign zll_main_io_two_in = zll_main_io_two1_in[17:9];
  assign zll_main_io_two3_in = zll_main_io_two_in[8:0];
  assign zll_main_two_in = zll_main_io_two3_in[7:0];
  assign binop_inR1 = {zll_main_two_in[7:0], 8'h2};
  assign lit_inR1 = zll_main_io_two1_in[8:0];
  assign main_io_three_in = zll_main_thrice_in[8:0];
  assign zll_main_io_three1_in = {main_io_three_in[8:0], main_io_three_in[8:0]};
  assign zll_main_io_three_in = zll_main_io_three1_in[17:9];
  assign zll_main_io_three3_in = zll_main_io_three_in[8:0];
  assign zll_main_three1_in = zll_main_io_three3_in[7:0];
  assign binop_inR2 = {zll_main_three1_in[7:0], 8'h3};
  assign lit_inR2 = zll_main_io_three1_in[8:0];
  assign res = {(lit_in[8] == 1'h0) ? 9'h0 : {1'h1, binop_in[15:8] + binop_in[7:0]}, (lit_inR1[8] == 1'h0) ? 9'h0 : {1'h1, binop_inR1[15:8] + binop_inR1[7:0]}, (lit_inR2[8] == 1'h0) ? 9'h0 : {1'h1, binop_inR2[15:8] + binop_inR2[7:0]}};
endmodule

module Main_refold (input logic [26:0] arg0,
  input logic [8:0] arg1,
  output logic [46:0] res);
  logic [35:0] main_conn1_in;
  logic [26:0] main_conn1_out;
  logic [26:0] zll_main_thrice2_in;
  logic [26:0] zll_main_thrice2_out;
  logic [26:0] zll_main_out3_in;
  logic [8:0] zll_main_out3_out;
  assign main_conn1_in = {arg0, arg1};
  Main_conn1  inst (main_conn1_in[35:9], main_conn1_in[8:0], main_conn1_out);
  assign zll_main_thrice2_in = main_conn1_out;
  ZLL_Main_thrice2  instR1 (zll_main_thrice2_in[26:0], zll_main_thrice2_out);
  assign zll_main_out3_in = zll_main_thrice2_out;
  ZLL_Main_out3  instR2 (zll_main_out3_in[26:0], zll_main_out3_out);
  assign res = {zll_main_out3_out, 2'h2, arg1, arg0};
endmodule

module ZLL_Main_out3 (input logic [26:0] arg0,
  output logic [8:0] res);
  logic [26:0] zll_main_out31_in;
  logic [17:0] zll_main_out32_in;
  assign zll_main_out31_in = arg0;
  assign zll_main_out32_in = {zll_main_out31_in[17:9], zll_main_out31_in[8:0]};
  assign res = zll_main_out32_in[8:0];
endmodule