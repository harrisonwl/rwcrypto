module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [93:0] __in0,
  output logic [4:0] __out0);
  // state registers
  // __st0: 159 bits, init 0x0
  logic [158:0] __st0;
  logic [158:0] __st0_next;
  // combinational logic
  wire [63:0] k = __st0[158:95];
  wire [11:0] d = __st0[94:83];
  wire [15:0] c = __st0[82:67];
  wire [0:0] hp = __st0[66];
  wire [15:0] pv = __st0[65:50];
  wire [0:0] lrn = __st0[49];
  wire [31:0] work = __st0[48:17];
  wire [15:0] r = __st0[16:1];
  wire [0:0] busy = __st0[0];
  wire [63:0] k2 = __in0[91:28];
  wire [11:0] d2 = __in0[27:16];
  wire [15:0] c2 = __in0[15:0];
  wire [31:0] hop = __in0[31:0];
  wire [0:0] Zt1 = r == 16'h210;
  wire [63:0] slice_in = (k >> {{6'h30{1'h0}}, (16'h40 == 16'h0) ? (16'hf - r) : ((16'hf - r) % 16'h40)}) & 64'h1;
  wire [15:0] ctr = work[15:0];
  wire [0:0] discOK = ((work >> 32'h10) & 32'hfff) == {20'h0, d};
  wire [15:0] delta = ctr - c;
  wire [0:0] Zt1R1 = delta == 16'h0;
  wire [0:0] Zt3 = delta <= 16'h10;
  wire [0:0] Zt5 = delta <= 16'h8000;
  wire [0:0] Zt8 = ctr == (pv + 16'h1);
  wire [35:0] Zds1 = (~discOK) ? {3'h4, c, 17'h0} :
    ((~Zt1R1) ? ((~Zt3) ? ((~Zt5) ? {3'h1, c, 17'h0} :
      ((~hp) ? {3'h2, c, 1'h1, ctr} :
        ((~Zt8) ? {3'h2, c, 1'h1, ctr} : {3'h3, ctr, 17'h0}))) : {3'h0, ctr, 17'h0}) : {3'h1, c, 17'h0});
  wire [15:0] lastC = Zds1[32:17];
  wire [0:0] have = Zds1[16];
  wire [15:0] pend = Zds1[15:0];
  wire [2:0] v = Zds1[35:33];
  wire [163:0] Zds = (~busy) ? ((__in0[93:92] == 2'h0) ? {k, d, c, hp, pv, lrn, work, r, 6'h0} :
    ((__in0[93:92] == 2'h1) ? {k2, d2, c2, {18'h1, {6'h36{1'h0}}}} :
      ((~lrn) ? {k, d, c, hp, pv, lrn, work, r, 6'h15} : {k, d, c, hp, pv, lrn, hop, 22'h28}))) :
    ((~Zt1) ? {k, d, c, hp, pv, lrn,
      (work << 32'h1) | (((((work >> 32'h1f) & 32'h1) ^ ((32'h3a5c742e >> ((((((work >> {6'h20{1'h0}}) & 32'h1) | (((work >> 32'h8) & 32'h1) << 32'h1)) | (((work >> 32'h13) & 32'h1) << 32'h2)) | (((work >> 32'h19) & 32'h1) << 32'h3)) | (((work >> 32'h1e) & 32'h1) << 32'h4))) & 32'h1)) ^ ((work >> 32'hf) & 32'h1)) ^ slice_in[31:0]),
      r + 16'h1, 6'h28} : {k, d, lastC, have, pend, lrn, 51'h2, v});
  wire [158:0] st$ = Zds[163:5];
  wire [4:0] out = Zds[4:0];
  wire [163:0] Zres = {out, st$};
  assign __st0_next = Zres[158:0];
  // outputs
  assign __out0 = Zres[163:159];
  // state register update
  initial __st0 = {8'h9f{1'h0}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= {8'h9f{1'h0}};
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule