{-# LANGUAGE DataKinds #-}
module DblX3 where

import ReWire
import ReWire.Bits
import Basic (Hex , X16(..))
import DoubleRound(doubleround)

-- data Inp a = Stall | Arg a
-- data Out a = DC    | Val a

dbl3 :: Hex (W 32) -> ReacT (Hex (W 32)) (Hex (W 32)) Identity ()
dbl3 x = do
           x' <- signal (doubleround (doubleround (doubleround x)))
           dbl3 x'

_0 :: W 32
_0 = lit 0

start :: ReacT (Hex (W 32)) (Hex (W 32)) Identity ()
start = dbl3 (X16 _0 _0 _0 _0 _0 _0 _0 _0 _0 _0 _0 _0 _0 _0 _0 _0)

{-

yosys -p "read_verilog -sv DblX3.sv; synth_ice40 -top top_level -json dblx3.json"

2.48. Printing statistics.

=== top_level ===

   Number of wires:               5849
   Number of wire bits:         120225
   Number of public wires:        5849
   Number of public wire bits:  120225
   Number of ports:                  4
   Number of port bits:           1024
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:               6171
     $scopeinfo                    123
     SB_CARRY                     2976
     SB_LUT4                      3072

-}
