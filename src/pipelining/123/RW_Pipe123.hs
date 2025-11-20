{-# LANGUAGE DataKinds #-}
-- module Pipelining.RW_Pipe123 where

import Prelude hiding ((+))
import ReWire
import ReWire.Bits hiding (one)

data Inp a = Stall | Arg a
data Out a = DC    | Val a

one , two , three :: W 8 -> W 8
one x   = x + lit 1
two x   = x + lit 2
three x = x + lit 3

io_one , io_two , io_three :: Inp (W 8) -> Out (W 8)
io_one Stall     = DC
io_one (Arg a)   = Val (one a)
io_two Stall     = DC
io_two (Arg a)   = Val (two a)
io_three Stall   = DC
io_three (Arg a) = Val (three a)

out3 :: (Out a, Out b, Out c) -> Out c
out3 (_ , _ , x) = x

conn :: (Out a, Out a, Out a) -> Inp a -> (Inp a, Inp a, Inp a)
conn (DC , DC , _) ix         = (ix , Stall , Stall)
conn (Val x1 , Val x2 , _) ix = (ix , Arg x1 , Arg x2)
conn (Val x1 , DC , _) ix     = (ix , Arg x1 , Stall)
conn (DC , Val x2 , _) ix     = (ix , Stall , Arg x2)

refold :: Monad m => (ii -> oi) -> (oi -> ox) -> (oi -> ix -> ii) -> oi -> ix -> ReacT ix ox m ()
refold f out conn oi ix = do
                            let ii = conn oi ix
                            let o = f ii
                            ix' <- signal (out o)
                            refold f out conn o ix'

thrice :: (Inp (W 8), Inp (W 8), Inp (W 8)) -> (Out (W 8), Out (W 8), Out (W 8))
thrice (i1 , i2 , i3) = (io_one i1 , io_two i2 , io_three i3)

pdbl3 :: Inp (W 8) -> ReacT (Inp (W 8)) (Out (W 8)) Identity ()
pdbl3 = refold thrice out3 conn (DC , DC , DC)

start :: ReacT (Inp (W 8)) (Out (W 8)) Identity ()
start = pdbl3 Stall

{-

yosys -p "read_verilog -sv Pipe123.sv; synth_ice40 -top top_level -json pipe123.json"

=== top_level ===

   Number of wires:                329
   Number of wire bits:           5532
   Number of public wires:         329
   Number of public wire bits:    5532
   Number of ports:                  6
   Number of port bits:             20
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:                178
     $scopeinfo                     15
     SB_CARRY                       29
     SB_DFFR                        38
     SB_LUT4                        96

nextpnr-ice40 --hx8k --package ct256 --json pipe123.json --asc pipe123.asc

Info: Max frequency for clock 'clk$SB_IO_IN_$glb_clk': 216.12 MHz (PASS at 12.00 MHz)

-}
