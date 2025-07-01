{-# LANGUAGE DataKinds #-}
-- module DblPipe3 where

import ReWire
import ReWire.Bits
import Basic (Hex , X16(..))
import DoubleRound(doubleround)

data Inp a = Stall | Arg a
data Out a = DC    | Val a

db :: Inp (Hex (W 32)) -> Out (Hex (W 32))
db Stall   = DC
db (Arg a) = Val (doubleround a)

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

three :: (Inp (Hex (W 32)), Inp (Hex (W 32)), Inp (Hex (W 32)))
               -> (Out (Hex (W 32)), Out (Hex (W 32)), Out (Hex (W 32)))
three (i1 , i2 , i3) = (db i1 , db i2 , db i3)

pdbl3 :: Inp (Hex (W 32)) -> ReacT (Inp (Hex (W 32))) (Out (Hex (W 32))) Identity ()
pdbl3 = refold three out3 conn (DC , DC , DC)

start :: ReacT (Inp (Hex (W 32))) (Out (Hex (W 32))) Identity ()
start = pdbl3 Stall

{-

rwc DblPipe3.hs

yosys -p "read_verilog -sv DblPipe3.sv; synth_ice40 -top top_level -json dblpipe3.json"

2.49. Printing statistics.

=== top_level ===

   Number of wires:              14316
   Number of wire bits:         1348940
   Number of public wires:       14316
   Number of public wire bits:  1348940
   Number of ports:                  6
   Number of port bits:           1028
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:               9247
     $scopeinfo                    630
     SB_CARRY                     2976
     SB_DFFR                      1028
     SB_LUT4                      4613

nextpnr-ice40 --hx8k --package ct256 --json dblpipe3.json --asc dblpipe3.asc


-}
