{-# LANGUAGE DataKinds #-}
module NoStallPipe123 where

import Prelude hiding ((+))
import ReWire (W , Bit , extrude)
import ReWire.Bits (lit , (+))
import ReWire.Interactive

import Control.Monad.Identity 
import Control.Monad.Resumption.Reactive 

one , two , three :: W 8 -> W 8
one x   = x + lit 1
two x   = x + lit 2
three x = x + lit 3

---------------------------------------
--       the no-stall version
---------------------------------------

out3 :: (a , b , c) -> c
out3 (_ , _ , x) = x

conn3 :: (W 8 , W 8 , W 8) -> W 8 -> (W 8 ,  W 8 , W 8)
conn3 (o1 , o2 , _) ix = (ix , o1 , o2)

times3 :: (W 8 , W 8 , W 8) -> (W 8 ,  W 8 , W 8)
times3 (i1 , i2 , i3) = (one i1 , two i2 , three i3)

pipeline :: Monad m => (ii -> oi) -> (oi -> ox) -> (oi -> ix -> ii) -> oi -> ix -> ReacT ix ox m ()
pipeline f out conn oi ix = do
                            let ii = conn oi ix
                            let o = f ii
                            ix' <- signal (out o)
                            pipeline f out conn o ix'

nostall :: W 8 -> ReacT (W 8) (W 8) Identity ()
nostall = pipeline times3 out3 conn3 (lit 0 , lit 0 , lit 0)

start :: ReacT (W 8) (W 8) Identity ()
start = nostall (lit 99)

---------------------------------------
--       running tests, etc.
---------------------------------------

ins :: [W 8]
ins = map lit [0x1..0xF]

exNS = runP start (lit 101 , lit 101) ins -- (Stall , DC) (map Arg ins)

-- exS  :: WriterPlus (Inp (W 8), Out (W 8)) (Maybe ((), Inp (W 8)))
-- exS = runP start (Stall , DC) (map Arg ins)

-- λ> pp exNS
-- "(0x65,0x65) :> (0x01,0x03) :> (0x02,0x05) :> (0x03,0x69) :> (0x04,0x07) :> (0x05,0x08) :> (0x06,0x09) :> (0x07,0x0A) :> (0x08,0x0B) :> (0x09,0x0C) :> (0x0A,0x0D) :> (0x0B,0x0E) :> (0x0C,0x0F) :> (0x0D,0x10) :> (0x0E,0x11) :> (0x0F,0x12) :+> Nothing"

-- λ> pp exS
-- "(Stall,DC) :> (Arg 0x01,DC) :> (Arg 0x02,DC) :> (Arg 0x03,DC) :> (Arg 0x04,Val 0x07) :> (Arg 0x05,Val 0x08) :> (Arg 0x06,Val 0x09) :> (Arg 0x07,Val 0x0A) :> (Arg 0x08,Val 0x0B) :> (Arg 0x09,Val 0x0C) :> (Arg 0x0A,Val 0x0D) :> (Arg 0x0B,Val 0x0E) :> (Arg 0x0C,Val 0x0F) :> (Arg 0x0D,Val 0x10) :> (Arg 0x0E,Val 0x11) :> (Arg 0x0F,Val 0x12) :+> Nothing"
-- λ> 

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
