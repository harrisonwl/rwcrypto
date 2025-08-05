{-# LANGUAGE DataKinds #-}
module Pipe123 where

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

refold :: Monad m => (ii -> oi) -> (oi -> ox) -> (oi -> ix -> ii) -> oi -> ix -> ReacT ix ox m ()
refold f out conn oi ix = do
                            let ii = conn oi ix
                            let o = f ii
                            ix' <- signal (out o)
                            refold f out conn o ix'

out3 :: (a , b , c) -> c
out3 (_ , _ , x) = x

---------------------------------------
--       the stalling version
---------------------------------------

io_one , io_two , io_three :: Inp (W 8) -> Out (W 8)
io_one Stall     = DC
io_one (Arg a)   = Val (one a)
io_two Stall     = DC
io_two (Arg a)   = Val (two a)
io_three Stall   = DC
io_three (Arg a) = Val (three a)

conn :: (Out a, Out a, Out a) -> Inp a -> (Inp a, Inp a, Inp a)
conn (DC , DC , _) ix         = (ix , Stall , Stall)
conn (Val x1 , Val x2 , _) ix = (ix , Arg x1 , Arg x2)
conn (Val x1 , DC , _) ix     = (ix , Arg x1 , Stall)
conn (DC , Val x2 , _) ix     = (ix , Stall , Arg x2)

thrice :: (Inp (W 8), Inp (W 8), Inp (W 8)) -> (Out (W 8), Out (W 8), Out (W 8))
thrice (i1 , i2 , i3) = (io_one i1 , io_two i2 , io_three i3)

pdbl3 :: Inp (W 8) -> ReacT (Inp (W 8)) (Out (W 8)) Identity ()
pdbl3 = refold thrice out3 conn (DC , DC , DC)

startS :: ReacT (Inp (W 8)) (Out (W 8)) Identity ()
startS = pdbl3 Stall

data Inp a = Stall | Arg a deriving Show
data Out a = DC    | Val a deriving Show

---------------------------------------
--       the no-stall version
---------------------------------------

cons :: (W 8 , W 8 , W 8) -> W 8 -> (W 8 ,  W 8 , W 8)
cons (x1 , x2 , _) ix = (ix , x1 , x2)

times3 :: (W 8 , W 8 , W 8) -> (W 8 ,  W 8 , W 8)
times3 (i1 , i2 , i3) = (one i1 , two i2 , three i3)

nostall :: W 8 -> ReacT (W 8) (W 8) Identity ()
nostall = refold times3 out3 cons (lit 0 , lit 0 , lit 0)

startNS :: ReacT (W 8) (W 8) Identity ()
startNS = nostall (lit 99)

---------------------------------------
--       running tests, etc.
---------------------------------------

instance Pretty a => Pretty (Inp a) where
  pp Stall   = "Stall"
  pp (Arg x) = "Arg " ++ pp x

instance Pretty a => Pretty (Out a) where
  pp DC      = "DC"
  pp (Val x) = "Val " ++ pp x

ins :: [W 8]
ins = map lit [0x1..0xF]

exNS = runP startNS (lit 101 , lit 101) ins -- (Stall , DC) (map Arg ins)

exS  :: WriterPlus (Inp (W 8), Out (W 8)) (Maybe ((), Inp (W 8)))
exS = runP startS (Stall , DC) (map Arg ins)

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
