{-# LANGUAGE DataKinds #-}
module Pipelining.RW_DoublePipe2 where

import Prelude hiding ((+))
import ReWire
import ReWire.Bits hiding (one)

import Salsa20.Basic(Hex)
import Salsa20.DoubleRound(doubleround)

-- 
-- This is a hacked up copy of ../pipelining/RW_Pipe123.hs
-- 

data Inp a = Stall | Arg a
data Out a = DC    | Val a

type HW32 = Hex (W 32)

io_dr5 :: Inp HW32 -> Out HW32
io_dr5 Stall   = DC
io_dr5 (Arg a) = Val (doubleround . doubleround . doubleround . doubleround . doubleround $ a)

out2 :: (Out a, Out a) -> Out a
out2 (_ , o2) = o2

out5 :: (Out a , Out a , Out a , Out a , Out a) -> Out a
out5 (_ , _ , _ , _ , o5) = o5

out10 :: (Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a) -> Out a
out10 (_ , _ , _ , _ , _ , _ , _ , _ , _ , o10) = o10

next :: Out a -> Inp a
next DC      = Stall
next (Val x) = Arg x

conn2 :: (Out a, Out a) -> Inp a -> (Inp a, Inp a)
conn2 (i , _) ix = (ix , next i)

conn5 :: (Out a , Out a , Out a , Out a , Out a) ->
         Inp a                                   ->
         (Inp a , Inp a , Inp a , Inp a , Inp a)
conn5 (i1 , i2 , i3 , i4 , _) ix = (ix , next i1 , next i2 , next i3 , next i4)

conn10 :: (Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a) ->
         Inp a                                   ->
         (Inp a , Inp a , Inp a , Inp a , Inp a , Inp a , Inp a , Inp a , Inp a , Inp a)
conn10 (i1 , i2 , i3 , i4 , i5 , i6 , i7 , i8 , i9 , _) ix
  = (ix , next i1 , next i2 , next i3 , next i4 , next i5 , next i6 , next i7 , next i8 , next i9)

refold :: Monad m => (ii -> oi) -> (oi -> ox) -> (oi -> ix -> ii) -> oi -> ix -> ReacT ix ox m ()
refold f out conn oi ix = do
                            let ii = conn oi ix
                            let o = f ii
                            ix' <- signal (out o)
                            refold f out conn o ix'

twice :: (Inp HW32, Inp HW32) -> (Out HW32, Out HW32)
twice (i1 , i2) = (io_dr5 i1 , io_dr5 i2)

pdbl2 :: Inp HW32 -> ReacT (Inp HW32) (Out HW32) Identity ()
pdbl2 = refold twice out2 conn2 (DC , DC)

start :: ReacT (Inp HW32) (Out HW32) Identity ()
start = pdbl2 Stall

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
