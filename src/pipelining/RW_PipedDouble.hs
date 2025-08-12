{-# LANGUAGE DataKinds #-}
module Pipelining.RW_PipedDouble where

import Prelude hiding ((+))
import ReWire
import ReWire.Bits hiding (one)

import Salsa20.Basic (Hex)
import Salsa20.DoubleRound (doubleround)

data Inp a = Stall | Arg a
data Out a = DC    | Val a

type HxW32 = Hex (W 32)

-- |
-- | Boilerplate.
-- |
-- | Each of these conn_/out_ and f*x functions could be constructed in a
-- | dependently-typed language like Agda. They appear as boilerplate here
-- | because ReWire is embedded in Haskell.
-- |

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

out2 :: (Out a, Out a) -> Out a
out2 (_ , o2) = o2


out5 :: (Out a , Out a , Out a , Out a , Out a) -> Out a
out5 (_ , _ , _ , _ , o5) = o5

out10 :: (Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a) -> Out a
out10 (_ , _ , _ , _ , _ , _ , _ , _ , _ , o10) = o10

-- |
-- |
-- |

dr1  :: Inp HxW32 -> Out HxW32
dr1 Stall   = DC
dr1 (Arg a) = Val (doubleround a)

dr2  :: Inp HxW32 -> Out HxW32
dr2 Stall   = DC
dr2 (Arg a) = Val (doubleround . doubleround $ a)

dr5  :: Inp HxW32 -> Out HxW32
dr5 Stall   = DC
dr5 (Arg a) = Val (doubleround . doubleround . doubleround . doubleround . doubleround $ a)

two :: (Inp HxW32 , Inp HxW32) -> (Out HxW32 , Out HxW32) 
two (i1 , i2) = (dr5 i1 , dr5 i2)
  
five :: (Inp HxW32 , Inp HxW32 , Inp HxW32 , Inp HxW32 , Inp HxW32) ->
        (Out HxW32 , Out HxW32 , Out HxW32 , Out HxW32 , Out HxW32) 
five (i0 , i1 , i2 , i3 , i4) = (dr2 i0 , dr2 i1 , dr2 i2 , dr2 i3 , dr2 i4)

ten :: (Inp HxW32 , Inp HxW32 , Inp HxW32 , Inp HxW32 , Inp HxW32 , Inp HxW32 , Inp HxW32 , Inp HxW32 , Inp HxW32 , Inp HxW32) ->
        (Out HxW32 , Out HxW32 , Out HxW32 , Out HxW32 , Out HxW32 , Out HxW32 , Out HxW32 , Out HxW32 , Out HxW32 , Out HxW32) 
ten (i0 , i1 , i2 , i3 , i4 , i5 , i6 , i7 , i8 , i9)
  = (dr1 i0 , dr1 i1 , dr1 i2 , dr1 i3 , dr1 i4 , dr1 i5 , dr1 i6 , dr1 i7 , dr1 i8 , dr1 i9)
  

pipeline :: Monad m => (ii -> oi) -> (oi -> ox) -> (oi -> ix -> ii) -> oi -> ix -> ReacT ix ox m ()
pipeline f out conn oi ix = do
                            let ii = conn oi ix
                            let o = f ii
                            ix' <- signal (out o)
                            pipeline f out conn o ix'
  
pipe2 :: Inp (Hex (W 32)) -> ReacT (Inp (Hex (W 32))) (Out (Hex (W 32))) Identity ()
pipe2 = pipeline two out2 conn2 (DC , DC)

pipe5 :: Inp (Hex (W 32)) -> ReacT (Inp (Hex (W 32))) (Out (Hex (W 32))) Identity ()
pipe5 = pipeline five out5 conn5 (DC , DC , DC , DC , DC)

pipe10 :: Inp (Hex (W 32)) -> ReacT (Inp (Hex (W 32))) (Out (Hex (W 32))) Identity ()
pipe10 = pipeline ten out10 conn10 (DC , DC , DC , DC , DC , DC , DC , DC , DC , DC)

start :: ReacT (Inp (Hex (W 32))) (Out (Hex (W 32))) Identity ()
-- start = pipe2 Stall
-- start = pipe5 Stall
start = pipe10 Stall

{-

1. Two stage: yosys -p "read_verilog -sv PipedDouble2.sv; synth_ice40 -top top_level -json pipe2.json"

=== top_level ===

   Number of wires:              44215
   Number of wire bits:         3516334
   Number of public wires:       44215
   Number of public wire bits:  3516334
   Number of ports:                  6
   Number of port bits:           1028
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:              24143
     $scopeinfo                   2125
     SB_CARRY                     9920
     SB_DFFR                       515
     SB_LUT4                     11583

2. Five stage: yosys -p "read_verilog -sv PipedDouble5.sv; synth_ice40 -top top_level -json pipe5.json"

=== top_level ===

   Number of wires:              47517
   Number of wire bits:         2697418
   Number of public wires:       47517
   Number of public wire bits:  2697418
   Number of ports:                  6
   Number of port bits:           1028
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:              39339
     $scopeinfo                   2160
     SB_CARRY                    13888
     SB_DFFR                      3080
     SB_LUT4                     20211

3. Ten stage: yosys -p "read_verilog -sv PipedDouble10.sv; synth_ice40 -top top_level -json pipe10.json"

=== top_level ===

   Number of wires:              48713
   Number of wire bits:         3215942
   Number of public wires:       48713
   Number of public wire bits:  3215942
   Number of ports:                  6
   Number of port bits:           1028
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:              43000
     $scopeinfo                   2110
     SB_CARRY                    11904
     SB_DFFR                      5645
     SB_LUT4                     23341

-}
