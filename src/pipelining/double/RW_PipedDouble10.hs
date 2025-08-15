{-# LANGUAGE DataKinds #-}
module Pipelining.RW_PipedDouble10 where

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

conn10 :: (Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a) ->
         Inp a                                   ->
         (Inp a , Inp a , Inp a , Inp a , Inp a , Inp a , Inp a , Inp a , Inp a , Inp a)
conn10 (i1 , i2 , i3 , i4 , i5 , i6 , i7 , i8 , i9 , _) ix
  = (ix , next i1 , next i2 , next i3 , next i4 , next i5 , next i6 , next i7 , next i8 , next i9)

out10 :: (Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a , Out a) -> Out a
out10 (_ , _ , _ , _ , _ , _ , _ , _ , _ , o10) = o10

-- |
-- |
-- |

dr1  :: Inp HxW32 -> Out HxW32
dr1 Stall   = DC
dr1 (Arg a) = Val (doubleround a)

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

pipe10 :: Inp (Hex (W 32)) -> ReacT (Inp (Hex (W 32))) (Out (Hex (W 32))) Identity ()
pipe10 = pipeline ten out10 conn10 (DC , DC , DC , DC , DC , DC , DC , DC , DC , DC)

start :: ReacT (Inp (Hex (W 32))) (Out (Hex (W 32))) Identity ()
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
