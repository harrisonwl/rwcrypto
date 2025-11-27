{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Sha3.Chi(chi) where

import Prelude hiding ((^) , (+))
import ReWire
import ReWire.Bits hiding ((+))
import ReWire.Finite
import ReWire.FiniteComp ((+))
import ReWire.Vectors        as RWV

import Sha3.Layout (A)

rd :: A -> Finite 5 -> Finite 5 -> W 64
rd a x y = index (index a x) y

action :: A -> Finite 5 -> Finite 5 -> W 64
action a x y = (rd a x y) ^
                   ( (bnot $ rd a (x + finite 1) y) .&. (rd a (x + finite 2) y) )

chi :: A -> A
chi a = generate $ \ x ->
            generate $ \ y -> action a x y

{-

--
-- Sanity checking follows.
--

a1 :: Vec 5 (Vec 5 (W 64))
a1 = fromList [row0 , row1 , row2 , row3 , row4]
  where
    row0 , row1 , row2 , row3 , row4 :: Vec 5 (W 64)
    row0 = fromList [ lit 0x123456789abcdef , lit 0x123456789abcdf0
                    , lit 0x123456789abcdf1 , lit 0x123456789abcdf2
                    , lit 0x123456789abcdf3 ]
    row1 = fromList [ lit 0x123456789abcdf4 , lit 0x123456789abcdf5
                    , lit 0x123456789abcdf6 , lit 0x123456789abcdf7
                    , lit 0x123456789abcdf8 ]
    row2 = fromList [ lit 0x123456789abcdf9 , lit 0x123456789abcdfa
                    , lit 0x123456789abcdfb , lit 0x123456789abcdfc
                    , lit 0x123456789abcdfd ]
    row3 = fromList [ lit 0x123456789abcdfe , lit 0x123456789abcdff
                    , lit 0x123456789abce00 , lit 0x123456789abce01
                    , lit 0x123456789abce02 ]
    row4 = fromList [ lit 0x123456789abce03 , lit 0x123456789abce04
                    , lit 0x123456789abce05 , lit 0x123456789abce06
                    , lit 0x123456789abce07 ]

    -- row0 = fromList [ lit 0x00 , lit 0x01 , lit 0x02 , lit 0x03 , lit 0x04 ]
    -- row1 = fromList [ lit 0x05 , lit 0x06 , lit 0x07 , lit 0x08 , lit 0x09 ]
    -- row2 = fromList [ lit 0x0a , lit 0x0b , lit 0x0c , lit 0x0d , lit 0x0e ]
    -- row3 = fromList [ lit 0x0f , lit 0x10 , lit 0x11 , lit 0x12 , lit 0x13 ]
    -- row4 = fromList [ lit 0x14 , lit 0x15 , lit 0x16 , lit 0x17 , lit 0x18 ]
                              
                              

-- | These two are for pretty printing. 
-- vec :: Vec m (W n) -> Vec m W.W64
-- vec = RWV.map conv64
-- pr (x , y) = ( conv64 x , conv64 y)
conv64 :: W n -> W.W64
conv64 = W.conv64 0 . Data.Vector.Sized.toList

-- pretty :: A -> A
pretty :: A -> Vec 5 (Vec 5 W.W64)
pretty = RWV.map (RWV.map conv64)

rubar :: Vec 5 (W 64) -> IO ()
rubar v = putStrLn $
            show (conv64 (index v (finite 0))) Prelude.++ " " Prelude.++
            show (conv64 (index v (finite 1))) Prelude.++ " " Prelude.++ 
            show (conv64 (index v (finite 2))) Prelude.++ " " Prelude.++ 
            show (conv64 (index v (finite 3))) Prelude.++ " " Prelude.++ 
            show (conv64 (index v (finite 4))) Prelude.++ "\n" 


fubar :: A -> IO ()
fubar a = do
             rubar (index a (finite 0))
             rubar (index a (finite 1))
             rubar (index a (finite 2))
             rubar (index a (finite 3))
             rubar (index a (finite 4))
λ> fubar a1
0123456789abcdef 0123456789abcdf0 0123456789abcdf1 0123456789abcdf2 0123456789abcdf3
0123456789abcdf4 0123456789abcdf5 0123456789abcdf6 0123456789abcdf7 0123456789abcdf8
0123456789abcdf9 0123456789abcdfa 0123456789abcdfb 0123456789abcdfc 0123456789abcdfd
0123456789abcdfe 0123456789abcdff 0123456789abce00 0123456789abce01 0123456789abce02
0123456789abce03 0123456789abce04 0123456789abce05 0123456789abce06 0123456789abce07

Original state:
['0x123456789abcdef', '0x123456789abcdf0', '0x123456789abcdf1', '0x123456789abcdf2', '0x123456789abcdf3']
['0x123456789abcdf4', '0x123456789abcdf5', '0x123456789abcdf6', '0x123456789abcdf7', '0x123456789abcdf8']
['0x123456789abcdf9', '0x123456789abcdfa', '0x123456789abcdfb', '0x123456789abcdfc', '0x123456789abcdfd']
['0x123456789abcdfe', '0x123456789abcdff', '0x123456789abce00', '0x123456789abce01', '0x123456789abce02']
['0x123456789abce03', '0x123456789abce04', '0x123456789abce05', '0x123456789abce06', '0x123456789abce07']


λ> fubar (chi a1)
0123456789abcde6 0123456789abcdfa 0123456789abcdf8 0123456789abcdfa 0123456789abcdf6
0123456789abcdf2 0123456789abcdf0 0123456789abcff6 0123456789abcff6 0123456789abcffa
0123456789abcff8 0123456789abcffa 0123456789abcdfe 0123456789abcdfa 0123456789abcdf8
0123456789abcc12 0123456789abcc0f 0123456789abcff0 0123456789abcff1 0123456789abcff2
0123456789abce13 0123456789abce01 0123456789abce03 0123456789abce03 0123456789abce0f

State after Chi transformation:
['0x123456789abcde6', '0x123456789abcdfa', '0x123456789abcdf8', '0x123456789abcdfa', '0x123456789abcdf6']
['0x123456789abcdf2', '0x123456789abcdf0', '0x123456789abcff6', '0x123456789abcff6', '0x123456789abcffa']
['0x123456789abcff8', '0x123456789abcffa', '0x123456789abcdfe', '0x123456789abcdfa', '0x123456789abcdf8']
['0x123456789abcc12', '0x123456789abcc0f', '0x123456789abcff0', '0x123456789abcff1', '0x123456789abcff2']
['0x123456789abce13', '0x123456789abce01', '0x123456789abce03', '0x123456789abce03', '0x123456789abce0f']

-}
