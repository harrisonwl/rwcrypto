{-# LANGUAGE DataKinds #-}
module Sha3.Pi(pi) where

import Prelude hiding (pi)
import ReWire hiding (StateT , put , get , Identity)
import ReWire.Bits (lit)
import ReWire.FiniteComp     as FC
import ReWire.Vectors        as RWV

import Control.Monad.Identity
import Control.Monad.State
-- ^^ ReWire doesn't define runStateT. It can and should, but doesn't.

import Sha3.Layout (A, C, D)

action :: A -> A
action a = generate $ \ x ->
             generate $ \ y ->
                         index (index a (access x y)) x
   where
     access :: Finite 5 -> Finite 5 -> Finite 5
     access x y = x FC.+ (3 FC.* y)

piM :: StateT (A , C , D) Identity ()
piM = do
         (a , c , d) <- get
         put (action a , c , d)

pi :: A -> A
pi a = fst3 (snd (runIdentity $ runStateT piM (a , c0 , d0)))
  where
    fst3 :: (a , b , c) -> a
    fst3 (x , _ , _) = x

    -- | theta should be independent of these initial values.
    c0 :: C
    c0 = fromList [ lit 0x0 , lit 0x0 , lit 0x0 , lit 0x0 , lit 0x0 ]
    d0 :: D
    d0 = fromList [ lit 0x00123456 , lit 0x00123456 , lit 0x00123456 , lit 0x00123456 , lit 0x00123456 ]


{-
Original state (before Pi):
[['0' '1' '2' '3' '4']
 ['5' '6' '7' '8' '9']
 ['A' 'B' 'C' 'D' 'E']
 ['F' '10' '11' '12' '13']
 ['14' '15' '16' '17' '18']]

State after Pi transformation:
[['0' 'F' '5' '14' 'A']
 ['6' '15' 'B' '1' '10']
 ['C' '2' '11' '7' '16']
 ['12' '8' '17' 'D' '3']
 ['18' 'E' '4' '13' '9']]

λ> go pi a1
0000 000f 0005 0014 000a
0006 0015 000b 0001 0010
000c 0002 0011 0007 0016
0012 0008 0017 000d 0003
0018 000e 0004 0013 0009

--- Testing junk below

c0 :: C
c0 = fromList [ lit 0x0 , lit 0x0 , lit 0x0 , lit 0x0 , lit 0x0 ]

d0 , d1 :: D
d0 = fromList [ lit 0x00123456 , lit 0x00123456 , lit 0x00123456 , lit 0x00123456 , lit 0x00123456 ]
d1 = fromList [ lit 0x14 , lit 0x15 , lit 0x16 , lit 0x17 , lit 0x18 ]

a1 :: A
a1 = fromList [row0 , row1 , row2 , row3 , row4]
  where
    row0 , row1 , row2 , row3 , row4 :: Vec 5 (W 64)
    row0 = fromList [ lit 0x00 , lit 0x01 , lit 0x02 , lit 0x03 , lit 0x04 ]
    row1 = fromList [ lit 0x05 , lit 0x06 , lit 0x07 , lit 0x08 , lit 0x09 ]
    row2 = fromList [ lit 0x0a , lit 0x0b , lit 0x0c , lit 0x0d , lit 0x0e ]
    row3 = fromList [ lit 0x0f , lit 0x10 , lit 0x11 , lit 0x12 , lit 0x13 ]
    row4 = fromList [ lit 0x14 , lit 0x15 , lit 0x16 , lit 0x17 , lit 0x18 ]
        
go :: StateT (A , C , D) Identity () -> A -> IO ()
go x a = fubar $ fst3 (snd (runIdentity $ runStateT x (a , c0 , d0)))
  where
    fst3 :: (a , b , c) -> a
    fst3 (x , _ , _) = x

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

conv64 :: W n -> W.W64
conv64 = W.conv64 0 . Data.Vector.Sized.toList

-}
