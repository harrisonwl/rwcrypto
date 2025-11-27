{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Sha3.Theta (theta) where

import Prelude hiding ((^))

import ReWire
import ReWire.Bits hiding ((@@))
import ReWire.Finite
import ReWire.Vectors (index, generate)

import Sha3.Layout (A , C , D , (@@))

incF5 , decF5 :: Vec 5 (Finite 5)
incF5 = fromList [ finite 1 , finite 2 , finite 3 , finite 4 , finite 0 ]
decF5 = fromList [ finite 4 , finite 0 , finite 1 , finite 2 , finite 3 ]

incF64 :: Vec 64 (Finite 64)
incF64 = fromList [ finite 1 , finite 2 , finite 3 , finite 4 , finite 5 , finite 6
          , finite 7 , finite 8 , finite 9 , finite 10 , finite 11 , finite 12
          , finite 13 , finite 14 , finite 15 , finite 16 , finite 17 , finite 18
          , finite 19 , finite 20 , finite 21 , finite 22 , finite 23 , finite 24
          , finite 25 , finite 26 , finite 27 , finite 28 , finite 29 , finite 30
          , finite 31 , finite 32 , finite 33 , finite 34 , finite 35 , finite 36
          , finite 37 , finite 38 , finite 39 , finite 40 , finite 41 , finite 42
          , finite 43 , finite 44 , finite 45 , finite 46 , finite 47 , finite 48
          , finite 49 , finite 50 , finite 51 , finite 52 , finite 53 , finite 54
          , finite 55 , finite 56 , finite 57 , finite 58 , finite 59 , finite 60
          , finite 61 , finite 62 , finite 63 , finite 0]            

{-
ran :: [Finite 64]
ran = [ finite 1 , finite 2 , finite 3 , finite 4 , finite 5 , finite 6
          , finite 7 , finite 8 , finite 9 , finite 10 , finite 11 , finite 12
          , finite 13 , finite 14 , finite 15 , finite 16 , finite 17 , finite 18
          , finite 19 , finite 20 , finite 21 , finite 22 , finite 23 , finite 24
          , finite 25 , finite 26 , finite 27 , finite 28 , finite 29 , finite 30
          , finite 31 , finite 32 , finite 33 , finite 34 , finite 35 , finite 36
          , finite 37 , finite 38 , finite 39 , finite 40 , finite 41 , finite 42
          , finite 43 , finite 44 , finite 45 , finite 46 , finite 47 , finite 48
          , finite 49 , finite 50 , finite 51 , finite 52 , finite 53 , finite 54
          , finite 55 , finite 56 , finite 57 , finite 58 , finite 59 , finite 60
          , finite 61 , finite 62 , finite 63 , finite 0]            
-}

theta :: A -> A
theta a = let
             c = step1 a
             d = step2 c
          in
             generate (\ x -> let
                                 dx :: W 64
                                 dx = index d x
                              in
                                 generate $ \ y -> index (index a x) y ^ dx)

step1 :: A -> C
step1 a = generate $ act1 a
  where
    act1 :: A -> Finite 5 -> W 64
    act1 a x = let
                  ax0 = a @@ (x , finite 0)
                  ax1 = a @@ (x , finite 1)
                  ax2 = a @@ (x , finite 2)
                  ax3 = a @@ (x , finite 3)
                  ax4 = a @@ (x , finite 4)
               in
                  ax0 ^ ax1 ^ ax2 ^ ax3 ^ ax4

step2 :: C -> D 
step2 c = generate $ \ x -> let
                               x1 , x2 :: Finite 5
                               x1 = index decF5 x
                               x2 = index incF5 x
                               cdec , cinc :: W 64
                               cdec = index c x1
                               cinc = index c x2
                            in
                               operation2 cdec cinc
  where
     operation2 :: W 64 -> W 64 -> W 64
     operation2 w1 w2 = generate $ \ z -> index w1 z `xor` index w2 (index incF64 z)

{-
Kruft.

theta' :: A -> A
theta' a = fst3 (snd (runIdentity $ runStateT thetaM (a , c0 , d0)))
  where
    fst3 :: (a , b , c) -> a
    fst3 (x , _ , _) = x

    -- | theta should be independent of these initial values.
    c0 :: C
    c0 = fromList [ lit 0x0 , lit 0x0 , lit 0x0 , lit 0x0 , lit 0x0 ]
    d0 :: D
    d0 = fromList [ lit 0x00123456 , lit 0x00123456 , lit 0x00123456 , lit 0x00123456 , lit 0x00123456 ]

thetaM :: StateT (A , C , D) Identity ()
thetaM = do
           step1M
           step2M
           step3M

step1M :: StateT (A, C, D) Identity ()
step1M = do
           action1 (finite 0)
           action1 (finite 1)
           action1 (finite 2)
           action1 (finite 3)
           action1 (finite 4)
   where
     action1 :: Finite 5 -> StateT (A, C, D) Identity ()
     action1 x = do
                    ax0 <- readA x (finite 0)
                    ax1 <- readA x (finite 1)
                    ax2 <- readA x (finite 2)
                    ax3 <- readA x (finite 3)
                    ax4 <- readA x (finite 4)
                    writeC x (ax0 ^ ax1 ^ ax2 ^ ax3 ^ ax4)

step2M :: StateT (A, C, D) Identity ()
step2M = do
            c <- readC
            putD (dact c)
   where
     dact :: C -> D 
     dact c = generate $ \ x -> let
                                   x1 , x2 :: Finite 5
                                   x1 = index decF5 x
                                   x2 = index incF5 x
                                   cdec , cinc :: W 64
                                   cdec = index c x1
                                   cinc = index c x2
                                in
                                   operation2 cdec cinc
     operation2 :: W 64 -> W 64 -> W 64
     operation2 w1 w2 = generate $ \ z -> index w1 z `xor` index w2 (index incF64 z)

step3M :: StateT (A , C , D) Identity ()
step3M = do
           (a , c , d) <- get
           let a' = act3 d a
           put (a' , c , d)
  where           
    act3 :: D -> A -> A
    act3 d a = generate (\ x -> let
                                    dx :: W 64
                                    dx = index d x
                                in
                                    generate $ \ y -> index (index a x) y ^ dx)

-}
