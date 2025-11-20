{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Sha3.Theta (theta) where

import Prelude hiding ((^))

import ReWire hiding (ReacT,Identity,signal,lift, get, put, StateT)
import ReWire.Bits
import ReWire.Finite
import ReWire.Vectors (index, generate)

import Control.Monad.Identity
import Control.Monad.State
-- ^^ ReWire doesn't define runStateT. It can and should, but doesn't.

import Sha3.Layout (A , C , D , readA , readC, writeC , putD)

theta :: A -> A
theta a = fst3 (snd (runIdentity $ runStateT thetaM (a , c0 , d0)))
  where
    fst3 :: (a , b , c) -> a
    fst3 (x , _ , _) = x

    -- | theta should be independent of these initial values.
    c0 :: C
    c0 = fromList [ lit 0x0 , lit 0x0 , lit 0x0 , lit 0x0 , lit 0x0 ]
    d0 :: D
    d0 = fromList [ lit 0x00123456 , lit 0x00123456 , lit 0x00123456 , lit 0x00123456 , lit 0x00123456 ]

incF5 , decF5 :: Vec 5 (Finite 5)
incF5 = fromList [ finite 1 , finite 2 , finite 3 , finite 4 , finite 0 ]
decF5 = fromList [ finite 4 , finite 0 , finite 1 , finite 2 , finite 3 ]

incF64 :: Vec 64 (Finite 64)
incF64 = fromList (Prelude.map finite ([1..63] Prelude.++ [0]))

step1 :: StateT (A, C, D) Identity ()
step1 = do
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

step2 :: StateT (A, C, D) Identity ()
step2 = do
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


step3 :: StateT (A , C , D) Identity ()
step3 = do
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

thetaM :: StateT (A , C , D) Identity ()
thetaM = do
           step1
           step2
           step3
