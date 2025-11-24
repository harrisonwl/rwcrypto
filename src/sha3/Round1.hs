{-# LANGUAGE DataKinds #-}
module Sha3.Round1 where

import Prelude hiding (pi)
import ReWire

import Sha3.Rnd(rnd)
import Sha3.Layout (A)

round1 :: (Maybe (A, Finite 24)) ->
          ReacT (Maybe (A, Finite 24)) (Maybe A) Identity ()
round1 (Just (a , ir)) = signal (Just a') >>= round1
    where
      a' :: A
      a' = rnd ir a
round1 Nothing         = signal Nothing >>= round1

start :: ReacT (Maybe (A, Finite 24)) (Maybe A) Identity ()
start = round1 Nothing         
