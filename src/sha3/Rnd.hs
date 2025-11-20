{-# LANGUAGE DataKinds #-}
module Sha3.Rnd (rnd) where

import Prelude hiding ((^) , (++) , pi)
import ReWire
-- import ReWire.Bits
--import ReWire.Finite
-- import ReWire.Vectors    

import Sha3.Layout (A)

import Sha3.Theta(theta)
import Sha3.Rho(rho)
import Sha3.Iota(iota)
import Sha3.Pi(pi)
import Sha3.Chi(chi)

rnd :: Finite 24 -> A -> A
rnd ir a = iota ir (chi (pi (rho (theta a))))
