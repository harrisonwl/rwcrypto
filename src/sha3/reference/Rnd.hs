{-# LANGUAGE DataKinds #-}
module Reference.Rnd (rnd) where

import Prelude hiding ((^) , (++) , pi)
import ReWire

import Reference.Layout (A, putA)

import Reference.Theta(theta)
import Reference.Rho(rho)
import Reference.Iota(iota)
import Reference.Pi(pi)
import Reference.Chi(chi)

rnd :: Finite 24 -> A -> A
-- rnd ir a = pi a
-- rnd ir a = iota ir (rho (theta a))
rnd ir a = iota ir (chi (pi (rho (theta a))))

{-
rndM ir a = do
               putA a
               thetaM
               mod
-}
