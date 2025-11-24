{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Sha3.Rho(rho) where

import ReWire
import ReWire.Bits           as B
import ReWire.Vectors        as RWV

import Sha3.Layout (A)

-- | I'm not sure these constants are correct.
-- | This table is Table 2, page 13, modulo 64.
rotationConstants :: Vec 5 (Vec 5 (W 64))
rotationConstants = fromList [row0 , row1 , row2 , row3 , row4]
  where
    row0 , row1 , row2 , row3 , row4 :: Vec 5 (W 64)
    row0 = fromList [ lit 0  , lit 36 , lit 3  , lit 41 , lit 18 ]
    row1 = fromList [ lit 1  , lit 44 , lit 10 , lit 45 , lit 2  ]
    row2 = fromList [ lit 62 , lit 6  , lit 43 , lit 15 , lit 61 ]
    row3 = fromList [ lit 28 , lit 55 , lit 25 , lit 21 , lit 56 ]
    row4 = fromList [ lit 27 , lit 20 , lit 39 , lit 8  , lit 14 ]

-- | D'Oh!!!
-- | Got bit *again* by the order of operations in rotR and rotL.
-- foo :: W 64
-- foo =  (lit 3) `rR` (lit 0x94eeea8b1f2ada84)
-- rR :: KnownNat m => W m -> W m -> W m
-- rR n w = (w >>. n) .|. (w <<. (lit (len w) ReWire.Bits.- n))

rho :: A -> A
rho a = generate $ \ x ->
        generate $ \ y -> let
                              axy = index (index a x) y
                              rxy = index (index rotationConstants x) y
                          in
                            ((rxy `rotL` axy) .|. ((lit 64 B.- rxy) `rotR` axy)) .&. (lit 0xFFFFFFFFFFFFFFFF)


-- # Rotation offsets (precomputed for Keccak's 5x5 state matrix)
-- RHO_OFFSETS = np.array([
--     [  0,  36,   3,  41,  18],
--     [  1,  44,  10,  45,   2],
--     [ 62,   6,  43,  15,  61],
--     [ 28,  55,  25,  21,  56],
--     [ 27,  20,  39,   8,  14]
-- ])

-- def rho(A):
--     """Apply the Rho step to the Keccak state."""
--     for x in range(5):
--         for y in range(5):
--             A[x, y] = ((A[x, y] << RHO_OFFSETS[x, y]) | (A[x, y] >> (64 - RHO_OFFSETS[x, y]))) & 0xFFFFFFFFFFFFFFFF
--     return A

-- action :: A -> Finite 5 -> Finite 5 -> W 64
-- action a x y = ((axy `rotL` rxy) .|. (axy `rotR` (lit 64 B.- rxy))) .&. (lit 0xFFFFFFFFFFFFFFFF)
--   where
--     axy = index (index a x) y
--     rxy = index (index rotationConstants x) y
