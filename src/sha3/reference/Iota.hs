{-# LANGUAGE DataKinds #-}
module Reference.Iota (iota, rc) where

import Prelude hiding ((^) , (++))
import ReWire
import ReWire.Bits
import ReWire.Finite
import ReWire.Vectors    

import Reference.Layout (A , wrArr, rdArr)

-- | These are the rounding constants (semi-)specified in
-- | Algorithms 5, 6, and 7, pages 16-17, of FIPS 202.
-- | The file RoundConstants.hs demonstrates the intended
-- | computation.
round_constants :: Vec 24 (W 64)
round_constants = fromList 
        [ lit 0x0000000000000001, lit 0x0000000000008082, lit 0x800000000000808A
        , lit 0x8000000080008000, lit 0x000000000000808B, lit 0x0000000080000001
        , lit 0x8000000080008081, lit 0x8000000000008009, lit 0x000000000000008A
        , lit 0x0000000000000088, lit 0x0000000080008009, lit 0x000000008000000A
        , lit 0x000000008000808B, lit 0x800000000000008B, lit 0x8000000000008089
        , lit 0x8000000000008003, lit 0x8000000000008002, lit 0x8000000000000080
        , lit 0x000000000000800A, lit 0x800000008000000A, lit 0x8000000080008081
        , lit 0x8000000000008080, lit 0x0000000080000001, lit 0x8000000080008008 ]

-- |
-- | This implements Algorithm 5, page 16, of NIST FIPS 202.
-- |
rc :: W 8 -> Bit
rc t8 = rc_constant `index` (toFinite t8)
   where
     rc_constant :: W 256
     rc_constant = h1 ++ h2
       where
         h1 :: W 128
         h1 = lit 0x80b1e87f90a7d57062b32fde6ee54a25
         h2 :: W 128
         h2 = lit 0xa339e361175edf0d35b504ec9303a471

-- | ^^^^^
-- | Snarfed from 321-31 of Specification.cry
-- | here:
-- https://github.com/GaloisInc/cryptol-specs/blob/master/Primitive/Keyless/Hash/SHA3/Specification.cry
-- |
    -- rc_hardcoded : Integer -> Bit
    -- rc_hardcoded t = constants @ (t') where
    --     constants = join [
    --         0x80b1e87f90a7d57062b32fde6ee54a25,
    --         0xa339e361175edf0d35b504ec9303a471
    --     ]
    --     // Cryptol's SMT interface disallows index by symbolic `Integer`.
    --     // To faciliate proofs, we convert to a bitvector.
    --     // Justification: for all integers i,
    --     // (i % 255) % 256 == (i % 255)
    --     t' = fromInteger (t % 255) : [8]

_RC :: W 64
_RC = lit 0

-- | This encodes lines 4 and 5 of Algorithm 6, page 16, FIPS 202
iota :: Finite 24 -> A -> A
iota ir a = wrArr a (finite 0 , finite 0) (lane00 ^ rc)
  where
    lane00 , rc :: W 64
    lane00 = rdArr a (finite 0 , finite 0)
    rc     = index round_constants ir
