{-# LANGUAGE DataKinds #-}
module Aes.Basic(State
                , Column
                , RoundKey
                , lkup
                , update
                , (!=)
                , (@@@)
                , toByte4
                , toW32 ) where

import Prelude hiding ((++))
import ReWire
import ReWire.Vectors hiding (update , (!=))
import ReWire.Finite as F
import ReWire.FiniteComp as FC

type State    = Vec 4 (Vec 4 (W 8))
type Column   = Vec 4 (W 8)
type RoundKey = Vec 4 (Vec 4 (W 8))

lkup :: Vec 4 (Vec 4 (W 8)) -> (Finite 4 , Finite 4) -> W 8
lkup s (i , j) = (s `index` i) `index` j

(@@@) :: (KnownNat n) => Vec n a -> W m -> a
w @@@ i = w `index` (F.toFinite i)


-- |
-- | This is factor function tweeked so that it takes (W 64) as input instead of Integer. 
-- |

toByte4 :: W 32 -> Vec 4 (W 8)
toByte4 w32 = fromList [s0 , s1 , s2 , s3]
  where
    s0 , s1 , s2 , s3 :: W 8
    s0 = slice (Proxy :: Proxy 0)  w32
    s1 = slice (Proxy :: Proxy 8)  w32
    s2 = slice (Proxy :: Proxy 16) w32
    s3 = slice (Proxy :: Proxy 24) w32

toW32 :: Vec 4 (W 8) -> W 32
toW32 v4 = x0 ++ x1 ++ x2 ++ x3
   where
     x0 , x1 , x2 , x3 :: W 8
     x0 = v4 `index` finite 0
     x1 = v4 `index` finite 1
     x2 = v4 `index` finite 2
     x3 = v4 `index` finite 3

  
-- |
-- | There was a bug (update wasn't defined properly in ReWire.Vectors) in previous
-- | version of ReWire. This bug is fixed in most recent release, which I haven't
-- | installed yet.
-- |
update :: KnownNat n => Vec n a -> Finite n -> a -> Vec n a
update v i x = generate (\ j -> if j FC.== i then x else index v j)

-- | assign new value a to index i
(!=) :: KnownNat n => Vec n a -> Finite n -> a -> Vec n a
v != i = update v i

infixr 2  !=


-- type SBox   = Vec 0x10 (Vec 0x10 (W 8))
-- type Index  = Finite 0x10

