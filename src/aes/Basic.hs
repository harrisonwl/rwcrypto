{-# LANGUAGE DataKinds #-}
module Aes.Basic( State
                , Column
                , RoundKey
                , Key
                , KeySchedule
                , lkup
                , update
                , (!=)
--                , (@@@)
                , toByte4
                , fromW32
                , toW32
                , roundkey
                , splitkey
                , splitkey128
                , splitkey192
                , joinkey
                , initState
                , finalState
                , transpose ) where

import Prelude hiding ((++))
import ReWire
import ReWire.Vectors hiding (update , (!=))
import ReWire.Finite as F
import ReWire.FiniteComp as FC

type State       = Vec 4 (Vec 4 (W 8))
type Column      = Vec 4 (W 8)
type RoundKey    = Vec 4 (Vec 4 (W 8))
type KeySchedule = Vec 60 (W 32)
type Key         = W 256

transpose :: State -> State
transpose s = generate $ \ i ->
              generate $ \ j -> s `index` j `index` i

-- |
-- | N.b., we represent the type of key as (8 x W 32) rather than (32 x W 8)
-- | It's way more sensible.
-- |

lkup :: Vec 4 (Vec 4 (W 8)) -> (Finite 4 , Finite 4) -> W 8
lkup s (i , j) = (s `index` i) `index` j

{-
lkup' :: Vec n (Vec m a) -> (Finite n, Finite m) -> a
lkup' s (i , j) = (s `index` i) `index` j
-}

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

fromW32 :: W 32 -> ( W 8 , W 8 , W 8 , W 8 )
fromW32 w32 = ( s0 , s1 , s2 , s3 )
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

splitkey :: W 256 -> Vec 8 (W 32)
splitkey key = fromList [s0 , s1 , s2 , s3 , s4 , s5 , s6 , s7]
  where
    s0 , s1 , s2 , s3 , s4 , s5 , s6 , s7 :: W 32
    s0 = slice (Proxy :: Proxy 0)   key
    s1 = slice (Proxy :: Proxy 32)  key
    s2 = slice (Proxy :: Proxy 64)  key
    s3 = slice (Proxy :: Proxy 96)  key
    s4 = slice (Proxy :: Proxy 128) key
    s5 = slice (Proxy :: Proxy 160) key
    s6 = slice (Proxy :: Proxy 192) key
    s7 = slice (Proxy :: Proxy 224) key

splitkey192 :: W 192 -> Vec 6 (W 32)
splitkey192 key = fromList [ s0 , s1 , s2 , s3 , s4 , s5 ]
  where
    s0 , s1 , s2 , s3 , s4 , s5 :: W 32
    s0 = slice (Proxy :: Proxy 0)   key
    s1 = slice (Proxy :: Proxy 32)  key
    s2 = slice (Proxy :: Proxy 64)  key
    s3 = slice (Proxy :: Proxy 96)  key
    s4 = slice (Proxy :: Proxy 128) key
    s5 = slice (Proxy :: Proxy 160) key

splitkey128 :: W 128 -> Vec 4 (W 32)
splitkey128 key = fromList [s0 , s1 , s2 , s3]
  where
    s0 , s1 , s2 , s3 :: W 32
    s0 = slice (Proxy :: Proxy 0)   key
    s1 = slice (Proxy :: Proxy 32)  key
    s2 = slice (Proxy :: Proxy 64)  key
    s3 = slice (Proxy :: Proxy 96)  key

joinkey :: Vec 8 (W 32) -> W 256
joinkey key = s0 ++ s1 ++ s2 ++ s3 ++ s4 ++ s5 ++ s6 ++ s7
  where
    s0 , s1 , s2 , s3 , s4 , s5 , s6 , s7 :: W 32
    s0 = key `index` (finite 0)
    s1 = key `index` (finite 1)
    s2 = key `index` (finite 2)
    s3 = key `index` (finite 3)
    s4 = key `index` (finite 4)
    s5 = key `index` (finite 5)
    s6 = key `index` (finite 6)
    s7 = key `index` (finite 7)
  

finalState :: State -> W 128 
finalState st  = out0  ++  out1 ++  out2 ++  out3 ++
                 out4  ++  out5 ++  out6 ++  out7 ++
                 out8  ++  out9 ++ out10 ++ out11 ++
                 out12 ++ out13 ++ out14 ++ out15
  where
    out0 , out1 , out2 , out3 , out4 , out5 , out6 , out7
        , out8 , out9 , out10 , out11 , out12 , out13 , out14 , out15 :: W 8
    out0  = st `lkup` (finite 0 , finite 0)
    out1  = st `lkup` (finite 1 , finite 0)
    out2  = st `lkup` (finite 2 , finite 0)
    out3  = st `lkup` (finite 3 , finite 0)
    out4  = st `lkup` (finite 0 , finite 1)
    out5  = st `lkup` (finite 1 , finite 1)
    out6  = st `lkup` (finite 2 , finite 1)
    out7  = st `lkup` (finite 3 , finite 1)
    out8  = st `lkup` (finite 0 , finite 2)
    out9  = st `lkup` (finite 1 , finite 2)
    out10 = st `lkup` (finite 2 , finite 2)
    out11 = st `lkup` (finite 3 , finite 2)
    out12 = st `lkup` (finite 0 , finite 3)
    out13 = st `lkup` (finite 1 , finite 3)
    out14 = st `lkup` (finite 2 , finite 3)
    out15 = st `lkup` (finite 3 , finite 3)
        
initState :: W 128 -> State
initState inp = fromList [ r0 , r1 , r2 , r3 ]
  where

    in0 , in1 , in2 , in3 , in4 , in5 , in6 , in7
        , in8 , in9 , in10 , in11 , in12 , in13 , in14 , in15 :: W 8
    in0  = slice (Proxy :: Proxy   0) inp
    in1  = slice (Proxy :: Proxy   8) inp
    in2  = slice (Proxy :: Proxy  16) inp
    in3  = slice (Proxy :: Proxy  24) inp
    in4  = slice (Proxy :: Proxy  32) inp
    in5  = slice (Proxy :: Proxy  40) inp
    in6  = slice (Proxy :: Proxy  48) inp
    in7  = slice (Proxy :: Proxy  56) inp
    in8  = slice (Proxy :: Proxy  64) inp
    in9  = slice (Proxy :: Proxy  72) inp
    in10 = slice (Proxy :: Proxy  80) inp
    in11 = slice (Proxy :: Proxy  88) inp
    in12 = slice (Proxy :: Proxy  96) inp
    in13 = slice (Proxy :: Proxy 104) inp
    in14 = slice (Proxy :: Proxy 112) inp
    in15 = slice (Proxy :: Proxy 120) inp

    r0 , r1 , r2 , r3 :: Vec 4 (W 8)

    r0 = fromList [ in0 , in4 ,  in8 , in12 ] 
    r1 = fromList [ in1 , in5 ,  in9 , in13 ] 
    r2 = fromList [ in2 , in6 , in10 , in14 ] 
    r3 = fromList [ in3 , in7 , in11 , in15 ]

-- | reads a RoundKey from the KeySchedule
-- | Extract a round key from the key schedule (AES-256)
-- | Each round key is 4 words (16 bytes) = Vec 4 (Vec 4 (W 8))
roundkey :: KeySchedule -> Finite 15 -> RoundKey
roundkey ks f15 = transpose $
                            fromList [ toByte4 (ks `index` i0)  
                                     , toByte4 (ks `index` i1) 
                                     , toByte4 (ks `index` i2)
                                     , toByte4 (ks `index` i3) ]
  where

    times4 :: Finite 15 -> Finite 60
    times4 f15 = (finite 4) FC.* (xfinite f15)

    i0 , i1 , i2 , i3 :: Finite 60
    i0 = times4 f15
    i1 = times4 f15 FC.+ finite 1
    i2 = times4 f15 FC.+ finite 2
    i3 = times4 f15 FC.+ finite 3

-- roundkey :: KeySchedule -> Finite 15 -> RoundKey
-- roundkey ks f15 = transpose $
--                             fromList [ toByte4 (ks `index` i0)  
--                                      , toByte4 (ks `index` i1) 
--                                      , toByte4 (ks `index` i2)
--                                      , toByte4 (ks `index` i3) ]
--   where

--     times4 :: Finite 15 -> Finite 60
--     times4 f15 = (finite 4) FC.* (xfinite f15)

--     i0 , i1 , i2 , i3 :: Finite 60
--     i0 = times4 f15
--     i1 = times4 f15 FC.+ finite 1
--     i2 = times4 f15 FC.+ finite 2
--     i3 = times4 f15 FC.+ finite 3



-- | Extract a round key from the key schedule (AES-256)
-- | Each round key is 4 words (16 bytes) = Vec 4 (Vec 4 (W 8))
-- | Kind of a hack; we need to add a
-- | primitive to do this:
xfinite :: Finite 15 -> Finite 60
xfinite i | i FC.== finite 0  = finite 0
          | i FC.== finite 1  = finite 1
          | i FC.== finite 2  = finite 2
          | i FC.== finite 3  = finite 3
          | i FC.== finite 4  = finite 4
          | i FC.== finite 5  = finite 5
          | i FC.== finite 6  = finite 6
          | i FC.== finite 7  = finite 7
          | i FC.== finite 8  = finite 8
          | i FC.== finite 9  = finite 9
          | i FC.== finite 10 = finite 10
          | i FC.== finite 11 = finite 11
          | i FC.== finite 12 = finite 12
          | i FC.== finite 13 = finite 13
          | otherwise         = finite 14

