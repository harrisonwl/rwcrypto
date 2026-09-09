{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Blake2b.RegisterFile where

-- |
-- | This reference semantics
-- | for Blake2b closely resembles the
-- | RFC 7693 pseudocode.
-- |

import Prelude hiding (take , drop , (^) , (+), (==), (&&) , (++) , (!!))
import ReWire hiding (error)
import ReWire.Bits
import ReWire.Finite
import qualified ReWire.FiniteComp as FC
import ReWire.Vectors hiding (update)

-----------------------------
-- Helpers and Idioms
-----------------------------

(!!) :: Vec n a -> Finite n -> a
v !! i = index v i

update :: KnownNat n => Vec n a -> Finite n -> a -> Vec n a
update v i x = generate (\ j -> if j FC.== i then x else index v j)

v_ , m_ :: Integer -> Finite 16
v_ = finite
m_ = finite

h_ :: Integer -> Finite 8
h_ = finite

ix :: KnownNat n => Integer -> Finite n
ix = finite

(@) :: KnownNat n => Vec n a -> Integer -> a
x @ i = x !! (finite i)

-----------------------------
-- Definitions
-----------------------------

type Storage s   = StateT s Identity

type Working       = Vec 16 (W 64) -- v[0..15]
type MessageBuffer = Vec 16 (W 64) -- m[0..15]
type HashState     = Vec 8 (W 64)  -- h[0..7]
type RegFile       = (Working , MessageBuffer , HashState)

readW :: StateT RegFile Identity Working
readW = do
  (v , _ , _) <- get
  return v

putW :: Working -> StateT RegFile Identity ()
putW v' = do
  (_ , m , h) <- get
  put (v' , m , h)

readWV :: Finite 16 -> StateT RegFile Identity (W 64)
readWV vi = do
  v <- readW
  return (v !! vi)

writeWV :: Finite 16 -> W 64 -> StateT RegFile Identity ()
writeWV vi w = do
  v <- readW
  putW (update v vi w)  

readM :: StateT RegFile Identity MessageBuffer
readM = do
  (_ , m , _) <- get
  return m

-- readMB :: Finite 16 -> StateT RegFile Identity (W 64)
-- readMB mi = do
--   m <- readM
--   return (m !! mi)

-- writeMB :: Finite 16 -> W 64 -> StateT RegFile Identity ()
-- writeMB mi w = do
--   m <- readM
--   putM (update m mi w)  

putM :: MessageBuffer -> StateT RegFile Identity ()
putM m' = do
  (v , _ , h) <- get
  put (v , m' , h)

readH :: StateT RegFile Identity HashState
readH = do
  (_ , _ , h) <- get
  return h

readHS :: Finite 8 -> Storage RegFile (W 64)
readHS hi = do
              hs <- readH
              return (hs !! hi)

writeHS :: Finite 8 -> W 64 -> StateT RegFile Identity ()
writeHS hi w = do
  h <- readH
  putH (update h hi w)  

putH :: HashState -> StateT RegFile Identity ()
putH h' = do
  (v , m , _) <- get
  put (v , m , h')
  
-- readReg :: Finite n -> StateT (Vec n b) Identity b
-- readReg v = do
--              rf <- get
--              return (index rf v)

-- setReg :: KnownNat n => Finite n -> w -> StateT (Vec n w) Identity ()
-- setReg r w = do
--                rf <- get
--                put (update rf r w)

(<==) :: Finite 16 -> StateT RegFile Identity (W 64) -> StateT RegFile Identity ()
w <== e = e >>= writeWV w

(<===) :: Finite 8 -> StateT RegFile Identity (W 64) -> StateT RegFile Identity ()
w <=== e = e >>= writeHS w

regfile0 :: RegFile
regfile0 = (generate $ \ r -> lit 0 , generate $ \ r -> lit 0 , generate $ \ r -> lit 0)

----------------------------------
-- 3.1.1. Mixing Function G     -- 
----------------------------------

-- | 2.1. G Rotation constants

_R1 , _R2 , _R3 , _R4 :: W 64
_R1 = lit 32
_R2 = lit 24
_R3 = lit 16
_R4 = lit 63

_G :: Finite 16 ->
      Finite 16 ->
      Finite 16 ->
      Finite 16 ->
      W 64      ->
      W 64      ->
     StateT RegFile Identity ()
_G a b c d x y    = do
                      a <== add3 a b x
                      d <== xorrotR d a _R1
                      c <== add c d
                      b <== xorrotR b c _R2
                      a <== add3 a b y
                      d <== xorrotR d a _R3
                      c <== add c d
                      b <== xorrotR b c _R4

   where

     add :: Finite 16 -> Finite 16 -> StateT RegFile Identity (W 64)
     add v1 v2 = do
       v <- readW
       let a = v !! v1
       let b = v !! v2
       return (a + b)

     add3 :: Finite 16 -> Finite 16 -> W 64 -> StateT RegFile Identity (W 64)
     add3 v1 v2 z = do
       a <- readWV v1
       b <- readWV v2
       return (a + b + z)

     xorrotR :: Finite 16 -> Finite 16 -> W 64 -> StateT RegFile Identity (W 64)
     xorrotR v1 v2 rc = do
                           va <- readWV v1
                           vb <- readWV v2
                           return (rotR rc (va ^ vb))

----------------------------------
-- 3.2. Compression Function F  -- 
----------------------------------

iv :: Vec 8 (W 64)
iv = fromList [iv0 , iv1 , iv2 , iv3 , iv4 , iv5 , iv6 , iv7]
  where
    iv0 , iv1 , iv2 , iv3 , iv4 , iv5 , iv6 , iv7 :: W 64
    iv0 = lit 0x6a09e667f3bcc908
    iv1 = lit 0xbb67ae8584caa73b
    iv2 = lit 0x3c6ef372fe94f82b
    iv3 = lit 0xa54ff53a5f1d36f1
    iv4 = lit 0x510e527fade682d1
    iv5 = lit 0x9b05688c2b3e6c1f
    iv6 = lit 0x1f83d9abfb41bd6b
    iv7 = lit 0x5be0cd19137e2179

type Permutation = Vec 16 (Finite 16)

sigma0 , sigma1, sigma2, sigma3, sigma4, sigma5, sigma6, sigma7, sigma8, sigma9 :: Permutation
sigma0 = fromList [ix 0 ,ix 1,ix 2,ix 3,ix 4,ix 5,ix 6,ix 7,ix 8,ix 9,ix 10,ix 11,ix 12,ix 13,ix 14,ix 15]
sigma1 = fromList [ix 14,ix 10,ix 4,ix 8,ix 9,ix 15,ix 13,ix 6,ix 1,ix 12,ix 0,ix 2,ix 11,ix 7,ix 5,ix 3]
sigma2 = fromList [ix 11,ix 8,ix 12,ix 0,ix 5,ix 2,ix 15,ix 13,ix 10,ix 14,ix 3,ix 6,ix 7,ix 1,ix 9,ix 4]
sigma3 = fromList [ix 7 ,ix 9,ix 3,ix 1,ix 13,ix 12,ix 11,ix 14,ix 2,ix 6,ix 5,ix 10,ix 4,ix 0,ix 15,ix 8]
sigma4 = fromList [ix 9 ,ix 0,ix 5,ix 7,ix 2,ix 4,ix 10,ix 15,ix 14,ix 1,ix 11,ix 12,ix 6,ix 8,ix 3,ix 13]
sigma5 = fromList [ix 2 ,ix 12,ix 6,ix 10,ix 0,ix 11,ix 8,ix 3,ix 4,ix 13,ix 7,ix 5,ix 15,ix 14,ix 1,ix 9]
sigma6 = fromList [ix 12,ix 5,ix 1,ix 15,ix 14,ix 13,ix 4,ix 10,ix 0,ix 7,ix 6,ix 3,ix 9,ix 2,ix 8,ix 11]
sigma7 = fromList [ix 13,ix 11,ix 7,ix 14,ix 12,ix 1,ix 3,ix 9,ix 5,ix 0,ix 15,ix 4,ix 8,ix 6,ix 2,ix 10]
sigma8 = fromList [ix 6 ,ix 15,ix 14,ix 9,ix 11,ix 3,ix 0,ix 8,ix 12,ix 2,ix 13,ix 7,ix 1,ix 4,ix 10,ix 5]
sigma9 = fromList [ix 10,ix 2,ix 8,ix 4,ix 7,ix 6,ix 1,ix 5,ix 15,ix 11,ix 9,ix 14,ix 3,ix 12,ix 13,ix 0]

permute :: KnownNat n1 => Vec n2 a -> Vec n1 (Finite n2) -> Vec n1 a
permute m p = generate $ \ i -> m !! (p !! i)

message_permutation :: Permutation -> StateT RegFile Identity ()
message_permutation sigma = do
  m <- readM
  let ms = permute m sigma
  _G (v_ 0) (v_ 4)  (v_ 8) (v_ 12) (ms @ 0) (ms @ 1)
  _G (v_ 1) (v_ 5)  (v_ 9) (v_ 13) (ms @ 2) (ms @ 3)
  _G (v_ 2) (v_ 6) (v_ 10) (v_ 14) (ms @ 4) (ms @ 5)
  _G (v_ 3) (v_ 7) (v_ 11) (v_ 15) (ms @ 6) (ms @ 7)

  _G (v_ 0) (v_ 5) (v_ 10) (v_ 15) (ms @ 8) (ms @ 9)
  _G (v_ 1) (v_ 6) (v_ 11) (v_ 12) (ms @ 10) (ms @ 11)
  _G (v_ 2) (v_ 7) (v_ 8)  (v_ 13) (ms @ 12) (ms @ 13)
  _G (v_ 3) (v_ 4) (v_ 9)  (v_ 14) (ms @ 14) (ms @ 15)

cryptographic_mixing :: Storage RegFile ()
cryptographic_mixing = do
                   message_permutation sigma0
                   message_permutation sigma1
                   message_permutation sigma2
                   message_permutation sigma3
                   message_permutation sigma4
                   message_permutation sigma5
                   message_permutation sigma6
                   message_permutation sigma7
                   message_permutation sigma8
                   message_permutation sigma9
                   message_permutation sigma0
                   message_permutation sigma1


--
-- There's a function, Data.Finite.weakenN, that turns a (Finite m) into
-- a (Finite (m + n)) which should be added to ReWire.FiniteComp.
-- Function weaken16 is just a hack to get what I need here. 
-- 
weaken16 :: Finite 8 -> Finite 16
weaken16 = back . there
  where
    there :: Finite 8 -> W 16
    there i =  fromFinite i
    back :: W 16 -> Finite 16
    back w3 = toFinite w3

_F :: W 128 -> Bit -> Storage RegFile ()
_F t f = do
           init_local_work_vector
           (v_ 12) <== do { w <- readWV (v_ 12) ; return $ w ^ lowword t }
           (v_ 13) <== do { w <- readWV (v_ 13) ; return $ w ^ highword t }
           if f then
                  (v_ 14) <== do { w <- readWV (v_ 14) ; return $ w ^ lit 0xffffffffffffffff }
                else
                  return ()
           cryptographic_mixing
           xor_two_halves

init_local_work_vector :: Storage RegFile ()
init_local_work_vector = do
                            (_ , m , h) <- get
                            put (h ++ iv , m , h)

xor_two_halves :: Storage RegFile ()
xor_two_halves = do
                    (v , m , h) <- get
                    let h' = generate (xor3 h v)
                    put (v , m , h')
   where
      xor3 :: HashState -> Working -> Finite 8 -> W 64
      xor3 h v i = (h !! i) ^ (v !! i') ^ (v !! (i' FC.+ finite 8))
        where
          i' :: Finite 16
          i' = weaken16 i

lowword :: W 128 -> W 64
lowword w = take w

highword :: W 128 -> W 64
highword w = drop w

-- type W64x8  = ( W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 )
-- type W64x16 = ( W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64
--               , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 )
type W64x8  = Vec 8 (W 64)
type W64x16 = Vec 16 (W 64)

initM :: W64x16 -> Storage RegFile ()
initM = putM

-- |
-- | This version is simplified assuming that dd==1 && kk==0
-- |

-- 
-- Number of bytes in a block. For Blake2b, it's 128
-- 
bb :: W 128
bb = lit 128

_BLAKE2b :: W 128 -> W 64 -> W 64 -> W64x16 -> Storage RegFile W64x8
_BLAKE2b ll kk nn d0 = do
                     initM d0           -- write into message vector
                     
                     putH iv            -- Initialization Vector.

                     -- Parameter block p[0]
                     (h_ 0) <=== do
                                   h0 <- readHS (h_ 0)
                                   return $ h0 ^ lit 0x01010000 ^ (kk <<. lit 8) ^ nn

                     if kk == lit 0
                       then
                         _F ll True
                       else
                         _F (ll + bb) True

                     readH
