{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Blake2b.Vectorized where

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

split :: Vec 40 (W 64) -> (Working , MessageBuffer , HashState)
split = \ rf -> (wv rf , mb rf , hs rf)
  where

    wv :: Vec 40 (W 64) -> Vec 16 (W 64)
    wv = take

    mb :: Vec 40 (W 64) -> Vec 16 (W 64)
    mb = slice (Proxy :: Proxy 16)

    hs :: Vec 40 (W 64) -> Vec 8 (W 64)
    hs = drop

-- quarter :: Vec 16 (W 64) -> (Vec 4 (W 64) , Vec 4 (W 64) , Vec 4 (W 64) , Vec 4 (W 64) )
-- quarter w = (nibble0 w , nibble1 w , nibble2 w , nibble4 w)
quarter :: Vec 16 (W 64) -> (Vec 4 (W 64) , Vec 4 (W 64) , Vec 4 (W 64) , Vec 4 (W 64) )
quarter w = (nibble0 w , nibble1 w , nibble2 w , nibble3 w)

  where

    nibble0 , nibble1 , nibble2 , nibble3 :: Vec 16 (W 64) -> Vec 4 (W 64)
    nibble0 w = take w
    nibble1 w = slice (Proxy :: Proxy 4) w
    nibble2 w = slice (Proxy :: Proxy 8) w
    nibble3 w = drop w

getRF :: StateT (Vec 40 (W 64)) Identity (Working , MessageBuffer , HashState)
getRF = get >>= return . split

-- setW :: Vec 16 (W 64) -> StateT (Vec 40 (W 64)) Identity ()
-- setW v = do
--            (_ , m , h) <- getRF
--            put (v ++ m ++ h)

-- setM :: Vec 16 (W 64) -> StateT (Vec 40 (W 64)) Identity ()
-- setM m = do
--            (v , _ , h) <- getRF
--            put (v ++ m ++ h)

-- setH :: Vec 8 (W 64) -> StateT (Vec 40 (W 64)) Identity ()
-- setH h = do
--            (v , m , _) <- getRF
--            put (v ++ m ++ h)

-----------------------------
-- Definitions
-----------------------------

type Storage s   = StateT s Identity

type Working       = Vec 16 (W 64) -- v[0..15]
type MessageBuffer = Vec 16 (W 64) -- m[0..15]
type HashState     = Vec 8 (W 64)  -- h[0..7]
type RegFile       = Vec 40 (W 64)

readW :: Storage RegFile Working
readW = do
  (v , _ , _) <- getRF
  return v

putW :: Working -> Storage RegFile ()
putW v' = do
  (_ , m , h) <- getRF
  put (v' ++ m ++ h)

readWV :: Finite 16 -> Storage RegFile (W 64)
readWV vi = do
  v <- readW
  return (v !! vi)

writeWV :: Finite 16 -> W 64 -> Storage RegFile ()
writeWV vi w = do
  v <- readW
  putW (update v vi w)  

readM :: Storage RegFile MessageBuffer
readM = do
  (_ , m , _) <- getRF
  return m

-- readMB :: Finite 16 -> StateT RegFile Identity (W 64)
-- readMB mi = do
--   m <- readM
--   return (m !! mi)

-- writeMB :: Finite 16 -> W 64 -> StateT RegFile Identity ()
-- writeMB mi w = do
--   m <- readM
--   putM (update m mi w)  

-- putM :: MessageBuffer -> StateT RegFile Identity ()
-- putM m' = do
--   (v , _ , h) <- get
--   put (v , m' , h)

-- readH :: StateT RegFile Identity HashState
-- readH = do
--   (_ , _ , h) <- get
--   return h

putH :: HashState -> Storage RegFile ()
putH h' = do
  (v , m , _) <- getRF
  put (v ++ m ++ h')
  
-- readReg :: Finite n -> StateT (Vec n b) Identity b
-- readReg v = do
--              rf <- get
--              return (index rf v)

-- setReg :: KnownNat n => Finite n -> w -> StateT (Vec n w) Identity ()
-- setReg r w = do
--                rf <- get
--                put (update rf r w)

(<==) :: Finite 16 -> Storage RegFile (W 64) -> Storage RegFile ()
w <== e = e >>= writeWV w

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
                            (_ , m , h) <- getRF
                            putW (h ++ iv)

xor_two_halves :: Storage RegFile ()
xor_two_halves = do
                    (v , m , h) <- getRF
                    let h' = generate (xor3 h v)
                    putH h'
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
