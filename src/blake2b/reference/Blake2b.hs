{-# LANGUAGE DataKinds #-}
module Reference.Blake2b where

-- |
-- | This reference semantics
-- | for Blake2b closely resembles the
-- | RFC 7693 pseudocode.
-- |

import Prelude hiding (take , drop , (^) , (+), (==), (&&))
import ReWire hiding (error)
import ReWire.Bits
import ReWire.Finite
import qualified ReWire.FiniteComp as FC
import ReWire.Vectors hiding (update)

-----------------------------
-- Definitions and helpers
-----------------------------

update :: KnownNat n => Vec n a -> Finite n -> a -> Vec n a
update v i x = generate (\ j -> if j FC.== i then x else index v j)

type Storage s   = StateT s Identity

type RegFile = Vec 40 (W 64)
type Reg     = Finite 40

v0 , v1 ,  v2 ,  v3 ,  v4 ,  v5 ,  v6 ,  v7 
   , v8 , v9 , v10 , v11 , v12 , v13 , v14 , v15 
   , m0 , m1 ,  m2 ,  m3 ,  m4 ,  m5 ,  m6 ,  m7
   , m8 , m9 , m10 , m11 , m12 , m13 , m14 , m15 
   , h0 , h1 ,  h2 ,  h3 ,  h4 ,  h5 ,  h6 ,  h7 :: Reg
v0  = finite 0 -- working vectors v[0..15]
v1  = finite 1
v2  = finite 2
v3  = finite 3
v4  = finite 4
v5  = finite 5
v6  = finite 6
v7  = finite 7
v8  = finite 8
v9  = finite 9         
v10 = finite 10         
v11 = finite 11          
v12 = finite 12          
v13 = finite 13          
v14 = finite 14          
v15 = finite 15          
m0  = finite 16 -- message buffer  m[0..15]
m1  = finite 17
m2  = finite 18
m3  = finite 19
m4  = finite 20
m5  = finite 21
m6  = finite 22
m7  = finite 23
m8  = finite 24
m9  = finite 25
m10 = finite 26
m11 = finite 27
m12 = finite 28
m13 = finite 29
m14 = finite 30
m15 = finite 31
h0  = finite 32 -- hash state h[0..7]
h1  = finite 33
h2  = finite 34
h3  = finite 35
h4  = finite 36
h5  = finite 37
h6  = finite 38
h7  = finite 39

{-# INLINE readReg #-}
readReg :: Reg -> Storage RegFile (W 64)
readReg v = do
             rf <- get
             return (index rf v)

{-# INLINE setReg #-}
setReg :: Reg -> W 64 -> Storage RegFile ()
setReg r w = do
               rf <- get
               put (update rf r w)

{-# INLINE (<==) #-}
(<==) :: Reg -> Storage RegFile (W 64) -> Storage RegFile ()
w <== e = e >>= setReg w

regfile0 :: RegFile
regfile0 = generate $ \ r -> lit 0

----------------------------------
-- 3.1.1. Mixing Function G     -- 
----------------------------------

-- | 2.1. G Rotation constants
_R1 , _R2 , _R3 , _R4 :: W 64
_R1 = lit 32
_R2 = lit 24
_R3 = lit 16
_R4 = lit 63

_G :: Reg -> Reg -> Reg -> Reg -> Reg -> Reg -> Storage RegFile ()
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
     add :: Reg -> Reg -> Storage RegFile (W 64)
     add v1 v2    = do
                      a <- readReg v1
                      b <- readReg v2
                      return (a + b)
     -- |
     -- | Need a theorem: forall w :: W 64, w `mod` (2 ^^ 64) == w.
     -- |
     add3 :: Reg -> Reg -> Reg -> Storage RegFile (W 64)
     add3 v1 v2 z = do
                      a <- readReg v1
                      b <- readReg v2
                      x <- readReg z
                      return (a + b + x)

     xorrotR :: Reg -> Reg -> W 64 -> Storage RegFile (W 64)
     xorrotR v1 v2 rc = do
                           va <- readReg v1
                           vb <- readReg v2
                           return (rotR rc (va ^ vb))


----------------------------------
-- 3.2. Compression Function G  -- 
----------------------------------

iv0 , iv1 , iv2 , iv3 , iv4 , iv5 , iv6 , iv7 :: W 64
iv0 = lit 0x6a09e667f3bcc908
iv1 = lit 0xbb67ae8584caa73b
iv2 = lit 0x3c6ef372fe94f82b
iv3 = lit 0xa54ff53a5f1d36f1
iv4 = lit 0x510e527fade682d1
iv5 = lit 0x9b05688c2b3e6c1f
iv6 = lit 0x1f83d9abfb41bd6b
iv7 = lit 0x5be0cd19137e2179
                     
-- Reg16 are permutations. I changed the name to make it conform to the
-- RFC7693 text.
-- 
type Reg16 = (Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg)

sigma0 , sigma1 , sigma2 , sigma3 , sigma4 , sigma5 , sigma6 , sigma7 , sigma8 , sigma9 :: Reg16

sigma0 = (m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)
sigma1 = (m14,m10,m4,m8,m9,m15,m13,m6,m1,m12,m0,m2,m11,m7,m5,m3)
sigma2 = (m11,m8,m12,m0,m5,m2,m15,m13,m10,m14,m3,m6,m7,m1,m9,m4)
sigma3 = (m7,m9,m3,m1,m13,m12,m11,m14,m2,m6,m5,m10,m4,m0,m15,m8)
sigma4 = (m9,m0,m5,m7,m2,m4,m10,m15,m14,m1,m11,m12,m6,m8,m3,m13)
sigma5 = (m2,m12,m6,m10,m0,m11,m8,m3,m4,m13,m7,m5,m15,m14,m1,m9)
sigma6 = (m12,m5,m1,m15,m14,m13,m4,m10,m0,m7,m6,m3,m9,m2,m8,m11)
sigma7 = (m13,m11,m7,m14,m12,m1,m3,m9,m5,m0,m15,m4,m8,m6,m2,m10)
sigma8 = (m6,m15,m14,m9,m11,m3,m0,m8,m12,m2,m13,m7,m1,m4,m10,m5)
sigma9 = (m10,m2,m8,m4,m7,m6,m1,m5,m15,m11,m9,m14,m3,m12,m13,m0)

-- |
-- | The body of the FOR loop in definition of F.
-- |
message_permutation :: Reg16 -> Storage RegFile ()
message_permutation (ms0,ms1,ms2,ms3,ms4,ms5,ms6,ms7,ms8,ms9,ms10,ms11,ms12,ms13,ms14,ms15) = 
      do
        _G v0 v4  v8 v12 ms0 ms1
        _G v1 v5  v9 v13 ms2 ms3
        _G v2 v6 v10 v14 ms4 ms5
        _G v3 v7 v11 v15 ms6 ms7

        _G v0 v5 v10 v15 ms8 ms9
        _G v1 v6 v11 v12 ms10 ms11
        _G v2 v7 v8  v13 ms12 ms13
        _G v3 v4 v9  v14 ms14 ms15

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

_F :: W 128 -> Bit -> Storage RegFile ()
_F t f = do
           init_local_work_vector
           v12 <== do { w <- readReg v12 ; return $ w ^ lowword t }
           v13 <== do { w <- readReg v13 ; return $ w ^ highword t }
           if f then
                  v14 <== do { w <- readReg v14 ; return $ w ^ lit 0xffffffffffffffff }
                else
                  return ()
           cryptographic_mixing
           xor_two_halves

init_local_work_vector :: Storage RegFile ()
init_local_work_vector = do
                     v0  <== readReg h0
                     v1  <== readReg h1
                     v2  <== readReg h2
                     v3  <== readReg h3
                     v4  <== readReg h4
                     v5  <== readReg h5
                     v6  <== readReg h6
                     v7  <== readReg h7
                     v8  <== return iv0
                     v9  <== return iv1
                     v10 <== return iv2
                     v11 <== return iv3
                     v12 <== return iv4
                     v13 <== return iv5
                     v14 <== return iv6
                     v15 <== return iv7

xor_two_halves :: Storage RegFile ()
xor_two_halves = do
                   h0 <== xor3 h0 v0 v8
                   h1 <== xor3 h1 v1 v9
                   h2 <== xor3 h2 v2 v10
                   h3 <== xor3 h3 v3 v11
                   h4 <== xor3 h4 v4 v12
                   h5 <== xor3 h5 v5 v13
                   h6 <== xor3 h6 v6 v14
                   h7 <== xor3 h7 v7 v15
                     where
                       xor3 :: Reg -> Reg -> Reg -> Storage RegFile (W 64)
                       xor3 r1 r2 r3 = do
                         w1 <- readReg r1
                         w2 <- readReg r2
                         w3 <- readReg r3
                         return $ w1 ^ w2 ^ w3
    
lowword :: W 128 -> W 64
lowword w = take w

highword :: W 128 -> W 64
highword w = drop w

type W64x8  = ( W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 )
type W64x16 = ( W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64
              , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 )

initM :: W64x16 -> Storage RegFile ()
initM (w0 , w1 , w2 , w3 , w4 , w5 , w6 , w7 , w8 , w9 , w10 , w11 , w12 , w13 , w14 , w15 ) = do
                         setReg m0 w0
                         setReg m1 w1
                         setReg m2 w2
                         setReg m3 w3
                         setReg m4 w4
                         setReg m5 w5
                         setReg m6 w6
                         setReg m7 w7
                         setReg m8 w8
                         setReg m9 w9
                         setReg m10 w10
                         setReg m11 w11
                         setReg m12 w12
                         setReg m13 w13
                         setReg m14 w14
                         setReg m15 w15


readH :: Storage RegFile W64x8
readH = do
  h0 <- readReg h0
  h1 <- readReg h1
  h2 <- readReg h2
  h3 <- readReg h3
  h4 <- readReg h4
  h5 <- readReg h5
  h6 <- readReg h6
  h7 <- readReg h7
  return (h0 , h1 , h2 , h3 , h4 , h5 , h6 , h7 )
  
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
                     
                     h0 <== return iv0  -- Initialization Vector.
                     h1 <== return iv1
                     h2 <== return iv2
                     h3 <== return iv3
                     h4 <== return iv4
                     h5 <== return iv5
                     h6 <== return iv6
                     h7 <== return iv7

                     -- Parameter block p[0]
                     h0 <== do
                              h0 <- readReg h0
                              return $ h0 ^ lit 0x01010000 ^ (kk <<. lit 8) ^ nn

                     if kk == lit 0
                       then
                         _F ll True
                       else
                         _F (ll + bb) True

                     readH
