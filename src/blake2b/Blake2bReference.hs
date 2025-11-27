{-# LANGUAGE DataKinds #-}
module Blake2b.Blake2bReference where

-- (
--   Reg(..) ,
--   RegFile(..) ,
--   trReg ,
--   readReg , setReg , (<==) , regfile0 ,
--   Storage , Inp(..) ,
--   _R1 , _R2 , _R3 , _R4 , _G ,
--   iv0 , iv1 , iv2 , iv3 , iv4 , iv5 , iv6 , iv7 ,
--   Reg16 ,
--   sigma0 , sigma1 , sigma2 , sigma3 , sigma4 ,
--   sigma5 , sigma6 , sigma7 , sigma8 , sigma9 ,
--   message_permutation , cryptographic_mixing ,
--   W64x8 , W64x16 , initM , readH
--                         ) where

-- |
-- | This reference semantics
-- | for Blake2b closely resembles the
-- | RFC 7693 pseudocode.
-- |

import Prelude hiding (take , drop , (^) , (+), (==), (&&))
import ReWire hiding (error)
import ReWire.Bits
import ReWire.Vectors

-----------------------------
-- Definitions and helpers
-----------------------------

data Reg  = V0 | V1 |  V2 |  V3 |  V4 |  V5 |  V6 |  V7 -- working vectors v[0..15]
          | V8 | V9 | V10 | V11 | V12 | V13 | V14 | V15
          | M0 | M1 |  M2 |  M3 |  M4 |  M5 |  M6 |  M7 -- message buffer  m[0..15]
          | M8 | M9 | M10 | M11 | M12 | M13 | M14 | M15
          | H0 | H1 |  H2 |  H3 |  H4 |  H5 |  H6 |  H7 -- hash state      h[0..7]

data RegFile = RegFile
                 { v0 , v1 ,  v2 ,  v3 ,  v4 ,  v5 ,  v6 ,  v7
                 , v8 , v9 , v10 , v11 , v12 , v13 , v14 , v15
                 , m0 , m1 ,  m2 ,  m3 ,  m4 ,  m5 ,  m6 ,  m7
                 , m8 , m9 , m10 , m11 , m12 , m13 , m14 , m15
                 , h0 , h1 ,  h2 ,  h3 ,  h4 ,  h5 ,  h6 ,  h7
                   :: W 64 }

type RF = Vector 40 (W 64)
          
trReg :: Reg -> RegFile -> W 64
trReg r = case r of       
               ----
               V0  -> v0
               V1  -> v1
               V2  -> v2
               V3  -> v3
               V4  -> v4
               V5  -> v5
               V6  -> v6
               V7  -> v7
               V8  -> v8
               V9  -> v9
               V10 -> v10
               V11 -> v11
               V12 -> v12
               V13 -> v13
               V14 -> v14
               V15 -> v15
               ----
               M0  -> m0
               M1  -> m1
               M2  -> m2
               M3  -> m3
               M4  -> m4
               M5  -> m5
               M6  -> m6
               M7  -> m7
               M8  -> m8
               M9  -> m9
               M10 -> m10
               M11 -> m11
               M12 -> m12
               M13 -> m13
               M14 -> m14
               M15 -> m15
               ----
               H0  -> h0
               H1  -> h1
               H2  -> h2
               H3  -> h3
               H4  -> h4
               H5  -> h5
               H6  -> h6
               H7  -> h7

readReg :: Reg -> Storage RegFile (W 64)
readReg v = do
             rf <- get
             return (trReg v rf)

setReg :: Reg -> W 64 -> Storage RegFile ()
setReg r w = do
               rf <- get
               case r of
                 ----
                 V0  -> put $ rf { v0 = w }
                 V1  -> put $ rf { v1 = w }
                 V2  -> put $ rf { v2 = w }
                 V3  -> put $ rf { v3 = w }
                 V4  -> put $ rf { v4 = w }
                 V5  -> put $ rf { v5 = w }
                 V6  -> put $ rf { v6 = w }
                 V7  -> put $ rf { v7 = w }
                 V8  -> put $ rf { v8 = w }
                 V9  -> put $ rf { v9 = w }
                 V10 -> put $ rf { v10 = w }
                 V11 -> put $ rf { v11 = w }
                 V12 -> put $ rf { v12 = w }
                 V13 -> put $ rf { v13 = w }
                 V14 -> put $ rf { v14 = w }
                 V15 -> put $ rf { v15 = w }
                 ----
                 M0  -> put $ rf { m0 = w }
                 M1  -> put $ rf { m1 = w }
                 M2  -> put $ rf { m2 = w }
                 M3  -> put $ rf { m3 = w }
                 M4  -> put $ rf { m4 = w }
                 M5  -> put $ rf { m5 = w }
                 M6  -> put $ rf { m6 = w }
                 M7  -> put $ rf { m7 = w }
                 M8  -> put $ rf { m8 = w }
                 M9  -> put $ rf { m9 = w }
                 M10 -> put $ rf { m10 = w }
                 M11 -> put $ rf { m11 = w }
                 M12 -> put $ rf { m12 = w }
                 M13 -> put $ rf { m13 = w }
                 M14 -> put $ rf { m14 = w }
                 M15 -> put $ rf { m15 = w }
                 ----
                 H0  -> put $ rf { h0 = w }
                 H1  -> put $ rf { h1 = w }
                 H2  -> put $ rf { h2 = w }
                 H3  -> put $ rf { h3 = w }
                 H4  -> put $ rf { h4 = w }
                 H5  -> put $ rf { h5 = w }
                 H6  -> put $ rf { h6 = w }
                 H7  -> put $ rf { h7 = w }

(<==) :: Reg -> Storage RegFile (W 64) -> Storage RegFile ()
w <== e = e >>= setReg w

regfile0 :: RegFile
regfile0 = RegFile {  v0 = lit 0
                   ,  v1 = lit 0 
                   ,  v2 = lit 0 
                   ,  v3 = lit 0 
                   ,  v4 = lit 0 
                   ,  v5 = lit 0 
                   ,  v6 = lit 0
                   ,  v7 = lit 0
                   ,  v8 = lit 0
                   ,  v9 = lit 0
                   , v10 = lit 0 
                   , v11 = lit 0
                   , v12 = lit 0
                   , v13 = lit 0
                   , v14 = lit 0
                   , v15 = lit 0
                   ,  m0 = lit 0
                   ,  m1 = lit 0 
                   ,  m2 = lit 0 
                   ,  m3 = lit 0 
                   ,  m4 = lit 0 
                   ,  m5 = lit 0 
                   ,  m6 = lit 0
                   ,  m7 = lit 0
                   ,  m8 = lit 0
                   ,  m9 = lit 0
                   , m10 = lit 0 
                   , m11 = lit 0
                   , m12 = lit 0
                   , m13 = lit 0
                   , m14 = lit 0
                   , m15 = lit 0
                   ,  h0 = lit 0
                   ,  h1 = lit 0 
                   ,  h2 = lit 0 
                   ,  h3 = lit 0 
                   ,  h4 = lit 0 
                   ,  h5 = lit 0 
                   ,  h6 = lit 0
                   ,  h7 = lit 0 }

type Storage s   = StateT s Identity
data Inp a       = Start a | DC a | Q0 a | Q1 a | Q2 a | Q3 a | Args a 

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
                     
-- These are permutations. I changed the name to make it conform to the
-- RFC7693 text.
-- 
type Reg16 = (Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg , Reg)

sigma0 , sigma1 , sigma2 , sigma3 , sigma4 , sigma5 , sigma6 , sigma7 , sigma8 , sigma9 :: Reg16

sigma0 = (M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15)
sigma1 = (M14,M10,M4,M8,M9,M15,M13,M6,M1,M12,M0,M2,M11,M7,M5,M3)
sigma2 = (M11,M8,M12,M0,M5,M2,M15,M13,M10,M14,M3,M6,M7,M1,M9,M4)
sigma3 = (M7,M9,M3,M1,M13,M12,M11,M14,M2,M6,M5,M10,M4,M0,M15,M8)
sigma4 = (M9,M0,M5,M7,M2,M4,M10,M15,M14,M1,M11,M12,M6,M8,M3,M13)
sigma5 = (M2,M12,M6,M10,M0,M11,M8,M3,M4,M13,M7,M5,M15,M14,M1,M9)
sigma6 = (M12,M5,M1,M15,M14,M13,M4,M10,M0,M7,M6,M3,M9,M2,M8,M11)
sigma7 = (M13,M11,M7,M14,M12,M1,M3,M9,M5,M0,M15,M4,M8,M6,M2,M10)
sigma8 = (M6,M15,M14,M9,M11,M3,M0,M8,M12,M2,M13,M7,M1,M4,M10,M5)
sigma9 = (M10,M2,M8,M4,M7,M6,M1,M5,M15,M11,M9,M14,M3,M12,M13,M0)

-- |
-- | The body of the FOR loop in definition of F.
-- |
message_permutation :: Reg16 -> Storage RegFile ()
message_permutation (ms0,ms1,ms2,ms3,ms4,ms5,ms6,ms7,ms8,ms9,ms10,ms11,ms12,ms13,ms14,ms15) = 
      do
        _G V0 V4  V8 V12 ms0 ms1
        _G V1 V5  V9 V13 ms2 ms3
        _G V2 V6 V10 V14 ms4 ms5
        _G V3 V7 V11 V15 ms6 ms7

        _G V0 V5 V10 V15 ms8 ms9
        _G V1 V6 V11 V12 ms10 ms11
        _G V2 V7 V8  V13 ms12 ms13
        _G V3 V4 V9  V14 ms14 ms15

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
           V12 <== do { w <- readReg V12 ; return $ w ^ lowword t }
           V13 <== do { w <- readReg V13 ; return $ w ^ highword t }
           if f then
                  V14 <== do { w <- readReg V14 ; return $ w ^ lit 0xffffffffffffffff }
                else
                  return ()
           cryptographic_mixing
           xor_two_halves
  where

     init_local_work_vector :: Storage RegFile ()
     init_local_work_vector = do
                     V0  <== readReg H0
                     V1  <== readReg H1
                     V2  <== readReg H2
                     V3  <== readReg H3
                     V4  <== readReg H4
                     V5  <== readReg H5
                     V6  <== readReg H6
                     V7  <== readReg H7
                     V8  <== return iv0
                     V9  <== return iv1
                     V10 <== return iv2
                     V11 <== return iv3
                     V12 <== return iv4
                     V13 <== return iv5
                     V14 <== return iv6
                     V15 <== return iv7

     xor_two_halves :: Storage RegFile ()
     xor_two_halves = do
                   H0 <== xor3 H0 V0 V8
                   H1 <== xor3 H1 V1 V9
                   H2 <== xor3 H2 V2 V10
                   H3 <== xor3 H3 V3 V11
                   H4 <== xor3 H4 V4 V12
                   H5 <== xor3 H5 V5 V13
                   H6 <== xor3 H6 V6 V14
                   H7 <== xor3 H7 V7 V15
                     where
                       xor3 :: Reg -> Reg -> Reg -> Storage RegFile (W 64)
                       xor3 r1 r2 r3 = do
                         w1 <- readReg r1
                         w2 <- readReg r2
                         w3 <- readReg r3
                         return $ w1 ^ w2 ^ w3
    
     lowword :: W 128 -> W 64
     lowword w = slice (Proxy :: Proxy 64) w

     highword :: W 128 -> W 64
     highword w = rslice (Proxy :: Proxy 64) w

type W64x8  = ( W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 )
type W64x16 = ( W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64
              , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 , W 64 )

initM :: W64x16 -> Storage RegFile ()
initM (w0 , w1 , w2 , w3 , w4 , w5 , w6 , w7 , w8 , w9 , w10 , w11 , w12 , w13 , w14 , w15 ) = do
                         setReg M0 w0
                         setReg M1 w1
                         setReg M2 w2
                         setReg M3 w3
                         setReg M4 w4
                         setReg M5 w5
                         setReg M6 w6
                         setReg M7 w7
                         setReg M8 w8
                         setReg M9 w9
                         setReg M10 w10
                         setReg M11 w11
                         setReg M12 w12
                         setReg M13 w13
                         setReg M14 w14
                         setReg M15 w15

readH :: Storage RegFile W64x8
readH = do
  h0 <- readReg H0
  h1 <- readReg H1
  h2 <- readReg H2
  h3 <- readReg H3
  h4 <- readReg H4
  h5 <- readReg H5
  h6 <- readReg H6
  h7 <- readReg H7
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
                     
                     H0 <== return iv0  -- Initialization Vector.
                     H1 <== return iv1
                     H2 <== return iv2
                     H3 <== return iv3
                     H4 <== return iv4
                     H5 <== return iv5
                     H6 <== return iv6
                     H7 <== return iv7

                     -- Parameter block p[0]
                     H0 <== do
                              h0 <- readReg H0
                              return $ h0 ^ lit 0x01010000 ^ (kk <<. lit 8) ^ nn

                     if kk == lit 0
                       then
                         _F ll True
                       else
                         _F (ll + bb) True

                     readH
