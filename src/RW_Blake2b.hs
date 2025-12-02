{-# LANGUAGE DataKinds #-}

import Prelude hiding ((^), (+), (==), (&&) , (++))
import ReWire hiding (error)
import ReWire.Bits
import ReWire.Vectors

import Blake2b.Reference hiding (_F , _BLAKE2b)

type Mealy i s o = ReacT i o (Storage s)

-- |
-- | These are the staging operators.
-- |

{-# INLINE stage_ #-}
stage_ :: Storage RegFile () -> Mealy (Inp (W 64, W 64 , W 64 , W 64)) RegFile (Maybe W64x8) (Inp (W 64, W 64 , W 64 , W 64))
stage_ x        = do
                   lift x
                   i' <- signal Nothing
                   return i' 

{-# INLINE _stage #-}
_stage :: Storage RegFile () -> Mealy (Inp (W 64, W 64 , W 64 , W 64)) RegFile (Maybe W64x8) ()
_stage x        = do
                   lift x
                   signal Nothing
                   return ()

data Inp a       = Start a | DC a | Q0 a | Q1 a | Q2 a | Q3 a | Args a 

_F :: W 128 -> Bit -> Mealy (Inp (W 64, W 64 , W 64 , W 64)) RegFile (Maybe W64x8) ()
_F t f = do
           _stage $ do
             init_local_work_vector
             v12 <== do { w <- readReg v12 ; return $ w ^ lowword t }
             v13 <== do { w <- readReg v13 ; return $ w ^ highword t }
             if f then
                    do
                      v14 <== do { w <- readReg v14 ; return $ w ^ lit 0xffffffffffffffff }
                  else
                    return ()
           _stage cryptographic_mixing
           _stage xor_two_halves

-- |
-- | This version is simplified assuming that dd==1 && kk==0
-- |

blake2b :: Inp (W 64, W 64 , W 64 , W 64) -> Mealy (Inp (W 64, W 64 , W 64 , W 64)) RegFile (Maybe W64x8) ()
blake2b (Q0 (w0 , w1 , w2 , w3)) = do
     i <- stage_ $ do
                    setReg m0 w0
                    setReg m1 w1
                    setReg m2 w2
                    setReg m3 w3
     blake2b i
blake2b (Q1 (w4 , w5 , w6 , w7)) = do
     i <- stage_ $ do
                    setReg m4 w4
                    setReg m5 w5
                    setReg m6 w6
                    setReg m7 w7
     blake2b i
blake2b (Q2 (w8 , w9 , w10 , w11)) = do
     i <- stage_ $ do
                    setReg m8 w8
                    setReg m9 w9
                    setReg m10 w10
                    setReg m11 w11
     blake2b i
blake2b (Q3 (w12 , w13 , w14 , w15)) = do
     lift $ do
       setReg m12 w12
       setReg m13 w13
       setReg m14 w14
       setReg m15 w15
     i <- signal Nothing
     blake2b i
blake2b (Args (ll_hi , ll_lo , kk , nn)) = do
     v <-_BLAKE2b (ll_hi ++ ll_lo) kk nn
     i <- signal (Just v)
     blake2b i

_BLAKE2b :: W 128 -> W 64 -> W 64 -> Mealy (Inp (W 64, W 64 , W 64 , W 64)) RegFile (Maybe W64x8) W64x8
_BLAKE2b ll kk nn = do
                     
                     stage_ $ do
                               setReg h0 iv0  -- Initialization Vector.
                               setReg h1 iv1
                               setReg h2 iv2
                               setReg h3 iv3
                               setReg h4 iv4
                               setReg h5 iv5
                               setReg h6 iv6
                               setReg h7 iv7

                            -- Parameter block p[0]
                               v <-  do
                                        h0 <- readReg h0
                                        return $ h0 ^ lit 0x01010000 ^ (kk <<. lit 8) ^ nn
                               setReg h0 v

                     if kk == lit 0
                       then
                         _F ll True
                       else
                         _F (ll + bb) True

                     lift readH

type W64x4  = ( W 64 , W 64 , W 64 , W 64 )

start :: ReacT (Inp W64x4) (Maybe W64x8) Identity ()
start = extrude (signal Nothing >>= blake2b) regfile0
--      ^^^^^^^                              ^^^^^^^^
--      provides the initial store 
