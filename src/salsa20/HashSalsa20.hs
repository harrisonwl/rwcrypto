{-# LANGUAGE DataKinds #-}
module HashSalsa20(hash_salsa20,hash) where

import Prelude hiding ((+) , (<>)) 
import ReWire
import ReWire.Bits ((+))
import Salsa20Basic (Hex, X16(..), X64(..))
import DoubleRound (doubleround)
import LittleEndian (littleendian,inv_littleendian,littleendian')

-----------------------------
-- Salsa20 hash function from page 6-7
-----------------------------

-- hash :: Hex (W 32) -> Hex (W 32)
-- hash (X16 w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15)
--                       = X16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae af
--    where
--      v0 , v1 , v2 , v3 , v4 , v5 , v6 , v7 , v8 , v9 , v10 , v11 , v12 , v13 , v14 , v15 :: W 32
--      v0  = littleendian' w0
--      v1  = littleendian' w1
--      v2  = littleendian' w2
--      v3  = littleendian' w3
--      v4  = littleendian' w4
--      v5  = littleendian' w5
--      v6  = littleendian' w6
--      v7  = littleendian' w7
--      v8  = littleendian' w8
--      v9  = littleendian' w9
--      v10 = littleendian' w10
--      v11 = littleendian' w11
--      v12 = littleendian' w12
--      v13 = littleendian' w13
--      v14 = littleendian' w14
--      v15 = littleendian' w15
     
--      dr10 :: Hex (W 32) -> Hex (W 32)
--      dr10 = doubleround . doubleround . doubleround . doubleround . doubleround .
--               doubleround . doubleround . doubleround . doubleround . doubleround 

--      z0 , z1 , z2 , z3 , z4 , z5 , z6 , z7 , z8 , z9 , z10 , z11 , z12 , z13 , z14 , z15 :: W 32
--      (X16 z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15)
--            = dr10 (X16 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)

--      a0 = littleendian' (z0 + v0)
--      a1 = littleendian' (z1 + v1)
--      a2 = littleendian' (z2 + v2)
--      a3 = littleendian' (z3 + v3)
--      a4 = littleendian' (z4 + v4)
--      a5 = littleendian' (z5 + v5)
--      a6 = littleendian' (z6 + v6)
--      a7 = littleendian' (z7 + v7)

--      a8 = littleendian' (z8 + v8)
--      a9 = littleendian' (z9 + v9)
--      aa = littleendian' (z10 + v10)
--      ab = littleendian' (z11 + v11)
--      ac = littleendian' (z12 + v12)
--      ad = littleendian' (z13 + v13)
--      ae = littleendian' (z14 + v14)
--      af = littleendian' (z15 + v15)

h :: Hex (W 32) -> Hex (W 32)
h w = (lendian . \ z -> plus z (lendian w)) . doubleround . doubleround . doubleround . doubleround . doubleround . doubleround . doubleround . doubleround . doubleround . doubleround . lendian $ w

(<>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f <> g = \ a -> f a >>= g

step :: Monad m => (a -> b) -> a -> m b
step f = return . f

hm :: Monad m => Hex (W 32) -> m (Hex (W 32))
hm = step doubleround <> step doubleround <> step doubleround <> step doubleround <>
     step doubleround <> step doubleround <> step doubleround <> step doubleround <>
     step doubleround <> step doubleround

--
-- This is a compositional form of hash_salsa20
--
hash :: Hex (W 32) -> Hex (W 32)
hash w = let
               v = lendian w
           in
              -- | 11 functions composed:
              (lendian . \ z -> plus z v) . doubleround . doubleround . doubleround . doubleround . doubleround . doubleround . doubleround . doubleround . doubleround . doubleround $ v



lendian :: Hex (W 32) -> Hex (W 32)
lendian (X16 w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15)
                     = X16 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        where
          v0 , v1 , v2 , v3 , v4 , v5 , v6 , v7 , v8 , v9 , v10 , v11 , v12 , v13 , v14 , v15 :: W 32
          v0  = littleendian' w0
          v1  = littleendian' w1
          v2  = littleendian' w2
          v3  = littleendian' w3
          v4  = littleendian' w4
          v5  = littleendian' w5
          v6  = littleendian' w6
          v7  = littleendian' w7
          v8  = littleendian' w8
          v9  = littleendian' w9
          v10 = littleendian' w10
          v11 = littleendian' w11
          v12 = littleendian' w12
          v13 = littleendian' w13
          v14 = littleendian' w14
          v15 = littleendian' w15

plus :: Hex (W 32) -> Hex (W 32) -> Hex (W 32)
plus (X16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
         (X16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
            = X16 (a0 + b0) (a1 + b1) (a2 + b2) (a3 + b3)
                  (a4 + b4) (a5 + b5) (a6 + b6) (a7 + b7)
                  (a8 + b8) (a9 + b9) (a10 + b10) (a11 + b11)
                  (a12 + b12) (a13 + b13) (a14 + b14) (a15 + b15)


hash_salsa20 :: X64 (W 8) -> X64 (W 8)
hash_salsa20 (X64   x_0  x_1  x_2  x_3  x_4  x_5  x_6  x_7  x_8  x_9 x_10 x_11 x_12 x_13 x_14 x_15
                   x_16 x_17 x_18 x_19 x_20 x_21 x_22 x_23 x_24 x_25 x_26 x_27 x_28 x_29 x_30 x_31
                   x_32 x_33 x_34 x_35 x_36 x_37 x_38 x_39 x_40 x_41 x_42 x_43 x_44 x_45 x_46 x_47
                   x_48 x_49 x_50 x_51 x_52 x_53 x_54 x_55 x_56 x_57 x_58 x_59 x_60 x_61 x_62 x_63 )
                        = X64
                             a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
                             a40 a41 a42 a43 a50 a51 a52 a53 a60 a61 a62 a63 a70 a71 a72 a73
                             a80 a81 a82 a83 a90 a91 a92 a93 aa0 aa1 aa2 aa3 ab0 ab1 ab2 ab3
                             ac0 ac1 ac2 ac3 ad0 ad1 ad2 ad3 ae0 ae1 ae2 ae3 af0 af1 af2 af3
   where
     x0 , x1 , x2 , x3 , x4 , x5 , x6 , x7 , x8 , x9 , x10 , x11 , x12 , x13 , x14 , x15 :: W 32
     x0  = littleendian (  x_0 ,  x_1 ,  x_2 ,  x_3 )
     x1  = littleendian (  x_4 ,  x_5 ,  x_6 ,  x_7 )
     x2  = littleendian (  x_8 ,  x_9 , x_10 , x_11 )
     x3  = littleendian ( x_12 , x_13 , x_14 , x_15 )
     x4  = littleendian ( x_16 , x_17 , x_18 , x_19 )
     x5  = littleendian ( x_20 , x_21 , x_22 , x_23 )
     x6  = littleendian ( x_24 , x_25 , x_26 , x_27 )
     x7  = littleendian ( x_28 , x_29 , x_30 , x_31 )
     x8  = littleendian ( x_32 , x_33 , x_34 , x_35 )
     x9  = littleendian ( x_36 , x_37 , x_38 , x_39 )
     x10 = littleendian ( x_40 , x_41 , x_42 , x_43 )
     x11 = littleendian ( x_44 , x_45 , x_46 , x_47 )
     x12 = littleendian ( x_48 , x_49 , x_50 , x_51 )
     x13 = littleendian ( x_52 , x_53 , x_54 , x_55 )
     x14 = littleendian ( x_56 , x_57 , x_58 , x_59 )
     x15 = littleendian ( x_60 , x_61 , x_62 , x_63 )

     dr10 :: Hex (W 32) -> Hex (W 32)
     dr10 = doubleround . doubleround . doubleround . doubleround . doubleround .
              doubleround . doubleround . doubleround . doubleround . doubleround 

     z0 , z1 , z2 , z3 , z4 , z5 , z6 , z7 , z8 , z9 , z10 , z11 , z12 , z13 , z14 , z15 :: W 32
     (X16 z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15)
           = dr10 (X16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)

     (a00 , a01 , a02 , a03) = inv_littleendian (z0 + x0)
     (a10 , a11 , a12 , a13) = inv_littleendian (z1 + x1)
     (a20 , a21 , a22 , a23) = inv_littleendian (z2 + x2)
     (a30 , a31 , a32 , a33) = inv_littleendian (z3 + x3)
     (a40 , a41 , a42 , a43) = inv_littleendian (z4 + x4)
     (a50 , a51 , a52 , a53) = inv_littleendian (z5 + x5)
     (a60 , a61 , a62 , a63) = inv_littleendian (z6 + x6)
     (a70 , a71 , a72 , a73) = inv_littleendian (z7 + x7)

     (a80 , a81 , a82 , a83) = inv_littleendian (z8 + x8)
     (a90 , a91 , a92 , a93) = inv_littleendian (z9 + x9)
     (aa0 , aa1 , aa2 , aa3) = inv_littleendian (z10 + x10)
     (ab0 , ab1 , ab2 , ab3) = inv_littleendian (z11 + x11)
     (ac0 , ac1 , ac2 , ac3) = inv_littleendian (z12 + x12)
     (ad0 , ad1 , ad2 , ad3) = inv_littleendian (z13 + x13)
     (ae0 , ae1 , ae2 , ae3) = inv_littleendian (z14 + x14)
     (af0 , af1 , af2 , af3) = inv_littleendian (z15 + x15)

