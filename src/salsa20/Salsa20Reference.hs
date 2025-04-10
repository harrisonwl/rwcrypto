{-# LANGUAGE DataKinds #-}
module Salsa20Reference where

-- |
-- | This is a refactoring of the reference semantics
-- | for Salsa20.
-- |

import Prelude hiding ((^), (+), (==), (&&), (++), take, drop)
import ReWire hiding (error)
import ReWire.Bits
import ReWire.Vectors

import ReWire.Interactive

fubar :: W 2 -> Bool
fubar w2 | w2 == lit 0x0 = True
         | otherwise     = False

-----------------------------
-- Definitions and helpers
-----------------------------

type W8     = W 8
type W32    = W 32
type W64    = W 64
type Quad a = (a , a , a , a)
type Oct a  = (a , a , a , a , a , a , a , a)
type Hex a  = (a , a , a , a , a , a , a , a , a , a , a , a , a , a , a , a )
data X64 a  = X64 a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
                  a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a 

-----------------------------
-- The quarterround function from page 2 of
-- Bernstein's "Salsa20 Specification"
-----------------------------

quarterround :: (W32 , W32 , W32 , W32) -> (W32 , W32 , W32 , W32)
quarterround (y0 , y1 , y2 , y3) = (z0 , z1 , z2 , z3)
  where
    z1 = y1 ^ rotL (lit 7) (y0 + y3)
    z2 = y2 ^ rotL (lit 9) (z1 + y0) 
    z3 = y3 ^ rotL (lit 13) (z2 + z1) 
    z0 = y0 ^ rotL (lit 18) (z3 + z2) 

-----------------------------
-- The rowround function from page 3 of
-- Bernstein's "Salsa20 Specification"
-----------------------------

rowround :: Hex W32 -> Hex W32
rowround (y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15)
   = (z0, z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14, z15)
     where
       ( z0,  z1,  z2,  z3) = quarterround ( y0,  y1,  y2,  y3)
       ( z5,  z6,  z7,  z4) = quarterround ( y5,  y6,  y7,  y4)
       (z10, z11,  z8,  z9) = quarterround (y10, y11,  y8,  y9)
       (z15, z12, z13, z14) = quarterround (y15, y12, y13, y14)

-----------------------------
-- The columnround function from page 4 of
-- Bernstein's "Salsa20 Specification"
-----------------------------

columnround :: Hex W32 -> Hex W32 
columnround (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
      = (y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15)
     where
        ( y0, y4, y8, y12)  = quarterround (x0, x4, x8, x12)
        ( y5, y9, y13, y1)  = quarterround (x5, x9, x13, x1)
        (y10, y14, y2, y6)  = quarterround (x10, x14, x2, x6)
        (y15,  y3, y7, y11) = quarterround (x15, x3, x7, x11)

-----------------------------
-- The doubleround function from page 5 of
-- Bernstein's "Salsa20 Specification"
-----------------------------

doubleround :: Hex W32 -> Hex W32
doubleround = rowround . columnround

-----------------------------
-- The littleendian function from page 6
-----------------------------

revbytes :: Quad W8 -> Quad W8
revbytes (b0,b1,b2,b3) = (b3,b2,b1,b0)

littleendian :: Quad W8 -> W32
littleendian = w8x4toW32 . revbytes
   where
     w8x4toW32 :: Quad W8 -> W32   
     w8x4toW32 (w1 , w2 , w3 , w4) = w1 ++ w2 ++ w3 ++ w4

inv_littleendian :: W32 -> Quad W8
inv_littleendian = revbytes . w32toW8x4
   where
     word1 :: W32 -> W8
     word1 w = take w

     word2 :: W32 -> W8
     word2 w = take (drop8 w)
       where
         drop8 :: W 32 -> W 24
         drop8 = drop

     word3 :: W32 -> W8
     word3 w = take (drop16 w)
       where
         drop16 :: W 32 -> W 16
         drop16 = drop

     word4 :: W 32 -> W 8
     word4 = drop

     w32toW8x4 :: W32 -> Quad W8
     w32toW8x4 w = (word1 w , word2 w , word3 w , word4 w)

     
-----------------------------
-- Salsa20 hash function from page 6-7
-----------------------------

hash_salsa20 :: X64 W8 -> X64 W8
hash_salsa20 (X64  x_0  x_1  x_2  x_3  x_4  x_5  x_6  x_7  x_8  x_9 x_10 x_11 x_12 x_13 x_14 x_15
                     x_16 x_17 x_18 x_19 x_20 x_21 x_22 x_23 x_24 x_25 x_26 x_27 x_28 x_29 x_30 x_31
                     x_32 x_33 x_34 x_35 x_36 x_37 x_38 x_39 x_40 x_41 x_42 x_43 x_44 x_45 x_46 x_47
                     x_48 x_49 x_50 x_51 x_52 x_53 x_54 x_55 x_56 x_57 x_58 x_59 x_60 x_61 x_62 x_63)
                        = X64
                             a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
                             a40 a41 a42 a43 a50 a51 a52 a53 a60 a61 a62 a63 a70 a71 a72 a73
                             a80 a81 a82 a83 a90 a91 a92 a93 aa0 aa1 aa2 aa3 ab0 ab1 ab2 ab3
                             ac0 ac1 ac2 ac3 ad0 ad1 ad2 ad3 ae0 ae1 ae2 ae3 af0 af1 af2 af3
   where
     x0 , x1 , x2 , x3 , x4 , x5 , x6 , x7 , x8 , x9 , x10 , x11 , x12 , x13 , x14 , x15 :: W32
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

     dr10 :: Hex W32 -> Hex W32
     dr10 = doubleround . doubleround . doubleround . doubleround . doubleround .
              doubleround . doubleround . doubleround . doubleround . doubleround 

     z0 , z1 , z2 , z3 , z4 , z5 , z6 , z7 , z8 , z9 , z10 , z11 , z12 , z13 , z14 , z15 :: W32
     (z0, z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14, z15)
           = dr10 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)

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

-----------------------------
-- Salsa20 Expansion Functions (Sect 9, p8)
-----------------------------

expand :: Quad W8 ->
          Quad W8 ->
          Quad W8 ->
          Quad W8 ->
          ( Hex W8   -- k0     / k   
          , Hex W8   -- n      / 
          , Hex W8)  -- k1     / k
           -> X64 W8
expand (x1,x2,x3,x4) (w1,w2,w3,w4) (v1,v2,v3,v4) (t1,t2,t3,t4)
       ( (y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16)
       , (z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,z11,z12,z13,z14,z15,z16)
       , (u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16)
       ) =
  X64 x1 x2 x3 x4
        --
        y1 y2 y3 y4 y5 y6 y7 y8
        y9 y10 y11 y12 y13 y14 y15 y16
        --
        w1 w2 w3 w4
        --
        u1 u2 u3 u4 u5 u6 u7 u8
        u9 u10 u11 u12 u13 u14 u15 u16
        --
        v1 v2 v3 v4
        --
        z1 z2 z3 z4 z5 z6 z7 z8
        z9 z10 z11 z12 z13 z14 z15 z16
        --
        t1 t2 t3 t4
        --


-- salsa20_256 :: Hex W8 -> Hex W8 -> Hex W8 -> X64 W8
-- salsa20_256 k0 k1 n = hash_salsa20 (expand sigma0 sigma1 sigma2 sigma3 (k0, k1, n))
-- salsa20_128 :: Hex W8 -> Hex W8 -> X64 W8
-- salsa20_128 k n     = hash_salsa20 (expand tau0 tau1 tau2 tau3 (k, k, n))

-- | N.b., the Show function for W8 will present numbers in hexadecimal, but
-- | Bernstein's examples use base 10. Can lead to confusion. Unfortunately,
-- | Bernstein's spec uses both base 10 and 16 for his examples. Leading to
-- | more confusion possibly.

-- | Called Salsa20_{k0,k1} in Bernstein, Section 9, page 8.
salsa20_256 :: (Hex W8, Hex W8, Hex W8) -> X64 W8
salsa20_256 = hash_salsa20 . expand sigma0 sigma1 sigma2 sigma3
  where
    sigma0, sigma1, sigma2, sigma3 :: Quad W8 
    sigma0 = (lit 101, lit 120, lit 112, lit 97)
    sigma1 = (lit 110, lit 100, lit 32, lit 51)
    sigma2 = (lit 50, lit 45, lit 98, lit 121)
    sigma3 = (lit 116, lit 101, lit 32, lit 107)

salsa20_128 :: (Hex W8, Hex W8) -> X64 W8
salsa20_128 = hash_salsa20 . expand tau0 tau1 tau2 tau3 . dup1
  where
    dup1 :: (a , b) -> (a , a , b)
    dup1 (k , n) = (k , k , n)
    tau0 , tau1, tau2, tau3 :: Quad W8        
    tau0   = (lit 101, lit 120, lit 112, lit 97)
    tau1   = (lit 110, lit 100, lit 32, lit 49)
    tau2   = (lit 54, lit 45, lit 98, lit 121)
    tau3   = (lit 116, lit 101, lit 32, lit 107)

----------------------
----------------------
----------------------
----------------------

-- |
-- | This is the infamous (_) function from Section 10. 
-- |
factor :: Integer -> Oct W8
factor i = (lit i0 , lit i1, lit i2, lit i3, lit i4, lit i5, lit i6, lit i7)
  where
    i0, i1, i2, i3, i4, i5, i6, i7 :: Integer
    (q0 , i0) = quotRem i 256
    (q1 , i1) = quotRem q0 256
    (q2 , i2) = quotRem q1 256
    (q3 , i3) = quotRem q2 256
    (q4 , i4) = quotRem q3 256
    (q5 , i5) = quotRem q4 256
    (q6 , i6) = quotRem q5 256
    (_  , i7) = quotRem q6 256
