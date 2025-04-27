{-# LANGUAGE DataKinds #-}
-- module RW_Salsa20 where

-- |
-- | This is a refactoring of the reference semantics
-- | for Salsa20.
-- |

import Prelude hiding ((^), (+), (==), (&&), (++), take, drop)
import ReWire 
import ReWire.Bits
import ReWire.Vectors

import ReWire.Interactive

-----------------------------
-- Definitions and helpers
-----------------------------

type W6     = W 6
type W8     = W 8
type W16    = W 16
type W24    = W 24
type W32    = W 32
type W64    = W 64
type Quad a = (a , a , a , a)
type Oct a  = (a , a , a , a , a , a , a , a)
type Hex a  = (a , a , a , a , a , a , a , a , a , a , a , a , a , a , a , a )
data X64 a  = X64 a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
                  a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a 


type Sto     = (W6 , W64 , Hex W8 {- k0 -} , Hex W8 {- k1 -} , Oct W8 {- v -})
data Input i = Reset i | Work W8

next :: StateT Sto Identity ()
next = get >>= \ (z,w,k0,k1,v) -> put (z + lit 1 , w + lit 1, k0 , k1 , v)

reset :: Hex W8 -> Hex W8 -> Oct W8 -> StateT Sto Identity ()
reset k0 k1 v = put (lit 0, lit 0 , k0 , k1 , v)

-----
-- | Salsa20 device, 256-bit version
-----

factorM :: StateT Sto Identity (Oct W8)
factorM     = get >>= \ (_,w,_,_,_) -> return (factor w)

-- |
-- | This is the infamous (_) function from Section 10. 
-- |
factor :: W64 -> (W8 , W8 , W8 , W8 , W8 , W8 , W8 , W8 )
factor w64 = (s0 , s1 , s2 , s3 , s4 , s5 , s6 , s7)
  where
    s0 , s1 , s2 , s3 , s4 , s5 , s6 , s7 :: W8
    s0 = slice (Proxy :: Proxy 0)  w64
    s1 = slice (Proxy :: Proxy 8)  w64
    s2 = slice (Proxy :: Proxy 16) w64
    s3 = slice (Proxy :: Proxy 24) w64
    s4 = slice (Proxy :: Proxy 32) w64
    s5 = slice (Proxy :: Proxy 40) w64
    s6 = slice (Proxy :: Proxy 48) w64
    s7 = slice (Proxy :: Proxy 56) w64


-----------------------------
-- The quarterround function from page 2 of
-- Bernstein's "Salsa20 Specification"
-----------------------------

-- {-# INLINE quarterround #-}
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

-- {-# INLINE rowround #-}
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

-- {-# INLINE columnround #-}
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

-- {-# INLINE doubleround #-}
doubleround :: Hex W32 -> Hex W32
doubleround = rowround . columnround

-----------------------------
-- The littleendian function from page 6
-----------------------------

-- {-# INLINE revbytes #-}
revbytes :: Quad W8 -> Quad W8
revbytes (b0,b1,b2,b3) = (b3,b2,b1,b0)

-- {-# INLINE littleendian #-}
littleendian :: Quad W8 -> W32
littleendian = w8x4toW32 . revbytes
   where
     -- {-# INLINE w8x4toW32 #-}
     w8x4toW32 :: Quad W8 -> W32   
     w8x4toW32 (w1 , w2 , w3 , w4) = w1 ++ w2 ++ w3 ++ w4

-- {-# INLINE inv_littleendian #-}
inv_littleendian :: W32 -> Quad W8
inv_littleendian = revbytes . w32toW8x4
   where
     word1 :: W32 -> W8
     word1 w = take w

     word2 :: W32 -> W8
     word2 w = take (drop8 w)
       where
         drop8 :: W32 -> W24
         drop8 = drop

     word3 :: W32 -> W8
     word3 w = take (drop16 w)
       where
         drop16 :: W32 -> W16
         drop16 = drop

     word4 :: W32 -> W8
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

-- | Called Salsa20_{k0,k1} in Bernstein, Section 9, page 8.
salsa20_256 :: (Hex W8, Hex W8, Hex W8) -> X64 W8
salsa20_256 = hash_salsa20 . expand sigma0 sigma1 sigma2 sigma3
  where
    sigma0, sigma1, sigma2, sigma3 :: Quad W8 
    sigma0 = (lit 101, lit 120, lit 112, lit 97)
    sigma1 = (lit 110, lit 100, lit 32, lit 51)
    sigma2 = (lit 50, lit 45, lit 98, lit 121)
    sigma3 = (lit 116, lit 101, lit 32, lit 107)


-------------------
-- From Iterator
-------------------

i_mod_64M :: X64 W8 -> StateT Sto Identity W8
i_mod_64M w = get >>= \ (z,_,_,_,_) -> return (ref w z)

ref :: X64 W8 -> W6 -> W8
ref (X64 x00 x01 x02 x03 x04 x05 x06 x07 x08 x09 x0a x0b x0c x0d x0e x0f
         x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x1a x1b x1c x1d x1e x1f
         x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x2a x2b x2c x2d x2e x2f
         x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x3a x3b x3c x3d x3e x3f) j
    = if j==lit 0x00 then x00 else
      if j==lit 0x01 then x01 else
      if j==lit 0x02 then x02 else
      if j==lit 0x03 then x03 else 
      if j==lit 0x04 then x04 else
      if j==lit 0x05 then x05 else
      if j==lit 0x06 then x06 else
      if j==lit 0x07 then x07 else
      if j==lit 0x08 then x08 else
      if j==lit 0x09 then x09 else
      if j==lit 0x0a then x0a else
      if j==lit 0x0b then x0b else
      if j==lit 0x0c then x0c else
      if j==lit 0x0d then x0d else
      if j==lit 0x0e then x0e else
      if j==lit 0x0f then x0f else
      if j==lit 0x10 then x10 else
      if j==lit 0x11 then x11 else
      if j==lit 0x12 then x12 else
      if j==lit 0x13 then x13 else
      if j==lit 0x14 then x14 else
      if j==lit 0x15 then x15 else
      if j==lit 0x16 then x16 else
      if j==lit 0x17 then x17 else
      if j==lit 0x18 then x18 else
      if j==lit 0x19 then x19 else
      if j==lit 0x1a then x1a else
      if j==lit 0x1b then x1b else
      if j==lit 0x1c then x1c else
      if j==lit 0x1d then x1d else
      if j==lit 0x1e then x1e else
      if j==lit 0x1f then x1f else
      if j==lit 0x20 then x20 else
      if j==lit 0x21 then x21 else
      if j==lit 0x22 then x22 else
      if j==lit 0x23 then x23 else
      if j==lit 0x24 then x24 else
      if j==lit 0x25 then x25 else
      if j==lit 0x26 then x26 else
      if j==lit 0x27 then x27 else
      if j==lit 0x28 then x28 else
      if j==lit 0x29 then x29 else
      if j==lit 0x2a then x2a else
      if j==lit 0x2b then x2b else
      if j==lit 0x2c then x2c else
      if j==lit 0x2d then x2d else
      if j==lit 0x2e then x2e else
      if j==lit 0x2f then x2f else
      if j==lit 0x30 then x30 else
      if j==lit 0x31 then x31 else
      if j==lit 0x32 then x32 else
      if j==lit 0x33 then x33 else
      if j==lit 0x34 then x34 else
      if j==lit 0x35 then x35 else
      if j==lit 0x36 then x36 else
      if j==lit 0x37 then x37 else
      if j==lit 0x38 then x38 else
      if j==lit 0x39 then x39 else
      if j==lit 0x3a then x3a else
      if j==lit 0x3b then x3b else
      if j==lit 0x3c then x3c else
      if j==lit 0x3d then x3d else
      if j==lit 0x3e then x3e else {- j==lit 0x3f -} x3f

-------------------
--
-------------------

salsa20_256M :: Hex W8 -> StateT Sto Identity (X64 W8)
salsa20_256M hw8 = get >>= \ (_,_,k0,k1,_) -> return (salsa20_256 (k0 , k1 , hw8))

action :: W8 -> StateT Sto Identity W8
action mj = factorM >>= spliceM >>= salsa20_256M >>= i_mod_64M >>= \ m -> next >>= \ _ -> return (mj ^ m)
-- | N.b., as written ^^^^^^^^^^^ is a one-fell-swoop device, computing each output byte in each cycle

spliceM :: Oct W8 -> StateT Sto Identity (Hex W8)
spliceM m   = get >>= \ (_,_,_,_,v) -> return (splice v m)
  where
    splice :: Oct W8 -> Oct W8 -> Hex W8
    splice (b0, b1, b2, b3, b4, b5, b6, b7) (b8, b9, b10, b11, b12, b13, b14, b15)
       = (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15) 

     
devcrypt256 :: Input (Hex W8, Hex W8, Oct W8) -> ReacT (Input (Hex W8, Hex W8, Oct W8)) (Maybe W8) (StateT Sto Identity) ()
devcrypt256 (Reset (k0 , k1 , v)) = (lift (reset k0 k1 v) >>= \ _ -> signal Nothing) >>= devcrypt256
devcrypt256 (Work m)              = (lift (action m) >>= signal . Just) >>= devcrypt256

start :: ReacT (Input (Hex W8, Hex W8, Oct W8)) (Maybe W8) Identity ()
start = extrude (devcrypt256 (Reset (k0 , k1 , v))) sigma0
  where
    
    k0 , k1 :: Hex W8
    k0  = ( lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
          , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0)
    k1  = ( lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
          , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0)
    v :: Oct W8
    v   = (lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0)

    sigma0 :: Sto
    sigma0 = (lit 0 , lit 0 , k0 , k1 , v)
