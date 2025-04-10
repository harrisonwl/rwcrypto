module ReferenceSalsa20 where
-- import ReWire
-- import ReWire.Bits 
-- import ReWire.Verilog

import qualified Prelude as P -- (Integer , Int , (^)) -- hiding ((^) , (+))
import BinaryArithmetic
import W8
import W32

-- import Iterator()
-- import Idioms

type Integer = P.Integer
type Int     = P.Int
type Bool    = P.Bool

-- | Rotate left once.
rolW32 :: W32 -> W32
rolW32 (W32 a7 a6 a5 a4 a3 a2 a1 a0 b7 b6 b5 b4 b3 b2 b1 b0 
            c7 c6 c5 c4 c3 c2 c1 c0 d7 d6 d5 d4 d3 d2 d1 d0) = 
                 (W32 a6 a5 a4 a3 a2 a1 a0 b7 b6 b5 b4 b3 b2 b1 b0 c7
                      c6 c5 c4 c3 c2 c1 c0 d7 d6 d5 d4 d3 d2 d1 d0 a7)

-- | There is no such built-in operator in Verilog; rolW32 would be
-- | written as the following:
--       wire [31:0] in;
--       wire [31:0] out;
--       assign out = {in[30:0], in[31]};

nrolW32 :: Int -> W32 -> W32
nrolW32 n w32 = if n P.<= 0 then w32 else nrolW32 (n P.- 1) (rolW32 w32)

-- nrolW32 :: Int -> W32 -> W32
-- nrolW32 n w32 | n P.<= 0 = w32
--               | n P.> 0  = nrolW32 (n P.- 1) (rolW32 w32)

-- |
-- | rolW32_{7,9,13,18} would need to be implemented in Verilog
-- |   along the lines outlined above for n=1.
-- |

rolW32_7  :: W32 -> W32
rolW32_7  = nrolW32 7
rolW32_9  :: W32 -> W32
rolW32_9  = nrolW32 9
rolW32_13 :: W32 -> W32
rolW32_13 = nrolW32 13
rolW32_18 :: W32 -> W32
rolW32_18 = nrolW32 18

-----------------------------
-- End of ReWire Figleaf
-----------------------------

-----------------------------
-- The quarterround function from page 2 of
-- Bernstein's "Salsa20 Specification"
-----------------------------

quarterround :: Quad W32 -> Quad W32
quarterround (y0 , y1 , y2 , y3) = (z0 , z1 , z2 , z3)
  where
    z1 = y1 ^ rolW32_7 (y0 + y3)
    z2 = y2 ^ rolW32_9 (z1 + y0)
    z3 = y3 ^ rolW32_13 (z2 + z1)
    z0 = y0 ^ rolW32_18 (z3 + z2)

quarterround' :: (Integer, Integer, Integer, Integer) -> Quad W32 -- (W32, W32, W32, W32)
quarterround' = quarterround P.. toW32x4

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

rowround' :: Hex Integer -> Hex W32
rowround' = rowround P.. toW32x16

-- |
-- | Straight out of Examples on page 3.
-- |
ex1 , ex2 :: (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer,
              Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)
ex1 = (0x00000001, 0x00000000, 0x00000000, 0x00000000,
       0x00000001, 0x00000000, 0x00000000, 0x00000000,
       0x00000001, 0x00000000, 0x00000000, 0x00000000,
       0x00000001, 0x00000000, 0x00000000, 0x00000000)

tst1 :: Bool
tst1 = fromW32x16 (rowround' ex1) P.== (0x08008145, 0x00000080, 0x00010200, 0x20500000,
                                        0x20100001, 0x00048044, 0x00000080, 0x00010000,
                                        0x00000001, 0x00002000, 0x80040000, 0x00000000,
                                        0x00000001, 0x00000200, 0x00402000, 0x88000100)

ex2 = (0x08521bd6, 0x1fe88837, 0xbb2aa576, 0x3aa26365,
       0xc54c6a5b, 0x2fc74c2f, 0x6dd39cc3, 0xda0a64f6,
       0x90a2f23d, 0x067f95a6, 0x06b35f61, 0x41e4732e,
       0xe859c100, 0xea4d84b7, 0x0f619bff, 0xbc6e965a)

tst2 :: Bool
tst2 = fromW32x16 (rowround' ex2) P.== (0xa890d39d, 0x65d71596, 0xe9487daa, 0xc8ca6a86,
                                        0x949d2192, 0x764b7754, 0xe408d9b9, 0x7a41b4d1,
                                        0x3402e183, 0x3c3af432, 0x50669f96, 0xd89ef0a8,
                                        0x0040ede5, 0xb545fbce, 0xd257ed4f, 0x1818882d)

-- | tst1 and tst2 are both True.

-----------------------------
-- The columnround function from page 4 of
-- Bernstein's "Salsa20 Specification"
-----------------------------

columnround :: Hex W32 -> Hex W32    -- W32x16 -> W32x16
columnround (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
      = (y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15)
     where
        ( y0, y4, y8, y12)  = quarterround (x0, x4, x8, x12)
        ( y5, y9, y13, y1)  = quarterround (x5, x9, x13, x1)
        (y10, y14, y2, y6)  = quarterround (x10, x14, x2, x6)
        (y15,  y3, y7, y11) = quarterround (x15, x3, x7, x11)

columnround' :: Hex Integer -> Hex Integer
columnround' = fromW32x16 P.. columnround P.. toW32x16

tst3 :: Bool
tst3 = columnround' ex1 P.== (0x10090288, 0x00000000, 0x00000000, 0x00000000,
                            0x00000101, 0x00000000, 0x00000000, 0x00000000,
                            0x00020401, 0x00000000, 0x00000000, 0x00000000,
                            0x40a04001, 0x00000000, 0x00000000, 0x00000000)

tst4 :: Bool
tst4 = columnround' ex2 P.== (0x8c9d190a, 0xce8e4c90, 0x1ef8e9d3, 0x1326a71a,
                              0x90a20123, 0xead3c4f3, 0x63a091a0, 0xf0708d69,
                              0x789b010c, 0xd195a681, 0xeb7d5504, 0xa774135c,
                              0x481c2027, 0x53a8e4b5, 0x4c1f89c5, 0x3f78c9c8)
       
-- | tst3 and tst4 are both True.

-----------------------------
-- The doubleround function from page 5 of
-- Bernstein's "Salsa20 Specification"
-----------------------------

doubleround :: Hex W32 -> Hex W32    -- W32x16 -> W32x16
doubleround = rowround P.. columnround

doubleround' :: Hex Integer -> Hex Integer
doubleround' = fromW32x16 P.. doubleround P.. toW32x16

ex3 :: Hex Integer
ex3 = (0x00000001, 0x00000000, 0x00000000, 0x00000000,
       0x00000000, 0x00000000, 0x00000000, 0x00000000,
       0x00000000, 0x00000000, 0x00000000, 0x00000000,
       0x00000000, 0x00000000, 0x00000000, 0x00000000)

ex4 :: Hex Integer
ex4 = (0xde501066, 0x6f9eb8f7, 0xe4fbbd9b, 0x454e3f57,
       0xb75540d3, 0x43e93a4c, 0x3a6f2aa0, 0x726d6b36,
       0x9243f484, 0x9145d1e8, 0x4fa9d247, 0xdc8dee11,
       0x054bf545, 0x254dd653, 0xd9421b6d, 0x67b276c1)

tst5 :: Bool
tst5 = doubleround' ex3 P.== (0x8186a22d, 0x0040a284, 0x82479210, 0x06929051,
                              0x08000090, 0x02402200, 0x00004000, 0x00800000,
                              0x00010200, 0x20400000, 0x08008104, 0x00000000,
                              0x20500000, 0xa0000040, 0x0008180a, 0x612a8020)

tst6 :: Bool
tst6 = doubleround' ex4 P.== (0xccaaf672, 0x23d960f7, 0x9153e63a, 0xcd9a60d0,
                              0x50440492, 0xf07cad19, 0xae344aa0, 0xdf4cfdfc,
                              0xca531c29, 0x8e7943db, 0xac1680cd, 0xd503ca00,
                              0xa74b2ad6, 0xbc331c5c, 0x1dda24c7, 0xee928277)       

-- | tst5 and tst6 are both True.

-----------------------------
-- The littleendian function from page 6
-----------------------------

revbytes :: Quad W8 -> Quad W8    -- (W8,W8,W8,W8) -> (W8,W8,W8,W8)
revbytes (b0,b1,b2,b3) = (b3,b2,b1,b0)

littleendian :: Quad W8 -> W32   -- (W8, W8, W8, W8) -> W32
littleendian = w8x4toW32 P.. revbytes

littleendian' :: (Integer, Integer, Integer, Integer) -> W32
littleendian' = littleendian P.. integerX4toW8x4
  where
    integerX4toW8x4 :: (Integer, Integer, Integer, Integer) -> Quad W8  -- (W8,W8,W8,W8)
    integerX4toW8x4 (b0 , b1 , b2 , b3) = (toW8 b0 , toW8 b1 , toW8 b2 , toW8 b3)

-- ghci> littleendian' (0, 0, 0, 0)
--   0x00000000
-- ghci> littleendian' (86, 75, 30, 9)
--   0x091e4b56
-- ghci> littleendian' (255, 255, 255, 250) 
--   0xfaffffff

inv_littleendian :: W32 -> Quad W8         -- (W8, W8, W8, W8)
inv_littleendian = revbytes P.. w32toW8x4

inv_littleendian' :: Integer -> Quad W8    -- (W8, W8, W8, W8)
inv_littleendian' = inv_littleendian P.. toW32

-- | 
-- | Sanity Check for inverse of littleendian
-- | 
-- ghci> inv_littleendian' 0x00000000
--   (00,00,00,00)
-- ghci> inv_littleendian' 0x091e4b56
--   (56,4b,1e,09)
-- ghci> inv_littleendian' 0xfaffffff
--   (ff,ff,ff,fa)

-----------------------------
-- Marshalling and Unmarshalling seems unavoidable
-----------------------------

w8x4toW32 :: Quad W8 -> W32    -- (W8,W8,W8,W8) -> W32
w8x4toW32 (W8 a31 a30 a29 a28 a27 a26 a25 a24, W8 a23 a22 a21 a20 a19 a18 a17 a16,
           W8 a15 a14 a13 a12 a11 a10 a9 a8,   W8 a7 a6 a5 a4 a3 a2 a1 a0)
                   = W32 a31 a30 a29 a28 a27 a26 a25 a24 a23 a22 a21 a20 a19 a18 a17 a16
                         a15 a14 a13 a12 a11 a10  a9  a8  a7  a6  a5  a4  a3  a2  a1  a0

w32toW8x4 :: W32 -> Quad W8   -- (W8,W8,W8,W8)
w32toW8x4 (W32 a31 a30 a29 a28 a27 a26 a25 a24 a23 a22 a21 a20 a19 a18 a17 a16
               a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 a0)
                = (W8 a31 a30 a29 a28 a27 a26 a25 a24, W8 a23 a22 a21 a20 a19 a18 a17 a16,
                   W8 a15 a14 a13 a12 a11 a10 a9 a8,   W8 a7 a6 a5 a4 a3 a2 a1 a0)

split_w8x64_w32x16 :: X64 W8 -> Hex W32
split_w8x64_w32x16 (X64  x_0  x_1  x_2  x_3  x_4  x_5  x_6  x_7  x_8  x_9 x_10 x_11 x_12 x_13 x_14 x_15
                x_16 x_17 x_18 x_19 x_20 x_21 x_22 x_23 x_24 x_25 x_26 x_27 x_28 x_29 x_30 x_31
                x_32 x_33 x_34 x_35 x_36 x_37 x_38 x_39 x_40 x_41 x_42 x_43 x_44 x_45 x_46 x_47
                x_48 x_49 x_50 x_51 x_52 x_53 x_54 x_55 x_56 x_57 x_58 x_59 x_60 x_61 x_62 x_63)
        = (    w8x4toW32 (x_0, x_1, x_2, x_3), w8x4toW32 (x_4, x_5, x_6, x_7),     w8x4toW32 (x_8, x_9, x_10, x_11),
           w8x4toW32 (x_12, x_13, x_14, x_15), w8x4toW32 (x_16, x_17, x_18, x_19), w8x4toW32 (x_20, x_21, x_22, x_23),
           w8x4toW32 (x_24, x_25, x_26, x_27), w8x4toW32 (x_28, x_29, x_30, x_31), w8x4toW32 (x_32, x_33, x_34, x_35),
           w8x4toW32 (x_36, x_37, x_38, x_39), w8x4toW32 (x_40, x_41, x_42, x_43), w8x4toW32 (x_44, x_45, x_46, x_47),
           w8x4toW32 (x_48, x_49, x_50, x_51), w8x4toW32 (x_52, x_53, x_54, x_55), w8x4toW32 (x_56, x_57, x_58, x_59),
           w8x4toW32 (x_60, x_61, x_62, x_63))

toW8x64 :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer ->
           Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer ->
           Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer ->
           Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer ->
           Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer ->
           Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer ->
           Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer ->
           Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer ->
           X64 W8
toW8x64 x0  x1  x2  x3  x4  x5  x6  x7  x8  x9 x10 x11 x12 x13 x14 x15
        x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
        x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
        x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63
          = X64 (toW8 x0) (toW8 x1) (toW8 x2) (toW8 x3) (toW8 x4) (toW8 x5) (toW8 x6) (toW8 x7) (toW8 x8) (toW8 x9) (toW8 x10) (toW8 x11) (toW8 x12) (toW8 x13) (toW8 x14) (toW8 x15
            ) (toW8 x16) (toW8 x17) (toW8 x18) (toW8 x19) (toW8 x20) (toW8 x21) (toW8 x22) (toW8 x23) (toW8 x24) (toW8 x25) (toW8 x26) (toW8 x27) (toW8 x28) (toW8 x29) (toW8 x30) (toW8 x31
            ) (toW8 x32) (toW8 x33) (toW8 x34) (toW8 x35) (toW8 x36) (toW8 x37) (toW8 x38) (toW8 x39) (toW8 x40) (toW8 x41) (toW8 x42) (toW8 x43) (toW8 x44) (toW8 x45) (toW8 x46) (toW8 x47
            ) (toW8 x48) (toW8 x49) (toW8 x50) (toW8 x51) (toW8 x52) (toW8 x53) (toW8 x54) (toW8 x55) (toW8 x56) (toW8 x57) (toW8 x58) (toW8 x59) (toW8 x60) (toW8 x61) (toW8 x62) (toW8 x63)

back :: Hex W32 -> X64 W8
back ( w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15)
         = (X64  x_0  x_1  x_2  x_3  x_4  x_5  x_6  x_7  x_8  x_9 x_10 x_11 x_12 x_13 x_14 x_15
                   x_16 x_17 x_18 x_19 x_20 x_21 x_22 x_23 x_24 x_25 x_26 x_27 x_28 x_29 x_30 x_31
                   x_32 x_33 x_34 x_35 x_36 x_37 x_38 x_39 x_40 x_41 x_42 x_43 x_44 x_45 x_46 x_47
                   x_48 x_49 x_50 x_51 x_52 x_53 x_54 x_55 x_56 x_57 x_58 x_59 x_60 x_61 x_62 x_63)
   where
      (x_0, x_1, x_2, x_3)     = w32toW8x4 w0
      (x_4, x_5, x_6, x_7)     = w32toW8x4 w1
      (x_8, x_9, x_10, x_11)   = w32toW8x4 w2
      (x_12, x_13, x_14, x_15) = w32toW8x4 w3
      (x_16, x_17, x_18, x_19) = w32toW8x4 w4
      (x_20, x_21, x_22, x_23) = w32toW8x4 w5
      (x_24, x_25, x_26, x_27) = w32toW8x4 w6
      (x_28, x_29, x_30, x_31) = w32toW8x4 w7
      (x_32, x_33, x_34, x_35) = w32toW8x4 w8
      (x_36, x_37, x_38, x_39) = w32toW8x4 w9
      (x_40, x_41, x_42, x_43) = w32toW8x4 w10
      (x_44, x_45, x_46, x_47) = w32toW8x4 w11
      (x_48, x_49, x_50, x_51) = w32toW8x4 w12
      (x_52, x_53, x_54, x_55) = w32toW8x4 w13
      (x_56, x_57, x_58, x_59) = w32toW8x4 w14
      (x_60, x_61, x_62, x_63) = w32toW8x4 w15
     
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
     dr10 = doubleround P.. doubleround P.. doubleround P.. doubleround P.. doubleround P..
              doubleround P.. doubleround P.. doubleround P.. doubleround P.. doubleround 

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

-- | N.b., the Show function for W8 will present numbers in hexadecimal, but
-- | Bernstein's examples use base 10. Can lead to confusion. Unfortunately,
-- | Bernstein's spec uses both base 10 and 16 for his examples. Leading to
-- | more confusion possibly.

toW8x4 :: (Integer, Integer, Integer, Integer) -> (W8, W8, W8, W8)
toW8x4 (i, j, k, l) = (toW8 i, toW8 j, toW8 k, toW8 l)

toW8x16 :: (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer,
            Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer) -> Hex W8
toW8x16 (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16)
          = (toW8 i1, toW8 i2, toW8 i3, toW8 i4, toW8 i5, toW8 i6, toW8 i7, toW8 i8,
             toW8 i9, toW8 i10, toW8 i11, toW8 i12, toW8 i13, toW8 i14, toW8 i15, toW8 i16)

sigma0, sigma1, sigma2, sigma3 :: Quad W8 
sigma0 = toW8x4 (101, 120, 112,  97)
sigma1 = toW8x4 (110, 100,  32,  51)
sigma2 = toW8x4 ( 50,  45,  98, 121)
sigma3 = toW8x4 (116, 101,  32, 107)

tau0 , tau1, tau2, tau3 :: Quad W8        
tau0   = toW8x4 (101, 120, 112,  97)
tau1   = toW8x4 (110, 100,  32,  49)
tau2   = toW8x4 ( 54,  45,  98, 121)
tau3   = toW8x4 (116, 101,  32, 107)

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
salsa20_256 :: Hex W8 -> Hex W8 -> Hex W8 -> X64 W8
salsa20_256 k0 k1 n = hash_salsa20 (expand sigma0 sigma1 sigma2 sigma3 (k0, k1, n))

salsa20_128 :: Hex W8 -> Hex W8 -> X64 W8
salsa20_128 k n     = hash_salsa20 (expand tau0 tau1 tau2 tau3 (k, k, n))

k0, k1, n :: Hex W8
k0 = toW8x16 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
k1 = toW8x16 (201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216)
n  = toW8x16 (101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116)

exp1, exp1_ans, exp2, exp2_ans :: X64 W8
exp1 = salsa20_256 k0 k1 n
exp2 = salsa20_128 k0 n

-- | cut and pasted from Bernstein.
exp1_ans = toW8x64  69 37 68 39 41 15  107  193  255  139  122  6  170  233  217  98
                    89  144  182  106   21  51  200  65  239  49  222  34  215  114  40  126
                    104  197  7  225 197 153  31  2 102  78  76 176  84 245 246 184
                    177 160 133 130  6  72 149 119 192 195 132 236 234 103 246  74

-- ghci> exp1
--     (69, 37, 68, 39, 41, 15, 107, 193, 255, 139, 122, 6, 170, 233, 217, 98,
--      89, 144, 182, 106, 21, 51, 200, 65, 239, 49, 222, 34, 215, 114, 40, 126,
--      104, 197, 7, 225, 197, 153, 31, 2, 102, 78, 76, 176, 84, 245, 246, 184,
--      177, 160, 133, 130, 6, 72, 149, 119, 192, 195, 132, 236, 234, 103, 246, 74)
-- ghci> exp1 == exp1_ans
--         True

exp2_ans = toW8x64 39 173  46 248  30 200  82  17  48  67 254 239  37  18  13 247
                   241 200  61 144  10  55  50 185  6  47 246 253 143  86 187 225
                   134  85 110 246 161 163  43 235 231  94 171  51 145 214 112  29 
                   14 232  5  16 151 140 183 141 171  9 122 181 104 182 177 193

-- ghci> exp2
--   (39, 173, 46, 248, 30, 200, 82, 17, 48, 67, 254, 239, 37, 18, 13, 247, 241, 200, 61, 144, 10, 55, 50, 185, 6, 47, 246, 253, 143, 86, 187, 225, 134, 85, 110, 246, 161, 163, 43, 235, 231, 94, 171, 51, 145, 214, 112, 29, 14, 232, 5, 16, 151, 140, 183, 141, 171, 9, 122, 181, 104, 182, 177, 193)
-- ghci> exp2_ans
--   (39, 173, 46, 248, 30, 200, 82, 17, 48, 67, 254, 239, 37, 18, 13, 247, 241, 200, 61, 144, 10, 55, 50, 185, 6, 47, 246, 253, 143, 86, 187, 225, 134, 85, 110, 246, 161, 163, 43, 235, 231, 94, 171, 51, 145, 214, 112, 29, 14, 232, 5, 16, 151, 140, 183, 141, 171, 9, 122, 181, 104, 182, 177, 193)
-- ghci> exp2 == exp2_ans
--   True

-- |
-- | Section 10: Salsa Encryption Function
-- |

-- |
-- | Some comments untangling Bernstein's notation:
-- |
-- 
-- OK, so let me see if I get this. In Salsa20_k(v)
--    * Salsa_k(v) is being defined as a 2^70 length sequence of bytes
--    * each of Salsa_k(v,\underline{j}) is a 64 (=2^6) length sequence of bytes
-- 
-- Therefore, Salsa_k is being overloaded here in 1 and 2.
-- Because there are 2^64 of Item 2's, each of which is 2^6, then Salsa_k(v) has 2^70 = 2^64 * 2^6 bytes total.
-- 
-- Anyhoo, then as there are used in Section 10,
--       (_ , _) :: Oct W8 -> Oct W8 -> W8x16
--       \underscore{_} :: Integer -> Oct W8 (edited) 
--
-- So, \underscore{1} = (0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1), for example

-- |
-- | This is the infamous (_) function from Section 10. 
-- |
factor :: Integer -> Oct W8
factor i = (toW8 i0 , toW8 i1, toW8 i2, toW8 i3, toW8 i4, toW8 i5, toW8 i6, toW8 i7)
  where
    i0, i1, i2, i3, i4, i5, i6, i7 :: Integer
    (q0 , i0) = P.quotRem i (2 P.^ 8)
    (q1 , i1) = P.quotRem q0 (2 P.^ 8)
    (q2 , i2) = P.quotRem q1 (2 P.^ 8)
    (q3 , i3) = P.quotRem q2 (2 P.^ 8)
    (q4 , i4) = P.quotRem q3 (2 P.^ 8)
    (q5 , i5) = P.quotRem q4 (2 P.^ 8)
    (q6 , i6) = P.quotRem q5 (2 P.^ 8)
    (_  , i7) = P.quotRem q6 (2 P.^ 8)

i_mod_64 :: X64 W8 -> Integer -> W8
i_mod_64 (X64   a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
                a40 a41 a42 a43 a50 a51 a52 a53 a60 a61 a62 a63 a70 a71 a72 a73
                a80 a81 a82 a83 a90 a91 a92 a93 aa0 aa1 aa2 aa3 ab0 ab1 ab2 ab3
                ac0 ac1 ac2 ac3 ad0 ad1 ad2 ad3 ae0 ae1 ae2 ae3 af0 af1 af2 af3)
     i = args P.!! (P.fromIntegral i `P.mod` 64)
  where
      args = [a00,a01,a02,a03,a10,a11,a12,a13,a20,a21,a22,a23,a30,a31,a32,a33
             ,a40,a41,a42,a43,a50,a51,a52,a53,a60,a61,a62,a63,a70,a71,a72,a73
             ,a80,a81,a82,a83,a90,a91,a92,a93,aa0,aa1,aa2,aa3,ab0,ab1,ab2,ab3
             ,ac0,ac1,ac2,ac3,ad0,ad1,ad2,ad3,ae0,ae1,ae2,ae3,af0,af1,af2,af3]

encrypt128 :: Hex W8 -> Oct W8 -> [W8] -> [W8]
encrypt128 k v m = [ mj ^ (i_mod_64 (salsa20_128 k (splice v (factor j))) j) | (mj , j) <- P.zip m ls ]
   where
     ls = [0..(2 P.^ 70)]
     -- i_mod_64 :: W8x64 -> Integer -> W8

encrypt256 :: Hex W8 -> Hex W8 -> Oct W8 -> [W8] -> [W8]
encrypt256 k0 k1 v m = [ mj ^ (i_mod_64 (salsa20_256 k0 k1 (splice v (factor j))) j) | (mj , j) <- P.zip m ls ]
   where
     ls = [0..(2 P.^ 70)]
     -- i_mod_64 :: W8x64 -> Integer -> W8

{-
-- | Q: What are encrypt128 and encrypt256?
-- | A: They *are* the reference specifications for Salsa20 encrypt/decrypt functions.
-- | A: They are *not* ReWire code.
-}
