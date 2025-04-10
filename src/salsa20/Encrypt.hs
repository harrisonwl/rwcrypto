{-# LANGUAGE DataKinds #-}
module Encrypt where

import Prelude hiding ((^))
import ReWire 
import ReWire.Bits (lit , (^))

import Idioms (Quad, Oct, Hex, X16(..), X64(..))
import Expansion(salsa20_k0k1)

-- |
-- | This is the infamous (_) function from Section 10. 
-- |
factor :: Integer -> Oct (W 8)
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

enc256 :: Hex (W 8) -> Hex (W 8) -> Oct (W 8) -> W 8 -> Integer -> W 8
enc256 k0 k1 v mj j = mj ^ (i_mod_64 (salsa20_k0k1 (k0 , k1) (splice v (factor j))) j)

splice :: Oct a -> Oct a -> Hex a
splice (b0, b1, b2, b3, b4, b5, b6, b7) (b8, b9, b10, b11, b12, b13, b14, b15)
       = (X16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) 

i_mod_64 :: X64 (W 8) -> Integer -> W 8
i_mod_64 (X64   a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
                a40 a41 a42 a43 a50 a51 a52 a53 a60 a61 a62 a63 a70 a71 a72 a73
                a80 a81 a82 a83 a90 a91 a92 a93 aa0 aa1 aa2 aa3 ab0 ab1 ab2 ab3
                ac0 ac1 ac2 ac3 ad0 ad1 ad2 ad3 ae0 ae1 ae2 ae3 af0 af1 af2 af3)
         i =
               args !! (fromIntegral i `mod` 64)
      where
         args = [a00,a01,a02,a03,a10,a11,a12,a13,a20,a21,a22,a23,a30,a31,a32,a33
                ,a40,a41,a42,a43,a50,a51,a52,a53,a60,a61,a62,a63,a70,a71,a72,a73
                ,a80,a81,a82,a83,a90,a91,a92,a93,aa0,aa1,aa2,aa3,ab0,ab1,ab2,ab3
                ,ac0,ac1,ac2,ac3,ad0,ad1,ad2,ad3,ae0,ae1,ae2,ae3,af0,af1,af2,af3]

encrypt256 :: Hex (W 8) -> Hex (W 8) -> Oct (W 8) -> [W 8] -> [W 8]
encrypt256 k0 k1 v m = [ mj ^ (i_mod_64 (salsa20_k0k1 (k0 , k1) (splice v (factor j))) j) | (mj , j) <- Prelude.zip m ls ]
   where
     ls = [0..1180591620717411303424] -- [0..2 ^ 70]
     splice :: Oct a -> Oct a -> Hex a
     splice (b0, b1, b2, b3, b4, b5, b6, b7) (b8, b9, b10, b11, b12, b13, b14, b15)
          = (X16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) 
     i_mod_64 :: X64 (W 8) -> Integer -> W 8
     i_mod_64 (X64   a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
                     a40 a41 a42 a43 a50 a51 a52 a53 a60 a61 a62 a63 a70 a71 a72 a73
                     a80 a81 a82 a83 a90 a91 a92 a93 aa0 aa1 aa2 aa3 ab0 ab1 ab2 ab3
                     ac0 ac1 ac2 ac3 ad0 ad1 ad2 ad3 ae0 ae1 ae2 ae3 af0 af1 af2 af3)
              i = args !! (fromIntegral i `mod` 64)
      where
         args = [a00,a01,a02,a03,a10,a11,a12,a13,a20,a21,a22,a23,a30,a31,a32,a33
                ,a40,a41,a42,a43,a50,a51,a52,a53,a60,a61,a62,a63,a70,a71,a72,a73
                ,a80,a81,a82,a83,a90,a91,a92,a93,aa0,aa1,aa2,aa3,ab0,ab1,ab2,ab3
                ,ac0,ac1,ac2,ac3,ad0,ad1,ad2,ad3,ae0,ae1,ae2,ae3,af0,af1,af2,af3]
