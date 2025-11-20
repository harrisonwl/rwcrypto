{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Salsa20.Testing (x16, x64) where

import ReWire
import ReWire.Bits(lit)
import Salsa20.Salsa20Basic
-- import Data.Bool (Bool)

-- | When you get into larger tuples, the Haskell Prelude doesn't
-- | provide class instances. So, one has to roll-your-own.

instance Functor X16 where
   fmap f (X16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae af)
     = X16 (f a0) (f a1) (f a2) (f a3) (f a4) (f a5) (f a6) (f a7) (f a8) (f a9) (f aa) (f ab) (f ac) (f ad) (f ae) (f af)

instance Eq a => Eq (X16 a) where
   (X16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae af)
      == (X16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf)
     = a0 == b0 && a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7
       && a8 == b8 && a9 == b9 && aa == ba && ab == bb && ac == bc && ad == bd && ae == be && af == bf

instance Functor X64 where
  fmap f (X64 a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
              a40 a41 a42 a43 a50 a51 a52 a53 a60 a61 a62 a63 a70 a71 a72 a73
              a80 a81 a82 a83 a90 a91 a92 a93 aa0 aa1 aa2 aa3 ab0 ab1 ab2 ab3
              ac0 ac1 ac2 ac3 ad0 ad1 ad2 ad3 ae0 ae1 ae2 ae3 af0 af1 af2 af3 ) =
    X64 (f a00) (f a01) (f a02) (f a03) (f a10) (f a11) (f a12) (f a13)
        (f a20) (f a21) (f a22) (f a23) (f a30) (f a31) (f a32) (f a33)
        (f a40) (f a41) (f a42) (f a43) (f a50) (f a51) (f a52) (f a53)
        (f a60) (f a61) (f a62) (f a63) (f a70) (f a71) (f a72) (f a73)
        (f a80) (f a81) (f a82) (f a83) (f a90) (f a91) (f a92) (f a93)
        (f aa0) (f aa1) (f aa2) (f aa3) (f ab0) (f ab1) (f ab2) (f ab3)
        (f ac0) (f ac1) (f ac2) (f ac3) (f ad0) (f ad1) (f ad2) (f ad3)
        (f ae0) (f ae1) (f ae2) (f ae3) (f af0) (f af1) (f af2) (f af3)

x16 :: KnownNat n =>
       Integer -> Integer -> Integer -> Integer ->
        Integer -> Integer -> Integer -> Integer ->
         Integer -> Integer -> Integer -> Integer ->
          Integer -> Integer -> Integer -> Integer -> X16 (W n)
x16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae af = fmap ReWire.Bits.lit (X16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae af)

x64 :: KnownNat n =>
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       Integer -> Integer -> Integer -> Integer ->
       X64 (W n)
x64 a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
    a40 a41 a42 a43 a50 a51 a52 a53 a60 a61 a62 a63 a70 a71 a72 a73
    a80 a81 a82 a83 a90 a91 a92 a93 aa0 aa1 aa2 aa3 ab0 ab1 ab2 ab3
    ac0 ac1 ac2 ac3 ad0 ad1 ad2 ad3 ae0 ae1 ae2 ae3 af0 af1 af2 af3
     = fmap ReWire.Bits.lit (X64 a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
                                 a40 a41 a42 a43 a50 a51 a52 a53 a60 a61 a62 a63 a70 a71 a72 a73
                                 a80 a81 a82 a83 a90 a91 a92 a93 aa0 aa1 aa2 aa3 ab0 ab1 ab2 ab3
                                 ac0 ac1 ac2 ac3 ad0 ad1 ad2 ad3 ae0 ae1 ae2 ae3 af0 af1 af2 af3)
  
instance Show a => Show (X64 a) where
  show (X64   a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
              a40 a41 a42 a43 a50 a51 a52 a53 a60 a61 a62 a63 a70 a71 a72 a73
              a80 a81 a82 a83 a90 a91 a92 a93 aa0 aa1 aa2 aa3 ab0 ab1 ab2 ab3
              ac0 ac1 ac2 ac3 ad0 ad1 ad2 ad3 ae0 ae1 ae2 ae3 af0 af1 af2 af3)
     = tupshow args
    where
      args = [a00,a01,a02,a03,a10,a11,a12,a13,a20,a21,a22,a23,a30,a31,a32,a33
             ,a40,a41,a42,a43,a50,a51,a52,a53,a60,a61,a62,a63,a70,a71,a72,a73
             ,a80,a81,a82,a83,a90,a91,a92,a93,aa0,aa1,aa2,aa3,ab0,ab1,ab2,ab3
             ,ac0,ac1,ac2,ac3,ad0,ad1,ad2,ad3,ae0,ae1,ae2,ae3,af0,af1,af2,af3]
      tupshow :: Show a => [a] -> String
      tupshow l = "(" ++ foldr1 (\ x xs -> x ++ ", " ++ xs) (map show l) ++ ")"
