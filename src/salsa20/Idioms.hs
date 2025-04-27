{-# LANGUAGE DataKinds #-}
module Idioms where

import ReWire
import ReWire.Bits(lit, (+))
import ReWire.Finite (finite)
import qualified ReWire.FiniteComp as FC


foo :: W 6 -> [W 6]
foo w6 = w6 : foo (w6 ReWire.Bits.+ lit 1)


type Quad a = (a , a , a , a)
type Oct a  = (a , a , a , a , a , a , a , a)
--type Hex a  = X16 a a a a  a a a a  a a a a  a a a a
type Hex a  = X16 a

-- | When you get into larger tuples, the Haskell Prelude doesn't
-- | provide class instances. So, one has to roll-your-own.

-- data X16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae af
--   = X16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae af deriving (Eq , Show)

data X16 a = X16 a a a a a a a a a a a a a a a a deriving (Eq , Show)

instance Functor X16 where
   fmap f (X16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae af)
     = X16 (f a0) (f a1) (f a2) (f a3) (f a4) (f a5) (f a6) (f a7) (f a8) (f a9) (f aa) (f ab) (f ac) (f ad) (f ae) (f af)

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
    
data X64 a  = X64 a a a a a a a a a a a a a a a a
                  a a a a a a a a a a a a a a a a
                  a a a a a a a a a a a a a a a a
                  a a a a a a a a a a a a a a a a deriving Eq

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

splice :: Oct a -> Oct a -> Hex a
splice (b0, b1, b2, b3, b4, b5, b6, b7) (b8, b9, b10, b11, b12, b13, b14, b15)
   = X16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15
  
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
--      vshow :: W8 -> String
--      vshow = show . val
      tupshow :: Show a => [a] -> String
      tupshow l = "(" ++ foldr1 (\ x xs -> x ++ ", " ++ xs) (map show l) ++ ")"

proj64 :: X64 a -> Finite 64 -> a
proj64 (X64 a00 a01 a02 a03 a04 a05 a06 a07
            a08 a09 a0a a0b a0c a0d a0e a0f
            a10 a11 a12 a13 a14 a15 a16 a17
            a18 a19 a1a a1b a1c a1d a1e a1f
            a20 a21 a22 a23 a24 a25 a26 a27
            a28 a29 a2a a2b a2c a2d a2e a2f
            a30 a31 a32 a33 a34 a35 a36 a37
            a38 a39 a3a a3b a3c a3d a3e a3f)
        i | i FC.== finite 0x00 = a00
          | i FC.== finite 0x01 = a01
          | i FC.== finite 0x02 = a02
          | i FC.== finite 0x03 = a03
          | i FC.== finite 0x04 = a04
          | i FC.== finite 0x05 = a05
          | i FC.== finite 0x06 = a06
          | i FC.== finite 0x07 = a07
          | i FC.== finite 0x08 = a08
          | i FC.== finite 0x09 = a09
          | i FC.== finite 0x0a = a0a
          | i FC.== finite 0x0b = a0b
          | i FC.== finite 0x0c = a0c
          | i FC.== finite 0x0d = a0d
          | i FC.== finite 0x0e = a0e
          | i FC.== finite 0x0f = a0f
          | i FC.== finite 0x10 = a10
          | i FC.== finite 0x11 = a11
          | i FC.== finite 0x12 = a12
          | i FC.== finite 0x13 = a13
          | i FC.== finite 0x14 = a14
          | i FC.== finite 0x15 = a15
          | i FC.== finite 0x16 = a16
          | i FC.== finite 0x17 = a17
          | i FC.== finite 0x18 = a18
          | i FC.== finite 0x19 = a19
          | i FC.== finite 0x1a = a1a
          | i FC.== finite 0x1b = a1b
          | i FC.== finite 0x1c = a1c
          | i FC.== finite 0x1d = a1d
          | i FC.== finite 0x1e = a1e
          | i FC.== finite 0x1f = a1f
          | i FC.== finite 0x20 = a20
          | i FC.== finite 0x21 = a21
          | i FC.== finite 0x22 = a22
          | i FC.== finite 0x23 = a23
          | i FC.== finite 0x24 = a24
          | i FC.== finite 0x25 = a25
          | i FC.== finite 0x26 = a26
          | i FC.== finite 0x27 = a27
          | i FC.== finite 0x28 = a28
          | i FC.== finite 0x29 = a29
          | i FC.== finite 0x2a = a2a
          | i FC.== finite 0x2b = a2b
          | i FC.== finite 0x2c = a2c
          | i FC.== finite 0x2d = a2d
          | i FC.== finite 0x2e = a2e
          | i FC.== finite 0x2f = a2f
          | i FC.== finite 0x30 = a30
          | i FC.== finite 0x31 = a31
          | i FC.== finite 0x32 = a32
          | i FC.== finite 0x33 = a33
          | i FC.== finite 0x34 = a34
          | i FC.== finite 0x35 = a35
          | i FC.== finite 0x36 = a36
          | i FC.== finite 0x37 = a37
          | i FC.== finite 0x38 = a38
          | i FC.== finite 0x39 = a39
          | i FC.== finite 0x3a = a3a
          | i FC.== finite 0x3b = a3b
          | i FC.== finite 0x3c = a3c
          | i FC.== finite 0x3d = a3d
          | i FC.== finite 0x3e = a3e
--          | i FC.== finite 0x3f = a3f
          | otherwise           = a3f


pi64 :: X64 a -> W 6 -> a
pi64 (X64 a00 a01 a02 a03 a04 a05 a06 a07 a08 a09 a0a a0b a0c a0d a0e a0f
          a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a1a a1b a1c a1d a1e a1f
          a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a2a a2b a2c a2d a2e a2f
          a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a3a a3b a3c a3d a3e a3f)
        i | i == lit 0x00 = a00
          | i == lit 0x01 = a01
          | i == lit 0x02 = a02
          | i == lit 0x03 = a03
          | i == lit 0x04 = a04
          | i == lit 0x05 = a05
          | i == lit 0x06 = a06
          | i == lit 0x07 = a07
          | i == lit 0x08 = a08
          | i == lit 0x09 = a09
          | i == lit 0x0a = a0a
          | i == lit 0x0b = a0b
          | i == lit 0x0c = a0c
          | i == lit 0x0d = a0d
          | i == lit 0x0e = a0e
          | i == lit 0x0f = a0f
          | i == lit 0x10 = a10
          | i == lit 0x11 = a11
          | i == lit 0x12 = a12
          | i == lit 0x13 = a13
          | i == lit 0x14 = a14
          | i == lit 0x15 = a15
          | i == lit 0x16 = a16
          | i == lit 0x17 = a17
          | i == lit 0x18 = a18
          | i == lit 0x19 = a19
          | i == lit 0x1a = a1a
          | i == lit 0x1b = a1b
          | i == lit 0x1c = a1c
          | i == lit 0x1d = a1d
          | i == lit 0x1e = a1e
          | i == lit 0x1f = a1f
          | i == lit 0x20 = a20
          | i == lit 0x21 = a21
          | i == lit 0x22 = a22
          | i == lit 0x23 = a23
          | i == lit 0x24 = a24
          | i == lit 0x25 = a25
          | i == lit 0x26 = a26
          | i == lit 0x27 = a27
          | i == lit 0x28 = a28
          | i == lit 0x29 = a29
          | i == lit 0x2a = a2a
          | i == lit 0x2b = a2b
          | i == lit 0x2c = a2c
          | i == lit 0x2d = a2d
          | i == lit 0x2e = a2e
          | i == lit 0x2f = a2f
          | i == lit 0x30 = a30
          | i == lit 0x31 = a31
          | i == lit 0x32 = a32
          | i == lit 0x33 = a33
          | i == lit 0x34 = a34
          | i == lit 0x35 = a35
          | i == lit 0x36 = a36
          | i == lit 0x37 = a37
          | i == lit 0x38 = a38
          | i == lit 0x39 = a39
          | i == lit 0x3a = a3a
          | i == lit 0x3b = a3b
          | i == lit 0x3c = a3c
          | i == lit 0x3d = a3d
          | i == lit 0x3e = a3e
--          | i == lit 0x3f = a3f
          | otherwise           = a3f

