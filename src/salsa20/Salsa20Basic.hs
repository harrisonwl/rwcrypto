{-# LANGUAGE DataKinds #-}
module Salsa20Basic (Oct, Hex, pi64, splice, Quad, X16(..), X64(..)) where

import Prelude hiding ((==))
import ReWire
import ReWire.Bits (lit , (==))

-- | When you get into larger tuples, the Haskell Prelude doesn't
-- | provide class instances. So, one has to roll-your-own.

data X16 a = X16 a a a a a a a a a a a a a a a a 

type Quad a = (a , a , a , a)
type Oct a  = (a , a , a , a , a , a , a , a)
type Hex a  = X16 a
    
data X64 a  = X64 a a a a a a a a a a a a a a a a
                  a a a a a a a a a a a a a a a a
                  a a a a a a a a a a a a a a a a
                  a a a a a a a a a a a a a a a a 

splice :: Oct a -> Oct a -> Hex a
splice (b0, b1, b2, b3, b4, b5, b6, b7) (b8, b9, b10, b11, b12, b13, b14, b15)
   = X16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15

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
          | otherwise     = a3f

