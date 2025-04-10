module Idioms where

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

{-

ws = X64   0xa0 0xa1 0xa2 0xa3 0xa5 0xa5 0xa6 0xa7 0xa8 0xa9 0xaa 0xab 0xac 0xad 0xae 0xaf
           0xb0 0xb1 0xb2 0xb3 0xb5 0xb5 0xb6 0xb7 0xb8 0xb9 0xba 0xbb 0xbc 0xbd 0xbe 0xbf
           0xc0 0xc1 0xc2 0xc3 0xc5 0xc5 0xc6 0xc7 0xc8 0xc9 0xca 0xcb 0xcc 0xcd 0xce 0xcf
           0xd0 0xd1 0xd2 0xd3 0xd5 0xd5 0xd6 0xd7 0xd8 0xd9 0xda 0xdb 0xdc 0xdd 0xde 0xdf

urf = [(i, args !! (fromIntegral i `mod` 64)) |  i <- [0..2^70]]
  where
    args = "*abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789*"    
    -- args = [0xa0,0xa1,0xa2,0xa3,0xa5,0xa5,0xa6,0xa7,0xa8,0xa9,0xaa,0xab,0xac,0xad,0xae,0xaf,
    --         0xb0,0xb1,0xb2,0xb3,0xb5,0xb5,0xb6,0xb7,0xb8,0xb9,0xba,0xbb,0xbc,0xbd,0xbe,0xbf,
    --         0xc0,0xc1,0xc2,0xc3,0xc5,0xc5,0xc6,0xc7,0xc8,0xc9,0xca,0xcb,0xcc,0xcd,0xce,0xcf,
    --         0xd0,0xd1,0xd2,0xd3,0xd5,0xd5,0xd6,0xd7,0xd8,0xd9,0xda,0xdb,0xdc,0xdd,0xde,0xdf]


plus64 :: X64 W8 -> X64 W8 -> X64 W8
plus64 (X64   a63 a62 a61 a60 a59 a58 a57 a56 a55 a54 a53 a52 a51 a50 a49 a48
              a47 a46 a45 a44 a43 a42 a41 a40 a39 a38 a37 a36 a35 a34 a33 a32
              a31 a30 a29 a28 a27 a26 a25 a24 a23 a22 a21 a20 a19 a18 a17 a16
              a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 a0)
       (X64   b63 b62 b61 b60 b59 b58 b57 b56 b55 b54 b53 b52 b51 b50 b49 b48
              b47 b46 b45 b44 b43 b42 b41 b40 b39 b38 b37 b36 b35 b34 b33 b32
              b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16
              b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0)
                    = (X64   c63 c62 c61 c60 c59 c58 c57 c56 c55 c54 c53 c52 c51 c50 c49 c48
                             c47 c46 c45 c44 c43 c42 c41 c40 c39 c38 c37 c36 c35 c34 c33 c32
                             c31 c30 c29 c28 c27 c26 c25 c24 c23 c22 c21 c20 c19 c18 c17 c16
                             c15 c14 c13 c12 c11 c10 c9 c8 c7 c6 c5 c4 c3 c2 c1 c0)
  where
   (c0,ci1) = carryadd a0 b0 C
   (c1,ci2) = carryadd a1 b1 ci1
   (c2,ci3) = carryadd a2 b2 ci2
   (c3,ci4) = carryadd a3 b3 ci3
   (c4,ci5) = carryadd a4 b4 ci4
   (c5,ci6) = carryadd a5 b5 ci5
   (c6,ci7) = carryadd a6 b6 ci6
   (c7,ci8) = carryadd a7 b7 ci7
   (c8,ci9) = carryadd a8 b8 ci8
   (c9,ci10) = carryadd a9 b9 ci9
   (c10,ci11) = carryadd a10 b10 ci10
   (c11,ci12) = carryadd a11 b11 ci11
   (c12,ci13) = carryadd a12 b12 ci12
   (c13,ci14) = carryadd a13 b13 ci13
   (c14,ci15) = carryadd a14 b14 ci14
   (c15,ci16) = carryadd a15 b15 ci15
   (c16,ci17) = carryadd a16 b16 ci16
   (c17,ci18) = carryadd a17 b17 ci17
   (c18,ci19) = carryadd a18 b18 ci18
   (c19,ci20) = carryadd a19 b19 ci19
   (c20,ci21) = carryadd a20 b20 ci20
   (c21,ci22) = carryadd a21 b21 ci21
   (c22,ci23) = carryadd a22 b22 ci22
   (c23,ci24) = carryadd a23 b23 ci23
   (c24,ci25) = carryadd a24 b24 ci24
   (c25,ci26) = carryadd a25 b25 ci25
   (c26,ci27) = carryadd a26 b26 ci26
   (c27,ci28) = carryadd a27 b27 ci27
   (c28,ci29) = carryadd a28 b28 ci28
   (c29,ci30) = carryadd a29 b29 ci29
   (c30,ci31) = carryadd a30 b30 ci30
   (c31,ci32) = carryadd a31 b31 ci31
   (c32,ci33) = carryadd a32 b32 ci32
   (c33,ci34) = carryadd a33 b33 ci33
   (c34,ci35) = carryadd a34 b34 ci34
   (c35,ci36) = carryadd a35 b35 ci35
   (c36,ci37) = carryadd a36 b36 ci36
   (c37,ci38) = carryadd a37 b37 ci37
   (c38,ci39) = carryadd a38 b38 ci38
   (c39,ci40) = carryadd a39 b39 ci39
   (c40,ci41) = carryadd a40 b40 ci40
   (c41,ci42) = carryadd a41 b41 ci41
   (c42,ci43) = carryadd a42 b42 ci42
   (c43,ci44) = carryadd a43 b43 ci43
   (c44,ci45) = carryadd a44 b44 ci44
   (c45,ci46) = carryadd a45 b45 ci45
   (c46,ci47) = carryadd a46 b46 ci46
   (c47,ci48) = carryadd a47 b47 ci47
   (c48,ci49) = carryadd a48 b48 ci48
   (c49,ci50) = carryadd a49 b49 ci49
   (c50,ci51) = carryadd a50 b50 ci50
   (c51,ci52) = carryadd a51 b51 ci51
   (c52,ci53) = carryadd a52 b52 ci52
   (c53,ci54) = carryadd a53 b53 ci53
   (c54,ci55) = carryadd a54 b54 ci54
   (c55,ci56) = carryadd a55 b55 ci55
   (c56,ci57) = carryadd a56 b56 ci56
   (c57,ci58) = carryadd a57 b57 ci57
   (c58,ci59) = carryadd a58 b58 ci58
   (c59,ci60) = carryadd a59 b59 ci59
   (c60,ci61) = carryadd a60 b60 ci60
   (c61,ci62) = carryadd a61 b61 ci61
   (c62,ci63) = carryadd a62 b62 ci62
   (c63,ci64) = carryadd a63 b63 ci63

add64 :: X64 W8 -> X64 W8 -> X64 W8
add64 (X64   a63 a62 a61 a60 a59 a58 a57 a56 a55 a54 a53 a52 a51 a50 a49 a48
             a47 a46 a45 a44 a43 a42 a41 a40 a39 a38 a37 a36 a35 a34 a33 a32
             a31 a30 a29 a28 a27 a26 a25 a24 a23 a22 a21 a20 a19 a18 a17 a16
             a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 a0)
       (X64   b63 b62 b61 b60 b59 b58 b57 b56 b55 b54 b53 b52 b51 b50 b49 b48
              b47 b46 b45 b44 b43 b42 b41 b40 b39 b38 b37 b36 b35 b34 b33 b32
              b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16
              b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0)
                    = (X64   c63 c62 c61 c60 c59 c58 c57 c56 c55 c54 c53 c52 c51 c50 c49 c48
                             c47 c46 c45 c44 c43 c42 c41 c40 c39 c38 c37 c36 c35 c34 c33 c32
                             c31 c30 c29 c28 c27 c26 c25 c24 c23 c22 c21 c20 c19 c18 c17 c16
                             c15 c14 c13 c12 c11 c10 c9 c8 c7 c6 c5 c4 c3 c2 c1 c0)
  where
    c0 = a0 + b0
    c1 = a1 + b1
    c2 = a2 + b2
    c3 = a3 + b3
    c4 = a4 + b4
    c5 = a5 + b5
    c6 = a6 + b6
    c7 = a7 + b7
    c8 = a8 + b8
    c9 = a9 + b9
    c10 = a10 + b10
    c11 = a11 + b11
    c12 = a12 + b12
    c13 = a13 + b13
    c14 = a14 + b14
    c15 = a15 + b15
    c16 = a16 + b16
    c17 = a17 + b17
    c18 = a18 + b18
    c19 = a19 + b19
    c20 = a20 + b20
    c21 = a21 + b21
    c22 = a22 + b22
    c23 = a23 + b23
    c24 = a24 + b24
    c25 = a25 + b25
    c26 = a26 + b26
    c27 = a27 + b27
    c28 = a28 + b28
    c29 = a29 + b29
    c30 = a30 + b30
    c31 = a31 + b31
    c32 = a32 + b32
    c33 = a33 + b33
    c34 = a34 + b34
    c35 = a35 + b35
    c36 = a36 + b36
    c37 = a37 + b37
    c38 = a38 + b38
    c39 = a39 + b39
    c40 = a40 + b40
    c41 = a41 + b41
    c42 = a42 + b42
    c43 = a43 + b43
    c44 = a44 + b44
    c45 = a45 + b45
    c46 = a46 + b46
    c47 = a47 + b47
    c48 = a48 + b48
    c49 = a49 + b49
    c50 = a50 + b50
    c51 = a51 + b51
    c52 = a52 + b52
    c53 = a53 + b53
    c54 = a54 + b54
    c55 = a55 + b55
    c56 = a56 + b56
    c57 = a57 + b57
    c58 = a58 + b58
    c59 = a59 + b59
    c60 = a60 + b60
    c61 = a61 + b61
    c62 = a62 + b62
    c63 = a63 + b63
-}
