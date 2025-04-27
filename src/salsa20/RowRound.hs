{-# LANGUAGE DataKinds #-}
module RowRound(rowround , alltests) where

import ReWire
-- import ReWire.Bits hiding ((==))
import Idioms (X16(..), Hex, x16)
import QuarterRound(quarterround)
-- import Data.Bool (Bool)

-----------------------------
-- The rowround function from page 3 of
-- Bernstein's "Salsa20 Specification"
-----------------------------

rowround :: Hex (W 32) -> Hex (W 32)
rowround (X16 y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15)
   = X16 z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15
     where
       ( z0,  z1,  z2,  z3) = quarterround ( y0,  y1,  y2,  y3)
       ( z5,  z6,  z7,  z4) = quarterround ( y5,  y6,  y7,  y4)
       (z10, z11,  z8,  z9) = quarterround (y10, y11,  y8,  y9)
       (z15, z12, z13, z14) = quarterround (y15, y12, y13, y14)


alltests :: [Bool]
alltests = [test1 , test2]

i1 , o1 , i2 , o2 :: X16 (W 32)
i1 = x16
        0x00000001 0x00000000 0x00000000 0x00000000
        0x00000001 0x00000000 0x00000000 0x00000000
        0x00000001 0x00000000 0x00000000 0x00000000
        0x00000001 0x00000000 0x00000000 0x00000000

o1 = x16
        0x08008145 0x00000080 0x00010200 0x20500000
        0x20100001 0x00048044 0x00000080 0x00010000
        0x00000001 0x00002000 0x80040000 0x00000000
        0x00000001 0x00000200 0x00402000 0x88000100

i2 = x16
        0x08521bd6 0x1fe88837 0xbb2aa576 0x3aa26365
        0xc54c6a5b 0x2fc74c2f 0x6dd39cc3 0xda0a64f6
        0x90a2f23d 0x067f95a6 0x06b35f61 0x41e4732e
        0xe859c100 0xea4d84b7 0x0f619bff 0xbc6e965a

o2 = x16
        0xa890d39d 0x65d71596 0xe9487daa 0xc8ca6a86
        0x949d2192 0x764b7754 0xe408d9b9 0x7a41b4d1
        0x3402e183 0x3c3af432 0x50669f96 0xd89ef0a8
        0x0040ede5 0xb545fbce 0xd257ed4f 0x1818882d

test1 , test2 :: Bool

test1 = rowround i1 == o1

test2 = rowround i2 == o2

-- test1 = rowround (X16
--                     (lit 0x00000001) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000)
--                     (lit 0x00000001) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000)
--                     (lit 0x00000001) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000)
--                     (lit 0x00000001) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000) )
--                    == X16
--                         (lit 0x08008145) (lit 0x00000080) (lit 0x00010200) (lit 0x20500000)
--                         (lit 0x20100001) (lit 0x00048044) (lit 0x00000080) (lit 0x00010000)
--                         (lit 0x00000001) (lit 0x00002000) (lit 0x80040000) (lit 0x00000000)
--                         (lit 0x00000001) (lit 0x00000200) (lit 0x00402000) (lit 0x88000100)
        
-- test2 = rowround (X16
--                     (lit 0x08521bd6) (lit 0x1fe88837) (lit 0xbb2aa576) (lit 0x3aa26365)
--                     (lit 0xc54c6a5b) (lit 0x2fc74c2f) (lit 0x6dd39cc3) (lit 0xda0a64f6)
--                     (lit 0x90a2f23d) (lit 0x067f95a6) (lit 0x06b35f61) (lit 0x41e4732e)
--                     (lit 0xe859c100) (lit 0xea4d84b7) (lit 0x0f619bff) (lit 0xbc6e965a) )
--                    == X16
--                         (lit 0xa890d39d) (lit 0x65d71596) (lit 0xe9487daa) (lit 0xc8ca6a86)
--                         (lit 0x949d2192) (lit 0x764b7754) (lit 0xe408d9b9) (lit 0x7a41b4d1)
--                         (lit 0x3402e183) (lit 0x3c3af432) (lit 0x50669f96) (lit 0xd89ef0a8)
--                         (lit 0x0040ede5) (lit 0xb545fbce) (lit 0xd257ed4f) (lit 0x1818882d)

