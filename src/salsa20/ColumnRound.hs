{-# LANGUAGE DataKinds #-}
module ColumnRound (columnround , alltests) where

import ReWire
import ReWire.Bits hiding ((==))
import Idioms (X16(..), Hex)
import QuarterRound(quarterround)


-----------------------------
-- The columnround function from page 4 of
-- Bernstein's "Salsa20 Specification"
-----------------------------

columnround :: Hex (W 32) -> Hex (W 32)
columnround (X16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)
      = X16 y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15
     where
        y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y13, y14, y15 :: W 32
        ( y0,  y4,  y8, y12) = quarterround (x0, x4, x8, x12)
        ( y5,  y9, y13,  y1) = quarterround (x5, x9, x13, x1)
        (y10, y14,  y2,  y6) = quarterround (x10, x14, x2, x6)
        (y15,  y3,  y7, y11) = quarterround (x15, x3, x7, x11)


alltests :: [Bool]
alltests = [test1 , test2]

test1 , test2 :: Bool
test1 = columnround (X16
                       (lit 0x00000001) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000)
                       (lit 0x00000001) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000)
                       (lit 0x00000001) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000)
                       (lit 0x00000001) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000) )
                        == X16
                             (lit 0x10090288) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000)
                             (lit 0x00000101) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000)
                             (lit 0x00020401) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000)
                             (lit 0x40a04001) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000)


test2 = columnround (X16
                       (lit 0x08521bd6) (lit 0x1fe88837) (lit 0xbb2aa576) (lit 0x3aa26365)
                       (lit 0xc54c6a5b) (lit 0x2fc74c2f) (lit 0x6dd39cc3) (lit 0xda0a64f6)
                       (lit 0x90a2f23d) (lit 0x067f95a6) (lit 0x06b35f61) (lit 0x41e4732e)
                       (lit 0xe859c100) (lit 0xea4d84b7) (lit 0x0f619bff) (lit 0xbc6e965a) )
                        == X16
                             (lit 0x8c9d190a) (lit 0xce8e4c90) (lit 0x1ef8e9d3) (lit 0x1326a71a)
                             (lit 0x90a20123) (lit 0xead3c4f3) (lit 0x63a091a0) (lit 0xf0708d69)
                             (lit 0x789b010c) (lit 0xd195a681) (lit 0xeb7d5504) (lit 0xa774135c)
                             (lit 0x481c2027) (lit 0x53a8e4b5) (lit 0x4c1f89c5) (lit 0x3f78c9c8)

