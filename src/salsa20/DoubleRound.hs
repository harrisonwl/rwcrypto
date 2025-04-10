{-# LANGUAGE DataKinds #-}
module DoubleRound (doubleround , alltests) where

import ReWire
import ReWire.Bits hiding ((==))
import Idioms (X16(..), Hex)
import RowRound(rowround)
import ColumnRound(columnround)

-----------------------------
-- The doubleround function from page 5 of
-- Bernstein's "Salsa20 Specification"
-----------------------------

doubleround :: Hex (W 32) -> Hex (W 32)
doubleround = rowround . columnround

alltests :: [Bool]
alltests = [test1 , test2]

test1 , test2 :: Bool
test1 = doubleround
           (X16
              (lit 0x00000001) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000)
              (lit 0x00000000) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000)
              (lit 0x00000000) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000)
              (lit 0x00000000) (lit 0x00000000) (lit 0x00000000) (lit 0x00000000) )
              == X16
                   (lit 0x8186a22d) (lit 0x0040a284) (lit 0x82479210) (lit 0x06929051)
                   (lit 0x08000090) (lit 0x02402200) (lit 0x00004000) (lit 0x00800000)
                   (lit 0x00010200) (lit 0x20400000) (lit 0x08008104) (lit 0x00000000)
                   (lit 0x20500000) (lit 0xa0000040) (lit 0x0008180a) (lit 0x612a8020)

test2 = doubleround
           (X16
              (lit 0xde501066) (lit 0x6f9eb8f7) (lit 0xe4fbbd9b) (lit 0x454e3f57)
              (lit 0xb75540d3) (lit 0x43e93a4c) (lit 0x3a6f2aa0) (lit 0x726d6b36)
              (lit 0x9243f484) (lit 0x9145d1e8) (lit 0x4fa9d247) (lit 0xdc8dee11)
              (lit 0x054bf545) (lit 0x254dd653) (lit 0xd9421b6d) (lit 0x67b276c1) )
              == X16
                   (lit 0xccaaf672) (lit 0x23d960f7) (lit 0x9153e63a) (lit 0xcd9a60d0)
                   (lit 0x50440492) (lit 0xf07cad19) (lit 0xae344aa0) (lit 0xdf4cfdfc)
                   (lit 0xca531c29) (lit 0x8e7943db) (lit 0xac1680cd) (lit 0xd503ca00)
                   (lit 0xa74b2ad6) (lit 0xbc331c5c) (lit 0x1dda24c7) (lit 0xee928277)
                   
