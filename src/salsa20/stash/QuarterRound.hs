module QuarterRound where

import Prelude hiding ((+) , (^))
import BinaryArithmetic -- ((+), (^))
import W32 
import Idioms

quarterround :: (W32 , W32 , W32 , W32) -> (W32 , W32 , W32 , W32)
quarterround (y0 , y1 , y2 , y3) = (z0 , z1 , z2 , z3)
  where
    z0 , z1 , z2 , z3 :: W32
    z1 = y1 ^ rotL 7 (y0 + y3)
    z2 = y2 ^ rotL 9 (z1 + y0) 
    z3 = y3 ^ rotL 13 (z2 + z1) 
    z0 = y0 ^ rotL 18 (z3 + z2) 

test1 , test2 , test3 , test4 , test5 , test6 , test7 :: Bool
test1 = quarterround i1 == o1
  where
    i1 , o1 :: Quad W32
    i1 = (0x00000000 , 0x00000000 , 0x00000000 , 0x00000000)
    o1 = (0x00000000 , 0x00000000 , 0x00000000 , 0x00000000)

test2 = quarterround i2 == o2
  where
    i2 , o2 :: Quad W32
    i2 = (0x00000001,0x00000000,0x00000000,0x00000000)
    o2 = (0x08008145,0x00000080,0x00010200,0x20500000)

-- tst2 :: Bool
-- tst2 = quarterround(0x00000001,0x00000000,0x00000000,0x00000000) == (0x08008145,0x00000080,0x00010200,0x20500000)

test3 = quarterround i3 == o3
  where
    i3 , o3 :: Quad W32
    i3 = (0x00000000,0x00000001,0x00000000,0x00000000)
    o3 = (0x88000100,0x00000001,0x00000200,0x00402000)
--                        (0x88000100,0x00000001,0x00000200,0x00402000)

test4 = quarterround(0x00000000,0x00000000,0x00000001,0x00000000)
              == (0x80040000,0x00000000,0x00000001,0x00002000)

test5 = quarterround(0x00000000,0x00000000,0x00000000,0x00000001)
              == (0x00048044,0x00000080,0x00010000,0x20100001)

test6 = quarterround(0xe7e8c006,0xc4f9417d,0x6479b4b2,0x68c67137)
              == (0xe876d72b,0x9361dfd5,0xf1460244,0x948541a3)

test7 = quarterround(0xd3917c5b,0x55f1c407,0x52a58a7a,0x8f887a3b)
              == (0x3e2f308c,0xd90a8f36,0x6ab2a923,0x2883524c)

alltests :: [Bool]
alltests = [test1 , test2 , test3 , test4 , test5 , test6 , test7 ]
