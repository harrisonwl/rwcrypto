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

test1 :: Quad W32
test1 = (lit 0x00000000 , lit 0x00000000 , lit 0x00000000 , lit 0x00000000)
