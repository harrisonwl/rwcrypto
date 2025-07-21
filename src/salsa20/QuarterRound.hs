{-# LANGUAGE DataKinds #-}
module Salsa20.QuarterRound(quarterround) where

import Prelude hiding ((+) , (^))
import ReWire
import ReWire.Bits (lit , rotL , (^) , (+))

-----------------------------
-- The quarterround function from page 2 of
-- Bernstein's "Salsa20 Specification"
-----------------------------

quarterround :: (W 32 , W 32 , W 32 , W 32) -> (W 32 , W 32 , W 32 , W 32)
quarterround (y0 , y1 , y2 , y3) = (z0 , z1 , z2 , z3)
  where
    z0 , z1 , z2 , z3 :: W 32
    z1 = y1 ^ rotL (lit 7) (y0 + y3)
    z2 = y2 ^ rotL (lit 9) (z1 + y0)
    z3 = y3 ^ rotL (lit 13) (z2 + z1)
    z0 = y0 ^ rotL (lit 18) (z3 + z2)
