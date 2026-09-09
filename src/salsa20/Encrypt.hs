{-# LANGUAGE DataKinds #-}
module Salsa20.Encrypt (encrypt , encrypt' , factor64 , splice , mod64) where

import Prelude hiding ((^) , (++))
import ReWire 
import ReWire.Bits ((^))
import ReWire.Vectors (slice)

import Salsa20.Salsa20Basic (Oct, Hex, pi64, splice)
import Salsa20.Expansion(salsa20_k0k1,salsa20_k0k1',back)

encrypt :: Hex (W 8) -> Hex (W 8) -> Oct (W 8) -> W 64 -> W 8 ->  W 8
encrypt k0 k1 v i mi = mi ^ ((salsa20_k0k1 (k0 , k1) (splice v (factor64 i))) `pi64` (mod64 i))

encrypt' :: Hex (W 8) -> Hex (W 8) -> Oct (W 8) -> W 64 -> W 8 ->  W 8
encrypt' k0 k1 v i mi = mi ^ (back (salsa20_k0k1' (k0 , k1) (splice v (factor64 i))) `pi64` (mod64 i))

-- |
-- | This is factor function tweeked so that it takes (W 64) as input instead of Integer. 
-- |
factor64 :: W 64 -> (W 8 , W 8 , W 8 , W 8 , W 8 , W 8 , W 8 , W 8 )
factor64 w64 = (s0 , s1 , s2 , s3 , s4 , s5 , s6 , s7)
  where
    s0 , s1 , s2 , s3 , s4 , s5 , s6 , s7 :: W 8
    s0 = slice (Proxy :: Proxy 0)  w64
    s1 = slice (Proxy :: Proxy 8)  w64
    s2 = slice (Proxy :: Proxy 16) w64
    s3 = slice (Proxy :: Proxy 24) w64
    s4 = slice (Proxy :: Proxy 32) w64
    s5 = slice (Proxy :: Proxy 40) w64
    s6 = slice (Proxy :: Proxy 48) w64
    s7 = slice (Proxy :: Proxy 56) w64

mod64 :: W 64 -> W 6
mod64 w64 = s0
  where
    s0 :: W 6
    s0 = slice (Proxy :: Proxy 58)  w64

