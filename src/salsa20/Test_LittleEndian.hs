{-# LANGUAGE DataKinds #-}
module LittleEndian(littleendian , inv_littleendian , alltests) where

import qualified Prelude as P
import ReWire
import ReWire.Bits (lit , (==))
import ReWire.Vectors (slice , (++))
import Idioms (Quad)

-----------------------------
-- The littleendian function from page 6
-----------------------------

revbytes :: Quad (W 8) -> Quad (W 8)   -- i.e., (W 8,W 8,W 8,W 8) -> (W 8,W 8,W 8,W 8)
revbytes (b0,b1,b2,b3) = (b3,b2,b1,b0)

littleendian :: Quad (W 8) -> W 32     -- i.e., (W 8, W 8, W 8, W 8) -> W 32
littleendian w8x4 = b3 ++ b2 ++ b1 ++ b0
  where
    b0 , b1 , b2 , b3 :: W 8
    (b3 , b2 , b1 , b0) = revbytes w8x4

------------------------------
-- "Note that littleendian is invertible."
------------------------------

inv_littleendian :: W 32 -> Quad (W 8)
inv_littleendian w = revbytes (slice0 w , slice1 w , slice2 w , slice3 w)

slice0 :: W 32 -> W 8
slice0 w32 = slice (Proxy :: Proxy 0)  w32

slice1 :: W 32 -> W 8
slice1 w32 = slice (Proxy :: Proxy 8)  w32

slice2 :: W 32 -> W 8
slice2 w32 = slice (Proxy :: Proxy 16)  w32

slice3 :: W 32 -> W 8
slice3 w32 = slice (Proxy :: Proxy 24)  w32

slicetest :: P.Bool
slicetest = (slice0 x , slice1 x , slice2 x , slice3 x) P.== (lit 0xde , lit 0xad , lit 0xbe , lit 0xef)
  where
    x :: W 32
    x = lit 0xdeadbeef

alltests :: [P.Bool]
alltests = [test1 , test2 , test3] P.++ invertible_tests

test1 , test2 , test3 :: P.Bool 
test1 = littleendian (lit 0,lit 0,lit 0,lit 0)         == lit 0x00000000
test2 = littleendian (lit 86,lit 75,lit 30,lit 9)      == lit 0x091e4b56
test3 = littleendian (lit 255,lit 255,lit 255,lit 250) == lit 0xfaffffff

invertible_tests :: [P.Bool]
invertible_tests = [ invert q0 P.== q0
                   , invert q1 P.== q1
                   , invert q2 P.== q2
                   ]
   where
     invert :: Quad (W 8) -> Quad (W 8)
     invert q = inv_littleendian (littleendian q)

     q0 , q1 , q2 :: Quad (W 8)
     q0 = (lit 0,lit 0,lit 0,lit 0)
     q1 = (lit 86,lit 75,lit 30,lit 9)
     q2 = (lit 255,lit 255,lit 255,lit 250)      
