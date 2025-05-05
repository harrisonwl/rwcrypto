{-# LANGUAGE DataKinds #-}
module LittleEndian(littleendian , inv_littleendian) where

import Prelude hiding ((++))
import ReWire
-- import ReWire.Bits (lit , (==))
import ReWire.Vectors (slice , (++))
import Basic (Quad)

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
