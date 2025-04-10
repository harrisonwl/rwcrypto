{-# LANGUAGE DataKinds #-}
module MNWExample where

import ReWire
import ReWire.Bits

x :: W 16
x = lit 0xff80

compute :: W 16 -> W 8
compute w = if msbit w && w @. 8 
            then w @@ (7,0)
            else w @@ (15,8)


{-
This seems like it ought to work:
λ> compute x
Vector *** Exception: rwPrimBitSlice: slice size mismatch
CallStack (from HasCallStack):
  error, called at /Users/bill/work/rwcrypto/external/ReWire/rewire-user/src/RWC/Primitives.hs:281:22 in main:RWC.Primitives
-}
