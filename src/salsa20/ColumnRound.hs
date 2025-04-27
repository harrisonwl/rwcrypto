{-# LANGUAGE DataKinds #-}
module ColumnRound (columnround) where

import ReWire
import Basic (X16(..), Hex)
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

