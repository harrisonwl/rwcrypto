{-# LANGUAGE DataKinds #-}
module Salsa20.DoubleRound (doubleround) where

import ReWire
import Salsa20.Salsa20Basic (Hex)
import Salsa20.RowRound(rowround)
import Salsa20.ColumnRound(columnround)

-----------------------------
-- The doubleround function from page 5 of
-- Bernstein's "Salsa20 Specification"
-----------------------------

{-# INLINE doubleround #-}
doubleround :: Hex (W 32) -> Hex (W 32)
doubleround = rowround . columnround
