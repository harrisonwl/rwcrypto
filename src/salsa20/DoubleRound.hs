{-# LANGUAGE DataKinds #-}
module DoubleRound (doubleround) where

import ReWire
import Basic (Hex)
import RowRound(rowround)
import ColumnRound(columnround)

-----------------------------
-- The doubleround function from page 5 of
-- Bernstein's "Salsa20 Specification"
-----------------------------

doubleround :: Hex (W 32) -> Hex (W 32)
doubleround = rowround . columnround
