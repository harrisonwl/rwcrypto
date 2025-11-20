{-# LANGUAGE DataKinds #-}
module Aes.AESBasic(State , Column , RoundKey) where

import ReWire
import ReWire.Vectors

type State    = Vec 4 (Vec 4 (W 8))
type Column   = Vec 4 (W 8)
type RoundKey = Vec 4 (Vec 4 (W 8))

-- type SBox   = Vec 0x10 (Vec 0x10 (W 8))
-- type Index  = Finite 0x10

