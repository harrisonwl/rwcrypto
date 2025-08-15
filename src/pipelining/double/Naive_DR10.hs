{-# LANGUAGE DataKinds #-}
module Pipelining.Naive_DR10 where

import Prelude hiding ((+))
import ReWire
import ReWire.Bits hiding (one)

import Salsa20.Basic (Hex)
import Salsa20.DoubleRound (doubleround)

type HxW32 = Hex (W 32)

dr10 :: HxW32 -> HxW32
dr10 = doubleround . doubleround . doubleround . doubleround . doubleround .
       doubleround . doubleround . doubleround . doubleround . doubleround 

loop :: Maybe HxW32 -> ReacT (Maybe HxW32) (Maybe HxW32) Identity ()
loop (Just hxw32) = signal (Just (dr10 hxw32)) >>= loop
loop Nothing      = signal Nothing >>= loop

start :: ReacT (Maybe HxW32) (Maybe HxW32) Identity ()
start = loop Nothing
