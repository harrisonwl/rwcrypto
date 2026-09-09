{-# LANGUAGE DataKinds #-}
module Aes.Test.AppendixB where

--
-- ****** THIS IS FOR AES-128, NOT AES-256, YOU DOPE.
--

import ReWire
import ReWire.Bits ((^) , lit)
import ReWire.Finite (finite)

import ReWire.Interactive (dshow , hex , xshow)

import Aes.Basic(KeySchedule,State,RoundKey,initState,transpose)
import Aes.KeyExp.Reference256(keyexpansion)
import Aes.AddRoundKey(addRoundKey)
import Aes.Cipher256(encrypt256,extractRoundKey)
import Aes.SubBytes(subbytes)
import Aes.ShiftRows(shiftrows)
import Aes.MixColumns(mixcolumns)

import Aes.Test.TestingFunctions(ps,mkstate)

-- | Appendix B, FIPS197-upd, page 34, row 1.

-- input :: State
-- input = mkstate
--           [ [0x32 , 0x88 , 0x31 , 0xe0]
--           , [0x43 , 0x5a , 0x31 , 0x37]
--           , [0xf6 , 0x30 , 0x98 , 0x07]
--           , [0xa8 , 0x8d , 0xa2 , 0x34] ]

-- s0 :: State 
-- s0 = mkstate [ [ 0x19 , 0xa0 , 0x9a , 0xe9 ]
--              , [ 0x3d , 0xf4 , 0xc6 , 0xf8 ]
--              , [ 0xe3 , 0xe2 , 0x8d , 0x48 ]
--              , [ 0xbe , 0x2b , 0x2a , 0x08 ] ]

key0 :: W 256
key0  = lit 0x2b7e151628aed2a6abf7158809cf4f3c

w :: KeySchedule
w = keyexpansion key0

input :: State
input = initState $ lit 0x3243f6a8885a308d313198a2e0370734

s0 :: State
s0 = addRoundKey (transpose (extractRoundKey w 0)) input

r1_1 :: State
r1_1 = addRoundKey (transpose (extractRoundKey w 1)) s0

r1_2 :: State
r1_2 = subbytes r1_1

r1_3 :: State
r1_3 = shiftrows r1_2

r1_4 :: State
r1_4 = mixcolumns r1_3

r1_5 :: State
r1_5 = addRoundKey (transpose (extractRoundKey w 2)) r1_4 

