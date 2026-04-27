{-# LANGUAGE DataKinds #-}
module AESTesting where

-- import Prelude (($) , Integer , map , (.) , IO , putStrLn , (++) , (==))
import ReWire
import ReWire.Bits ((^) , lit)
import ReWire.Finite (finite)
import ReWire.Vectors (index , generate)

import ReWire.Interactive (dshow , hex , xshow)

import Aes.Basic(KeySchedule,State,RoundKey,initState,finalState)
import Aes.Operations.SubBytes(subbytes)
import Aes.Operations.AddRoundKey(addRoundKey)
import Aes.Operations.ShiftRows(shiftrows)
import Aes.Operations.MixColumns(mixcolumns)
import Aes.KeyExp.KeyExpansion256(keyexpand)

import Aes.Cipher128(encrypt128)
import Aes.Cipher192(encrypt192)
import Aes.Cipher256(encrypt256)
import Aes.InvCipher256(decrypt256)

import Aes.Test.TestingFunctions
import Aes.KATs(kats,kats128,kats192)

runkats :: [Bool]
runkats = map (\ (k , t , a) -> a == (finalState $ encrypt256 k t)) tests
  where
   tests :: [(W 256 , W 128 , W 128)]
   tests = map (\ (k , t , a) -> (lit k , lit t , lit a)) kats

runkats128 :: [Bool]
runkats128 = map (\ (k , t , a) -> a == (finalState $ encrypt128 k t)) tests
  where
   tests :: [(W 128 , W 128 , W 128)]
   tests = map (\ (k , t , a) -> (lit k , lit t , lit a)) kats128

runkats192 :: [Bool]
runkats192 = map (\ (k , t , a) -> a == (finalState $ encrypt192 k t)) tests
  where
   tests :: [(W 192 , W 128 , W 128)]
   tests = map (\ (k , t , a) -> (lit k , lit t , lit a)) kats192

-- |
-- | Here's a Cryptol example KAT. Defined in test/Testing.cry
-- |
-- // https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/AES_Core256.pdf
-- 
-- keyex : [256]
-- keyex = 0x603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4
-- 
-- plaintext : [128]
-- plaintext = 0x6BC1BEE22E409F96E93D7E117393172A
-- 
-- Testing> encrypt keyex plaintext
-- 0xf3eed1bdb5d2a03c064b5a7e3db181f8
-- Testing> 

input :: State
input = mkstate
          [ [0x32 , 0x88 , 0x31 , 0xe0]
          , [0x43 , 0x5a , 0x31 , 0x37]
          , [0xf6 , 0x30 , 0x98 , 0x07]
          , [0xa8 , 0x8d , 0xa2 , 0x34] ]

----------
-- N.b., the roundkey computed from 
-- λ> ps $ addRoundKey input (extractRoundKey w (finite 1))
--   0x19 0xF6 0x24 0xF6
--   0x6B 0xF4 0xE3 0x91
--   0x5D 0xC7 0x8D 0x8F
--   0xA1 0x42 0xED 0x08
--   ^^^^^^^^^^^^^^^^^^^ Wrong!
-- λ> ps $ addRoundKey input (transpose (extractRoundKey w (finite 1)))
--   0x19 0xA0 0x9A 0xE9
--   0x3D 0xF4 0xC6 0xF8
--   0xE3 0xE2 0x8D 0x48
--   0xBE 0x2B 0x2A 0x08
--   ^^^^^^^^^^^^^^^^^^^ Right!
---------
s1 :: State
s1 = mkstate
        [ [0xd4, 0xe0, 0xb8, 0x1e]
        , [0xbf, 0xb4, 0x41, 0x27]
        , [0x5d, 0x52, 0x11, 0x98]
        , [0x30, 0xae, 0xf1, 0xe5] ]

s2 :: State
s2 = mkstate [ [0x04, 0xe0, 0x48, 0x28 ]
             , [0x66, 0xcb, 0xf8, 0x06 ]
             , [0x81, 0x19, 0xd3, 0x26 ]
             , [0xe5, 0x9a, 0x7a, 0x4c]]

ex1 :: W 256
ex1 = lit 0x94eeea8b1f2ada84adf103313eae6670952419a1f4b16d53d83f13e63c9f6b11

-- From both nist.fips.197-upd1 (Appendix A3) and AES_Core256
keyex :: W 256
keyex = lit 0x603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4

plaintext :: W 128
plaintext = lit 0x6BC1BEE22E409F96E93D7E117393172A

-- xshow $ thereandback keyex plaintext
-- "0x6BC1BEE22E409F96E93D7E117393172A"

crypttext :: W 128
crypttext = lit 0xf3eed1bdb5d2a03c064b5a7e3db181f8

-- decrypt keyex crypttext
-- 0x6bc1bee22e409f96e93d7e117393172a

go :: W 256 -> W 128 -> W 128
go k w = finalState (encrypt256 k w)

thereandback :: W 256 -> W 128 -> W 128
thereandback k w = finalState $ decrypt256 k (encrypt256 k w)

-- | Corresponding to msgToState are:
-- | initState  :: W 128 -> State
-- | finalState :: State -> W 128 
-- |
-- msgToState 0x6BC1BEE22E409F96E93D7E117393172A
-- [[0x6b, 0x2e, 0xe9, 0x73], [0xc1, 0x40, 0x3d, 0x93],
--  [0xbe, 0x9f, 0x7e, 0x17], [0xe2, 0x96, 0x11, 0x2a]]
--
-- ShiftRows (msgToState 0x6BC1BEE22E409F96E93D7E117393172A)
-- [[0x6b, 0x2e, 0xe9, 0x73], [0x40, 0x3d, 0x93, 0xc1],
--  [0x7e, 0x17, 0xbe, 0x9f], [0x2a, 0xe2, 0x96, 0x11]]
-- "0x6B937E962EC11711E940BE2A733D9FE2"
--  0x6b407e2a2e3d17e2e993be9673c19f11
-- "0x6B967E93C12E1117BE40E92AE29F3D73"

-- 0xd2c9f01d9582ea9ae1001b41755db045
-- 0xD295E175C982005DF0EA1BB01D9A4145
-- 0x3EF7B44564950ECC1955E4289C1816EE



-- k0 is keyex ^^^ split into bytes
k0 :: Vec 32 (W 8) 
k0 = fromList
        [ lit 0x60 , lit 0x3d , lit 0xeb , lit 0x10 , lit 0x15 , lit 0xca , lit 0x71 , lit 0xbe
        , lit 0x2b , lit 0x73 , lit 0xae , lit 0xf0 , lit 0x85 , lit 0x7d , lit 0x77 , lit 0x81
        , lit 0x1f , lit 0x35 , lit 0x2c , lit 0x07 , lit 0x3b , lit 0x61 , lit 0x08 , lit 0xd7
        , lit 0x2d , lit 0x98 , lit 0x10 , lit 0xa3 , lit 0x09 , lit 0x14 , lit 0xdf , lit 0xf4 ]

