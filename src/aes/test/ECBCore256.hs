{-# LANGUAGE DataKinds #-}
module AES.Test.ECBCore256 where

-- | This is a test generated from the ECB AES_Core256.pdf
-- | https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/AES_Core256.pdf
-- | It is an extended example showing all the steps in an example.

import Prelude (($) , Integer , map , (.) , IO , putStrLn , (++))
import ReWire
import ReWire.Bits ((^) , lit)
import ReWire.Finite (finite)
import ReWire.Vectors (index , generate)

import ReWire.Interactive (dshow , hex , xshow)

import Aes.Basic(State,RoundKey,KeySchedule,initState,transpose)
import Aes.SubBytes(subbytes)
import Aes.AddRoundKey(addRoundKey)
import Aes.Cipher256(encrypt256,extractRoundKey)
import Aes.ShiftRows(shiftrows)
import Aes.MixColumns(mixcolumns)
import Aes.KeyExp.Reference256(keyexpansion)
import Aes.Test.TestingFunctions

-- From both nist.fips.197-upd1 (Appendix A3) and AES_Core256
keyex :: W 256
keyex = lit 0x603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4

plaintext0 , plaintext1 , plaintext2 , plaintext3 :: W 128
plaintext0 = lit 0x6BC1BEE22E409F96E93D7E117393172A
plaintext1 = lit 0xAE2D8A571E03AC9C9EB76FAC45AF8E51
plaintext2 = lit 0x30C81C46A35CE411E5FBC1191A0A52EF
plaintext3 = lit 0xF69F2445DF4F9B17AD2B417BE66C3710

-- | Testing code
  
-- | The keyschedule for keyex
w :: KeySchedule
w = keyexpansion keyex

s0 :: State
s0 = initState plaintext0

s1_1 :: State
s1_1 = addRoundKey (transpose (extractRoundKey w 0)) s0

-------------- Round 1 ----------------

s1_2 :: State
s1_2 = subbytes s1_1

s1_3 :: State
s1_3 = shiftrows s1_2

s1_4 :: State
s1_4 = mixcolumns s1_3

s1_5 :: State
s1_5 = addRoundKey (transpose (extractRoundKey w 1)) s1_4

-------------- Round 2 ----------------

s2_2 :: State
s2_2 = subbytes s1_5

s2_3 :: State
s2_3 = shiftrows s2_2

s2_4 :: State
s2_4 = mixcolumns s2_3

s2_5 :: State
s2_5 = addRoundKey (transpose (extractRoundKey w 2)) s2_4

-------------- Round 3 ----------------

s3_5 :: State
s3_5 = addRoundKey (transpose (extractRoundKey w 3))
                   (mixcolumns (shiftrows (subbytes s2_5)))
