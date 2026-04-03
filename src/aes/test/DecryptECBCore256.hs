{-# LANGUAGE DataKinds #-}
module AES.Test.DecryptECBCore256 where

-- | This is a test generated from the ECB AES_Core256.pdf
-- | https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/AES_Core256.pdf
-- | It is an extended example showing all the steps in an example.
-- | This is for the decrypt that starts on page 9.

import Prelude (($) , Integer , map , (.) , IO , putStrLn , (++) , Bool(..))
import ReWire
import ReWire.Bits ((^) , lit)
import ReWire.Finite (finite)
import ReWire.Vectors (index , generate)

import ReWire.Interactive (dshow , hex , xshow)

import Aes.Basic(State,RoundKey,KeySchedule,initState)
import Aes.InvSubBytes(invsubbytes)
import Aes.AddRoundKey(addRoundKey)
-- import Aes.Cipher256(encrypt256,extractRoundKey)
import Aes.InvShiftRows(invshiftrows)
import Aes.InvMixColumns(invmixcolumns)
import Aes.KeyExp.Reference256 (keyexpansion , extractRoundKey)

import Aes.Test.TestingFunctions

-- From both nist.fips.197-upd1 (Appendix A3) and AES_Core256
keyex :: W 256
keyex = lit 0x603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4

ciphertext0 , ciphertext1 , ciphertext2 , ciphertext3 :: W 128
ciphertext0 = lit 0xF3EED1BDB5D2A03C064B5A7E3DB181F8
ciphertext1 = lit 0x591CCB10D410ED26DC5BA74A31362870
ciphertext2 = lit 0xB6ED21B99CA6F4F9F153E7B1BEAFED1D
ciphertext3 = lit 0x23304B7A39F9F3FF067D8D8F9E24ECC7

invround :: KeySchedule -> Finite 15 -> State -> State
invround w r s = invmixcolumns
                    (addRoundKey (extractRoundKey w r)
                                 (invsubbytes (invshiftrows s)))

invfinalround :: KeySchedule -> State -> State
invfinalround w s = addRoundKey (extractRoundKey w 0)
                                (invsubbytes (invshiftrows s))

s0 :: State
s0 = initState ciphertext0

w :: KeySchedule
w = keyexpansion keyex

-- KeyAddition 0DA6416C 53CA2D37 0226A93A 4DDDE2E6

s0_1 :: State
s0_1 = addRoundKey (extractRoundKey w 14) s0

-- > ps s0_1
--   0x0D 0x53 0x02 0x4D
--   0xA6 0xCA 0x26 0xDD
--   0x41 0x2D 0xA9 0xE2
--   0x6C 0x37 0x3A 0xE6

-- Substitution F3C5F8B8 5010FAB2 6A23B7A2 65C93BF5

s0_2 :: State
s0_2 = invsubbytes s0_1

  -- 0xF3 0x50 0x6A 0x65
  -- 0xC5 0x10 0x23 0xC9
  -- 0xF8 0xFA 0xB7 0x3B
  -- 0xB8 0xB2 0xA2 0xF5

-- ShiftRow F3C9B7B2 50C53BA2 6A10F8F5 6523FAB8

s0_3 :: State
s0_3 = invshiftrows s0_2

  -- 0xF3 0x50 0x6A 0x65
  -- 0xC9 0xC5 0x10 0x23
  -- 0xB7 0x3B 0xF8 0xFA
  -- 0xB2 0xA2 0xF5 0xB8

{-
Still working on this one, below is mostly kruft.

-- | Testing code

round :: KeySchedule -> Finite 15 -> State -> State
round w r s = addRoundKey (extractRoundKey w r)
                          (mixcolumns (shiftrows (subbytes s)))

finalround :: KeySchedule -> Finite 15 -> State -> State
finalround w r s = addRoundKey (extractRoundKey w r)
                               ({- mixcolumns -} (shiftrows (subbytes s)))
  
-- | The keyschedule for keyex
w :: KeySchedule
w = keyexpansion keyex

s0 :: State
s0 = initState plaintext0

s1_1 :: State
-- s1_1 = addRoundKey (transpose (extractRoundKey w 0)) s0
s1_1 = round w 0 s0

-------------- Round 1 ----------------

s1_2 :: State
s1_2 = subbytes s1_1

s1_3 :: State
s1_3 = shiftrows s1_2

s1_4 :: State
s1_4 = mixcolumns s1_3

s1_5 :: State
-- s1_5 = addRoundKey (transpose (extractRoundKey w 1)) s1_4
s1_5 = round w 1 s1_1

-------------- Round 2 ----------------

s2_2 :: State
s2_2 = subbytes s1_5

s2_3 :: State
s2_3 = shiftrows s2_2

s2_4 :: State
s2_4 = mixcolumns s2_3

s2_5 :: State
-- s2_5 = addRoundKey (transpose (extractRoundKey w 2)) s2_4
s2_5 = round w 2 s1_5

-------------- Round 3 ----------------

s3_5 :: State
-- s3_5 = addRoundKey (transpose (extractRoundKey w 3))
--                    (mixcolumns (shiftrows (subbytes s2_5)))
s3_5 = round w 3 s2_5

-------------- Round 4 ----------------

ref_s4_5 :: State
ref_s4_5 = toState (lit 0x063CC23D , lit 0xAA5E6BCB , lit 0x37F19B52 , lit 0xC5459511)

s4_5 :: State
-- s4_5 = addRoundKey (transpose (extractRoundKey w 4))
--                    (mixcolumns (shiftrows (subbytes s3_5)))
s4_5 = round w 4 s3_5

-- > s4_5 Prelude.== ref_s4_5
-- True

-------------- Round 5 ----------------

ref_s5_5 :: State
ref_s5_5 = toState (lit 0x15C8B068 , lit 0x90D4966D , lit 0x3F07BDE9 , lit 0x2186CAD6)

s5_5 :: State
-- s5_5 = addRoundKey (transpose (extractRoundKey w 5))
--                    (mixcolumns (shiftrows (subbytes s4_5)))
s5_5 = round w 5 s4_5

-- check5_5 :: Bool
-- check5_5 = s5_5 Prelude.== ref_s5_5

-------------- Round 13 ----------------

ref_s13_5 :: State
ref_s13_5 = toState (lit 0xF3C9B7B2 , lit 0x50C53BA2 , lit 0x6A10F8F5 , lit 0x6523FAB8)

ans_5 :: State
-- ans_5 = addRoundKey (transpose (extractRoundKey w 14))
--                    ({- mixcolumns -} (shiftrows (subbytes ref_s13_5)))
ans_5 = finalround w 14 ref_s13_5

ref_ans_5 :: State
ref_ans_5 = toState (lit 0xF3EED1BD , lit 0xB5D2A03C , lit 0x064B5A7E , lit 0x3DB181F8)

-- λ> ans_5 Prelude.== ref_ans_5
-- True
-- 
-- Therefore, the guts of AES256 works. 

ref0 :: State
ref0 = encrypt256 keyex s0

-}
