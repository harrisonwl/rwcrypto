{-# LANGUAGE DataKinds #-}
module AES.Test.ECBCore256 where

-- | This is a test generated from the ECB AES_Core256.pdf
-- | https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/AES_Core256.pdf
-- | It is an extended example showing all the steps in an example.

import Prelude (($) , Integer , map , (.) , IO , putStrLn , (++) , Bool(..))
import ReWire
import ReWire.Bits ((^) , lit)
import ReWire.Finite (finite)
import ReWire.Vectors (index , generate)

import ReWire.Interactive (dshow , hex , xshow)

import Aes.Basic(State,RoundKey,KeySchedule,initState)
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

