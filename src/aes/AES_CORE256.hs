{-# LANGUAGE DataKinds #-}
module AES.Test.AES_CORE256 where

-- | This is a test generated from the ECB AES_Core256.pdf
-- | https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/AES_Core256.pdf
-- | It is an extended example showing all the steps in an example.
-- Cursor-generated from AES_Core256.pdf: four 8-digit hex words per line as Haskell tuples
-- Text lines from the PDF appear as comments; labels before numbers appear after the tuple.

import ReWire
import ReWire.Bits (lit)

import Aes.Basic(State , KeySchedule , initState , transpose , fromW32)
import Aes.SubBytes(subbytes)
import Aes.AddRoundKey(addRoundKey)
import Aes.Cipher256(extractRoundKey)
import Aes.ShiftRows(shiftrows)
import Aes.MixColumns(mixcolumns)
import Aes.KeyExp.Reference256(keyexpansion)
import Aes.Test.TestingFunctions

-- | Top-level 
runchecks :: Bool
runchecks =  checkL rounds w s0
          && checkL rounds2 w s1
          && checkL rounds3 w s2
          && checkL rounds4 w s3

data Round = Round (Finite 15)
                   (W 32 , W 32 , W 32 , W 32)  -- Substitution
                   (W 32 , W 32 , W 32 , W 32)  -- ShiftRow
                   (W 32 , W 32 , W 32 , W 32)  -- MixColumn
                   (W 32 , W 32 , W 32 , W 32)  -- KeyAddition
           | Final (Finite 15)
                   (W 32 , W 32 , W 32 , W 32)  -- Substitution
                   (W 32 , W 32 , W 32 , W 32)  -- ShiftRow
                   (W 32 , W 32 , W 32 , W 32)  -- KeyAddition

eval :: Round -> KeySchedule -> State -> State
eval (Round i su sh mc ka) w s = ka'
  where
    su' = subbytes s
    sh' = shiftrows su'
    mc' = mixcolumns sh'
    ka' = addRoundKey (transpose (extractRoundKey w i)) mc'
eval (Final i su sh ka) w s = ka'
  where
    su' = subbytes s
    sh' = shiftrows su'
    ka' = addRoundKey (transpose (extractRoundKey w i)) sh'


check :: Round -> KeySchedule -> State -> ( State , Bool )
check (Round i su sh mc ka) w s = ( ka' , w32s2state su Prelude.== su' &&
                                          w32s2state sh Prelude.== sh' &&
                                          w32s2state mc Prelude.== mc' &&
                                          w32s2state ka Prelude.== ka' )
  where
    su' = subbytes s
    sh' = shiftrows su'
    mc' = mixcolumns sh'
    ka' = addRoundKey (transpose (extractRoundKey w i)) mc'
check (Final i su sh ka) w s = ( ka' , w32s2state su Prelude.== su' &&
                                       w32s2state sh Prelude.== sh' &&
                                       w32s2state ka Prelude.== ka' )
  where
    su' = subbytes s
    sh' = shiftrows su'
    ka' = addRoundKey (transpose (extractRoundKey w i)) sh'

checkL :: [Round] -> KeySchedule -> State -> Bool
checkL [] _ _     = True
checkL (r:rs) w s = b && checkL rs w s'
  where
    (s' , b) = check r w s


-- | Testing code

-- | Example is a State represented as a tuple of W32's, each of
-- | which is a *column* in the State. See AES_CORE256.{hs,pdf}.
-- | This comes straight out of each line in AES_CORE256.pdf.

w32s2state :: (W 32 , W 32 , W 32 , W 32) -> State
w32s2state (col0 , col1 , col2 , col3) = fromList [ r0 , r1 , r2 , r3 ]
  where
    
    r0 , r1 , r2 , r3 :: Vec 4 (W 8)
    r0 = fromList [v00 , v01 , v02 , v03] 
    r1 = fromList [v10 , v11 , v12 , v13] 
    r2 = fromList [v20 , v21 , v22 , v23] 
    r3 = fromList [v30 , v31 , v32 , v33]

    v00 , v10 , v20 , v30 :: W 8
    (v00 , v10 , v20 , v30) = fromW32 col0
    v01 , v11 , v21 , v31 :: W 8
    (v01 , v11 , v21 , v31) = fromW32 col1
    v02 , v12 , v22 , v32 :: W 8
    (v02 , v12 , v22 , v32) = fromW32 col2
    v03 , v13 , v23 , v33 :: W 8
    (v03 , v13 , v23 , v33) = fromW32 col3

  
-- | The keyschedule for keyex


keyex :: W 256
keyex = lit 0x603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4

plaintext0 , plaintext1 , plaintext2 , plaintext3 :: W 128
plaintext0 = lit 0x6BC1BEE22E409F96E93D7E117393172A
plaintext1 = lit 0xAE2D8A571E03AC9C9EB76FAC45AF8E51
plaintext2 = lit 0x30C81C46A35CE411E5FBC1191A0A52EF
plaintext3 = lit 0xF69F2445DF4F9B17AD2B417BE66C3710

w :: KeySchedule
w = keyexpansion keyex

-- 
-- s0' :: State
-- s0' = initState plaintext0
-- c1_1 :: (W 32 , W 32 , W 32 , W 32)
-- c1_1 = (lit 0x0BFC55F2 , lit 0x3B8AEE28 , lit 0xC24ED0E1 , lit 0xF6EE60AB)  -- KeyAddition

-- |
-- | Start of first cipher call
-- |

s0 :: State
s0 = addRoundKey (transpose (extractRoundKey w 0)) (initState plaintext0)

-------------- Round 1 ----------------

s1_2 :: State
s1_2 = subbytes s0

c1_2 :: (W 32 , W 32 , W 32 , W 32)
c1_2 = (lit 0x2BB0FC89 , lit 0xE27E2834 , lit 0x252F70F8 , lit 0x4228D062)  -- Substitution

s1_3 :: State
s1_3 = shiftrows s1_2

c1_3 :: (W 32 , W 32 , W 32 , W 32)
c1_3 = (lit 0x2B7E7062 , lit 0xE22FD089 , lit 0x2528FC34 , lit 0x42B028F8)  -- ShiftRow

s1_4 :: State
s1_4 = mixcolumns s1_3

c1_4 :: (W 32 , W 32 , W 32 , W 32)
c1_4 = (lit 0xC62513B7 , lit 0xF75EF6CB , lit 0xFA5EB2D3 , lit 0x9FB9B1B5)  -- MixColumn

s1_5 :: State
s1_5 = addRoundKey (transpose (extractRoundKey w 1)) s1_4

c1_5 :: (W 32 , W 32 , W 32 , W 32)
c1_5 = (lit 0xD9103FB0 , lit 0xCC3FFE1C , lit 0xD7C6A270 , lit 0x96AD6E41)  -- KeyAddition

{-
-- --------------------------------------------------------------
-- ##############################################################
-- Block Cipher Modes of Operation
-- Electronic Codebook (ECB)
-- Plaintext is
(lit 0x6BC1BEE2 , lit 0x2E409F96 , lit 0xE93D7E11 , lit 0x7393172A)
(lit 0xAE2D8A57 , lit 0x1E03AC9C , lit 0x9EB76FAC , lit 0x45AF8E51)
(lit 0x30C81C46 , lit 0xA35CE411 , lit 0xE5FBC119 , lit 0x1A0A52EF)
(lit 0xF69F2445 , lit 0xDF4F9B17 , lit 0xAD2B417B , lit 0xE66C3710)
-- ##############################################################
-- ==============================================================
-- ECB-AES256 (Encryption)
-- Key is
(lit 0x603DEB10 , lit 0x15CA71BE , lit 0x2B73AEF0 , lit 0x857D7781)
(lit 0x1F352C07 , lit 0x3B6108D7 , lit 0x2D9810A3 , lit 0x0914DFF4)

-- Plaintext is
(lit 0x6BC1BEE2 , lit 0x2E409F96 , lit 0xE93D7E11 , lit 0x7393172A)
(lit 0xAE2D8A57 , lit 0x1E03AC9C , lit 0x9EB76FAC , lit 0x45AF8E51)
(lit 0x30C81C46 , lit 0xA35CE411 , lit 0xE5FBC119 , lit 0x1A0A52EF)
(lit 0xF69F2445 , lit 0xDF4F9B17 , lit 0xAD2B417B , lit 0xE66C3710)

(lit 0x0BFC55F2 , lit 0x3B8AEE28 , lit 0xC24ED0E1 , lit 0xF6EE60AB)  -- KeyAddition
-}

rounds :: [Round]
rounds = [r1 , r2 , r3 , r4 , r5 , r6 , r7 , r8 , r9 , r10 , r11 , r12 , r13 , r14]

-- Round = 1
r1 :: Round
r1 = Round 1
     (lit 0x2BB0FC89 , lit 0xE27E2834 , lit 0x252F70F8 , lit 0x4228D062)  -- Substitution
     (lit 0x2B7E7062 , lit 0xE22FD089 , lit 0x2528FC34 , lit 0x42B028F8)  -- ShiftRow
     (lit 0xC62513B7 , lit 0xF75EF6CB , lit 0xFA5EB2D3 , lit 0x9FB9B1B5)  -- MixColumn
     (lit 0xD9103FB0 , lit 0xCC3FFE1C , lit 0xD7C6A270 , lit 0x96AD6E41)  -- KeyAddition

-- Round = 2
r2 :: Round
r2 = Round 2
     (lit 0x35CA75E7 , lit 0x4B75BB9C , lit 0x0EB43A51 , lit 0x90959F83)  -- Substitution
     (lit 0x35753A83 , lit 0x4BB49FE7 , lit 0x0E95759C , lit 0x90CABB51)  -- ShiftRow
     (lit 0x4C12AA0D , lit 0x2965E823 , lit 0x513CCED1 , lit 0x9498C478)  -- MixColumn
     (lit 0xD7B1FE1C , lit 0xA70CCD8C , lit 0xF426458E , lit 0xB4FF38A6)  -- KeyAddition

-- Round = 3
r3 :: Round
r3 = Round 3
     (lit 0x0EC8BB9C , lit 0x5CFEBD64 , lit 0xBFF76E19 , lit 0x8D160724)  -- Substitution
     (lit 0x0EFE6E24 , lit 0x5CF7079C , lit 0xBF16BB64 , lit 0x8DC8BD19)  -- ShiftRow
     (lit 0x4F7F40CA , lit 0x213C1A37 , lit 0x802168BF , lit 0xE6C30FCB)  -- MixColumn
     (lit 0xE7CFDCD0 , lit 0xB2ED8EFA , lit 0x3E68ECD1 , lit 0x519E5451)  -- KeyAddition


-- Round = 4
r4 :: Round
r4 = Round 4
     (lit 0x948A8670 , lit 0x3755192D , lit 0xB245CE3E , lit 0xD10B20D1)  -- Substitution
     (lit 0x9455CED1 , lit 0x37452070 , lit 0xB20B862D , lit 0xD18A193E)  -- ShiftRow
     (lit 0xD3A62E85 , lit 0xF1ADA2DC , lit 0xC918D91A , lit 0x1BCB2B87)  -- MixColumn
     (lit 0x063CC23D , lit 0xAA5E6BCB , lit 0x37F19B52 , lit 0xC5459511)  -- KeyAddition
-- Round = 5
r5 :: Round
r5 = Round 5
     (lit 0x6FEB2527 , lit 0xAC587F1F , lit 0x9AA11400 , lit 0xA66E2A82)  -- Substitution
     (lit 0x6F581482 , lit 0xACA12A27 , lit 0x9A6E251F , lit 0xA6EB7F00)  -- ShiftRow
     (lit 0xA06182E2 , lit 0xB6AC302A , lit 0xA7369FC0 , lit 0x0EEAB365)  -- MixColumn
     (lit 0x15C8B068 , lit 0x90D4966D , lit 0x3F07BDE9 , lit 0x2186CAD6)  -- KeyAddition

-- Round = 6
r6 :: Round
r6 = Round 6
     (lit 0x59E8E745 , lit 0x6048903C , lit 0x75C57A1E , lit 0xFD4474F6)  -- Substitution
     (lit 0x59487AF6 , lit 0x60C57445 , lit 0x7544E73C , lit 0xFDE8901E)  -- ShiftRow
     (lit 0xE6B1E42E , lit 0xA528829B , lit 0xFDF3A044 , lit 0x4C830C58)  -- MixColumn
     (lit 0x679D6583 , lit 0x7FF7CA21 , lit 0xD9C5AAB6 , lit 0xB63BB83C)  -- KeyAddition

-- Round = 7
r7 :: Round
r7 = Round 7
     (lit 0x855E4DEC , lit 0xD26874FD , lit 0x35A6AC4E , lit 0x4EE26CEB)  -- Substitution
     (lit 0x8568ACEB , lit 0xD2A66CEC , lit 0x35E24DFD , lit 0x4E5E744E)  -- ShiftRow
     (lit 0xEE51889D , lit 0xCEDD8364 , lit 0xE7C05111 , lit 0x44202A64)  -- MixColumn
     (lit 0x76943754 , lit 0x70609AEA , lit 0xC14C6AB6 , lit 0x4DC06870)  -- KeyAddition

-- Round = 8
r8 :: Round
r8 = Round 8
     (lit 0x38229A20 , lit 0x51D0B887 , lit 0x7829024E , lit 0xE3BA4551)  -- Substitution
     (lit 0x38D00251 , lit 0x51294520 , lit 0x78BA9A87 , lit 0xE322B84E)  -- ShiftRow
     (lit 0x48D41F38 , lit 0xBCEC92DF , lit 0x38257FBD , lit 0x4D3A7838)  -- MixColumn
     (lit 0x20D46494 , lit 0x0E33A1C9 , lit 0xAECC4659 , lit 0x216BF5B8)  -- KeyAddition

-- Round = 9
r9 :: Round
r9 = Round 9
     (lit 0xB7484322 , lit 0xABC332DD , lit 0xE44B5ACB , lit 0xFD7FE66C)  -- Substitution
     (lit 0xB7C35A6C , lit 0xAB4BE622 , lit 0xE47F43DD , lit 0xFD4832CB)  -- ShiftRow
     (lit 0x1DA87483 , lit 0x542E510F , lit 0xCC0261AA , lit 0xC0F097EB)  -- MixColumn
     (lit 0xD5BC9687 , lit 0x2287AA85 , lit 0x9C27A187 , lit 0x993515D2)  -- KeyAddition

-- Round = 10
r10 :: Round
r10 = Round 10
      (lit 0x03659017 , lit 0x9317AC97 , lit 0xDECC3217 , lit 0xEE9659B5)  -- Substitution
      (lit 0x031732B5 , lit 0x93CC5917 , lit 0xDE969097 , lit 0xEE65AC17)  -- ShiftRow
      (lit 0xB8CEB451 , lit 0x3CECD415 , lit 0x01D5D14A , lit 0xD3DCF1CE)  -- MixColumn
      (lit 0x66DDDD36 , lit 0x50208E64 , lit 0xFBF0B2DF , lit 0x45A81FDB)  -- KeyAddition

-- Round = 11
r11 :: Round
r11 = Round 11
      (lit 0x33C1C105 , lit 0x53B71943 , lit 0x0F8C379E , lit 0x6EC2C0B9)  -- Substitution
      (lit 0x33B737B9 , lit 0x538CC005 , lit 0x0FC2C143 , lit 0x6EC1199E)  -- ShiftRow
      (lit 0x2AA63ABC , lit 0xEC0E4BB3 , lit 0xC18B9194 , lit 0x0342244D)  -- MixColumn
      (lit 0x7220F0E1 , lit 0xC2217A64 , lit 0xBF81606E , lit 0x248D578E)  -- KeyAddition

-- Round = 12
r12 :: Round
r12 = Round 12
      (lit 0x40B78CF8 , lit 0x25FDDA43 , lit 0x080CD09F , lit 0x365D5B19)  -- Substitution
      (lit 0x40FDD019 , lit 0x250C5BF8 , lit 0x085D8C43 , lit 0x36B7DA9F)  -- ShiftRow
      (lit 0x55D32DDF , lit 0xFD288CD3 , lit 0x387E934F , lit 0xEBA99412)  -- MixColumn
      (lit 0x214F6A74 , lit 0xE5789109 , lit 0xDA0BED00 , lit 0x9FA80448)  -- KeyAddition

-- Round = 13
r13 :: Round
r13 = Round 13
      (lit 0xFD840292 , lit 0xD9BC8101 , lit 0x572B5563 , lit 0xDBC2F252)  -- Substitution
      (lit 0xFDBC5552 , lit 0xD92BF292 , lit 0x57C20201 , lit 0xDB848163)  -- ShiftRow
      (lit 0x39331D51 , lit 0xB410A096 , lit 0xF0CF923B , lit 0xD833E3B5)  -- MixColumn
      (lit 0xF3C9B7B2 , lit 0x50C53BA2 , lit 0x6A10F8F5 , lit 0x6523FAB8)  -- KeyAddition

-- Final Round = 14
r14 :: Round
r14 = Final 14
      (lit 0x0DDDA937 , lit 0x53A6E23A , lit 0x02CA41E6 , lit 0x4D262D6C)  -- Substitution
      (lit 0x0DA6416C , lit 0x53CA2D37 , lit 0x0226A93A , lit 0x4DDDE2E6)  -- ShiftRow
      (lit 0xF3EED1BD , lit 0xB5D2A03C , lit 0x064B5A7E , lit 0x3DB181F8)  -- KeyAddition

-- |
-- | Start of second call to cipher
-- |

s1 :: State
s1 = addRoundKey (transpose (extractRoundKey w 0)) (initState plaintext1)

rounds2 :: [Round]
rounds2 = [ r2_1 , r2_2 , r2_3 , r2_4 , r2_5 , r2_6 , r2_7 
          , r2_8 , r2_9 , r2_10 , r2_11 , r2_12 , r2_13 , r2_14]

-- | same as s1
c_1 :: (W 32 , W 32 , W 32 , W 32)
c_1 = (lit 0xCE106147 , lit 0x0BC9DD22 , lit 0xB5C4C15C , lit 0xC0D2F9D0)  -- KeyAddition

-- Round = 1
r2_1 :: Round
r2_1 = Round 1
        (lit 0x8BCAEFA0 , lit 0x2BDDC193 , lit 0xD51C784A , lit 0xBAB59970)  -- Substitution
        (lit 0x8BDD7870 , lit 0x2B1C99A0 , lit 0xD5B5EF93 , lit 0xBACAC14A)  -- ShiftRow
        (lit 0x79D236C3 , lit 0x4B03E5A3 , lit 0x091D0B03 , lit 0xA127374A)  -- MixColumn
        (lit 0x66E71AC4 , lit 0x7062ED74 , lit 0x24851BA0 , lit 0xA833E8BE)  -- KeyAddition

-- Round = 2
r2_2 :: Round
r2_2 = Round 2
       (lit 0x3394A21C , lit 0x51AA5592 , lit 0x3697AFE0 , lit 0xC2C39BAE)  -- Substitution
       (lit 0x33AAAFAE , lit 0x51979B1C , lit 0x36C3A292 , lit 0xC29455E0)  -- ShiftRow
       (lit 0x82383517 , lit 0x87CECFC7 , lit 0x02C40704 , lit 0x8DEEC747)  -- MixColumn
       (lit 0x199B6106 , lit 0x09A7EA68 , lit 0xA7DE8C5B , lit 0xAD893B99)  -- KeyAddition

-- Round = 3
r2_3 :: Round
r2_3 = Round 3
       (lit 0xD414EF6F , lit 0x015C8745 , lit 0x5C1D6439 , lit 0x95A7E2EE)  -- Substitution
       (lit 0xD45C64EE , lit 0x011DE26F , lit 0x5CA7EF45 , lit 0x95148739)  -- ShiftRow
       (lit 0xDD2E6998 , lit 0xA8697222 , lit 0xE066F126 , lit 0xB316DF45)  -- MixColumn
       (lit 0x759EF582 , lit 0x3BB8E6EF , lit 0x5E2F7548 , lit 0x044B84DF)  -- KeyAddition

-- Round = 4
r2_4 :: Round
r2_4 = Round 4
       (lit 0x9D0BE613 , lit 0xE26C8EDF , lit 0x58159D52 , lit 0xF2B35F9E)  -- Substitution
       (lit 0x9D6C9D9E , lit 0xE2155F13 , lit 0x58B3E6DF , lit 0xF20B8E52)  -- ShiftRow
       (lit 0x9667696A , lit 0xAC3A7C51 , lit 0x47CB4618 , lit 0x3E3F082C)  -- MixColumn
       (lit 0x43FD85D2 , lit 0xF7C9B546 , lit 0xB9220450 , lit 0xE0B1B6BA)  -- KeyAddition

-- Round = 5
r2_5 :: Round
r2_5 = Round 5
       (lit 0x1A5497B5 , lit 0x68DDD55A , lit 0x5693F253 , lit 0xE1C84EF4)  -- Substitution
       (lit 0x1ADDF2F4 , lit 0x68934EB5 , lit 0x56C8975A , lit 0xE154D553)  -- ShiftRow
       (lit 0x4E423FF2 , lit 0x8532A314 , lit 0x22254511 , lit 0xA37EF11F)  -- MixColumn
       (lit 0xFBEB0D78 , lit 0xA34A0553 , lit 0xBA146738 , lit 0x8C1288AC)  -- KeyAddition

-- Round = 6
r2_6 :: Round
r2_6 = Round 6
       (lit 0x0FE9D7BC , lit 0x0AD66BED , lit 0xF4FA8507 , lit 0x64C9C491)  -- Substitution
       (lit 0x0FD68591 , lit 0x0AFAC4BC , lit 0xF4C9D7ED , lit 0x64E96B07)  -- ShiftRow
       (lit 0x6BBD607B , lit 0x790EBC43 , lit 0x89F2A4D8 , lit 0x84175220)  -- MixColumn
       (lit 0xEA91E1D6 , lit 0xA3D1F4F9 , lit 0xADC4AE2A , lit 0x7EAFE644)  -- KeyAddition

-- Round = 7
r2_7 :: Round
r2_7 = Round 7
       (lit 0x8781F8F6 , lit 0x0A3EBF99 , lit 0x951CE4E5 , lit 0xF3798E1B)  -- Substitution
       (lit 0x873EE41B , lit 0x0A1C8EF6 , lit 0x9579F899 , lit 0xF381BFE5)  -- ShiftRow
       (lit 0xA8D7477E , lit 0x484D107B , lit 0xDBEDB70C , lit 0x3FD523E1)  -- MixColumn
       (lit 0x3012F8B7 , lit 0xF6F009F5 , lit 0xFD618CAB , lit 0x363561F5)  -- KeyAddition

-- Round = 8
r2_8 :: Round
r2_8 = Round 8
       (lit 0x04C941A9 , lit 0x428C01E6 , lit 0x54EF6462 , lit 0x0596EFE6)  -- Substitution
       (lit 0x048C64E6 , lit 0x42EFEFA9 , lit 0x549641E6 , lit 0x05C90162)  -- ShiftRow
       (lit 0x054D7133 , lit 0xE804888F , lit 0xAE4671FC , lit 0x29ED6803)  -- MixColumn
       (lit 0x6D4D0A9F , lit 0x5ADBBB99 , lit 0x38AF4818 , lit 0x45BCE583)  -- KeyAddition

-- Round = 9
r2_9 :: Round
r2_9 = Round 9
       (lit 0x3CE367DB , lit 0xBEB9EAEE , lit 0x077952AD , lit 0x6E65D9EC)  -- Substitution
       (lit 0x3CB952EC , lit 0xBE79D9DB , lit 0x076567EE , lit 0x6EE3EAAD)  -- ShiftRow
       (lit 0x164F0E6C , lit 0xEEE718D4 , lit 0x288A85CC , lit 0xA53BAEFA)  -- MixColumn
       (lit 0xDE5BEC68 , lit 0x984EE35E , lit 0x78AF45E1 , lit 0xFCFE2CC3)  -- KeyAddition

-- Round = 10
r2_10 :: Round
r2_10 = Round 10
        (lit 0x1D39CE45 , lit 0x462F1158 , lit 0xBC796EF8 , lit 0xB0BB712E)  -- Substitution
        (lit 0x1D2F6E2E , lit 0x46797145 , lit 0xBCBBCE58 , lit 0xB03911F8)  -- ShiftRow
        (lit 0x0BDF9C3A , lit 0x33621248 , lit 0x23C0681A , lit 0xD909B808)  -- MixColumn
        (lit 0xD5CCF55D , lit 0x5FAE4839 , lit 0xD9E50B8F , lit 0x4F7D561D)  -- KeyAddition

-- Round = 11
r2_11 :: Round
r2_11 = Round 11
        (lit 0x034BE64C , lit 0xCFE45212 , lit 0x35D92B73 , lit 0x84FFB1A4)  -- Substitution
        (lit 0x03E42BA4 , lit 0xCFD9B14C , lit 0x35FFE612 , lit 0x844B5273)  -- ShiftRow
        (lit 0xBE094699 , lit 0x08E2BBBA , lit 0x84F32B62 , lit 0xEF97FE68)  -- MixColumn
        (lit 0xE68F8CC4 , lit 0x26CD8A6D , lit 0xFAF9DA98 , lit 0xC8588DAB)  -- KeyAddition

-- Round = 12
r2_12 :: Round
r2_12 = Round 12
        (lit 0x8E73641C , lit 0xF7BD7E3C , lit 0x2D995746 , lit 0xE86A5D62)  -- Substitution
        (lit 0x8EBD5762 , lit 0xF7995D1C , lit 0x2D6A643C , lit 0xE8737E46)  -- ShiftRow
        (lit 0xEE743BA7 , lit 0x0425F0FE , lit 0xBC69CB01 , lit 0x66CAADA2)  -- MixColumn
        (lit 0x9AE87C0C , lit 0x1C75ED24 , lit 0x5E1CB54E , lit 0x12CB3DF8)  -- KeyAddition

-- Round = 13
r2_13 :: Round
r2_13 = Round 13
        (lit 0xB89B10FE , lit 0x9C9D5536 , lit 0x589CD52F , lit 0xC91F2741)  -- Substitution
        (lit 0xB89DD541 , lit 0x9C9C27FE , lit 0x581F1036 , lit 0xC99B552F)  -- ShiftRow
        (lit 0x43BC5719 , lit 0x452857E3 , lit 0xB7603D8B , lit 0x453489D0)  -- MixColumn
        (lit 0x8946FDFA , lit 0xA1FDCCD7 , lit 0x2DBF5745 , lit 0xF82490DD)  -- KeyAddition

-- Round = 14
r2_14 :: Round
r2_14 = Final 14
        (lit 0xA75A542D , lit 0x32544B0E , lit 0xD8085B6E , lit 0x413660C1)  -- Substitution
        (lit 0xA7545BC1 , lit 0x3208602D , lit 0xD836540E , lit 0x415A4B6E)  -- ShiftRow
        (lit 0x591CCB10 , lit 0xD410ED26 , lit 0xDC5BA74A , lit 0x31362870)  -- KeyAddition


-- |
-- | Start of third call to cipher
-- |

rounds3 :: [Round]
rounds3 = [ r3_1 , r3_2 , r3_3 , r3_4 , r3_5 , r3_6 , r3_7 
          , r3_8 , r3_9 , r3_10 , r3_11 , r3_12 , r3_13 , r3_14]

s2 :: State
s2 = addRoundKey (transpose (extractRoundKey w 0)) (initState plaintext2)

c2 :: State -- from the doc, same as s2
c2 = w32s2state (lit 0x50F5F756 , lit 0xB69695AF , lit 0xCE886FE9 , lit 0x9F77256E)  -- KeyAddition

-- Round = 1
r3_1 :: Round
r3_1 = Round 1
       (lit 0x53E668B1 , lit 0x4E902A79 , lit 0x8BC4A81E , lit 0xDBF53F9F)  -- Substitution
       (lit 0x5390A89F , lit 0x4EC43FB1 , lit 0x8BF56879 , lit 0xDBE62A1E)  -- ShiftRow
       (lit 0x3A1432E8 , lit 0x452D3C50 , lit 0x18BB25E9 , lit 0xA86C4B86)  -- MixColumn
       (lit 0x25211EEF , lit 0x7E4C3487 , lit 0x3523354A , lit 0xA1789472)  -- KeyAddition

-- Round = 2
r3_2 :: Round
r3_2 = Round 2
       (lit 0x3FFD72DF , lit 0xF3291817 , lit 0x962696D6 , lit 0x32BC2240)  -- Substitution
       (lit 0x3F299640 , lit 0xF32622DF , lit 0x96BC7217 , lit 0x32FD18D6)  -- ShiftRow
       (lit 0xD38CE17E , lit 0x6A06EBAF , lit 0x8D74F741 , lit 0xB62D9E04)  -- MixColumn
       (lit 0x482FB56F , lit 0xE46FCE00 , lit 0x286E7C1E , lit 0x964A62DA)  -- KeyAddition

-- Round = 3
r3_3 :: Round
r3_3 = Round 3
       (lit 0x5215D5A8 , lit 0x69A88B63 , lit 0x349F1072 , lit 0x90D6AA57)  -- Substitution
       (lit 0x52A81057 , lit 0x699FAAA8 , lit 0x34D6D563 , lit 0x90158B72)  -- ShiftRow
       (lit 0x007E23E0 , lit 0x6A015AC5 , lit 0xBF84F699 , lit 0xFD4E1ED1)  -- MixColumn
       (lit 0xA8CEBFFA , lit 0xF9D0CE08 , lit 0x01CD72F7 , lit 0x4A13454B)  -- KeyAddition

-- Round = 4
r3_4 :: Round
r3_4 = Round 4
       (lit 0xC28B082D , lit 0x99708B30 , lit 0x7CBD4068 , lit 0xD67D6EB3)  -- Substitution
       (lit 0xC27040B3 , lit 0x99BD6E2D , lit 0x7C7D0830 , lit 0xD68B8B68)  -- ShiftRow
       (lit 0xFC51FC10 , lit 0xB6678F39 , lit 0x47AE4191 , lit 0xD235E8B1)  -- MixColumn
       (lit 0x29CB10A8 , lit 0xED94462E , lit 0xB94703D9 , lit 0x0CBB5627)  -- KeyAddition

-- Round = 5
r3_5 :: Round
r3_5 = Round 5
       (lit 0xA51FCAC2 , lit 0x55225A31 , lit 0x56A07B35 , lit 0xFEEAB1CC)  -- Substitution
       (lit 0xA5227BCC , lit 0x55A0B1C2 , lit 0x56EACA31 , lit 0xFE1F5A35)  -- ShiftRow
       (lit 0x80A03E2E , lit 0x2204D171 , lit 0x72ED60B8 , lit 0xA91B0A36)  -- MixColumn
       (lit 0x35090CA4 , lit 0x047C7736 , lit 0xEADC4291 , lit 0x86777385)  -- KeyAddition

-- Round = 6
r3_6 :: Round
r3_6 = Round 6
       (lit 0x9601FE49 , lit 0xF210F505 , lit 0x87862C81 , lit 0x44F58F97)  -- Substitution
       (lit 0x96102C97 , lit 0xF2868F49 , lit 0x87F5FE05 , lit 0x4401F581)  -- ShiftRow
       (lit 0xBC557CA8 , lit 0xA826AA96 , lit 0xEA6A9A93 , lit 0xFFC32C21)  -- MixColumn
       (lit 0x3D79FD05 , lit 0x72F9E22C , lit 0xCE5C9061 , lit 0x057B9845)  -- KeyAddition

-- Round = 7
r3_7 :: Round
r3_7 = Round 7
       (lit 0x27B6546B , lit 0x40999871 , lit 0x8B4A60EF , lit 0x6B21466E)  -- Substitution
       (lit 0x2799606E , lit 0x404A466B , lit 0x8B215471 , lit 0x6BB698EF)  -- ShiftRow
       (lit 0xF0C0CC4C , lit 0x73753B1A , lit 0x4B449111 , lit 0x6040DC56)  -- MixColumn
       (lit 0x68057385 , lit 0xCDC82294 , lit 0x6DC8AAB6 , lit 0x69A09E42)  -- KeyAddition

-- Round = 8
r3_8 :: Round
r3_8 = Round 8
       (lit 0x456B8F97 , lit 0xBDE89322 , lit 0x3CE8AC4E , lit 0xF9E00B2C)  -- Substitution
       (lit 0x45E8AC2C , lit 0xBDE80B97 , lit 0x3CE08F22 , lit 0xF96B934E)  -- ShiftRow
       (lit 0x294D9AD3 , lit 0xDEFCE10A , lit 0xEE4FBF6F , lit 0x89CF7D74)  -- MixColumn
       (lit 0x414DE17F , lit 0x6C23D21C , lit 0x78A6868B , lit 0xE59EF0F4)  -- KeyAddition

-- Round = 9
r3_9 :: Round
r3_9 = Round 9
       (lit 0x83E3F8D2 , lit 0x5026B59C , lit 0xBC24443D , lit 0xD90B8CBF)  -- Substitution
       (lit 0x832644BF , lit 0x50248CD2 , lit 0xBC0BF89C , lit 0xD9E3B53D)  -- ShiftRow
       (lit 0x8CBCF799 , lit 0x92451AE7 , lit 0x1A25E30F , lit 0x1FFD0C5C)  -- MixColumn
       (lit 0x44A8159D , lit 0xE4ECE16D , lit 0x4A002322 , lit 0x46388E65)  -- KeyAddition

-- Round = 10
r3_10 :: Round
r3_10 = Round 10
        (lit 0x1BC2595E , lit 0x69CEF83C , lit 0xD6632693 , lit 0x5A07194D)  -- Substitution
        (lit 0x1BCE264D , lit 0x6963195E , lit 0xD607593C , lit 0x5AC2F893)  -- ShiftRow
        (lit 0x14BB4E5F , lit 0x30DADA7D , lit 0xDB0F2747 , lit 0x8245DDE9)  -- MixColumn
        (lit 0xCAA82738 , lit 0x5C16800C , lit 0x212A44D2 , lit 0x143133FC)  -- KeyAddition

-- Round = 11
r3_11 :: Round
r3_11 = Round 11
        (lit 0x74C2CC07 , lit 0x4A47CDFE , lit 0xFDE51BB5 , lit 0xFAC7C3B0)  -- Substitution
        (lit 0x74471BB0 , lit 0x4AE5C307 , lit 0xFDC7CCFE , lit 0xFAC2CDB5)  -- ShiftRow
        (lit 0x8A67CEBB , lit 0x64C23BF6 , lit 0x81D9A0F0 , lit 0xCA9C7D6B)  -- MixColumn
        (lit 0xD2E104E6 , lit 0x4AED0A21 , lit 0xFFD3510A , lit 0xED530EA8)  -- KeyAddition

-- Round = 12
r3_12 :: Round
r3_12 = Round 12
        (lit 0xB5F8F28E , lit 0xD65567FD , lit 0x1666D167 , lit 0x55EDABC2)  -- Substitution
        (lit 0xB555D1C2 , lit 0xD666AB8E , lit 0x16EDF2FD , lit 0x55F86767)  -- ShiftRow
        (lit 0x9DB504DF , lit 0x387274AB , lit 0x0F2718C4 , lit 0xB970CAAE)  -- MixColumn
        (lit 0xE9294374 , lit 0x20226971 , lit 0xED52668B , lit 0xCD715AF4)  -- KeyAddition

-- Round = 13
r3_13 :: Round
r3_13 = Round 13
        (lit 0x1EA51A92 , lit 0xB793F9A3 , lit 0x5500333D , lit 0xBDA3BEBF)  -- Substitution
        (lit 0x1E9333BF , lit 0xB700BE92 , lit 0x55A31AA3 , lit 0xBDA5F93D)  -- ShiftRow
        (lit 0x1EC931E7 , lit 0x59FC7D43 , lit 0xED853C1B , lit 0x51C1B6FA)  -- MixColumn
        (lit 0xD4339B04 , lit 0xBD29E677 , lit 0x775A56D5 , lit 0xECD1AFF7)  -- KeyAddition

r3_14 :: Round
r3_14 = Final 14
        (lit 0x48C314F2 , lit 0x7AA58EF5 , lit 0xF5BEB103 , lit 0xCE3E7968)  -- Substitution
        (lit 0x48A5B168 , lit 0x7ABE79F2 , lit 0xF53E14F5 , lit 0xCEC38E03)  -- ShiftRow
        (lit 0xB6ED21B9 , lit 0x9CA6F4F9 , lit 0xF153E7B1 , lit 0xBEAFED1D)  -- KeyAddition


-- |
-- | Start of fourth call to cipher
-- |

rounds4 :: [Round]
rounds4 = [ r4_1 , r4_2 , r4_3 , r4_4 , r4_5 , r4_6 , r4_7 
          , r4_8 , r4_9 , r4_10 , r4_11 , r4_12 , r4_13 , r4_14]

s3 :: State
s3 = addRoundKey (transpose (extractRoundKey w 0)) (initState plaintext3)

c3 :: State -- from the doc, same as s3
c3 = w32s2state (lit 0x96A2CF55 , lit 0xCA85EAA9 , lit 0x8658EF8B , lit 0x63114091)  -- KeyAddition

-- Round = 1
r4_1 :: Round
r4_1 = Round 1
       (lit 0x903A8AFC , lit 0x749787D3 , lit 0x446ADF3D , lit 0xFB820981)  -- Substitution
       (lit 0x9097DF81 , lit 0x746A09FC , lit 0x44828AD3 , lit 0xFB3A873D)  -- ShiftRow
       (lit 0xC75E3AFA , lit 0xA347131C , lit 0x4C0DA779 , lit 0x192093D1)  -- MixColumn
       (lit 0xD86B16FD , lit 0x98261BCB , lit 0x6195B7DA , lit 0x10344C25)  -- KeyAddition

-- Round = 2
r4_2 :: Round
r4_2 = Round 2
       (lit 0x617F4754 , lit 0x46F7AF1F , lit 0xEF2AA957 , lit 0xCA18293F)  -- Substitution
       (lit 0x61F7A93F , lit 0x462A2954 , lit 0xEF18471F , lit 0xCA7FAF57)  -- ShiftRow
       (lit 0x564B9E83 , lit 0x8F3DC261 , lit 0xB509584B , lit 0xF689093B)  -- MixColumn
       (lit 0xCDE8CA92 , lit 0x0154E7CE , lit 0x1013D314 , lit 0xD6EEF5E5)  -- KeyAddition

-- Round = 3
r4_3 :: Round
r4_3 = Round 3
       (lit 0xBD9B744F , lit 0x7C20948B , lit 0xCA7D66FA , lit 0xF628E6D9)  -- Substitution
       (lit 0xBD2066D9 , lit 0x7C7DE64F , lit 0xCA28748B , lit 0xF69B94FA)  -- ShiftRow
       (lit 0xBE8E2133 , lit 0xD6F80781 , lit 0x088D8C14 , lit 0x2F864BE1)  -- MixColumn
       (lit 0x163EBD29 , lit 0x4529934C , lit 0xB6C4087A , lit 0x98DB107B)  -- KeyAddition

-- Round = 4
r4_4 :: Round
r4_4 = Round 4
       (lit 0x47B27AA5 , lit 0x6EA5DC29 , lit 0x4E1C30DA , lit 0x46B9CA21)  -- Substitution
       (lit 0x47A53021 , lit 0x6E1CCAA5 , lit 0x4EB97A29 , lit 0x46B2DCDA)  -- ShiftRow
       (lit 0x6B67E11E , lit 0x97B60935 , lit 0x1F807843 , lit 0x479C220B)  -- MixColumn
       (lit 0xBEFD0DA6 , lit 0xCC45C022 , lit 0xE1693A0B , lit 0x99129C9D)  -- KeyAddition

-- Round = 5
r4_5 :: Round
r4_5 = Round 5
       (lit 0xAE54D724 , lit 0x4B6EBA93 , lit 0xF8F9802B , lit 0xEEC9DE5E)  -- Substitution
       (lit 0xAE6E805E , lit 0x4BF9DE24 , lit 0xF8C9D793 , lit 0xEE54BA2B)  -- ShiftRow
       (lit 0x2BB739BB , lit 0x7CFF79B2 , lit 0xEF802A30 , lit 0xAAB8A891)  -- MixColumn
       (lit 0x9E1E0B31 , lit 0x5A87DFF5 , lit 0x77B10819 , lit 0x85D4D122)  -- KeyAddition

-- Round = 6
r4_6 :: Round
r4_6 = Round 6
       (lit 0x0B722BC7 , lit 0xBE179EE6 , lit 0xF5C830D4 , lit 0x97483E93)  -- Substitution
       (lit 0x0B173093 , lit 0xBEC83EC7 , lit 0xF5482BE6 , lit 0x97729ED4)  -- ShiftRow
       (lit 0x8CE6D207 , lit 0xDDB058BA , lit 0xE4FEDAB0 , lit 0xE91EA5FD)  -- MixColumn
       (lit 0x0DCA53AA , lit 0x076F1000 , lit 0xC0C8D042 , lit 0x13A61199)  -- KeyAddition

-- Round = 7
r4_7 :: Round
r4_7 = Round 7
       (lit 0xD774EDAC , lit 0xC5A8CA63 , lit 0xBAE8702C , lit 0x7D2482EE)  -- Substitution
       (lit 0xD7A870EE , lit 0xC5E882AC , lit 0xBA24ED63 , lit 0x7D74CA2C)  -- ShiftRow
       (lit 0xC8E2B67D , lit 0x9C3FDD7D , lit 0x8DBDFADA , lit 0x80FCF261)  -- MixColumn
       (lit 0x502709B4 , lit 0x2282C4F3 , lit 0xAB31C17D , lit 0x891CB075)  -- KeyAddition

-- Round = 8
r4_8 :: Round
r4_8 = Round 8
       (lit 0x53CC018D , lit 0x93131C0D , lit 0x62C778FF , lit 0xA79CE79D)  -- Substitution
       (lit 0x5313789D , lit 0x93C7E78D , lit 0x629C010D , lit 0xA7CC1CFF)  -- ShiftRow
       (lit 0x76600CBF , lit 0x05B90D8F , lit 0x774FEB21 , lit 0xF9FF49C7)  -- MixColumn
       (lit 0x1E607713 , lit 0xB7663E99 , lit 0xE1A6D2C5 , lit 0x95AEC447)  -- KeyAddition

-- Round = 9
r4_9 :: Round
r4_9 = Round 9
       (lit 0x72D0F57D , lit 0xA933B2EE , lit 0xF824B5A6 , lit 0x2AE41CA0)  -- Substitution
       (lit 0x7233B5A0 , lit 0xA9241C7D , lit 0xF8E4F5EE , lit 0x2AD0B2A6)  -- ShiftRow
       (lit 0xA470CB4B , lit 0x44B83222 , lit 0xC7C1C4C5 , lit 0x2BFA744B)  -- MixColumn
       (lit 0x6C64294F , lit 0x3211C9A8 , lit 0x97E404E8 , lit 0x723FF672)  -- KeyAddition

-- Round = 10
r4_10 :: Round
r4_10 = Round 10
        (lit 0x5043A584 , lit 0x2382DDC2 , lit 0x8869F29B , lit 0x40754240)  -- Substitution
        (lit 0x5082F240 , lit 0x23694284 , lit 0x8875A5C2 , lit 0x4043DD9B)  -- ShiftRow
        (lit 0x8F02ED00 , lit 0x3BB3595D , lit 0xF354F1CC , lit 0x03211473)  -- MixColumn
        (lit 0x51118467 , lit 0x577F032C , lit 0x09719259 , lit 0x9555FA66)  -- KeyAddition

-- Round = 11
r4_11 :: Round
r4_11 = Round 11
        (lit 0xD1825F85 , lit 0x5BD27B71 , lit 0x01A34FCB , lit 0x2AFC2D33)  -- Substitution
        (lit 0xD1D24F33 , lit 0x5BA32D85 , lit 0x01FC5F71 , lit 0x2A827BCB)  -- ShiftRow
        (lit 0xA88CC893 , lit 0xE0F43672 , lit 0x3372D042 , lit 0x7973180A)  -- MixColumn
        (lit 0xF00A02CE , lit 0xCEDB07A5 , lit 0x4D7821B8 , lit 0x5EBC6BC9)  -- KeyAddition

-- Round = 12
r4_12 :: Round
r4_12 = Round 12
        (lit 0x8C67778B , lit 0x8BB9C506 , lit 0xE3BCFD6C , lit 0x58657FDD)  -- Substitution
        (lit 0x8CB9FDDD , lit 0x8BBC7F8B , lit 0xE3657706 , lit 0x5867C56C)  -- ShiftRow
        (lit 0xF324A86A , lit 0x26E24F48 , lit 0x03B66220 , lit 0xB0AE1A92)  -- MixColumn
        (lit 0x87B8EFC1 , lit 0x3EB25292 , lit 0xE1C31C6F , lit 0xC4AF8AC8)  -- KeyAddition

-- Round = 13
r4_13 :: Round
r4_13 = Round 13
        (lit 0x176CDF78 , lit 0xB237004F , lit 0xF82E9CA8 , lit 0x1C797EE8)  -- Substitution
        (lit 0x17379CE8 , lit 0xB22E7E78 , lit 0xF879DF4F , lit 0x1C6C00A8)  -- ShiftRow
        (lit 0x032E2059 , lit 0x0B14E86D , lit 0xF03FF52B , lit 0x246C9303)  -- MixColumn
        (lit 0xC9D48ABA , lit 0xEFC17359 , lit 0x6AE09FE5 , lit 0x997C8A0E)  -- KeyAddition

r4_14 :: Round
r4_14 = Final 14
        (lit 0xDD487EF4 , lit 0xDF788FCB , lit 0x02E1DBD9 , lit 0xEE107EAB)  -- Substitution
        (lit 0xDD78DBAB , lit 0xDFE17EF4 , lit 0x02107ECB , lit 0xEE488FD9)  -- ShiftRow
        (lit 0x23304B7A , lit 0x39F9F3FF , lit 0x067D8D8F , lit 0x9E24ECC7)  -- KeyAddition

{-


-- Ciphertext is
(lit 0xF3EED1BD , lit 0xB5D2A03C , lit 0x064B5A7E , lit 0x3DB181F8)
--
-- -- 8 of 15 --
--
-- --------------------------------------------------------------
(lit 0x591CCB10 , lit 0xD410ED26 , lit 0xDC5BA74A , lit 0x31362870)
(lit 0xB6ED21B9 , lit 0x9CA6F4F9 , lit 0xF153E7B1 , lit 0xBEAFED1D)
(lit 0x23304B7A , lit 0x39F9F3FF , lit 0x067D8D8F , lit 0x9E24ECC7)
-- ==============================================================
-- ECB-AES256 (Decryption)
-- Key is
(lit 0x603DEB10 , lit 0x15CA71BE , lit 0x2B73AEF0 , lit 0x857D7781)
(lit 0x1F352C07 , lit 0x3B6108D7 , lit 0x2D9810A3 , lit 0x0914DFF4)
-- Ciphertext is
(lit 0xF3EED1BD , lit 0xB5D2A03C , lit 0x064B5A7E , lit 0x3DB181F8)
(lit 0x591CCB10 , lit 0xD410ED26 , lit 0xDC5BA74A , lit 0x31362870)
(lit 0xB6ED21B9 , lit 0x9CA6F4F9 , lit 0xF153E7B1 , lit 0xBEAFED1D)
(lit 0x23304B7A , lit 0x39F9F3FF , lit 0x067D8D8F , lit 0x9E24ECC7)
(lit 0x0DA6416C , lit 0x53CA2D37 , lit 0x0226A93A , lit 0x4DDDE2E6)  -- KeyAddition
(lit 0xF3C5F8B8 , lit 0x5010FAB2 , lit 0x6A23B7A2 , lit 0x65C93BF5)  -- Substitution
(lit 0xF3C9B7B2 , lit 0x50C53BA2 , lit 0x6A10F8F5 , lit 0x6523FAB8)  -- ShiftRow
-- Round = 13
(lit 0x39331D51 , lit 0xB410A096 , lit 0xF0CF923B , lit 0xD833E3B5)  -- KeyAddition
(lit 0xFDBC5552 , lit 0xD92BF292 , lit 0x57C20201 , lit 0xDB848163)  -- InvMixColumn
(lit 0x2178ED48 , lit 0xE50B0474 , lit 0xDAA86A09 , lit 0x9F4F9100)  -- Substitution
(lit 0x214F6A74 , lit 0xE5789109 , lit 0xDA0BED00 , lit 0x9FA80448)  -- ShiftRow
-- Round = 12
(lit 0x55D32DDF , lit 0xFD288CD3 , lit 0x387E934F , lit 0xEBA99412)  -- KeyAddition
(lit 0x40FDD019 , lit 0x250C5BF8 , lit 0x085D8C43 , lit 0x36B7DA9F)  -- InvMixColumn
(lit 0x7221608E , lit 0xC28157E1 , lit 0xBF8DF064 , lit 0x24207A6E)  -- Substitution
(lit 0x7220F0E1 , lit 0xC2217A64 , lit 0xBF81606E , lit 0x248D578E)  -- ShiftRow
-- Round = 11
(lit 0x2AA63ABC , lit 0xEC0E4BB3 , lit 0xC18B9194 , lit 0x0342244D)  -- KeyAddition
(lit 0x33B737B9 , lit 0x538CC005 , lit 0x0FC2C143 , lit 0x6EC1199E)  -- InvMixColumn
(lit 0x6620B2DB , lit 0x50F01F36 , lit 0xFBA8DD64 , lit 0x45DD8EDF)  -- Substitution
(lit 0x66DDDD36 , lit 0x50208E64 , lit 0xFBF0B2DF , lit 0x45A81FDB)  -- ShiftRow
-- Round = 10
(lit 0xB8CEB451 , lit 0x3CECD415 , lit 0x01D5D14A , lit 0xD3DCF1CE)  -- KeyAddition
(lit 0x031732B5 , lit 0x93CC5917 , lit 0xDE969097 , lit 0xEE65AC17)  -- InvMixColumn
(lit 0xD587A1D2 , lit 0x22271587 , lit 0x9C359685 , lit 0x99BCAA87)  -- Substitution
(lit 0xD5BC9687 , lit 0x2287AA85 , lit 0x9C27A187 , lit 0x993515D2)  -- ShiftRow
-- Round = 9
(lit 0x1DA87483 , lit 0x542E510F , lit 0xCC0261AA , lit 0xC0F097EB)  -- KeyAddition
(lit 0xB7C35A6C , lit 0xAB4BE622 , lit 0xE47F43DD , lit 0xFD4832CB)  -- InvMixColumn
(lit 0x203346B8 , lit 0x0ECCF594 , lit 0xAE6B64C9 , lit 0x21D4A159)  -- Substitution
--
-- -- 9 of 15 --
--
(lit 0x20D46494 , lit 0x0E33A1C9 , lit 0xAECC4659 , lit 0x216BF5B8)  -- ShiftRow
-- Round = 8
(lit 0x48D41F38 , lit 0xBCEC92DF , lit 0x38257FBD , lit 0x4D3A7838)  -- KeyAddition
(lit 0x38D00251 , lit 0x51294520 , lit 0x78BA9A87 , lit 0xE322B84E)  -- InvMixColumn
(lit 0x76606A70 , lit 0x704C6854 , lit 0xC1C037EA , lit 0x4D949AB6)  -- Substitution
(lit 0x76943754 , lit 0x70609AEA , lit 0xC14C6AB6 , lit 0x4DC06870)  -- ShiftRow
-- Round = 7
(lit 0xEE51889D , lit 0xCEDD8364 , lit 0xE7C05111 , lit 0x44202A64)  -- KeyAddition
(lit 0x8568ACEB , lit 0xD2A66CEC , lit 0x35E24DFD , lit 0x4E5E744E)  -- InvMixColumn
(lit 0x67F7AA3C , lit 0x7FC5B883 , lit 0xD93B6521 , lit 0xB69DCAB6)  -- Substitution
(lit 0x679D6583 , lit 0x7FF7CA21 , lit 0xD9C5AAB6 , lit 0xB63BB83C)  -- ShiftRow
-- Round = 6
(lit 0xE6B1E42E , lit 0xA528829B , lit 0xFDF3A044 , lit 0x4C830C58)  -- KeyAddition
(lit 0x59487AF6 , lit 0x60C57445 , lit 0x7544E73C , lit 0xFDE8901E)  -- InvMixColumn
(lit 0x15D4BDD6 , lit 0x9007CA68 , lit 0x3F86B06D , lit 0x21C896E9)  -- Substitution
(lit 0x15C8B068 , lit 0x90D4966D , lit 0x3F07BDE9 , lit 0x2186CAD6)  -- ShiftRow
-- Round = 5
(lit 0xA06182E2 , lit 0xB6AC302A , lit 0xA7369FC0 , lit 0x0EEAB365)  -- KeyAddition
(lit 0x6F581482 , lit 0xACA12A27 , lit 0x9A6E251F , lit 0xA6EB7F00)  -- InvMixColumn
(lit 0x065E9B11 , lit 0xAAF1953D , lit 0x3745C2CB , lit 0xC53C6B52)  -- Substitution
(lit 0x063CC23D , lit 0xAA5E6BCB , lit 0x37F19B52 , lit 0xC5459511)  -- ShiftRow
-- Round = 4
(lit 0xD3A62E85 , lit 0xF1ADA2DC , lit 0xC918D91A , lit 0x1BCB2B87)  -- KeyAddition
(lit 0x9455CED1 , lit 0x37452070 , lit 0xB20B862D , lit 0xD18A193E)  -- InvMixColumn
(lit 0xE7EDEC51 , lit 0xB26854D0 , lit 0x3E9EDCFA , lit 0x51CF8ED1)  -- Substitution
(lit 0xE7CFDCD0 , lit 0xB2ED8EFA , lit 0x3E68ECD1 , lit 0x519E5451)  -- ShiftRow
-- Round = 3
(lit 0x4F7F40CA , lit 0x213C1A37 , lit 0x802168BF , lit 0xE6C30FCB)  -- KeyAddition
(lit 0x0EFE6E24 , lit 0x5CF7079C , lit 0xBF16BB64 , lit 0x8DC8BD19)  -- InvMixColumn
(lit 0xD70C45A6 , lit 0xA726381C , lit 0xF4FFFE8C , lit 0xB4B1CD8E)  -- Substitution
(lit 0xD7B1FE1C , lit 0xA70CCD8C , lit 0xF426458E , lit 0xB4FF38A6)  -- ShiftRow
-- Round = 2
(lit 0x4C12AA0D , lit 0x2965E823 , lit 0x513CCED1 , lit 0x9498C478)  -- KeyAddition
(lit 0x35753A83 , lit 0x4BB49FE7 , lit 0x0E95759C , lit 0x90CABB51)  -- InvMixColumn
(lit 0xD93FA241 , lit 0xCCC66EB0 , lit 0xD7AD3F1C , lit 0x9610FE70)  -- Substitution
(lit 0xD9103FB0 , lit 0xCC3FFE1C , lit 0xD7C6A270 , lit 0x96AD6E41)  -- ShiftRow
-- Round = 1
(lit 0xC62513B7 , lit 0xF75EF6CB , lit 0xFA5EB2D3 , lit 0x9FB9B1B5)  -- KeyAddition
(lit 0x2B7E7062 , lit 0xE22FD089 , lit 0x2528FC34 , lit 0x42B028F8)  -- InvMixColumn
(lit 0x0B8AD0AB , lit 0x3B4E60F2 , lit 0xC2EE5528 , lit 0xF6FCEEE1)  -- Substitution
(lit 0x0BFC55F2 , lit 0x3B8AEE28 , lit 0xC24ED0E1 , lit 0xF6EE60AB)  -- ShiftRow
(lit 0x6BC1BEE2 , lit 0x2E409F96 , lit 0xE93D7E11 , lit 0x7393172A)  -- KeyAddition
(lit 0xA7545BC1 , lit 0x3208602D , lit 0xD836540E , lit 0x415A4B6E)  -- KeyAddition
(lit 0x89FD57DD , lit 0xA1BF90FA , lit 0x2D24FDD7 , lit 0xF846CC45)  -- Substitution
(lit 0x8946FDFA , lit 0xA1FDCCD7 , lit 0x2DBF5745 , lit 0xF82490DD)  -- ShiftRow
--
-- -- 10 of 15 --
--
-- Round = 13
(lit 0x43BC5719 , lit 0x452857E3 , lit 0xB7603D8B , lit 0x453489D0)  -- KeyAddition
(lit 0xB89DD541 , lit 0x9C9C27FE , lit 0x581F1036 , lit 0xC99B552F)  -- InvMixColumn
(lit 0x9A75B5F8 , lit 0x1C1C3D0C , lit 0x5ECB7C24 , lit 0x12E8ED4E)  -- Substitution
(lit 0x9AE87C0C , lit 0x1C75ED24 , lit 0x5E1CB54E , lit 0x12CB3DF8)  -- ShiftRow
-- Round = 12
(lit 0xEE743BA7 , lit 0x0425F0FE , lit 0xBC69CB01 , lit 0x66CAADA2)  -- KeyAddition
(lit 0x8EBD5762 , lit 0xF7995D1C , lit 0x2D6A643C , lit 0xE8737E46)  -- InvMixColumn
(lit 0xE6CDDAAB , lit 0x26F98DC4 , lit 0xFA588C6D , lit 0xC88F8A98)  -- Substitution
(lit 0xE68F8CC4 , lit 0x26CD8A6D , lit 0xFAF9DA98 , lit 0xC8588DAB)  -- ShiftRow
-- Round = 11
(lit 0xBE094699 , lit 0x08E2BBBA , lit 0x84F32B62 , lit 0xEF97FE68)  -- KeyAddition
(lit 0x03E42BA4 , lit 0xCFD9B14C , lit 0x35FFE612 , lit 0x844B5273)  -- InvMixColumn
(lit 0xD5AE0B1D , lit 0x5FE5565D , lit 0xD97DF539 , lit 0x4FCC488F)  -- Substitution
(lit 0xD5CCF55D , lit 0x5FAE4839 , lit 0xD9E50B8F , lit 0x4F7D561D)  -- ShiftRow
-- Round = 10
(lit 0x0BDF9C3A , lit 0x33621248 , lit 0x23C0681A , lit 0xD909B808)  -- KeyAddition
(lit 0x1D2F6E2E , lit 0x46797145 , lit 0xBCBBCE58 , lit 0xB03911F8)  -- InvMixColumn
(lit 0xDE4E45C3 , lit 0x98AF2C68 , lit 0x78FEEC5E , lit 0xFC5BE3E1)  -- Substitution
(lit 0xDE5BEC68 , lit 0x984EE35E , lit 0x78AF45E1 , lit 0xFCFE2CC3)  -- ShiftRow
-- Round = 9
(lit 0x164F0E6C , lit 0xEEE718D4 , lit 0x288A85CC , lit 0xA53BAEFA)  -- KeyAddition
(lit 0x3CB952EC , lit 0xBE79D9DB , lit 0x076567EE , lit 0x6EE3EAAD)  -- InvMixColumn
(lit 0x6DDB4883 , lit 0x5AAFE59F , lit 0x38BC0A99 , lit 0x454DBB18)  -- Substitution
(lit 0x6D4D0A9F , lit 0x5ADBBB99 , lit 0x38AF4818 , lit 0x45BCE583)  -- ShiftRow
-- Round = 8
(lit 0x054D7133 , lit 0xE804888F , lit 0xAE4671FC , lit 0x29ED6803)  -- KeyAddition
(lit 0x048C64E6 , lit 0x42EFEFA9 , lit 0x549641E6 , lit 0x05C90162)  -- InvMixColumn
(lit 0x30F08CF5 , lit 0xF66161B7 , lit 0xFD35F8F5 , lit 0x361209AB)  -- Substitution
(lit 0x3012F8B7 , lit 0xF6F009F5 , lit 0xFD618CAB , lit 0x363561F5)  -- ShiftRow
-- Round = 7
(lit 0xA8D7477E , lit 0x484D107B , lit 0xDBEDB70C , lit 0x3FD523E1)  -- KeyAddition
(lit 0x873EE41B , lit 0x0A1C8EF6 , lit 0x9579F899 , lit 0xF381BFE5)  -- InvMixColumn
(lit 0xEAD1AE44 , lit 0xA3C4E6D6 , lit 0xADAFE1F9 , lit 0x7E91F42A)  -- Substitution
(lit 0xEA91E1D6 , lit 0xA3D1F4F9 , lit 0xADC4AE2A , lit 0x7EAFE644)  -- ShiftRow
-- Round = 6
(lit 0x6BBD607B , lit 0x790EBC43 , lit 0x89F2A4D8 , lit 0x84175220)  -- KeyAddition
(lit 0x0FD68591 , lit 0x0AFAC4BC , lit 0xF4C9D7ED , lit 0x64E96B07)  -- InvMixColumn
(lit 0xFB4A67AC , lit 0xA3148878 , lit 0xBA120D53 , lit 0x8CEB0538)  -- Substitution
(lit 0xFBEB0D78 , lit 0xA34A0553 , lit 0xBA146738 , lit 0x8C1288AC)  -- ShiftRow
-- Round = 5
(lit 0x4E423FF2 , lit 0x8532A314 , lit 0x22254511 , lit 0xA37EF11F)  -- KeyAddition
(lit 0x1ADDF2F4 , lit 0x68934EB5 , lit 0x56C8975A , lit 0xE154D553)  -- InvMixColumn
(lit 0x43C904BA , lit 0xF722B6D2 , lit 0xB9B18546 , lit 0xE0FDB550)  -- Substitution
(lit 0x43FD85D2 , lit 0xF7C9B546 , lit 0xB9220450 , lit 0xE0B1B6BA)  -- ShiftRow
-- Round = 4
--
-- -- 11 of 15 --
--
(lit 0x9667696A , lit 0xAC3A7C51 , lit 0x47CB4618 , lit 0x3E3F082C)  -- KeyAddition
(lit 0x9D6C9D9E , lit 0xE2155F13 , lit 0x58B3E6DF , lit 0xF20B8E52)  -- InvMixColumn
(lit 0x75B875DF , lit 0x3B2F8482 , lit 0x5E4BF5EF , lit 0x049EE648)  -- Substitution
(lit 0x759EF582 , lit 0x3BB8E6EF , lit 0x5E2F7548 , lit 0x044B84DF)  -- ShiftRow
-- Round = 3
(lit 0xDD2E6998 , lit 0xA8697222 , lit 0xE066F126 , lit 0xB316DF45)  -- KeyAddition
(lit 0xD45C64EE , lit 0x011DE26F , lit 0x5CA7EF45 , lit 0x95148739)  -- InvMixColumn
(lit 0x19A78C99 , lit 0x09DE3B06 , lit 0xA7896168 , lit 0xAD9BEA5B)  -- Substitution
(lit 0x199B6106 , lit 0x09A7EA68 , lit 0xA7DE8C5B , lit 0xAD893B99)  -- ShiftRow
-- Round = 2
(lit 0x82383517 , lit 0x87CECFC7 , lit 0x02C40704 , lit 0x8DEEC747)  -- KeyAddition
(lit 0x33AAAFAE , lit 0x51979B1C , lit 0x36C3A292 , lit 0xC29455E0)  -- InvMixColumn
(lit 0x66621BBE , lit 0x7085E8C4 , lit 0x24331A74 , lit 0xA8E7EDA0)  -- Substitution
(lit 0x66E71AC4 , lit 0x7062ED74 , lit 0x24851BA0 , lit 0xA833E8BE)  -- ShiftRow
-- Round = 1
(lit 0x79D236C3 , lit 0x4B03E5A3 , lit 0x091D0B03 , lit 0xA127374A)  -- KeyAddition
(lit 0x8BDD7870 , lit 0x2B1C99A0 , lit 0xD5B5EF93 , lit 0xBACAC14A)  -- InvMixColumn
(lit 0xCEC9C1D0 , lit 0x0BC4F947 , lit 0xB5D26122 , lit 0xC010DD5C)  -- Substitution
(lit 0xCE106147 , lit 0x0BC9DD22 , lit 0xB5C4C15C , lit 0xC0D2F9D0)  -- ShiftRow
(lit 0xAE2D8A57 , lit 0x1E03AC9C , lit 0x9EB76FAC , lit 0x45AF8E51)  -- KeyAddition
(lit 0x48A5B168 , lit 0x7ABE79F2 , lit 0xF53E14F5 , lit 0xCEC38E03)  -- KeyAddition
(lit 0xD42956F7 , lit 0xBD5AAF04 , lit 0x77D19B77 , lit 0xEC33E6D5)  -- Substitution
(lit 0xD4339B04 , lit 0xBD29E677 , lit 0x775A56D5 , lit 0xECD1AFF7)  -- ShiftRow
-- Round = 13
(lit 0x1EC931E7 , lit 0x59FC7D43 , lit 0xED853C1B , lit 0x51C1B6FA)  -- KeyAddition
(lit 0x1E9333BF , lit 0xB700BE92 , lit 0x55A31AA3 , lit 0xBDA5F93D)  -- InvMixColumn
(lit 0xE92266F4 , lit 0x20525A74 , lit 0xED714371 , lit 0xCD29698B)  -- Substitution
(lit 0xE9294374 , lit 0x20226971 , lit 0xED52668B , lit 0xCD715AF4)  -- ShiftRow
-- Round = 12
(lit 0x9DB504DF , lit 0x387274AB , lit 0x0F2718C4 , lit 0xB970CAAE)  -- KeyAddition
(lit 0xB555D1C2 , lit 0xD666AB8E , lit 0x16EDF2FD , lit 0x55F86767)  -- InvMixColumn
(lit 0xD2ED51A8 , lit 0x4AD30EE6 , lit 0xFF530421 , lit 0xEDE10A0A)  -- Substitution
(lit 0xD2E104E6 , lit 0x4AED0A21 , lit 0xFFD3510A , lit 0xED530EA8)  -- ShiftRow
-- Round = 11
(lit 0x8A67CEBB , lit 0x64C23BF6 , lit 0x81D9A0F0 , lit 0xCA9C7D6B)  -- KeyAddition
(lit 0x74471BB0 , lit 0x4AE5C307 , lit 0xFDC7CCFE , lit 0xFAC2CDB5)  -- InvMixColumn
(lit 0xCA1644FC , lit 0x5C2A3338 , lit 0x2131270C , lit 0x14A880D2)  -- Substitution
(lit 0xCAA82738 , lit 0x5C16800C , lit 0x212A44D2 , lit 0x143133FC)  -- ShiftRow
-- Round = 10
(lit 0x14BB4E5F , lit 0x30DADA7D , lit 0xDB0F2747 , lit 0x8245DDE9)  -- KeyAddition
(lit 0x1BCE264D , lit 0x6963195E , lit 0xD607593C , lit 0x5AC2F893)  -- InvMixColumn
(lit 0x44EC2365 , lit 0xE4008E9D , lit 0x4A38156D , lit 0x46A8E122)  -- Substitution
(lit 0x44A8159D , lit 0xE4ECE16D , lit 0x4A002322 , lit 0x46388E65)  -- ShiftRow
-- Round = 9
(lit 0x8CBCF799 , lit 0x92451AE7 , lit 0x1A25E30F , lit 0x1FFD0C5C)  -- KeyAddition
--
-- -- 12 of 15 --
--
(lit 0x832644BF , lit 0x50248CD2 , lit 0xBC0BF89C , lit 0xD9E3B53D)  -- InvMixColumn
(lit 0x412386F4 , lit 0x6CA6F07F , lit 0x789EE11C , lit 0xE54DD28B)  -- Substitution
(lit 0x414DE17F , lit 0x6C23D21C , lit 0x78A6868B , lit 0xE59EF0F4)  -- ShiftRow
-- Round = 8
(lit 0x294D9AD3 , lit 0xDEFCE10A , lit 0xEE4FBF6F , lit 0x89CF7D74)  -- KeyAddition
(lit 0x45E8AC2C , lit 0xBDE80B97 , lit 0x3CE08F22 , lit 0xF96B934E)  -- InvMixColumn
(lit 0x68C8AA42 , lit 0xCDC89E85 , lit 0x6DA07394 , lit 0x690522B6)  -- Substitution
(lit 0x68057385 , lit 0xCDC82294 , lit 0x6DC8AAB6 , lit 0x69A09E42)  -- ShiftRow
-- Round = 7
(lit 0xF0C0CC4C , lit 0x73753B1A , lit 0x4B449111 , lit 0x6040DC56)  -- KeyAddition
(lit 0x2799606E , lit 0x404A466B , lit 0x8B215471 , lit 0x6BB698EF)  -- InvMixColumn
(lit 0x3DF99045 , lit 0x725C9805 , lit 0xCE7BFD2C , lit 0x0579E261)  -- Substitution
(lit 0x3D79FD05 , lit 0x72F9E22C , lit 0xCE5C9061 , lit 0x057B9845)  -- ShiftRow
-- Round = 6
(lit 0xBC557CA8 , lit 0xA826AA96 , lit 0xEA6A9A93 , lit 0xFFC32C21)  -- KeyAddition
(lit 0x96102C97 , lit 0xF2868F49 , lit 0x87F5FE05 , lit 0x4401F581)  -- InvMixColumn
(lit 0x357C4285 , lit 0x04DC73A4 , lit 0xEA770C36 , lit 0x86097791)  -- Substitution
(lit 0x35090CA4 , lit 0x047C7736 , lit 0xEADC4291 , lit 0x86777385)  -- ShiftRow
-- Round = 5
(lit 0x80A03E2E , lit 0x2204D171 , lit 0x72ED60B8 , lit 0xA91B0A36)  -- KeyAddition
(lit 0xA5227BCC , lit 0x55A0B1C2 , lit 0x56EACA31 , lit 0xFE1F5A35)  -- InvMixColumn
(lit 0x29940327 , lit 0xED4756A8 , lit 0xB9BB102E , lit 0x0CCB46D9)  -- Substitution
(lit 0x29CB10A8 , lit 0xED94462E , lit 0xB94703D9 , lit 0x0CBB5627)  -- ShiftRow
-- Round = 4
(lit 0xFC51FC10 , lit 0xB6678F39 , lit 0x47AE4191 , lit 0xD235E8B1)  -- KeyAddition
(lit 0xC27040B3 , lit 0x99BD6E2D , lit 0x7C7D0830 , lit 0xD68B8B68)  -- InvMixColumn
(lit 0xA8D0724B , lit 0xF9CD45FA , lit 0x0113BF08 , lit 0x4ACECEF7)  -- Substitution
(lit 0xA8CEBFFA , lit 0xF9D0CE08 , lit 0x01CD72F7 , lit 0x4A13454B)  -- ShiftRow
-- Round = 3
(lit 0x007E23E0 , lit 0x6A015AC5 , lit 0xBF84F699 , lit 0xFD4E1ED1)  -- KeyAddition
(lit 0x52A81057 , lit 0x699FAAA8 , lit 0x34D6D563 , lit 0x90158B72)  -- InvMixColumn
(lit 0x486F7CDA , lit 0xE46E626F , lit 0x284AB500 , lit 0x962FCE1E)  -- Substitution
(lit 0x482FB56F , lit 0xE46FCE00 , lit 0x286E7C1E , lit 0x964A62DA)  -- ShiftRow
-- Round = 2
(lit 0xD38CE17E , lit 0x6A06EBAF , lit 0x8D74F741 , lit 0xB62D9E04)  -- KeyAddition
(lit 0x3F299640 , lit 0xF32622DF , lit 0x96BC7217 , lit 0x32FD18D6)  -- InvMixColumn
(lit 0x254C3572 , lit 0x7E2394EF , lit 0x35781E87 , lit 0xA121344A)  -- Substitution
(lit 0x25211EEF , lit 0x7E4C3487 , lit 0x3523354A , lit 0xA1789472)  -- ShiftRow
-- Round = 1
(lit 0x3A1432E8 , lit 0x452D3C50 , lit 0x18BB25E9 , lit 0xA86C4B86)  -- KeyAddition
(lit 0x5390A89F , lit 0x4EC43FB1 , lit 0x8BF56879 , lit 0xDBE62A1E)  -- InvMixColumn
(lit 0x50966F6E , lit 0xB6882556 , lit 0xCE77F7AF , lit 0x9FF595E9)  -- Substitution
(lit 0x50F5F756 , lit 0xB69695AF , lit 0xCE886FE9 , lit 0x9F77256E)  -- ShiftRow
(lit 0x30C81C46 , lit 0xA35CE411 , lit 0xE5FBC119 , lit 0x1A0A52EF)  -- KeyAddition
(lit 0xDD78DBAB , lit 0xDFE17EF4 , lit 0x02107ECB , lit 0xEE488FD9)  -- KeyAddition
--
-- -- 13 of 15 --
--
(lit 0xC9C19F0E , lit 0xEFE08ABA , lit 0x6A7C8A59 , lit 0x99D473E5)  -- Substitution
(lit 0xC9D48ABA , lit 0xEFC17359 , lit 0x6AE09FE5 , lit 0x997C8A0E)  -- ShiftRow
-- Round = 13
(lit 0x032E2059 , lit 0x0B14E86D , lit 0xF03FF52B , lit 0x246C9303)  -- KeyAddition
(lit 0x17379CE8 , lit 0xB22E7E78 , lit 0xF879DF4F , lit 0x1C6C00A8)  -- InvMixColumn
(lit 0x87B21CC8 , lit 0x3EC38AC1 , lit 0xE1AFEF92 , lit 0xC4B8526F)  -- Substitution
(lit 0x87B8EFC1 , lit 0x3EB25292 , lit 0xE1C31C6F , lit 0xC4AF8AC8)  -- ShiftRow
-- Round = 12
(lit 0xF324A86A , lit 0x26E24F48 , lit 0x03B66220 , lit 0xB0AE1A92)  -- KeyAddition
(lit 0x8CB9FDDD , lit 0x8BBC7F8B , lit 0xE3657706 , lit 0x5867C56C)  -- InvMixColumn
(lit 0xF0DB21C9 , lit 0xCE786BCE , lit 0x4DBC02A5 , lit 0x5E0A07B8)  -- Substitution
(lit 0xF00A02CE , lit 0xCEDB07A5 , lit 0x4D7821B8 , lit 0x5EBC6BC9)  -- ShiftRow
-- Round = 11
(lit 0xA88CC893 , lit 0xE0F43672 , lit 0x3372D042 , lit 0x7973180A)  -- KeyAddition
(lit 0xD1D24F33 , lit 0x5BA32D85 , lit 0x01FC5F71 , lit 0x2A827BCB)  -- InvMixColumn
(lit 0x517F9266 , lit 0x5771FA67 , lit 0x0955842C , lit 0x95110359)  -- Substitution
(lit 0x51118467 , lit 0x577F032C , lit 0x09719259 , lit 0x9555FA66)  -- ShiftRow
-- Round = 10
(lit 0x8F02ED00 , lit 0x3BB3595D , lit 0xF354F1CC , lit 0x03211473)  -- KeyAddition
(lit 0x5082F240 , lit 0x23694284 , lit 0x8875A5C2 , lit 0x4043DD9B)  -- InvMixColumn
(lit 0x6C110472 , lit 0x32E4F64F , lit 0x973F29A8 , lit 0x7264C9E8)  -- Substitution
(lit 0x6C64294F , lit 0x3211C9A8 , lit 0x97E404E8 , lit 0x723FF672)  -- ShiftRow
-- Round = 9
(lit 0xA470CB4B , lit 0x44B83222 , lit 0xC7C1C4C5 , lit 0x2BFA744B)  -- KeyAddition
(lit 0x7233B5A0 , lit 0xA9241C7D , lit 0xF8E4F5EE , lit 0x2AD0B2A6)  -- InvMixColumn
(lit 0x1E66D247 , lit 0xB7A6C413 , lit 0xE1AE7799 , lit 0x95603EC5)  -- Substitution
(lit 0x1E607713 , lit 0xB7663E99 , lit 0xE1A6D2C5 , lit 0x95AEC447)  -- ShiftRow
-- Round = 8
(lit 0x76600CBF , lit 0x05B90D8F , lit 0x774FEB21 , lit 0xF9FF49C7)  -- KeyAddition
(lit 0x5313789D , lit 0x93C7E78D , lit 0x629C010D , lit 0xA7CC1CFF)  -- InvMixColumn
(lit 0x5082C175 , lit 0x2231B0B4 , lit 0xAB1C09F3 , lit 0x8927C47D)  -- Substitution
(lit 0x502709B4 , lit 0x2282C4F3 , lit 0xAB31C17D , lit 0x891CB075)  -- ShiftRow
-- Round = 7
(lit 0xC8E2B67D , lit 0x9C3FDD7D , lit 0x8DBDFADA , lit 0x80FCF261)  -- KeyAddition
(lit 0xD7A870EE , lit 0xC5E882AC , lit 0xBA24ED63 , lit 0x7D74CA2C)  -- InvMixColumn
(lit 0x0D6FD099 , lit 0x07C811AA , lit 0xC0A65300 , lit 0x13CA1042)  -- Substitution
(lit 0x0DCA53AA , lit 0x076F1000 , lit 0xC0C8D042 , lit 0x13A61199)  -- ShiftRow
-- Round = 6
(lit 0x8CE6D207 , lit 0xDDB058BA , lit 0xE4FEDAB0 , lit 0xE91EA5FD)  -- KeyAddition
(lit 0x0B173093 , lit 0xBEC83EC7 , lit 0xF5482BE6 , lit 0x97729ED4)  -- InvMixColumn
(lit 0x9E870822 , lit 0x5AB1D131 , lit 0x77D40BF5 , lit 0x851EDF19)  -- Substitution
(lit 0x9E1E0B31 , lit 0x5A87DFF5 , lit 0x77B10819 , lit 0x85D4D122)  -- ShiftRow
-- Round = 5
(lit 0x2BB739BB , lit 0x7CFF79B2 , lit 0xEF802A30 , lit 0xAAB8A891)  -- KeyAddition
(lit 0xAE6E805E , lit 0x4BF9DE24 , lit 0xF8C9D793 , lit 0xEE54BA2B)  -- InvMixColumn
(lit 0xBE453A9D , lit 0xCC699CA6 , lit 0xE1120D22 , lit 0x99FDC00B)  -- Substitution
--
-- -- 14 of 15 --
--
(lit 0xBEFD0DA6 , lit 0xCC45C022 , lit 0xE1693A0B , lit 0x99129C9D)  -- ShiftRow
-- Round = 4
(lit 0x6B67E11E , lit 0x97B60935 , lit 0x1F807843 , lit 0x479C220B)  -- KeyAddition
(lit 0x47A53021 , lit 0x6E1CCAA5 , lit 0x4EB97A29 , lit 0x46B2DCDA)  -- InvMixColumn
(lit 0x1629087B , lit 0x45C41029 , lit 0xB6DBBD4C , lit 0x983E937A)  -- Substitution
(lit 0x163EBD29 , lit 0x4529934C , lit 0xB6C4087A , lit 0x98DB107B)  -- ShiftRow
-- Round = 3
(lit 0xBE8E2133 , lit 0xD6F80781 , lit 0x088D8C14 , lit 0x2F864BE1)  -- KeyAddition
(lit 0xBD2066D9 , lit 0x7C7DE64F , lit 0xCA28748B , lit 0xF69B94FA)  -- InvMixColumn
(lit 0xCD54D3E5 , lit 0x0113F592 , lit 0x10EECACE , lit 0xD6E8E714)  -- Substitution
(lit 0xCDE8CA92 , lit 0x0154E7CE , lit 0x1013D314 , lit 0xD6EEF5E5)  -- ShiftRow
-- Round = 2
(lit 0x564B9E83 , lit 0x8F3DC261 , lit 0xB509584B , lit 0xF689093B)  -- KeyAddition
(lit 0x61F7A93F , lit 0x462A2954 , lit 0xEF18471F , lit 0xCA7FAF57)  -- InvMixColumn
(lit 0xD826B725 , lit 0x98954CFD , lit 0x613416CB , lit 0x106B1BDA)  -- Substitution
(lit 0xD86B16FD , lit 0x98261BCB , lit 0x6195B7DA , lit 0x10344C25)  -- ShiftRow
-- Round = 1
(lit 0xC75E3AFA , lit 0xA347131C , lit 0x4C0DA779 , lit 0x192093D1)  -- KeyAddition
(lit 0x9097DF81 , lit 0x746A09FC , lit 0x44828AD3 , lit 0xFB3A873D)  -- InvMixColumn
(lit 0x9685EF91 , lit 0xCA584055 , lit 0x8611CFA9 , lit 0x63A2EA8B)  -- Substitution
(lit 0x96A2CF55 , lit 0xCA85EAA9 , lit 0x8658EF8B , lit 0x63114091)  -- ShiftRow
(lit 0xF69F2445 , lit 0xDF4F9B17 , lit 0xAD2B417B , lit 0xE66C3710)  -- KeyAddition
-- Plaintext is
(lit 0x6BC1BEE2 , lit 0x2E409F96 , lit 0xE93D7E11 , lit 0x7393172A)
(lit 0xAE2D8A57 , lit 0x1E03AC9C , lit 0x9EB76FAC , lit 0x45AF8E51)
(lit 0x30C81C46 , lit 0xA35CE411 , lit 0xE5FBC119 , lit 0x1A0A52EF)
(lit 0xF69F2445 , lit 0xDF4F9B17 , lit 0xAD2B417B , lit 0xE66C3710)
-- **************************************************************
--
-- -- 15 of 15 --
--
--
-}
