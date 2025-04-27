module Test_ReferenceSalsa20 where

import Prelude as P
import BinaryArithmetic hiding ((^),(+))
import W8
import W32

-- import Iterator
import Idioms
import ReferenceSalsa20 hiding (Integer)

{-

-- |
-- | Functions to test against String
-- |

w8ToChar :: W8 -> Char
w8ToChar = toEnum . val

charToW8 :: Char -> W8
charToW8 = toW8 . fromIntegral . fromEnum

-----------------------------
-- Example from https://pycryptodome.readthedocs.io/en/latest/src/cipher/salsa20.html
-----------------------------

-------------
-- | The functions {bar,foo}W8x8 and integer i are just for testing
-------------

barW8x8 :: (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer) -> Integer
barW8x8 (i0 , i1, i2, i3, i4, i5, i6, i7)
  = i0 + (2^8 * i1) + (2^16 * i2) + (2^24 * i3) + (2^32 * i4) + (2^40 * i5) + (2^48 * i6) + (2^56 * i7)

i = barW8x8 (1 , 2 , 3 , 4 , 5 , 6 , 7 , 8)

fooW8x8 :: Integer -> (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)
fooW8x8 i = (i0 , i1, i2, i3, i4, i5, i6, i7)
  where
    i0, i1, i2, i3, i4, i5, i6, i7 :: Integer
    (q0 , i0) = quotRem i (2^8)
    (q1 , i1) = quotRem q0 (2^8)
    (q2 , i2) = quotRem q1 (2^8)
    (q3 , i3) = quotRem q2 (2^8)
    (q4 , i4) = quotRem q3 (2^8)
    (q5 , i5) = quotRem q4 (2^8)
    (q6 , i6) = quotRem q5 (2^8)
    (_  , i7) = quotRem q6 (2^8)
    

mkk0k1 :: String -> (Hex W8 , Hex W8)
mkk0k1 secret = ( (b31, b30, b29, b28, b27, b26, b25, b24, b23, b22, b21, b20, b19, b18, b17, b16)
                , (b15, b14, b13, b12, b11, b10,  b9,  b8,  b7,  b6,  b5,   b4, b3,  b2,  b1,  b0))
   where
             [ b31 , b30 , b29 , b28 , b27 , b26 , b25 , b24
              , b23 , b22 , b21 , b20 , b19 , b18 , b17 , b16
              , b15 , b14 , b13 , b12 , b11 , b10 ,  b9 ,  b8
              ,  b7 ,  b6 ,  b5 ,  b4 ,  b3 ,  b2 ,  b1 ,  b0 ] = map charToW8 secret

mknonce :: String -> Oct W8
mknonce v = (b7,  b6,  b5,   b4, b3,  b2,  b1,  b0)
   where
             [ b7 ,  b6 ,  b5 ,  b4 ,  b3 ,  b2 ,  b1 ,  b0 ] = map charToW8 v

encrypt128 :: Hex W8 -> Oct W8 -> [W8] -> [W8]
encrypt128 k v m = [ mj ^ (i_mod_64 (salsa20_128 k (splice v (factor j))) j) | (mj , j) <- P.zip m ls ]
   where
     ls = [0..(2 P.^ 70)]
     -- i_mod_64 :: W8x64 -> Integer -> W8

encrypt256 :: Hex W8 -> Hex W8 -> Oct W8 -> [W8] -> [W8]
encrypt256 k0 k1 v m = [ mj ^ (i_mod_64 (salsa20_256 k0 k1 (splice v (factor j))) j) | (mj , j) <- P.zip m ls ]
   where
     ls = [0..(2 P.^ 70)]
     -- i_mod_64 :: W8x64 -> Integer -> W8

there :: String -> String -> [Char] -> [W8]
there secret nonce text = encrypt256 k0 k1 v m
  where
     m :: [W8]
     m        = map charToW8 text
     (k0, k1) = mkk0k1 secret
     v        = mknonce nonce

thereandback :: String -> String -> [Char] -> [W8]
thereandback secret nonce text = encrypt256 k0 k1 v (encrypt256 k0 k1 v m)
  where
     m :: [W8]
     m        = map charToW8 text
     (k0, k1) = mkk0k1 secret
     v        = mknonce nonce

secret    = "*Thirty-two byte (256 bits) key*"
plaintext = "Attack at dawn"
nonce     = "12345678"

test1 = there secret nonce plaintext
test2 = putStrLn $ map w8ToChar (thereandback secret nonce plaintext)
test3 = putStrLn $ map w8ToChar (thereandback secret nonce sonnet129)

sonnet129 = "Th' expense of spirit in a waste of shame\nIs lust in action; and till action, lust\nIs perjured, murd'rous, bloody, full of blame,\nSavage, extreme, rude, cruel, not to trust,\nEnjoyed no sooner but despised straight, \nPast reason hunted; and, no sooner had \nPast reason hated as a swallowed bait \nOn purpose laid to make the taker mad; \nMad in pursuit and in possession so, \nHad, having, and in quest to have, extreme; \nA bliss in proof and proved, a very woe; \nBefore, a joy proposed; behind, a dream. \n  All this the world well knows; yet none knows well \n  To shun the heaven that leads men to this hell."

-- | 
-- ghci> test1
--    [xc7,x5d,x18,xe9,x41,x9a,x98,x9d,xde,x57,x9b,xfc,xf1,x86]
-- ghci> test2
--    "Attack at dawn"

t0 :: X64 W8
t0 = X64 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
           
-- ghci> t0 == hash_salsa20 t0
--    True

t1 = toW8x64 211 159  13 115  76  55  82 183  3 117 222  37 191 187 234 136
             49 237 179  48  1 106 178 219 175 199 166  48  86  16 179 207
             31 240  32  63  15  83  93 161 116 147  48 113 238  55 204
             36  79 201 235  79  3  81 156  47 203  26 244 243  88 118 104  54

ans1 = toW8x64 109  42 178 168 156 240 248 238 168 196 190 203  26 110 170 154
               29   29 150  26 150  30 235 249 190 163 251  48  69 144  51  57
               118  40 152 157 180  57  27  94 107  42 236  35  27 111 114 114 
               219 236 232 135 111 155 110  18  24 232  95 158 179  19  48 202

-- ghci> ans1 == hash_salsa20 t1
--    True

t2 = toW8x64 88 118 104  54  79 201 235  79  3  81 156  47 203  26 244 243
             191 187 234 136 211 159  13 115  76  55  82 183  3 117 222  37
             86  16 179 207  49 237 179  48  1 106 178 219 175 199 166  48 
             238  55 204  36  31 240  32  63  15  83  93 161 116 147  48 113

ans2 = toW8x64 179 19 48 202 219 236 232 135 111 155 110  18  24 232  95 158 
               26 110 170 154 109  42 178 168 156 240 248 238 168 196 190 203 
               69 144  51  57  29  29 150  26 150  30 235 249 190 163 251  48 
               27 111 114 114 118  40 152 157 180  57  27  94 107  42 236  35

-- ghci> ans2 == hash_salsa20 t2
--   True

t3 = toW8x64 6 124  83 146  38 191  9  50  4 161  47 222 122 182 223 185 
             75  27  0 216  16 122  7  89 162 104 101 147 213  21  54  95  225
             253 139 176 105 132  23 116  76  41 176 207 221  34 157 108  94  94
             99  52  90 117  91 220 146 190 239 143 196 176 130 186

ans3 = toW8x64 8  18  38 199 119  76 215  67 173 127 144 162 103
               212 176 217  192  19 233  33 159 197 154 160 128 243 219
               65 171 136 135 225  123  11  68  86 237  82  20 155 133 189
               9  83 167 116 194  78 122 127 195 185 185 204 188  90 245
               9 183 248 226  85 245 104

-- | Output is identical to ans3:
-- (8, 18, 38, 199, 119, 76, 215, 67, 173, 127, 144, 162, 103,
--  212, 176, 217, 192, 19, 233, 33, 159, 197, 154, 160, 128, 243, 219,
--  65, 171, 136, 135, 225, 123, 11, 68, 86, 237, 82, 20, 155, 133, 189,
--  9, 83, 167, 116, 194, 78, 122, 127, 195, 185, 185, 204, 188, 90, 245,
--  9, 183, 248, 226, 85, 245, 104)
-- 
-- main = putStrLn (show $ ans3 == ncomp 1000000 hash_salsa20 t3)
-- Thu Nov  3 14:15:12 EDT 2022
-- $ ./main             
-- True
ncomp :: (Eq t, Num t) => t -> (b -> b) -> b -> b
ncomp 0 f x = x
ncomp n f x = let z = f x in (z `seq` (ncomp (n-1) f z))

-- ghci> ncomp 1000000 hash_salsa20 t3 == ans3
--   *** Exception: stack overflow

-}
