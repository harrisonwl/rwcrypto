{-# LANGUAGE DataKinds #-}
module Test_Encrypt (alltests) where

import Prelude hiding ((^) , (++))
import ReWire 
import ReWire.Bits (lit , (^) , toInteger , (+))
import ReWire.Vectors (slice , (++))
import ReWire.Finite(finite)
import qualified ReWire.FiniteComp as FC

import Basic (Quad, Oct, Hex, X16(..), X64(..))
import Expansion(salsa20_k0k1)
import Encrypt (encrypt)
import qualified ReWire.Interactive as RI
import GHC.Integer (Integer)
import Data.String (String)
import Data.Bool (Bool)

encode64 :: Hex (W 8) -> Hex (W 8) -> Oct (W 8) -> W 64 -> [W 8] ->  [W 8]
encode64 k0 k1 v _ []        = []
encode64 k0 k1 v i (mi : ms) = encrypt k0 k1 v i mi : encode64 k0 k1 v (i ReWire.Bits.+ lit 1) ms

encrypt64 :: String -> String
encrypt64 text = map w8ToChar (encode64 k0 k1 v (lit 0) m)
  where
     m :: [W 8]
     m        = Prelude.map charToW8 text
     (k0, k1) = mkk0k1 secret
     v        = mknonce nonce

thereandback :: String -> String
thereandback = encrypt64 . encrypt64
  
splice :: Oct a -> Oct a -> Hex a
splice (b0, b1, b2, b3, b4, b5, b6, b7) (b8, b9, b10, b11, b12, b13, b14, b15)
       = (X16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) 

mkk0k1 :: String -> (Hex (W 8) , Hex (W 8))
mkk0k1 secret = ( (X16 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16)
                , (X16 b15 b14 b13 b12 b11 b10  b9  b8  b7  b6  b5   b4 b3  b2  b1  b0))
   where
             [ b31 , b30 , b29 , b28 , b27 , b26 , b25 , b24
              , b23 , b22 , b21 , b20 , b19 , b18 , b17 , b16
              , b15 , b14 , b13 , b12 , b11 , b10 ,  b9 ,  b8
              ,  b7 ,  b6 ,  b5 ,  b4 ,  b3 ,  b2 ,  b1 ,  b0 ] = Prelude.map charToW8 secret

mknonce :: String -> Oct (W 8)
mknonce v = (b7,  b6,  b5,   b4, b3,  b2,  b1,  b0)
   where
             [ b7 ,  b6 ,  b5 ,  b4 ,  b3 ,  b2 ,  b1 ,  b0 ] = Prelude.map charToW8 v

charToW8 :: Char -> W 8
charToW8 = lit . fromIntegral . fromEnum

w8ToChar :: W 8 -> Char
w8ToChar = toEnum . fromInteger . ReWire.Bits.toInteger

secret    = "*Thirty-two byte (256 bits) key*"
nonce     = "12345678"

ex1 , ex2 , ex3 :: String
ex1 = thereandback plaintext
ex2 = thereandback godzilla_haiku
ex3 = thereandback sonnet129

test1 , test2 , test3 :: Bool
test1 = thereandback plaintext      == plaintext
test2 = thereandback godzilla_haiku == godzilla_haiku
test3 = thereandback sonnet129      == sonnet129

alltests :: [Bool]
alltests = [test1 , test2 , test3]

plaintext :: String
plaintext = "Attack at dawn"

godzilla_haiku :: String
godzilla_haiku = "With artillery\nYou greet your nuclear child\nAm I the monster?"

sonnet129 :: String
sonnet129 = "Th' expense of spirit in a waste of shame\nIs lust in action; and till action, lust\nIs perjured, murd'rous, bloody, full of blame,\nSavage, extreme, rude, cruel, not to trust,\nEnjoyed no sooner but despised straight, \nPast reason hunted; and, no sooner had \nPast reason hated as a swallowed bait \nOn purpose laid to make the taker mad; \nMad in pursuit and in possession so, \nHad, having, and in quest to have, extreme; \nA bliss in proof and proved, a very woe; \nBefore, a joy proposed; behind, a dream. \n  All this the world well knows; yet none knows well \n  To shun the heaven that leads men to this hell."

{-
-- |
-- | This is factor function tweeked so that it takes (W 64) as input instead of Integer. 
-- |
factor64 :: W 64 -> (W 8 , W 8 , W 8 , W 8 , W 8 , W 8 , W 8 , W 8 )
factor64 w64 = (s0 , s1 , s2 , s3 , s4 , s5 , s6 , s7)
  where
    s0 , s1 , s2 , s3 , s4 , s5 , s6 , s7 :: W 8
    s0 = slice (Proxy :: Proxy 0)  w64
    s1 = slice (Proxy :: Proxy 8)  w64
    s2 = slice (Proxy :: Proxy 16) w64
    s3 = slice (Proxy :: Proxy 24) w64
    s4 = slice (Proxy :: Proxy 32) w64
    s5 = slice (Proxy :: Proxy 40) w64
    s6 = slice (Proxy :: Proxy 48) w64
    s7 = slice (Proxy :: Proxy 56) w64

mod64 :: W 64 -> W 6
mod64 w64 = s0
  where
    s0 :: W 6
    s0 = slice (Proxy :: Proxy 58)  w64
-}

-- |
-- | *************************************************
-- | *************************************************
-- | ** Phasing out what's below.
-- | *************************************************
-- | *************************************************
-- |
{-

encode :: Hex (W 8) -> Hex (W 8) -> Oct (W 8) -> [W 8] -> Integer -> [W 8]
encode k0 k1 v [] _        = []
encode k0 k1 v (mi : ms) i = (mi ^ ((salsa20_k0k1 (k0 , k1) (splice v (factor i))) `proj64` (finite i))) : encode k0 k1 v ms (i Prelude.+ 1)

encrypt :: String -> [W 8]
encrypt plaintext = encode k0 k1 v m 0
  where
     m :: [W 8]
     m        = Prelude.map charToW8 plaintext
     (k0, k1) = mkk0k1 secret
     v        = mknonce nonce

thereandback :: String -> String
thereandback plaintext = map w8ToChar (encrypt (map w8ToChar (encrypt plaintext)))

-- |
-- | This is the infamous (_) function from Section 10. 
-- |
factor :: Integer -> Oct (W 8)
factor i = (lit i0 , lit i1, lit i2, lit i3, lit i4, lit i5, lit i6, lit i7)
  where
    i0, i1, i2, i3, i4, i5, i6, i7 :: Integer
    (q0 , i0) = quotRem i 256
    (q1 , i1) = quotRem q0 256
    (q2 , i2) = quotRem q1 256
    (q3 , i3) = quotRem q2 256
    (q4 , i4) = quotRem q3 256
    (q5 , i5) = quotRem q4 256
    (q6 , i6) = quotRem q5 256
    (_  , i7) = quotRem q6 256

enc256 :: Hex (W 8) -> Hex (W 8) -> Oct (W 8) -> W 8 -> Integer -> W 8
-- enc256 k0 k1 v mj j = mj ^ (i_mod_64 (salsa20_k0k1 (k0 , k1) (splice v (factor j))) j)
enc256 k0 k1 v mj j = mj ^ ((salsa20_k0k1 (k0 , k1) (splice v (factor j))) `proj64` (finite j))
-- enc256 k0 k1 v mj j = mj ^ ((salsa20_k0k1 (k0 , k1) (splice v (factor j))) `pi64` j)


{-
----
----

----
----


there :: String -> String -> [Char] -> [W8]
there secret nonce text = encrypt256 k0 k1 v m
  where
     m :: [W8]
     m        = Prelude.map charToW8 text
     (k0, k1) = mkk0k1 secret
     v        = mknonce nonce

thereandback :: String -> String -> [Char] -> [W8]
thereandback secret nonce text = encrypt256 k0 k1 v (encrypt256 k0 k1 v m)
  where
     m :: [W8]
     m        = Prelude.map charToW8 text
     (k0, k1) = mkk0k1 secret
     v        = mknonce nonce

test1 = putStrLn $ Prelude.map w8ToChar $ there secret nonce plaintext
test2 = putStrLn $ Prelude.map w8ToChar (thereandback secret nonce plaintext)
test3 = putStrLn $ Prelude.map w8ToChar (thereandback secret nonce sonnet129)
-}

-- | Recall that
--     proj64 :: X64 a -> Finite 64 -> a

i_mod_64 :: X64 (W 8) -> Integer -> W 8
i_mod_64 (X64   a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
                a40 a41 a42 a43 a50 a51 a52 a53 a60 a61 a62 a63 a70 a71 a72 a73
                a80 a81 a82 a83 a90 a91 a92 a93 aa0 aa1 aa2 aa3 ab0 ab1 ab2 ab3
                ac0 ac1 ac2 ac3 ad0 ad1 ad2 ad3 ae0 ae1 ae2 ae3 af0 af1 af2 af3)
         i =
               args !! (fromIntegral i `mod` 64)
      where
         args = [a00,a01,a02,a03,a10,a11,a12,a13,a20,a21,a22,a23,a30,a31,a32,a33
                ,a40,a41,a42,a43,a50,a51,a52,a53,a60,a61,a62,a63,a70,a71,a72,a73
                ,a80,a81,a82,a83,a90,a91,a92,a93,aa0,aa1,aa2,aa3,ab0,ab1,ab2,ab3
                ,ac0,ac1,ac2,ac3,ad0,ad1,ad2,ad3,ae0,ae1,ae2,ae3,af0,af1,af2,af3]

grunt :: Hex (W 8) -> Hex (W 8) -> Oct (W 8) -> Integer -> X64 (W 8)
grunt k0 k1 v j = (salsa20_k0k1 (k0 , k1) (splice v (factor j)))

encrypt256 :: Hex (W 8) -> Hex (W 8) -> Oct (W 8) -> [W 8] -> [W 8]
encrypt256 k0 k1 v m = [ mj ^ (i_mod_64 (salsa20_k0k1 (k0 , k1) (splice v (factor j))) j) | (mj , j) <- Prelude.zip m ls ]
   where
     ls = [0..1180591620717411303424] -- [0..2 ^ 70]
     splice :: Oct a -> Oct a -> Hex a
     splice (b0, b1, b2, b3, b4, b5, b6, b7) (b8, b9, b10, b11, b12, b13, b14, b15)
          = (X16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) 
     i_mod_64 :: X64 (W 8) -> Integer -> W 8
     i_mod_64 (X64   a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
                     a40 a41 a42 a43 a50 a51 a52 a53 a60 a61 a62 a63 a70 a71 a72 a73
                     a80 a81 a82 a83 a90 a91 a92 a93 aa0 aa1 aa2 aa3 ab0 ab1 ab2 ab3
                     ac0 ac1 ac2 ac3 ad0 ad1 ad2 ad3 ae0 ae1 ae2 ae3 af0 af1 af2 af3)
              i = args !! (fromIntegral i `mod` 64)
      where
         args = [a00,a01,a02,a03,a10,a11,a12,a13,a20,a21,a22,a23,a30,a31,a32,a33
                ,a40,a41,a42,a43,a50,a51,a52,a53,a60,a61,a62,a63,a70,a71,a72,a73
                ,a80,a81,a82,a83,a90,a91,a92,a93,aa0,aa1,aa2,aa3,ab0,ab1,ab2,ab3
                ,ac0,ac1,ac2,ac3,ad0,ad1,ad2,ad3,ae0,ae1,ae2,ae3,af0,af1,af2,af3]
-}
