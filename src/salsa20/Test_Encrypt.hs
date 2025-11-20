{-# LANGUAGE DataKinds #-}
module Salsa20.Test_Encrypt (alltests , mknonce , mkk0k1 , w8ToChar , charToW8) where

import Prelude hiding ((+))
import ReWire
import ReWire.Bits (lit,toInteger,(+))
import Salsa20.Salsa20Basic (Oct, Hex, X16(..))
import Salsa20.Encrypt (encrypt,encrypt')

encryptS2O :: String -> String
encryptS2O text = map w8ToChar (encode64 k0 k1 v (lit 0) m)
  where
     m :: [W 8]
     m        = map charToW8 text
     (k0, k1) = mkk0k1 secret
     v        = mknonce nonce

     secret , nonce :: String
     secret    = "*Thirty-two byte (256 bits) key*"
     nonce     = "12345678"

     encode64 :: Hex (W 8) -> Hex (W 8) -> Oct (W 8) -> W 64 -> [W 8] ->  [W 8]
     encode64 _ _ _ _ []          = []
     encode64 k0 k1 v i (mi : ms) = encrypt k0 k1 v i mi : encode64 k0 k1 v (i + lit 1) ms

thereandback :: String -> String
thereandback = encryptS2O . encryptS2O

mkk0k1 :: String -> (Hex (W 8) , Hex (W 8))
mkk0k1 secret = ( (X16 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16)
                , (X16 b15 b14 b13 b12 b11 b10  b9  b8  b7  b6  b5   b4 b3  b2  b1  b0))
   where
             [ b31 , b30 , b29 , b28 , b27 , b26 , b25 , b24
              , b23 , b22 , b21 , b20 , b19 , b18 , b17 , b16
              , b15 , b14 , b13 , b12 , b11 , b10 ,  b9 ,  b8
              ,  b7 ,  b6 ,  b5 ,  b4 ,  b3 ,  b2 ,  b1 ,  b0 ] = map charToW8 secret

mknonce :: String -> Oct (W 8)
mknonce v = (b7,  b6,  b5,   b4, b3,  b2,  b1,  b0)
   where
             [ b7 ,  b6 ,  b5 ,  b4 ,  b3 ,  b2 ,  b1 ,  b0 ] = map charToW8 v

charToW8 :: Char -> W 8
charToW8 = lit . fromIntegral . fromEnum

w8ToChar :: W 8 -> Char
w8ToChar = toEnum . fromInteger . ReWire.Bits.toInteger

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
