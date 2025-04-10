{-# LANGUAGE DataKinds #-}
module TestRefSalsa20 where

-- |
-- | Testing the reference semantics
-- | for Salsa20.
-- |

import Prelude hiding ((^), (+), (&&), take, drop)
import ReWire hiding (error)
import ReWire.Bits
import ReWire.Vectors

import ReWire.Interactive
import Salsa20Reference


--------
--------
--------
ppQ32 :: Quad W32 -> IO ()
ppQ32 (a , b , c , d) = 
   putStrLn $
         "(" Prelude.++ xshow a Prelude.++ " , " Prelude.++ xshow b Prelude.++ " , " Prelude.++ xshow c Prelude.++ " , " Prelude.++ xshow d Prelude.++ ")\n "

qex0 :: Quad W32
qex0 = (lit 0x00000001, lit 0x00000000, lit 0x00000000, lit 0x00000000)

qex1 :: Quad W32
qex1 = (lit 0x00000001, lit 0x00000002, lit 0x00000003, lit 0x00000004)

ppHex32 :: Hex W32 -> IO ()
ppHex32 (a , b , c , d , e , f , g , h , i , j , k , l , m , n , o , p) = 
   putStrLn $
         "(" Prelude.++ xshow a Prelude.++ " , " Prelude.++ xshow b Prelude.++ " , " Prelude.++ xshow c Prelude.++ " , " Prelude.++ xshow d Prelude.++ " , \n "
         Prelude.++    xshow e Prelude.++ " , " Prelude.++ xshow f Prelude.++ " , " Prelude.++ xshow g Prelude.++ " , " Prelude.++ xshow h Prelude.++ " , \n "
            Prelude.++ xshow i Prelude.++ " , " Prelude.++ xshow j Prelude.++ " , " Prelude.++ xshow k Prelude.++ " , " Prelude.++ xshow l Prelude.++ " , \n "
            Prelude.++ xshow m Prelude.++ " , " Prelude.++ xshow n Prelude.++ " , " Prelude.++ xshow o  Prelude.++ " , " Prelude.++ xshow p Prelude.++ ")\n"  

ex1 , ex2 , ex3 , ex4 :: Hex W32
ex1 = (lit 0x00000001, lit 0x00000000, lit 0x00000000, lit 0x00000000,
       lit 0x00000001, lit 0x00000000, lit 0x00000000, lit 0x00000000,
       lit 0x00000001, lit 0x00000000, lit 0x00000000, lit 0x00000000,
       lit 0x00000001, lit 0x00000000, lit 0x00000000, lit 0x00000000)

ex2 = (lit 0x08521bd6, lit 0x1fe88837, lit 0xbb2aa576, lit 0x3aa26365,
       lit 0xc54c6a5b, lit 0x2fc74c2f, lit 0x6dd39cc3, lit 0xda0a64f6,
       lit 0x90a2f23d, lit 0x067f95a6, lit 0x06b35f61, lit 0x41e4732e,
       lit 0xe859c100, lit 0xea4d84b7, lit 0x0f619bff, lit 0xbc6e965a)

ex3 = (lit 0x00000001, lit 0x00000000, lit 0x00000000, lit 0x00000000,
       lit 0x00000000, lit 0x00000000, lit 0x00000000, lit 0x00000000,
       lit 0x00000000, lit 0x00000000, lit 0x00000000, lit 0x00000000,
       lit 0x00000000, lit 0x00000000, lit 0x00000000, lit 0x00000000)

ex4 = (lit 0xde501066, lit 0x6f9eb8f7, lit 0xe4fbbd9b, lit 0x454e3f57,
       lit 0xb75540d3, lit 0x43e93a4c, lit 0x3a6f2aa0, lit 0x726d6b36,
       lit 0x9243f484, lit 0x9145d1e8, lit 0x4fa9d247, lit 0xdc8dee11,
       lit 0x054bf545, lit 0x254dd653, lit 0xd9421b6d, lit 0x67b276c1)

----
----
w8ToChar :: W8 -> Char
w8ToChar = toEnum . fromInteger . ReWire.Bits.toInteger

charToW8 :: Char -> W8
charToW8 = lit . fromIntegral . fromEnum
----
----

mkk0k1 :: String -> (Hex W8 , Hex W8)
mkk0k1 secret = ( (b31, b30, b29, b28, b27, b26, b25, b24, b23, b22, b21, b20, b19, b18, b17, b16)
                , (b15, b14, b13, b12, b11, b10,  b9,  b8,  b7,  b6,  b5,   b4, b3,  b2,  b1,  b0))
   where
             [ b31 , b30 , b29 , b28 , b27 , b26 , b25 , b24
              , b23 , b22 , b21 , b20 , b19 , b18 , b17 , b16
              , b15 , b14 , b13 , b12 , b11 , b10 ,  b9 ,  b8
              ,  b7 ,  b6 ,  b5 ,  b4 ,  b3 ,  b2 ,  b1 ,  b0 ] = Prelude.map charToW8 secret

mknonce :: String -> Oct W8
mknonce v = (b7,  b6,  b5,   b4, b3,  b2,  b1,  b0)
   where
             [ b7 ,  b6 ,  b5 ,  b4 ,  b3 ,  b2 ,  b1 ,  b0 ] = Prelude.map charToW8 v

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

secret    = "*Thirty-two byte (256 bits) key*"
plaintext = "Attack at dawn"
nonce     = "12345678"

test1 = putStrLn $ Prelude.map w8ToChar $ there secret nonce plaintext
test2 = putStrLn $ Prelude.map w8ToChar (thereandback secret nonce plaintext)
test3 = putStrLn $ Prelude.map w8ToChar (thereandback secret nonce sonnet129)

sonnet129 = "Th' expense of spirit in a waste of shame\nIs lust in action; and till action, lust\nIs perjured, murd'rous, bloody, full of blame,\nSavage, extreme, rude, cruel, not to trust,\nEnjoyed no sooner but despised straight, \nPast reason hunted; and, no sooner had \nPast reason hated as a swallowed bait \nOn purpose laid to make the taker mad; \nMad in pursuit and in possession so, \nHad, having, and in quest to have, extreme; \nA bliss in proof and proved, a very woe; \nBefore, a joy proposed; behind, a dream. \n  All this the world well knows; yet none knows well \n  To shun the heaven that leads men to this hell."

encrypt256 :: Hex W8 -> Hex W8 -> Oct W8 -> [W8] -> [W8]
encrypt256 k0 k1 v m = [ mj ^ (i_mod_64 (salsa20_256 (k0 , k1 , splice v (factor j))) j) | (mj , j) <- Prelude.zip m ls ]
   where
     ls = [0..1180591620717411303424] -- [0..2 ^ 70]
     splice :: Oct a -> Oct a -> Hex a
     splice (b0, b1, b2, b3, b4, b5, b6, b7) (b8, b9, b10, b11, b12, b13, b14, b15)
          = (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15) 
     i_mod_64 :: X64 W8 -> Integer -> W8
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
