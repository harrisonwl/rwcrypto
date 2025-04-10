module TwoBlockMessage where

import Data.Char (ord)
import BinaryArithmetic (int2bin)
import W8
import W32

m :: String
m = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"

-- length m = 56
-- l = 56 * 8 = 448 bits.

m2bits :: [W8]
m2bits = map (toW8 . toInteger . ord) m

mkword :: Char -> Char -> Char -> Char -> W32
mkword a b c d = toW32 $
                    (toInteger (ord a) * 16 Prelude.^ 6) +
                    (toInteger (ord b) * 16 Prelude.^ 4) +
                    (toInteger (ord c) * 16 Prelude.^ 2) +
                     toInteger (ord d)

data Either4 a b c = Zero | One a | Two b | Three c

-- pad :: String -> [W32]
pad :: String -> [W32] -> ([W32], Either4 Char (Char , Char)  (Char , Char , Char))
pad [] acc                   = (reverse acc , Zero)
pad (a : []) acc             = (reverse acc , One a)
pad (a : b : []) acc         = (reverse acc , Two (a , b))
pad (a : b : c : []) acc     = (reverse acc , Three (a , b , c))
pad (a : b : c : d : cs) acc = pad cs (mkword a b c d : acc)
