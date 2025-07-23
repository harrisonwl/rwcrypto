{-# LANGUAGE DataKinds #-}
module Test_Expansion where

import Prelude hiding ((==))
import ReWire 
import ReWire.Bits (lit , (==))
import Basic (Quad, Hex, X16(..), X64(..))
import Testing (x64, x16)
import Expansion (salsa20_k0k1, salsa20_k, expandk0k1, expandk)
-- import HashSalsa20 (hash_salsa20)

import ReWire.Vectors as RV

{-
alltests :: [Bool]
alltests = [test1 , test2 , test3 , test4]

k0 , n , k1 :: Hex (W 8)
k0 = x16 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
k1 = x16 201 202 203 204 205 206 207 208
         209 210 211 212 213 214 215 216
n  = x16 101 102 103 104 105 106 107 108
         109 110 111 112 113 114 115 116
-- | Checks that the first example is prepared correctly.
test1 :: Bool
test1 = expandk0k1 (k0 , k1 , n) == x1
  where
    x1 :: X64 (W 8)
    x1 = x64
            101 120 112  97  1  2  3  4  5  6  7  8  9  10  11  12 
            13  14  15  16 110 100  32  51 101 102 103 104 105 106 107 108 
            109 110 111 112 113 114 115 116  50  45  98 121 201 202 203 204 
            205 206 207 208 209 210 211 212 213 214 215 216 116 101  32 107

test2 :: Bool
test2 = salsa20_k0k1 (k0 , k1) n == o1
  where
    o1 :: X64 (W 8)
    o1 = x64
             69  37  68  39  41  15 107 193 255 139 122  6 170 233 217  98 
             89 144 182 106  21  51 200  65 239  49 222  34 215 114  40 126 
            104 197  7 225 197 153  31  2 102  78  76 176  84 245 246 184 
            177 160 133 130  6  72 149 119 192 195 132 236 234 103 246  74

-- | Checks that the second example is prepared correctly.               
test3 :: Bool
test3 = expandk k0 n == x3
  where
    x3 :: X64 (W 8)
    x3 = x64 
            101 120 112  97  1  2  3  4  5  6  7  8  9  10  11  12 
             13  14  15  16 110 100  32  49 101 102 103 104 105 106 107 108 
            109 110 111 112 113 114 115 116  54  45  98 121  1  2  3  4 
              5  6  7  8  9  10  11  12  13  14  15  16 116 101  32 107

test4 = salsa20_k k0 n == o2
  where
    o2 :: X64 (W 8)
    o2 = x64 
             39 173  46 248  30 200  82  17  48  67 254 239  37  18  13 247 
            241 200  61 144  10  55  50 185  6  47 246 253 143  86 187 225 
            134  85 110 246 161 163  43 235 231  94 171  51 145 214 112  29 
             14 232  5  16 151 140 183 141 171  9 122 181 104 182 177 193
-}
