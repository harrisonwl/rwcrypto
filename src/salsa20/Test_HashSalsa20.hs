{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test_HashSalsa20(alltests) where

import ReWire
import Salsa20Basic (X64(..))
import Testing (x64)
import HashSalsa20 (hash_salsa20)

alltests :: [Bool]
alltests = [ test1 , test2 , test3 {- , test4 -} ]

---------------------------------------------------------------------------------
-- N.b., evaluating test4 from the end of Section 8 ("Salsa20^1000000 ...")
-- makes my little MacBook Air run out of memory. There's undoubtedly a way
-- around this, but I've decided to press on at the moment. So, I've left it
-- commented out in alltests.
---------------------------------------------------------------------------------

test1 :: Bool
test1 = hash_salsa20 i1 == i1

i1 :: X64 (W 8)
i1 = x64 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 

i2 :: X64 (W 8)
i2 = x64 211 159 13 115 76 55 82 183
           3 117 222 37 191 187 234 136
          49 237 179 48 1 106 178 219
         175 199 166 48 86 16 179 207
          31 240 32 63 15 83 93 161
         116 147 48 113 238 55 204 36
          79 201 235 79 3 81 156 47
         203 26 244 243 88 118 104 54

o2 :: X64 (W 8)
o2 = x64 109 42 178 168 156 240 248 238
         168 196 190 203 26 110 170 154
          29 29 150 26 150 30 235 249
         190 163 251 48 69 144 51 57
         118 40 152 157 180 57 27 94
         107 42 236 35 27 111 114 114
         219 236 232 135 111 155 110 18
          24 232 95 158 179 19 48 202

test2 :: Bool
test2 = hash_salsa20 i2 == o2

i3 , o3 :: X64 (W 8)

i3 = x64 88 118 104 54 79 201 235 79 3 81 156 47 203 26 244 243 191 187 234 136 211 159 13 115 76 55 82 183 3 117 222 37 86 16 179 207 49 237 179 48 1 106 178 219 175 199 166 48 238 55 204 36 31 240 32 63 15 83 93 161 116 147 48 113

o3 = x64 179 19 48 202 219 236 232 135 111 155 110 18 24 232 95 158 26 110 170 154 109 42 178 168 156 240 248 238 168 196 190 203 69 144 51 57 29 29 150 26 150 30 235 249 190 163 251 48 27 111 114 114 118 40 152 157 180 57 27 94 107 42 236 35

test3 :: Bool
test3 = hash_salsa20 i3 == o3

{-
i4 , o4 :: X64 (W 8)
i4 = X64
         (lit 6) (lit 124) (lit 83) (lit 146) (lit 38) (lit 191) (lit 9) (lit 50)
         (lit 4) (lit 161) (lit 47) (lit 222) (lit 122) (lit 182) (lit 223) (lit 185)
         (lit 75) (lit 27) (lit 0) (lit 216) (lit 16) (lit 122) (lit 7) (lit 89)
         (lit 162) (lit 104) (lit 101) (lit 147) (lit 213) (lit 21) (lit 54) (lit 95)
         (lit 225) (lit 253) (lit 139) (lit 176) (lit 105) (lit 132) (lit 23) (lit 116)
         (lit 76) (lit 41) (lit 176) (lit 207) (lit 221) (lit 34) (lit 157) (lit 108)
         (lit 94) (lit 94) (lit 99) (lit 52) (lit 90) (lit 117) (lit 91) (lit 220)
         (lit 146) (lit 190) (lit 239) (lit 143) (lit 196) (lit 176) (lit 130) (lit 186)

o4 = X64
         (lit 8) (lit 18) (lit 38) (lit 199)     (lit 119) (lit 76) (lit 215) (lit 67)
         (lit 173) (lit 127) (lit 144) (lit 162) (lit 103) (lit 212) (lit 176) (lit 217)
         (lit 192) (lit 19) (lit 233) (lit 33)   (lit 159) (lit 197) (lit 154) (lit 160)
         (lit 128) (lit 243) (lit 219) (lit 65) (lit 171) (lit 136) (lit 135) (lit 225)
         (lit 123) (lit 11) (lit 68) (lit 86)    (lit 237) (lit 82) (lit 20) (lit 155)
         (lit 133) (lit 189) (lit 9) (lit 83)    (lit 167) (lit 116) (lit 194) (lit 78)
         (lit 122) (lit 127) (lit 195) (lit 185) (lit 185) (lit 204) (lit 188) (lit 90)
         (lit 245) (lit 9) (lit 183) (lit 248)   (lit 226) (lit 85) (lit 245) (lit 104)

iter :: Integer -> (a -> a) -> a -> a
iter i f a = if i <= 0 then a else iter (i Prelude.- 1) f (f a)

test4 :: Bool
test4 = iter 1000000 hash_salsa20 i4 == o4
-}

instance Eq a => Eq (X64 a) where
  (X64 a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33
       a40 a41 a42 a43 a50 a51 a52 a53 a60 a61 a62 a63 a70 a71 a72 a73
       a80 a81 a82 a83 a90 a91 a92 a93 aa0 aa1 aa2 aa3 ab0 ab1 ab2 ab3
       ac0 ac1 ac2 ac3 ad0 ad1 ad2 ad3 ae0 ae1 ae2 ae3 af0 af1 af2 af3 ) ==
        (X64 b00 b01 b02 b03 b10 b11 b12 b13 b20 b21 b22 b23 b30 b31 b32 b33
             b40 b41 b42 b43 b50 b51 b52 b53 b60 b61 b62 b63 b70 b71 b72 b73
             b80 b81 b82 b83 b90 b91 b92 b93 ba0 ba1 ba2 ba3 bb0 bb1 bb2 bb3
             bc0 bc1 bc2 bc3 bd0 bd1 bd2 bd3 be0 be1 be2 be3 bf0 bf1 bf2 bf3 )
        = a00 == b00 && a01 == b01 && a02 == b02 && a03 == b03 &&
          a10 == b10 && a11 == b11 && a12 == b12 && a13 == b13 &&
          a20 == b20 && a21 == b21 && a22 == b22 && a23 == b23 &&
          a30 == b30 && a31 == b31 && a32 == b32 && a33 == b33 &&
          a40 == b40 && a41 == b41 && a42 == b42 && a43 == b43 &&
          a50 == b50 && a51 == b51 && a52 == b52 && a53 == b53 &&
          a60 == b60 && a61 == b61 && a62 == b62 && a63 == b63 &&
          a70 == b70 && a71 == b71 && a72 == b72 && a73 == b73 &&
          a80 == b80 && a81 == b81 && a82 == b82 && a83 == b83 &&
          a90 == b90 && a91 == b91 && a92 == b92 && a93 == b93 &&
          aa0 == ba0 && aa1 == ba1 && aa2 == ba2 && aa3 == ba3 &&
          ab0 == bb0 && ab1 == bb1 && ab2 == bb2 && ab3 == bb3 &&
          ac0 == bc0 && ac1 == bc1 && ac2 == bc2 && ac3 == bc3 &&
          ad0 == bd0 && ad1 == bd1 && ad2 == bd2 && ad3 == bd3 &&
          ae0 == be0 && ae1 == be1 && ae2 == be2 && ae3 == be3 &&
          af0 == bf0 && af1 == bf1 && af2 == bf2 && af3 == bf3
