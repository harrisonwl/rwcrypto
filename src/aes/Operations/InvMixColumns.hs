{-# LANGUAGE DataKinds #-}
module Aes.Operations.InvMixColumns (invmixcolumns) where

import Prelude (Integer , ($) , IO , putStrLn , (++) , (==))
import ReWire
import ReWire.Bits           as B
import ReWire.Finite
import ReWire.FiniteComp     as FC hiding ((*) , (+))
import ReWire.Vectors hiding ((++))
-- import ReWire.Interactive (dshow , hex , xshow , bshow)

import Aes.Basic(State , Column , transpose)
import Aes.Operations.GF28(mult)

xtimes :: W 8 -> W 8
xtimes x = if (x >>. lit 7) B.== lit 0
             then x <<. lit 1               
             else ((x <<. lit 1) ^ lit 0x1B)

-- | Multiplication by 2 in GF(2^8). It's also called xtimes in FIPS197.
mult2 :: W 8 -> W 8
mult2 x = xtimes x

-- Check out section 4.2.1 in FIPS197 about how to express
-- multiplication in terms of xtime.

-- | Multiplication by 3 in GF(2^8)
mult3 :: W 8 -> W 8
mult3 x = mult2 x ^ x

multt3 :: W 8 -> W 8
multt3 x = lit 0x3 `mult` x

mult4 :: W 8 -> W 8
mult4 x = mult2 x ^ mult2 x

mult8 :: W 8 -> W 8
mult8 x = mult4 x ^ mult4 x

mult9 , mult0b, mult0d, mult0e :: W 8 -> W 8
mult9 x  = mult8 x ^ x
mult0b x = mult8 x ^ mult2 x ^ x
mult0d x = mult8 x ^ mult4 x ^ x
mult0e x = mult8 x ^ mult4 x ^ mult2 x

-- 4/1: complete this and change invMixColumn
mul9 , mul0b, mul0d, mul0e :: W 8 -> W 8
mul9 x  = lit 0x09 `mult` x
mul0b x = lit 0x0b `mult` x
mul0d x = lit 0x0d `mult` x
mul0e x = lit 0x0e `mult` x
    -- differs from mul0e

-- | Apply MixColumns transformation to a single column
invMixColumn :: Column -> Column
invMixColumn c = fromList
               [ mul0e a0 ^ mul0b a1 ^ mul0d a2 ^ mul9 a3
               , mul9 a0  ^ mul0e a1 ^ mul0b a2 ^ mul0d a3
               , mul0d a0 ^ mul9 a1  ^ mul0e a2 ^ mul0b a3
               , mul0b a0 ^ mul0d a1 ^ mul9 a2  ^ mul0e a3
                 ]   
  where
    a0 , a1 , a2 , a3 :: W 8
    a0 = c `index` 0
    a1 = c `index` 1
    a2 = c `index` 2
    a3 = c `index` 3
  
-- | reads the j-th column in a state.
column :: Finite 4 -> State -> Column
column j s = generate $ \ i -> (s `index` i) `index` j

type Row = Vec 4 (W 8)
-- | reads the j-th column in a state.
row :: Finite 4 -> State -> Row
row i s = generate $ \ j -> (s `index` i) `index` j

invmixcolumns :: State -> State
invmixcolumns s = transpose $ generate $ \ j -> invMixColumn (column j s)

{-
-- | Testing code

pc :: Column -> IO ()
pc c = do
         let v0 = xshow $ c `index` 0
         let v1 = xshow $ c `index` 1
         let v2 = xshow $ c `index` 2
         let v3 = xshow $ c `index` 3
         putStrLn $ "  [" ++ v0 ++ " , " ++ v1  ++ " , " ++ v2  ++ " , " ++ v3 ++ "]"

pp :: State -> Finite 4 -> IO ()
pp s i = do
            let v0 = xshow $ index (s `index` i) 0
            let v1 = xshow $ index (s `index` i) 1
            let v2 = xshow $ index (s `index` i) 2
            let v3 = xshow $ index (s `index` i) 3
            putStrLn $ "  " ++ v0 ++ " " ++ v1  ++ " " ++ v2  ++ " " ++ v3 

ps :: State -> IO ()
ps s = do
         pp s (finite 0)
         pp s (finite 1)
         pp s (finite 2)
         pp s (finite 3)

mkstate :: [[Integer]] -> State
mkstate [ [v00 , v01 , v02 , v03] 
        , [v10 , v11 , v12 , v13] 
        , [v20 , v21 , v22 , v23] 
        , [v30 , v31 , v32 , v33] ]
           = fromList [ r0 , r1 , r2 , r3 ]
  where
    r0 , r1 , r2 , r3 :: Vec 4 (W 8)
    r0 = fromList [lit v00 , lit v01 , lit v02 , lit v03] 
    r1 = fromList [lit v10 , lit v11 , lit v12 , lit v13] 
    r2 = fromList [lit v20 , lit v21 , lit v22 , lit v23] 
    r3 = fromList [lit v30 , lit v31 , lit v32 , lit v33]

rst :: State
rst = mkstate $
      [ [0x1b, 0x75, 0x4a, 0xc0]
      , [0xf3, 0xf8, 0xd2, 0x2c]
      , [0xb3, 0x0f, 0xcb, 0x79]
      , [0x81, 0x78, 0xb9, 0x8d] ]

-- Cryptols InvMixColumns rst:
imc :: State
imc = mkstate [[0x16, 0xec, 0xa8, 0x09], [0xf0, 0x50, 0xac, 0xec], [0x11, 0x79, 0xe6, 0x80], [0x2d, 0x3f, 0x08, 0x7d]]

-- | It appears that invmixcolumns is working.
-- λ> ps imc
--   0x16 0xEC 0xA8 0x09
--   0xF0 0x50 0xAC 0xEC
--   0x11 0x79 0xE6 0x80
--   0x2D 0x3F 0x08 0x7D
-- λ> ps $ invmixcolumns rst
--   0x16 0xEC 0xA8 0x09
--   0xF0 0x50 0xAC 0xEC
--   0x11 0x79 0xE6 0x80
--   0x2D 0x3F 0x08 0x7D
  
-- Spec.cry's mixcolumns answer
mc :: State
mc = mkstate $
        [ [0x0a, 0x8e, 0x8b, 0x1b]
        , [0xa9, 0xf7, 0x0a, 0x9e]
        , [0x0d, 0x1b, 0xc5, 0x92]
        , [0x74, 0x98, 0xae, 0x0f] ]

  -- 0x23 0xEF 0xA1 0x89
  -- 0x10 0x59 0xC0 0x7C
  -- 0xDE 0x92 0xBA 0xF8
  -- 0xA5 0x2C 0x1C 0x58

-- Testing> rst
-- [[0x1b, 0x75, 0x4a, 0xc0], [0xf3, 0xf8, 0xd2, 0x2c],
--  [0xb3, 0x0f, 0xcb, 0x79], [0x81, 0x78, 0xb9, 0x8d]]
-- Testing> MixColumns rst
-- [[0x0a, 0x8e, 0x8b, 0x1b], [0xa9, 0xf7, 0x0a, 0x9e],
--  [0x0d, 0x1b, 0xc5, 0x92], [0x74, 0x98, 0xae, 0x0f]]

-------
-- | These are taken from row 1 of Appendix B, page 34, FIPS-197-upd
-- | Pre- and post-mixcolumns.
-------

i0 :: State
i0 = mkstate
        [ [0xd4, 0xe0, 0xb8, 0x1e]
        , [0xbf, 0xb4, 0x41, 0x27]
        , [0x5d, 0x52, 0x11, 0x98]
        , [0x30, 0xae, 0xf1, 0xe5] ]

c0 :: Column
c0 = fromList [ s0 , s1 , s2 , s3 ]

s0 , s1 , s2 , s3 :: W 8
s0 = lit 212
s1 = lit 191
s2 = lit 93
s3 = lit 48

crud :: W 8
crud = lit 0x3E

e0 :: State
e0 = mkstate [ [0x04, 0xe0, 0x48, 0x28 ]
             , [0x66, 0xcb, 0xf8, 0x06 ]
             , [0x81, 0x19, 0xd3, 0x26 ]
             , [0xe5, 0x9a, 0x7a, 0x4c]]

col1 :: Vec 4 (W 8)
col1 = fromList [ lit 0x0 , lit 0x1 , lit 0x2 , lit 0x3 ]
col2 :: Vec 4 (W 8)
col2 = fromList [ lit 0xd4 , lit 0xbf , lit 0x5d , lit 0x30 ]

-- pc (mixColumn col1)
--   [2 , 7 , 12 , 5]
-- _mix_column(col1) =  [2, 7, 0, 5]

-- pc (column 0 rst)
--   [27 , 243 , 179 , 129]
-- pc $ mixColumn (column 0 rst)
--   [10 , 169 , 13 , 116]

-- firstrow , secondrow , thirdrow :: W 8 -> W 8 -> W 8 -> W 8 -> W 8
-- firstrow s0 s1 s2 s3 = (mul2 s0) ^ (mul3 s1) ^ s2 ^ s3
-- secondrow s0 s1 s2 s3 = s0 ^ (mul2 s1) ^ (mul3 s2) ^ s3
-- thirdrow s0 s1 s2 s3 = s0 ^ s1 ^ (mul2 s2) ^ (mul3 s3)
-- --thirdrow s0 s1 s2 s3 = s0 ^ s1 ^ (lit 2 * s2) ^ (lit 3 * s3)
-- -- firstrow (lit 27) (lit 243) (lit 179) (lit 129)
-}
