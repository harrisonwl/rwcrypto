{-# LANGUAGE DataKinds #-}
module Aes.MixColumns (mixcolumns , transpose) where

import Prelude (Integer , ($) , IO , putStrLn , (++))
import ReWire
import ReWire.Bits           as B
import ReWire.Finite
import ReWire.FiniteComp     as FC hiding ((*) , (+))
import ReWire.Vectors hiding ((++))
import ReWire.Interactive (dshow , hex , xshow , bshow)

import Aes.Basic(State , Column , transpose)

-- | Multiplication by 2 in GF(2^8). It's also called xtimes in FIPS197.
mul2 :: W 8 -> W 8
mul2 x = if (x >>. lit 7) B.== lit 0
         then x <<. lit 1               
         else ((x <<. lit 1) ^ lit 0x1B)

-- | Multiplication by 3 in GF(2^8)
mul3 :: W 8 -> W 8
mul3 x = mul2 x ^ x

-- | Apply MixColumns transformation to a single column
mixColumn :: Column -> Column
mixColumn c = fromList
               [ mul2 s0 ^ mul3 s1 ^ s2 ^ s3,  -- First row:  02*s0 ^ 03*s1 ^ 01*s2 ^ 01*s3
                 s0 ^ mul2 s1 ^ mul3 s2 ^ s3,  -- Second row: 01*s0 ^ 02*s1 ^ 03*s2 ^ 01*s3
                 s0 ^ s1 ^ mul2 s2 ^ mul3 s3,  -- Third row:  01*s0 ^ 01*s1 ^ 02*s2 ^ 03*s3
                 mul3 s0 ^ s1 ^ s2 ^ mul2 s3   -- Fourth row: 03*s0 ^ 01*s1 ^ 01*s2 ^ 02*s3
                 ]   
  where
    s0 , s1 , s2 , s3 :: W 8
    s0 = c `index` 0
    s1 = c `index` 1
    s2 = c `index` 2
    s3 = c `index` 3
  
-- | reads the j-th column in a state.
column :: Finite 4 -> State -> Column
column j s = generate $ \ i -> (s `index` i) `index` j

mixcolumns :: State -> State
mixcolumns s = transpose $ generate $ \ j -> mixColumn (column j s)

-- | Testing code

pc :: Column -> IO ()
pc c = do
         let v0 = dshow $ c `index` 0
         let v1 = dshow $ c `index` 1
         let v2 = dshow $ c `index` 2
         let v3 = dshow $ c `index` 3
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
