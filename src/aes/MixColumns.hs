{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module MixColumns (mixColumns) where

import Prelude (Integer , ($) , IO , putStrLn , (++))
import ReWire
import ReWire.Bits           as B
import ReWire.Finite
import ReWire.FiniteComp     as FC
import ReWire.Vectors hiding ((++))
import ReWire.Interactive (dshow , hex , xshow)

import AESBasic

-- type State  = Vec 4 (Vec 4 (W 8))
-- type Column = Vec 4 (W 8)

-- | Multiplication by 2 in GF(2^8)
-- This is equivalent to left shift by 1, with conditional XOR by 0x1B
mul2 :: W 8 -> W 8
mul2 x = if (x .&. lit 0x80) /= lit 0
         then ((x <<. lit 1) ^ lit 0x1B) .&. lit 0xFF
         else (x <<. lit 1) .&. lit 0xFF

-- | Multiplication by 3 in GF(2^8)
-- This is equivalent to x * 2 + x = x * 3
mul3 :: W 8 -> W 8
mul3 x = mul2 x ^ x

-- | Apply MixColumns transformation to a single column
mixColumn :: Column -> Column
mixColumn c = fromList
               [ mul2 s0 ^ mul3 s1 ^ s2 ^ s3,  -- First row: 02*s0 + 03*s1 + 01*s2 + 01*s3
                 s0 ^ mul2 s1 ^ mul3 s2 ^ s3,  -- Second row: 01*s0 + 02*s1 + 03*s2 + 01*s3
                 s0 ^ s1 ^ mul2 s2 ^ mul3 s3,  -- Third row: 01*s0 + 01*s1 + 02*s2 + 03*s3
                 mul3 s0 ^ s1 ^ s2 ^ mul2 s3   -- Fourth row: 03*s0 + 01*s1 + 01*s2 + 02*s3
                 ]   
  where
    s0 , s1 , s2 , s3 :: W 8
    s0 = c `index` 0
    s1 = c `index` 1
    s2 = c `index` 2
    s3 = c `index` 3


-- | Apply MixColumns transformation to the entire state
mixColumns :: State -> State
mixColumns s = generate $ \ i -> mixColumn (s `index` i)

-- | Testing code

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
    
i0 :: State
i0 = mkstate
        [ [0xd4, 0xe0, 0xb8, 0x1e]
        , [0xbf, 0xb4, 0x41, 0x27]
        , [0x5d, 0x52, 0x11, 0x98]
        , [0x30, 0xae, 0xf1, 0xe5] ]

e0 :: State
e0 = mkstate [ [0x04, 0xe0, 0x48, 0x28 ]
             , [0x66, 0xcb, 0xf8, 0x06 ]
             , [0x81, 0x19, 0xd3, 0x26 ]
             , [0xe5, 0x9a, 0x7a, 0x4c]]

