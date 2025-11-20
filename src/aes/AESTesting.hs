{-# LANGUAGE DataKinds #-}
module AESTesting where

import Prelude (($) , Integer , map , (.) , IO , putStrLn , (++))
import ReWire
import ReWire.Bits ((^) , lit)
import ReWire.Finite (finite)
import ReWire.Vectors (index , generate)

import ReWire.Interactive (dshow , hex , xshow)

import Aes.AddRoundKey(addRoundKey)

type State    = Vec 4 (Vec 4 (W 8))
type RoundKey = Vec 4 (Vec 4 (W 8))



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
    
s0 :: State
s0 = mkstate
        [ [0xd4, 0xe0, 0xb8, 0x1e]
        , [0xbf, 0xb4, 0x41, 0x27]
        , [0x5d, 0x52, 0x11, 0x98]
        , [0x30, 0xae, 0xf1, 0xe5] ]

s1 :: State
s1 = mkstate [ [0x04, 0xe0, 0x48, 0x28 ]
             , [0x66, 0xcb, 0xf8, 0x06 ]
             , [0x81, 0x19, 0xd3, 0x26 ]
             , [0xe5, 0x9a, 0x7a, 0x4c]]
