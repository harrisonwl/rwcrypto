{-# LANGUAGE DataKinds #-}
module Aes.InvShiftRows (invshiftrows) where

import ReWire
import ReWire.Bits
import ReWire.Finite
import ReWire.Interactive (dshow , hex , xshow)

import ReWire.FiniteComp as FC
import ReWire.Vectors(index , generate)

import Aes.Basic(State , Column)

invshiftrows :: State -> State
invshiftrows v = generate $ \ i ->
                 generate $ \ j ->
                               (v `index` i) `index` (j FC.- i)
                           

-- | Testing code
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

{-

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

e :: State
e = mkstate $
      [ [0x0, 0x1, 0x2, 0x3]
      , [0x4, 0x5, 0x6, 0x7]
      , [0x8, 0x9, 0xa, 0xb]
      , [0xc, 0xd, 0xd, 0xf] ]
    

sr :: State
sr = mkstate $
       [ [0x1b, 0x75, 0x4a, 0xc0]
       , [0xf8, 0xd2, 0x2c, 0xf3]
       , [0xcb, 0x79, 0xb3, 0x0f]
       , [0x8d, 0x81, 0x78, 0xb9] ]

-- Spec.cry's mixcolumns answer
mc :: State
mc = mkstate $
        [ [0x0a, 0x8e, 0x8b, 0x1b]
        , [0xa9, 0xf7, 0x0a, 0x9e]
        , [0x0d, 0x1b, 0xc5, 0x92]
        , [0x74, 0x98, 0xae, 0x0f] ]

e0 :: State
e0 = mkstate [ [0x04, 0xe0, 0x48, 0x28 ]
             , [0x66, 0xcb, 0xf8, 0x06 ]
             , [0x81, 0x19, 0xd3, 0x26 ]
             , [0xe5, 0x9a, 0x7a, 0x4c]]

i0 :: State
i0 = mkstate
        [ [0xd4, 0xe0, 0xb8, 0x1e]
        , [0xbf, 0xb4, 0x41, 0x27]
        , [0x5d, 0x52, 0x11, 0x98]
        , [0x30, 0xae, 0xf1, 0xe5] ]


-}
