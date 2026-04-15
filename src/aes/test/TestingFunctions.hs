{-# LANGUAGE DataKinds #-}
module Aes.Test.TestingFunctions where

import Prelude (($) , Integer , map , (.) , IO , putStrLn , (++))
import ReWire
import ReWire.Bits ((^) , lit)
import ReWire.Finite (finite)
import ReWire.Vectors (index , generate)

import ReWire.Interactive (dshow , hex , xshow)

import Aes.Basic(Column,KeySchedule,State,RoundKey,initState,toByte4,fromW32,transpose)
import Aes.Operations.SubBytes(subbytes)
import Aes.Operations.AddRoundKey(addRoundKey)
import Aes.Cipher256(encrypt256)
import Aes.Operations.ShiftRows(shiftrows)
import Aes.Operations.MixColumns(mixcolumns)
import Aes.KeyExp.Reference256(keyexpand)

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


toState :: (W 32 , W 32 , W 32 , W 32) -> State -- (Column , Column , Column , Column )
toState (w0 , w1 , w2 , w3) = transpose $ fromList [col0 , col1 , col2 , col3] -- (col0 , col1 , col2 , col3)
  where
    col0 , col1 , col2 , col3 :: Column
    col0 = toByte4 w0
    col1 = toByte4 w1
    col2 = toByte4 w2
    col3 = toByte4 w3
