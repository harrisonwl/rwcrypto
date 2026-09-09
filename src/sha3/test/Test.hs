{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Test.Test where

import Text.Read (readMaybe)
import System.IO (readFile)

import Prelude hiding (pi)

-- import ReWire
import ReWire.Bits
import ReWire.Finite
import ReWire.FiniteComp     as FC
import ReWire.Vectors        as RWV

import ReWire hiding (ReacT,Identity,signal,lift, get, put, StateT)

import Reference.Layout (A , C , D , readA , readC, writeC , putD)
import Reference.Theta(theta)
import Reference.Rho(rho)
import Reference.Iota(iota)
import Reference.Pi(pi)
import Reference.Chi(chi)
import Reference.Rnd(rnd)

sha3dir :: String
sha3dir = "/Users/bill/work/rwcrypto/src/sha3/test/testvectors/"

  
mkA :: [[W 64]] -> A
mkA ls = fromList $ Prelude.map fromList ls

test :: (A -> A) -> FilePath -> IO (Maybe [Bool])
test f file = do
    c <- readFile (sha3dir Prelude.++ file)
    mtb <- mkTestBattery c
    case mtb of
          Just tb -> return $ Just (Prelude.map (\ (a,a') -> f a Prelude.== a') tb)
          Nothing -> return $ Nothing
      -- Nothing -> do
      --   putStrLn $ "No such file: " Prelude.++ sha3dir Prelude.++ file
      --   return Nothing
        

-- ugly
conv :: ([[Integer]], [[Integer]]) -> (A, A)
conv (l1,l2) = (mkA $ Prelude.map (Prelude.map lit) l1 , mkA $ Prelude.map (Prelude.map lit) l2)

mkTestBattery :: String -> IO (Maybe [(A , A)])
mkTestBattery f = do
                     let mc = gitrdone f
                     case mc of
                       Just ls -> return $ Just $ fmap conv ls
                       Nothing -> return Nothing
   where
     gitrdone :: String -> Maybe [([[Integer]],[[Integer]])]
     gitrdone f = readMaybe f
