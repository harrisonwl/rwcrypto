{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Sha3.Layout ( A , C , D
                   , readA , readC , writeC , putD , rdArr , wrArr) where

import ReWire
import ReWire.Finite()
import ReWire.Vectors (update,index,(!=))

-- | state array 
type A = Vec 5 (Vec 5 (W 64))
type C = Vec 5 (W 64)
type D = Vec 5 (W 64)

readA :: Finite 5 -> Finite 5 -> StateT (A , C , D) Identity (W 64)
readA x y = do
               (a , _ , _) <- get
               return (index (index a x) y)
               
readC :: StateT (A , C , D) Identity C
readC = do
          (_ , c , _) <- get
          return c
          
writeC :: Finite 5 -> W 64 -> StateT (A , C , D) Identity ()
writeC x v = do
                 (a , c , d) <- get
                 put (a , update c x v , d)

putD :: D -> StateT (A , C , D) Identity ()
putD d = do
           (a , c , _) <- get
           put (a , c , d)

rdArr :: Vec n1 (Vec n2 a) -> (Finite n1 , Finite n2) -> a
rdArr a (x , y) = index (index a x) y

wrArr :: (KnownNat n1, KnownNat n2) =>
         Vec n1 (Vec n2 a) -> (Finite n1, Finite n2) -> a -> Vec n1 (Vec n2 a)
wrArr a (i , j) w = let
                        plane0  = index a i
                        plane0' = (plane0 != j) w
                    in
                        (a != i) plane0'
