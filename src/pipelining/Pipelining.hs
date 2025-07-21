module Pipelining(pipe3) where

-- | This is demonstration Haskell code to explain
-- | the two forms of pipelining that we use.

import Prelude hiding ((<>))
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Resumption.Reactive 
import Control.Monad.Resumption.Connectors

import DoubleRound(doubleround)

data Inp a = Stall | Arg a deriving (Eq , Show)
data Out a = DC    | Val a deriving (Eq , Show)

out :: o1 -> o2
out = undefined
conn :: i2 -> o1 -> i1
conn = undefined
now :: (i1 , s , o1)
now = undefined
f1 :: i1 -> s -> (o1 , s)
f1 = undefined

f2 :: (o1 -> a1) -> (p1 -> c -> t2) -> (a2, b1, c) -> (t2 -> p2 -> (o1, b2)) -> p1 -> p2 -> (a1, b2)
f2 out conn now f1 i2 s = let
                             (_ , _ , o1) = now
                             i1           = conn i2 o1
                             (o1' , s')   = f1 i1 s
                          in
                             (out o1' , s')
              

foldre :: Monad m => (ii -> oi) -> (oi -> ox) -> (oi -> ix -> ii) -> oi -> ix -> ReacT ix ox m ()
foldre f out conn oi ix = do
                            let ii = conn oi ix
                            let o = f ii
                            ix' <- signal (out o)
                            foldre f out conn o ix'

delay :: Monad m => (a -> m b) -> a -> ReacT i (Out o) m b
delay f a =  do
                b <- lift (f a)
                signal DC
                return b

output :: Monad m => (a -> m b) -> a -> ReacT i (Out b) m i
output f a = do
                b <- lift (f a)
                signal (Val b)

(<>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f <> g = \ a -> f a >>= g

stage3 :: Monad m =>
          (a -> b) ->
          (b -> c) ->
          (c -> d) ->
          a -> ReacT i (Out d) m i
stage3 f g h = delay (return . f) <> delay (return . g) <> output (return . h)

----
-- Pipelines (depth = 3)
----

fi2o :: (a -> b) -> Inp a -> Out b
fi2o f Stall   = DC
fi2o f (Arg a) = Val (f a)

o2i :: Out a -> Inp a
o2i DC      = Stall
o2i (Val a) = Arg a

fo2i :: (a -> b) -> Out a -> Inp b
fo2i f DC      = Stall
fo2i f (Val a) = Arg (f a)

pipeline3 :: (o1 -> i2) -> (o2 -> i3) -> (o1, o2, o3) -> i1 -> (i1, i2, i3)
pipeline3 f1 f2 (o1 , o2 , o3) i1 = (i1 , f1 o1 , f2 o2)

cross3 :: (i1 -> o1) -> (i2 -> o2) -> (i3 -> o3) -> (i1, i2, i3) -> (o1, o2, o3)
cross3 f g h (i1 , i2 , i3) = (f i1 , g i2 , h i3)

out3 :: (Out a, Out b, Out c) -> Out c
out3 (_ , _ , x) = x

epip3 :: Monad m =>
         (Out o1 -> Inp i2) ->
         (Out o2 -> Inp i3) ->
         (i1 -> o1)         ->
         (i2 -> o2)         ->
         (i3 -> o3)         ->
         Inp i1             ->
         ReacT (Inp i1) (Out o3) m ()
epip3 oi1 oi2 f g h = foldre (cross3 (fi2o f) (fi2o g) (fi2o h)) out3 conn (DC , DC , DC)
  where
--    conn :: (Out a, Out a, Out a) -> Inp a -> (Inp a, Inp a, Inp a)
    conn =  pipeline3 oi1 oi2 -- o2i o2i

pipe3 :: Monad m => (a -> a) -> (a -> a) -> (a -> a) -> Inp a -> ReacT (Inp a) (Out a) m ()
pipe3 f g h = foldre (cross3 (fi2o f) (fi2o g) (fi2o h)) out3 conn (DC , DC , DC)
  where

    conn :: (Out a, Out a, Out a) -> Inp a -> (Inp a, Inp a, Inp a)
    conn (DC , DC , _) ix         = (ix , Stall , Stall)
    conn (Val x1 , Val x2 , _) ix = (ix , Arg x1 , Arg x2)
    conn (Val x1 , DC , _) ix     = (ix , Arg x1 , Stall)
    conn (DC , Val x2 , _) ix     = (ix , Stall , Arg x2)

----
-- Pipelines (depth = 11)
----

-- pipeline11 :: (o -> i)                                    ->
--               (o , o , o , o , o , o , o , o , o , o , o) ->
--               i                                           ->
--               (i , i , i , i , i , i , i , i , i , i , i)
pipeline11 f (o1 , o2 , o3 , o4 , o5 , o6 , o7 , o8 , o9 , o10 , o11) i1
  = (i1 , f o1 , f o2 , f o3 , f o4 , f o5 , f o6 , f o7 , f o8 , f o9 , f o10) 

-- cross11 :: (u1 -> a1)   -> (u2 -> a2)   -> (u3 -> a3) ->
--            (u4 -> a4)   -> (u5 -> a5)   -> (u6 -> a6) ->
--            (u7 -> a7)   -> (u8 -> a8)   -> (u9 -> a9) ->
--            (u10 -> a10) -> (u11 -> a11)               ->
--            (u1 , u2 , u3 , u4 , u5 , u6 , u7 , u8 , u9 , u10 , u11) ->
--            (a1 , a2 , a3 , a4 , a5 , a6 , a7 , a8 , a9 , a10 , a11)
cross11 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 (u1 , u2 , u3 , u4 , u5 , u6 , u7 , u8 , u9 , u10 , u11) =
  (f1 u1 , f2 u2 , f3 u3 , f4 u4 , f5 u5 , f6 u6 , f7 u7 , f8 u8 , f9 u9 , f10 u10 , f11 u11)

pipe11 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 
 = foldre f out11 conn11 dontcare11
  where
     f = cross11 (fi2o f1) (fi2o f2) (fi2o f3) (fi2o f4) (fi2o f5) (fi2o f6) (fi2o f7) (fi2o f8) (fi2o f9) (fi2o f10) (fi2o f11)

-- out11 :: (Out a, Out a, Out a , Out a, Out a, Out a , Out a, Out a, Out a , Out a, Out a) -> Out a
out11 (_ , _ , _ , _ , _ , _ , _ , _ , _ , _ , x ) = x

-- conn11 :: (Out a, Out a, Out a, Out a, Out a, Out a, Out a, Out a, Out a, Out a, Out a) -> 
--           Inp a ->
--           (Inp a, Inp a, Inp a, Inp a, Inp a, Inp a, Inp a, Inp a, Inp a, Inp a, Inp a)
conn11 = pipeline11 o2i

--dontcare11 :: (Out a, Out a, Out a, Out a, Out a, Out a, Out a, Out a, Out a, Out a, Out a)
dontcare11 = (DC , DC , DC , DC , DC , DC , DC , DC , DC , DC , DC)  


-----------
-- Test code below
-----------

-- bare f g =   
-- bare f g h = \ (a , b , c) -> (f a , g b , h c)
-- iterate f g h = 

data WriterPlus w a = w :> WriterPlus w a | w :+> a

instance (Show w , Show a) => Show (WriterPlus w a) where
  show (w :> ws) = show w ++ " :> " ++ show ws
  show (w :+> a) = show w ++ " :+> " ++ show a

unroll :: (i -> ReacT i o Identity a) -> [i] -> [Either (i, a) (i, o)]
unroll f []     = []
unroll f (i:is) = case (f i) of
          ReacT (Identity (Left a))      -> [Left (i , a)]
          ReacT (Identity (Right (o,k))) -> Right (i,o) : unroll k is

inc :: Int -> Int
inc i = i + 1
{-
ex1 :: Int -> ReacT Int (Out Int) Identity Int
ex1 = stage3 inc inc inc <> ex1

ex2 :: Inp Int -> ReacT (Inp Int) (Out Int) Identity ()
ex2 = pipe3 inc inc inc

args1 , args2 :: [ Inp Int ]
args1 = map Arg [0..10]
args2 = [ Arg 0 , Stall , Arg 1 , Stall , Stall , Arg 2 , Arg 3 , Stall , Stall , Stall ]

test args = unroll (pipe3 inc inc inc) args == unroll (epip3 inc inc inc) args
-}

{- begets:

  [ Right (Arg 0,DC) , Right (Stall,DC) , Right (Arg 1,Val 3)
  ,Right (Stall,DC) , Right (Stall,Val 4) , Right (Arg 2,DC),Right (Stall,DC),Right (Stall,Val 5),Right (Stall,DC)]
-}

{-
λ> unroll ex2 (map Arg [0..10])
  [ Right (Arg 0,DC),Right (Arg 1,DC),Right (Arg 2,Val 3)
  , Right (Arg 3,Val 4),Right (Arg 4,Val 5),Right (Arg 5,Val 6),Right (Arg 6,Val 7),Right (Arg 7,Val 8),Right (Arg 8,Val 9),Right (Arg 9,Val 10),Right (Arg 10,Val 11)]
-}

step :: (i -> o) -> i -> ReacT i o Identity i
step f = \ i -> inj (return (f i)) >>= signal
  where
    inj :: Identity a -> ReacT i o Identity a
    inj (Identity a) = ReacT (Identity (Left a))

-- unroll ex1 [0..10]
-- [Right (0,DC),Right (1,DC),Right (2,Val 3),Right (3,DC),Right (4,DC)
-- ,Right (5,Val 6),Right (6,DC),Right (7,DC),Right (8,Val 9),Right (9,DC)
-- ,Right (10,DC)]

