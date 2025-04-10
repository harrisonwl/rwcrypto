{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module ReferenceSHA256 where

-- | In with the new.
import Prelude hiding ((^) , (+) , (==))
import ReWire
import ReWire.Bits -- hiding (rotR)
import ReWire.Vectors
import ReWire.Interactive hiding (trace)

import qualified BinaryArithmetic
import qualified W32 as WWW

import Debug.Trace

-- import Control.Monad.Identity
-- import Control.Monad.State
-- import Control.Monad.Resumption.Reactive

-------------------------------------------------------------------------------------------
--- The following constitutes the reference semantics for SHA256. It is a straightforward
--- adaptation into monadic style of the algorithm given in the NIST document. See, in
--- particular, the "sha256" function below.
-------------------------------------------------------------------------------------------

--------------------------------------------
--- The standard functions
--------------------------------------------

type W32 = WWW.W32

tovec32 :: W32 -> W 32
tovec32 w32 = lit $ WWW.fromW32 w32

toW32 :: W 32 -> W32
toW32 = WWW.toW32 . ReWire.Bits.toInteger 

{-
-- | this is the replacement that works fine.
rotR :: W 32 -> W 32 -> W 32
rotR x i = tovec32 $ WWW.rotR (toW32 x) (fromInteger (ReWire.Bits.toInteger i))

rotateR :: KnownNat m => W m -> W m -> W m
rotateR n w = (w >>. n) .|. (w <<. (lit (len w) ReWire.Bits.- n))

b :: W 8
b = lit 0x88

k :: W 8
k = lit 1
-}

ch :: W 32 -> W 32 -> W 32 -> W 32
ch x y z = (x .&. y) ^ (bnot x .&. z) 

maj :: W 32 -> W 32 -> W 32 -> W 32
maj x y z = (x .&. y) ^ (x .&. z) ^ (y .&. z)

-- | 
-- | ORDER OF OPERANDS TO rotR!!!!!!!
-- | 

-- bigsigma0 :: W 32 -> W 32
-- bigsigma0 x = (rotR x (lit 2)) ^ (rotR x (lit 13)) ^ (rotR x (lit 22))

bigsigma0 :: W 32 -> W 32
bigsigma0 x = (rotR (lit 2) x) ^ (rotR (lit 13) x) ^ (rotR (lit 22) x)

-- bigsigma1 :: W 32 -> W 32
-- bigsigma1 x = (rotR x (lit 6)) ^ (rotR x (lit 11)) ^ (rotR x (lit 25))

bigsigma1 :: W 32 -> W 32
bigsigma1 x = (rotR (lit 6) x) ^ (rotR (lit 11) x) ^ (rotR (lit 25) x)

-- sigma0 :: W 32 -> W 32
-- sigma0 x = (rotR x (lit 7)) ^ (rotR x (lit 18)) ^ (x >>. (lit 3))

sigma0 :: W 32 -> W 32
sigma0 x = (rotR (lit 7) x) ^ (rotR (lit 18) x) ^ (x >>. (lit 3))

-- sigma1 :: W 32 -> W 32
-- sigma1 x = (rotR x (lit 17)) ^ (rotR x (lit 19)) ^ (x >>. (lit 10))
-- --          ^^^^ rotR appears to be the culprit.

sigma1 :: W 32 -> W 32
sigma1 x = (rotR (lit 17) x) ^ (rotR (lit 19) x) ^ (x >>. (lit 10))
--          ^^^^ rotR appears to be the culprit.



-------------------------------------------
--- The hashing algorithm
-------------------------------------------

type Oct a = ( a , a , a , a , a , a , a , a )
type Hex a = ( a , a , a , a , a , a , a , a , a , a , a , a , a , a , a , a )

type M = StateT (Oct (W 32))
            (StateT (Hex (W 32))
                (StateT (Oct (W 32))
                    (StateT (W 6) Identity)))

getDigest :: M (Oct (W 32))
getDigest = lift (lift get)
putDigest :: Oct (W 32) -> M ()
putDigest = lift . lift . put

getBlock :: M (Hex (W 32))
getBlock = lift get
putBlock :: Hex (W 32) -> M ()
putBlock = lift . put

getIntDig :: M (Oct (W 32))
getIntDig = get
putIntDig :: Oct (W 32) -> M ()
putIntDig = put

getCtr :: M (W 6)
getCtr = lift (lift (lift get))
putCtr :: W 6 -> M ()
putCtr = lift . lift . lift . put

intermediate :: M ()
intermediate =  do
  (h1 , h2 , h3 , h4 , h5 , h6 , h7 , h8) <- getDigest
  (a , b , c , d , e , f , g , h)         <- getIntDig
  putDigest (a+h1 , b+h2 , c+h3 , d+h4 , e+h5 , f+h6 , g+h7 , h+h8)

-------------------------------------------
--- SHA-256 scheduler algorithm
-------------------------------------------

sched :: M (W 32)
sched = do
           s <- getBlock
           case s of
            (w00 , _ , _ , _ , _ , _ , _ , _ , _ , _ , _ , _ , _ , _ , _ , _) -> do
                                                          putBlock (updateSched s)
                                                          return w00

updateSched :: Hex (W 32) -> Hex (W 32)
updateSched ( w00 , w01 , w02 , w03 , w04 , w05 , w06 , w07
            , w08 , w09 , w10 , w11 , w12 , w13 , w14 , w15 ) = 
                 ( w01 , w02 , w03 , w04 , w05 , w06 , w07 , w08 
                 , w09 , w10 , w11 , w12 , w13 , w14 , w15 , w16)
  where
    w16 = sigma1 w14 + w09 + sigma0 w01 + w00

-------------------------------------------
--- SHA-256 compression algorithm
-------------------------------------------

compress :: W 32 -> W 32 -> M ()
compress k w = do
                  dig' <- getIntDig
                  putIntDig (step256 k w dig')

step256 :: W 32 -> W 32 -> Oct (W 32) -> Oct (W 32)
step256 k w (a , b , c , d , e , f , g , h) = (a' , b' , c' , d' , e' , f' , g' , h')
            where
              t1 = h + bigsigma1 e + ch e f g + k + w
              t2 = bigsigma0 a + maj a b c
              h' = g
              g' = f
              f' = e
              e' = d + t1
              d' = c
              c' = b
              b' = a
              a' = t1 + t2
  
initialSHA256State :: Oct (W 32)
initialSHA256State = ( lit 0x6a09e667 , lit 0xbb67ae85 , lit 0x3c6ef372 , lit 0xa54ff53a
                     , lit 0x510e527f , lit 0x9b05688c , lit 0x1f83d9ab , lit 0x5be0cd19 )

seed :: W 6 -> W 32
seed x | x == lit 0  = lit 0x428a2f98
       | x == lit 1  = lit 0x71374491
       | x == lit 2  = lit 0xb5c0fbcf
       | x == lit 3  = lit 0xe9b5dba5
       | x == lit 4  = lit 0x3956c25b
       | x == lit 5  = lit 0x59f111f1
       | x == lit 6  = lit 0x923f82a4
       | x == lit 7  = lit 0xab1c5ed5
       | x == lit 8  = lit 0xd807aa98
       | x == lit 9  = lit 0x12835b01
       | x == lit 10 = lit 0x243185be
       | x == lit 11 = lit 0x550c7dc3
       | x == lit 12 = lit 0x72be5d74
       | x == lit 13 = lit 0x80deb1fe
       | x == lit 14 = lit 0x9bdc06a7
       | x == lit 15 = lit 0xc19bf174
       | x == lit 16 = lit 0xe49b69c1
       | x == lit 17 = lit 0xefbe4786
       | x == lit 18 = lit 0x0fc19dc6
       | x == lit 19 = lit 0x240ca1cc
       | x == lit 20 = lit 0x2de92c6f
       | x == lit 21 = lit 0x4a7484aa
       | x == lit 22 = lit 0x5cb0a9dc
       | x == lit 23 = lit 0x76f988da
       | x == lit 24 = lit 0x983e5152
       | x == lit 25 = lit 0xa831c66d
       | x == lit 26 = lit 0xb00327c8
       | x == lit 27 = lit 0xbf597fc7
       | x == lit 28 = lit 0xc6e00bf3
       | x == lit 29 = lit 0xd5a79147
       | x == lit 30 = lit 0x06ca6351
       | x == lit 31 = lit 0x14292967
       | x == lit 32 = lit 0x27b70a85
       | x == lit 33 = lit 0x2e1b2138
       | x == lit 34 = lit 0x4d2c6dfc
       | x == lit 35 = lit 0x53380d13
       | x == lit 36 = lit 0x650a7354
       | x == lit 37 = lit 0x766a0abb
       | x == lit 38 = lit 0x81c2c92e
       | x == lit 39 = lit 0x92722c85
       | x == lit 40 = lit 0xa2bfe8a1
       | x == lit 41 = lit 0xa81a664b
       | x == lit 42 = lit 0xc24b8b70
       | x == lit 43 = lit 0xc76c51a3
       | x == lit 44 = lit 0xd192e819
       | x == lit 45 = lit 0xd6990624
       | x == lit 46 = lit 0xf40e3585
       | x == lit 47 = lit 0x106aa070
       | x == lit 48 = lit 0x19a4c116
       | x == lit 49 = lit 0x1e376c08
       | x == lit 50 = lit 0x2748774c
       | x == lit 51 = lit 0x34b0bcb5
       | x == lit 52 = lit 0x391c0cb3
       | x == lit 53 = lit 0x4ed8aa4a
       | x == lit 54 = lit 0x5b9cca4f
       | x == lit 55 = lit 0x682e6ff3
       | x == lit 56 = lit 0x748f82ee
       | x == lit 57 = lit 0x78a5636f
       | x == lit 58 = lit 0x84c87814
       | x == lit 59 = lit 0x8cc70208
       | x == lit 60 = lit 0x90befffa
       | x == lit 61 = lit 0xa4506ceb
       | x == lit 62 = lit 0xbef9a3f7
       | otherwise   = lit 0xc67178f2

innerloop :: M ()
innerloop = do
               ctr <- getCtr
               s <- sched
               compress (seed ctr) s
               putCtr (ctr + lit 1)
               if ctr == lit 63
                 then intermediate
                 else innerloop

------
-- Testing code follows
------

mainloop :: [Hex (W 32)] -> M ()
mainloop []             = return ()
mainloop (hw32 : hw32s) = do
                             hi_1 <- getDigest
                             putIntDig hi_1
                             putBlock hw32
                             putCtr (lit 0)
                             innerloop
                             mainloop hw32s

sha256 :: [Hex (W 32)] -> M (Oct (W 32))
sha256 hws = do
  putDigest initialSHA256State
  mainloop hws
  getDigest
--  d <- getDigest
--  Prelude.error (show d)

x :: W 32
x = lit 345 

instance Show (Tup8 (W 32)) where
  show (Tup8 a0 a1 a2 a3 a4 a5 a6 a7) = "(" Prelude.++
                                            (show $ toW32 a0) Prelude.++ "," Prelude.++ 
                                            (show $ toW32 a1) Prelude.++ "," Prelude.++ 
                                            (show $ toW32 a2) Prelude.++ "," Prelude.++ 
                                            (show $ toW32 a3) Prelude.++ "," Prelude.++ 
                                            (show $ toW32 a4) Prelude.++ "," Prelude.++ 
                                            (show $ toW32 a5) Prelude.++ "," Prelude.++ 
                                            (show $ toW32 a6) Prelude.++ "," Prelude.++
                                            (show $ toW32 a7) Prelude.++ ")" 

data Tup8 a = Tup8 !a !a !a !a !a !a !a !a
instance ShowBin a => Pretty (Tup8 a) where
  pp (Tup8 a0 a1 a2 a3 a4 a5 a6 a7) = "arrived" -- bshow a0
  -- pp (Tup8 a0 a1 a2 a3 a4 a5 a6 a7) = "(\t" Prelude.++ pp a0 Prelude.++ "\n" Prelude.++
  --                                     ",\t" Prelude.++ pp a1 Prelude.++ "\n" Prelude.++
  --                                     ",\t" Prelude.++ pp a2 Prelude.++ "\n" Prelude.++
  --                                     ",\t" Prelude.++ pp a3 Prelude.++ "\n" Prelude.++
  --                                     ",\t" Prelude.++ pp a4 Prelude.++ "\n" Prelude.++
  --                                     ",\t" Prelude.++ pp a5 Prelude.++ "\n" Prelude.++
  --                                     ",\t" Prelude.++ pp a6 Prelude.++ "\n" Prelude.++
  --                                     ",\t" Prelude.++ pp a7 Prelude.++ ")\n"

inj8 :: Oct a -> Tup8 a
inj8 (a0 , a1 , a2 , a3 , a4 , a5 , a6 , a7) = Tup8 a0 a1 a2 a3 a4 a5 a6 a7

-- run_sha256 :: [Hex (W 32)] -> Oct (W 32)
-- run_sha256 input = runIdentity $ runStateT (runStateT (runStateT (runStateT (sha256 input) undefined) undefined) undefined) undefined

run_sha256 :: [Hex (W 32)] -> Tup8 (W 32) -- Oct (W 32)
run_sha256 input = (inj8 $ fst $ fst $ fst $ fst $ runIdentity $ runStateT (runStateT (runStateT (runStateT (sha256 input) (perror "1")) (perror "2")) (perror "3")) (perror "4"))
    where
      perror = Prelude.error
     
padded1 :: [Hex (W 32)]
padded1 = [( lit 0x61626380 , lit 0x00000000
           , lit 0x00000000 , lit 0x00000000
           , lit 0x00000000 , lit 0x00000000
           , lit 0x00000000 , lit 0x00000000
           , lit 0x00000000 , lit 0x00000000
           , lit 0x00000000 , lit 0x00000000
           , lit 0x00000000 , lit 0x00000000
           , lit 0x00000000 , lit 0x00000018 )]


{-
--
-- example 1
--
msg1    = "abc"

padded1 :: [Hex Word32]
padded1 = [Hex 0x61626380 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000
               0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 0x00000018]

hashed1 = Oct 0xba7816bf 0x8f01cfea 0x414140de 0x5dae2223 0xb00361a3 0x96177a9c 0xb410ff61 0xf20015ad
-}
