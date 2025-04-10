module ExecSHA256 where

-- | In with the new.
import Prelude hiding ((^) , (+) , (&&) , negate)
import BinaryArithmetic
import W6
import W32

-- import ReWire
-- import ReWire.Bits
-- import ReWire.Vectors
-- import ReWire.Interactive
-- type W6  = W 6
-- type W32 = W 32

import Control.Monad.Identity
import Control.Monad.State
-- import Control.Monad.Resumption.Reactive

-- | Based on 
-- |   Secure Hash Standard (SHS) (FIPS PUB 180-4)
-- |

-------------------------------------------------------------------------------------------
--- The following constitutes the reference semantics for SHA256. It is a straightforward
--- adaptation into monadic style of the algorithm given in the NIST document. See, in
--- particular, the "sha256" function below.
-------------------------------------------------------------------------------------------

--------------------------------------------
--- The standard functions
--------------------------------------------

-- |
-- | The numbers refer to NIST 180-4, page 10.
-- |

ch :: W32 -> W32 -> W32 -> W32             -- (4.2)
ch x y z = (x .&. y) ^ (bnot x .&. z)

maj :: W32 -> W32 -> W32 -> W32            -- (4.3) 
maj x y z = (x .&. y) ^ (x .&. z) ^ (y .&. z) 

bigsigma0 :: W32 -> W32                    -- (4.4)
bigsigma0 x = (rotR 2 x) ^ (rotR 13 x) ^ (rotR 22 x)

bigsigma1 :: W32 -> W32                    -- (4.5)
bigsigma1 x = (rotR 6 x) ^ (rotR 11 x) ^ (rotR 25 x)

sigma0 :: W32 -> W32                       -- (4.6)
sigma0 x = (rotR 7 x) ^ (rotR 18 x) ^ (x >>. 3)

sigma1 :: W32 -> W32                       -- (4.7)
sigma1 x = (rotR 17 x) ^ (rotR 19 x) ^ (x >>. 10)


type Oct a = ( a , a , a , a , a , a , a , a )
type Hex a = ( a , a , a , a , a , a , a , a , a , a , a , a , a , a , a , a )

type M = StateT (Oct W32)         -- Hash Value: (_H0,...,_H7)
            (StateT (Hex W32)     -- Message Schedule queue
                (StateT (Oct W32) -- Working Variables: (a,b,c,d,e,f,g,h)
                    (StateT W6    -- Counter t = 0,...,63
                        Identity))) 


getHash :: M (Oct W32)
getHash = get
putHash :: Oct W32 -> M ()
putHash = put

getBlock :: M (Hex W32)
getBlock = lift get
putBlock :: Hex W32 -> M ()
putBlock = lift . put

getWorking :: M (Oct W32)
getWorking = lift (lift get)
putWorking :: Oct W32 -> M ()
putWorking = lift . lift . put

getCtr :: M W6
getCtr = lift (lift (lift get))
putCtr :: W6 -> M ()
putCtr = lift . lift . lift . put

-- | SHA-256 constants, Sect 4.2.2, page 11. 
_K :: W6 -> W32
_K t | t == lit 0  = lit 0x428a2f98
     | t == lit 1  = lit 0x71374491
     | t == lit 2  = lit 0xb5c0fbcf
     | t == lit 3  = lit 0xe9b5dba5
     | t == lit 4  = lit 0x3956c25b
     | t == lit 5  = lit 0x59f111f1
     | t == lit 6  = lit 0x923f82a4
     | t == lit 7  = lit 0xab1c5ed5
     | t == lit 8  = lit 0xd807aa98
     | t == lit 9  = lit 0x12835b01
     | t == lit 10 = lit 0x243185be
     | t == lit 11 = lit 0x550c7dc3
     | t == lit 12 = lit 0x72be5d74
     | t == lit 13 = lit 0x80deb1fe
     | t == lit 14 = lit 0x9bdc06a7
     | t == lit 15 = lit 0xc19bf174
     | t == lit 16 = lit 0xe49b69c1
     | t == lit 17 = lit 0xefbe4786
     | t == lit 18 = lit 0x0fc19dc6
     | t == lit 19 = lit 0x240ca1cc
     | t == lit 20 = lit 0x2de92c6f
     | t == lit 21 = lit 0x4a7484aa
     | t == lit 22 = lit 0x5cb0a9dc
     | t == lit 23 = lit 0x76f988da
     | t == lit 24 = lit 0x983e5152
     | t == lit 25 = lit 0xa831c66d
     | t == lit 26 = lit 0xb00327c8
     | t == lit 27 = lit 0xbf597fc7
     | t == lit 28 = lit 0xc6e00bf3
     | t == lit 29 = lit 0xd5a79147
     | t == lit 30 = lit 0x06ca6351
     | t == lit 31 = lit 0x14292967
     | t == lit 32 = lit 0x27b70a85
     | t == lit 33 = lit 0x2e1b2138
     | t == lit 34 = lit 0x4d2c6dfc
     | t == lit 35 = lit 0x53380d13
     | t == lit 36 = lit 0x650a7354
     | t == lit 37 = lit 0x766a0abb
     | t == lit 38 = lit 0x81c2c92e
     | t == lit 39 = lit 0x92722c85
     | t == lit 40 = lit 0xa2bfe8a1
     | t == lit 41 = lit 0xa81a664b
     | t == lit 42 = lit 0xc24b8b70
     | t == lit 43 = lit 0xc76c51a3
     | t == lit 44 = lit 0xd192e819
     | t == lit 45 = lit 0xd6990624
     | t == lit 46 = lit 0xf40e3585
     | t == lit 47 = lit 0x106aa070
     | t == lit 48 = lit 0x19a4c116
     | t == lit 49 = lit 0x1e376c08
     | t == lit 50 = lit 0x2748774c
     | t == lit 51 = lit 0x34b0bcb5
     | t == lit 52 = lit 0x391c0cb3
     | t == lit 53 = lit 0x4ed8aa4a
     | t == lit 54 = lit 0x5b9cca4f
     | t == lit 55 = lit 0x682e6ff3
     | t == lit 56 = lit 0x748f82ee
     | t == lit 57 = lit 0x78a5636f
     | t == lit 58 = lit 0x84c87814
     | t == lit 59 = lit 0x8cc70208
     | t == lit 60 = lit 0x90befffa
     | t == lit 61 = lit 0xa4506ceb
     | t == lit 62 = lit 0xbef9a3f7
     | otherwise   = lit 0xc67178f2

-- | this is the initial hash value (H^0) from Section 5.3.3, page 15.
_H0 :: Oct W32
_H0 = ( lit 0x6A09E667
      , lit 0xBB67AE85
      , lit 0x3C6EF372
      , lit 0xA54FF53A
      , lit 0x510E527F
      , lit 0x9B05688C
      , lit 0x1F83D9AB
      , lit 0x5BE0CD19 )

-------------------------------------------
--- The hashing algorithm
-------------------------------------------

-------------------------------------------
--- SHA-256 scheduler algorithm
-------------------------------------------

sched :: M W32
sched = do
   s <- getBlock
   case s of
    (w00,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> do
                                       putBlock (updateSched s)
                                       return w00

updateSched :: Hex W32 -> Hex W32
updateSched ( w00 , w01 , w02 , w03 , w04 , w05 , w06 , w07
            , w08 , w09 , w10 , w11 , w12 , w13 , w14 , w15 ) =
                 ( w01 , w02 , w03 , w04 , w05 , w06 , w07 , w08 
                 , w09 , w10 , w11 , w12 , w13 , w14 , w15 , w16)
  where
    w16 = sigma1 w14 + w09 + sigma0 w01 + w00

-------------------------------------------
--- SHA-256 compression algorithm
-------------------------------------------

compress :: W32 -> W32 -> M ()
compress k w = do
                  wv  <- getWorking
                  putWorking (step256 k w wv) 
    where step256 :: W32 -> W32 -> Oct W32 -> Oct W32
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

-- | Steps 1.-4., Section 6.2.2, pages 22-3.
innerloop :: M ()
innerloop = do
               ctr <- getCtr
               s <- sched
               compress (_K ctr) s
               putCtr (ctr + lit 1)
               if ctr == lit 63
                 then step4
                 else innerloop   

-- | Section 6.2.2, page 23. Last step of inner loop.
-- | 4. Compute the ith intermediate hash value H^i.
step4 :: M ()
step4 = do
  (h1 , h2 , h3 , h4 , h5 , h6 , h7 , h8) <- getHash 
  (a , b , c , d , e , f , g , h)         <- getWorking
  putHash (a + h1 , b + h2 , c + h3 , d + h4 , e + h5 , f + h6 , g + h7 , h + h8)

------
-- Testing code follows
------

mainloop :: [Hex W32] -> M ()
mainloop []             = return ()
mainloop (hw32 : hw32s) = do
                             putCtr (lit 0)  -- set t=0
                             putBlock hw32   -- Section 6.2.2 (1.) 
                             hi_1 <- getHash
                            -- observe
                             putWorking hi_1 -- Section 6.2.2 (2.)
                             -- obs "initial" getWorking
                             innerloop
                             mainloop hw32s

sha256 :: [Hex W32] -> M (Oct W32)
sha256 hws = do
  putHash _H0
  mainloop hws
  getHash

run_sha256 :: [Hex W32] -> Oct W32
run_sha256 input = fst $ fst $ fst $ fst $ runIdentity $ runStateT (runStateT (runStateT (runStateT (sha256 input) undefined) undefined) undefined) undefined

-- (0xba7816bf,0x8f01cfea,0x414140de,0x5dae2223,0xb00361a3,0x96177a9c,0xb410ff61,0xf20015ad)
-- (0xba7816bf,0x8f01cfea,0x414140de,0x5dae2223,0xb00361a3,0x96177a9c,0xb410ff61,0xf20015ad)

--   (0xba7816bf BA7816BF
--   ,0x8f01cfea 8F01CFEA
--   ,0x414140de 414140DE
--   ,0x5dae2223 5DAE2223
--   ,0xb00361a3 B00361A3
--   ,0x96177a9c 96177A9C
--   ,0xb410ff61 B410FF61
--   ,0xf20015ad F20015AD
-- )

--
-- example 1
--
msg1    = "abc"

padded1 :: [Hex W32]
padded1 = [( 0x61626380 , 0x00000000 , 0x00000000 , 0x00000000
           , 0x00000000 , 0x00000000 , 0x00000000 , 0x00000000
           , 0x00000000 , 0x00000000 , 0x00000000 , 0x00000000
           , 0x00000000 , 0x00000000 , 0x00000000 , 0x00000018 )]

-- x :: W32
-- x = lit 0x61626380

-- 01100001 01100010 01100011 10000000

-- hashed1 = ( 0xba7816bf , 0x8f01cfea , 0x414140de , 0x5dae2223 , 0xb00361a3 , 0x96177a9c , 0xb410ff61 , 0xf20015ad)


{-
x :: W32
x = lit 345 

instance Show (Tup8 W32) where
  show (Tup8 a0 a1 a2 a3 a4 a5 a6 a7) = show a0

data Tup8 a = Tup8 !a !a !a !a !a !a !a !a
instance ShowBin a => Pretty (Tup8 a) where
  pp (Tup8 a0 a1 a2 a3 a4 a5 a6 a7) = bshow a0
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


run_sha256 :: [Hex W32] -> Tup8 W32 -- Oct W32
run_sha256 input = inj8 $ fst $ fst $ fst $ fst $ runIdentity $ runStateT (runStateT (runStateT (runStateT (sha256 input) undefined) undefined) undefined) undefined


-}
