{-# LANGUAGE DataKinds #-}
module Exec_Salsa20Encrypt(start) where

-- | This is intended to be executable with GHCi.

import Prelude hiding ((^) , (+))
import ReWire hiding (ReacT, Identity , signal , lift , get , put , StateT)
import ReWire.Bits (lit , (^) , (+) , toInteger)
import Idioms (Oct, Hex, X16(..) , x16 , pi64)
import Expansion(salsa20_k0k1)
import Encrypt( factor64 , mod64 , splice )

import qualified ReWire.Interactive as RI

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Resumption.Reactive


mkk0k1 :: String -> (Hex (W 8) , Hex (W 8))
mkk0k1 secret = ( (X16 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16)
                , (X16 b15 b14 b13 b12 b11 b10  b9  b8  b7  b6  b5   b4 b3  b2  b1  b0))
   where
             [ b31 , b30 , b29 , b28 , b27 , b26 , b25 , b24
              , b23 , b22 , b21 , b20 , b19 , b18 , b17 , b16
              , b15 , b14 , b13 , b12 , b11 , b10 ,  b9 ,  b8
              ,  b7 ,  b6 ,  b5 ,  b4 ,  b3 ,  b2 ,  b1 ,  b0 ] = Prelude.map charToW8 secret

mknonce :: String -> Oct (W 8)
mknonce v = (b7,  b6,  b5,   b4, b3,  b2,  b1,  b0)
   where
             [ b7 ,  b6 ,  b5 ,  b4 ,  b3 ,  b2 ,  b1 ,  b0 ] = Prelude.map charToW8 v

charToW8 :: Char -> W 8
charToW8 = lit . fromIntegral . fromEnum

w8ToChar :: W 8 -> Char
w8ToChar = toEnum . fromInteger . ReWire.Bits.toInteger

secret    = "*Thirty-two byte (256 bits) key*"
plaintext = "Attack at dawn"
nonce     = "12345678"

unroll :: ReacT I O Identity () -> [I] -> [O]
unroll (ReacT (Identity _)) []                    = []
unroll (ReacT (Identity (Right (o, k)))) (i : is) = o : unroll (k i) is

mkinputs msg = K0 k0 : K1 k1 : Nonce v : Reset : m
  where
     m :: [I]
     m        = Prelude.map (Go . charToW8) msg
     (k0, k1) = mkk0k1 secret
     v        = mknonce nonce


-- |
-- | This is for the Salsa20_k0k1 expansion function.
-- |

type RegFile = ( Hex (W 8) , Hex (W 8) , Oct (W 8) , W 64 )

rf0 :: RegFile
rf0 = ( x16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      , x16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      , (lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 )
      , lit 0 )

type S = StateT RegFile Identity

putk0 , putk1 :: Hex (W 8) -> S ()
putk0 k0 = do
              (_ , k1 , v , i) <- get
              put (k0 , k1 , v , i)
putk1 k1 = do
              (k0 , _ , v , i) <- get
              put (k0 , k1 , v , i)

putv :: Oct (W 8) -> S ()
putv v   = do
              (k0 , k1 , _ , i) <- get
              put (k0 , k1 , v , i)

putctr :: W 64 -> S ()
putctr i = do
              (k0 , k1 , v , _) <- get
              put (k0 , k1 , v , i)

data I = K0 (Hex (W 8))     -- K0 input
       | K1 (Hex (W 8))     -- K1 input
       | Nonce (Oct (W 8))  -- input nonce
       | Reset              -- sets counter to 0
       | Go (W 8)           -- receive input byte for encryption
       | Idle               -- No-op.

instance Show I where
  show (K0 k0) = show $ fmap RI.pp k0
  show (K1 k1) = show $ fmap RI.pp k1
  show (Nonce _) = "nonce"
  show Reset   = "reset"
  show (Go w)  = RI.pp w
  show Idle    = "idle"
  
data O = Ack                -- Nothing to output
       | Out (W 8)          -- Encrypted byte

type Dev = ReacT I O S 

encrypt :: W 8 -> S (W 8)
encrypt mi = do
                (k0 , k1 , v , i) <- get
                let mi' = (mi ^ ((salsa20_k0k1 (k0 , k1) (splice v (factor64 i))) `pi64` (mod64 i)))
                putctr (i + lit 1)
                return mi'

action :: I -> Dev I
action (K0 k0)   = do
                      lift (putk0 k0)
                      signal Ack
action (K1 k1)   = do
                      lift (putk1 k1)
                      signal Ack
action (Nonce v) = do
                      lift (putv v)
                      signal Ack
action (Go mi)   = do
                      mi' <- lift $ encrypt mi
                      signal (Out mi')
action Reset     = do
                      lift $ putctr (lit 0)
                      signal Ack
action Idle      = signal Ack                      

loop :: I -> Dev ()
loop inp = action inp >>= loop

start :: ReacT I O Identity ()
start = extrude (signal Ack >>= loop) rf0
