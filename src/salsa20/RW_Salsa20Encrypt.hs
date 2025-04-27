{-# LANGUAGE DataKinds #-}
-- module RW_Salsa20Encrypt (start) where

-- | This is intended to be compilable with rwc.

import Prelude hiding ((^) , (+))
import ReWire 
import ReWire.Bits ( lit , (^) , (+) )
import Basic (Oct, Hex, pi64 , X16(..) )
import Encrypt( encrypt )

-- |
-- | This is for the Salsa20_k0k1 expansion function.
-- |

type RegFile = ( Hex (W 8) , Hex (W 8) , Oct (W 8) , W 64 )

rf0 :: RegFile
rf0 = ( X16 z z z z z z z z z z z z z z z z
      , X16 z z z z z z z z z z z z z z z z
      , (z , z , z , z , z , z , z , z )
      , lit 0 )
  where
    z :: W 8
    z = lit 0
    
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
  
data O = Ack                -- Nothing to output
       | Out (W 8)          -- Encrypted byte

type Dev = ReacT I O S 

encryptM :: W 8 -> S (W 8)
encryptM mi = do
                 (k0 , k1 , v , i) <- get
                 let mi' = encrypt k0 k1 v i mi
                       --(mi ^ ((salsa20_k0k1 (k0 , k1) (splice v (factor64 i))) `pi64` (mod64 i)))
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
                      mi' <- lift $ encryptM mi
                      signal (Out mi')
action Reset     = do
                      lift $ putctr (lit 0)
                      signal Ack
action Idle      = signal Ack                      

loop :: I -> Dev ()
loop inp = action inp >>= loop

start :: ReacT I O Identity ()
start = extrude (signal Ack >>= loop) rf0
