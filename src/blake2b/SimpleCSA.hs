{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^))
import ReWire
import ReWire.Bits

type Storage s       = StateT s Identity 
type Mealy i s o a = ReacT i o (Storage s) a

f :: W 8 -> W 8 -> W 8 -> (W 8, W 8)
f a b c = ( (ab .|. ac .|. bc ) <<. lit 1 , (a ^ b) ^ c )
  where
    ab = a .&. b
    ac = a .&. c
    bc = b .&. c
    
-- Constants for a running example.
_40 , _25 , _20 , _41 , _0 :: W 8
_40 = lit 40
_25 = lit 25
_20 = lit 20
_41 = lit 41
_0  = lit 0

-- |
-- | Example 1. 
-- |

data Ans a = DC | Val a

csa :: Mealy (W 8) (W 8 , W 8 , W 8) (Ans (W 8, W 8)) ()
csa = do
          a <- signal DC
          b <- signal DC
          c <- signal DC
          signal (Val (f a b c))
          csa
  
start :: ReacT (W 8) (Ans (W 8 , W 8)) Identity ()
start = extrude csa (lit 0 , lit 0 , lit 0)
--      ^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^^
--      provides the initial store 
