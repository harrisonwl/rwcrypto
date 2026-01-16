{-# LANGUAGE DataKinds #-}
module Aes.Exec_KeyExpansion where

import Prelude as P hiding ((-) , (*) , (==) , (<) , (^) , (/) , head , tail , round)
import ReWire hiding (put , get , signal , lift)
import ReWire.Bits as RB hiding ((<) , (*))
import ReWire.Vectors hiding (update , (!=))
import ReWire.Finite
import ReWire.FiniteComp as FC
import Aes.ExtensionalSemantics

import Aes.Basic(update , (!=) , (@@@) , toW32 , toByte4)
import Aes.SubBytes(subword)
import Aes.RotWord(rotword)

import ReWire.Interactive(Pretty , pretty , pp)
import qualified Data.Vector.Sized                 as V
--import Debug.Trace

-- | This reflects the AES-256 type of KeyExpansion (^^^)
type KeySchedule = Vec 60 (W 32)
type Key         = Vec 32 (W 8) 

data I a = KB a  
         | Round
         | Cont
         | Read Integer -- just for instrumentation
        -- | M128 | M192 | M256 -- modes

instance Show (I a) where
  show (KB _)   = "Key ?"
  show Round    = "Round"
  show Cont     = "Cont"
  show (Read i) = "Read " P.++ show i

instance Pretty (I a) where
  pp (KB _)   = "Key ?"
  pp Round    = "Round"
  pp Cont     = "Cont"
  pp (Read i) = "Read " P.++ show i


------
-- Hardware Semantics
------

hdl :: I Key -> Re (I Key) (KeySchedule, W 6) (Maybe (W 32)) ()
hdl (KB k)    = do
                  lift $ put (initKS k ks0 , lit 8)
                  i <- signal Nothing
                  hdl i
hdl Round     = do
                  lift round
                  i <- signal Nothing
                  hdl i
hdl Cont      = do
                  i <- signal Nothing
                  hdl i
hdl (Read ix) = do
                  (ks , _) <- lift get
                  i <- signal (Just (ks `index` (finite ix)))
                  hdl i
                  
exec :: Re (I i) s o () -> (I i, s, o) -> [I i] -> Stream (I i, s, o)
exec x w is  = re_inf x w (istr is)

  where

    istr :: [I a] -> Stream (I a)
    istr is = appendStr is (rep Cont)

    appendStr :: [a] -> Stream a -> Stream a
    appendStr [] str       = str
    appendStr (a : as) str = a :< appendStr as str

-- |
-- | Main function for simulation.
-- |

-- run :: Integer -> IO ()
run i = pretty $ P.map (\ (_ , _ , x) -> x) $ takeStr (P.length ins0 P.+ 1) (exec (hdl Cont) w0 ins0)

  where

     w0 :: (I Key , (KeySchedule , W 6) , Maybe (W 32))
     w0 = (Cont , (ks0 , lit 7) , Nothing)

     ins0 :: [I Key]
     ins0 = mkcall k0 i -- Uses example from above (k0)

mkcall :: a -> Integer -> [I a]
mkcall k ix = [KB k] P.++ rounds P.++ [Read ix , Cont]
   where
     rounds = P.take 13 (repeat Round)

------
-- Standard Semantics
------

go :: Integer -> IO ()
go i = pretty $ fst $ runST (keyexpand i k0) (undefined, undefined)

-- |
-- | Standard semantics for Key Expansion
-- |
keyexpand :: Integer -> Key -> ST (KeySchedule, W 6) (W 32)
keyexpand i k = do
                  put (initKS k ks0 , lit 8)
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  rdKS (finite i)

rdKS :: Finite n -> ST (Vec n w, s) w
rdKS i = do
            (ks , _) <- get
            return (ks `index` i)

-- From both nist.fips.197-upd1 (Appendix A3) and AES_Core256
keyex :: W 256
keyex = lit 0x603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4

-- k0 is keyex ^^^ split into bytes
k0 :: Key
k0 = fromList
        [ lit 0x60 , lit 0x3d , lit 0xeb , lit 0x10 , lit 0x15 , lit 0xca , lit 0x71 , lit 0xbe
        , lit 0x2b , lit 0x73 , lit 0xae , lit 0xf0 , lit 0x85 , lit 0x7d , lit 0x77 , lit 0x81
        , lit 0x1f , lit 0x35 , lit 0x2c , lit 0x07 , lit 0x3b , lit 0x61 , lit 0x08 , lit 0xd7
        , lit 0x2d , lit 0x98 , lit 0x10 , lit 0xa3 , lit 0x09 , lit 0x14 , lit 0xdf , lit 0xf4 ]

ks0 :: KeySchedule
ks0 = fromList
        [ lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 ]

initKS :: Key -> KeySchedule -> KeySchedule
initKS k = expand (lit 0) k .
             expand (lit 1) k .
             expand (lit 2) k .
             expand (lit 3) k .
             expand (lit 4) k . 
             expand (lit 5) k . 
             expand (lit 6) k . 
             expand (lit 7) k 
   where
     expand :: W 6 -> Vec 32 (W 8) -> Vec 60 (W 32) -> Vec 60 (W 32)
     expand i k w = w != toFinite i $ merge k (toFinite i)

     merge :: Vec 32 (W 8) -> Finite 32 -> W 32
     merge key i = (key `index` i4) ReWire.Vectors.++ (key `index` i41) ReWire.Vectors.++ (key `index` i42) ReWire.Vectors.++ (key `index` i43)
       where
         i4 , i41 , i42 , i43 :: Finite 32
         i4  = finite 4 * i
         i41 = finite 4 * i FC.+ finite 1
         i42 = finite 4 * i FC.+ finite 2
         i43 = finite 4 * i FC.+ finite 3

assign :: W 6 -> W 32 -> KeySchedule -> KeySchedule
assign c w32 ks = ks != (toFinite c) $ w32

expand :: ST (KeySchedule, W 6) ()
expand = do
            (ks , c ) <- get
            let w32 = body c ks
            put (assign c w32 ks , c RB.+ lit 1)

round :: ST (KeySchedule, W 6) ()
round = expand >> expand >> expand >> expand

body :: W 6 -> KeySchedule -> W 32
body i w = let
               wi1 , wi8 , temp :: W 32 
               imod8 :: W 6
               wi1   = w @@@ (i RB.- lit 1) -- == temp in Fig. 11.
               wi8   = w @@@ (i RB.- lit 8)
               imod8 = i % (lit 8)
               temp  = if (imod8 RB.== lit 0)
                        then
                           subword(rotword wi1) ^ (toW32 (rcon (i / (lit 8))))
                        else if (imod8 RB.== lit 4)
                               then
                                 subword wi1
                               else
                                 wi1
           in
               wi8 ^ temp

w0 :: KeySchedule
w0 = initKS k0 ks0

rcon :: W 6 -> Vec 4 (W 8)
rcon i = table5 @@@ (i RB.- lit 1)
  where
    -- | N.b., in Table 5 on page 17 of nist.fips.197-upd, rcon is indexed from 1 to 10.
    -- | Thanks NIST!
    table5 :: Vec 10 (Vec 4 (W 8))
    table5 = fromList [
                fromList [ lit 0x01, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x02, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x04, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x08, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x10, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x20, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x40, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x80, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x1b, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x36, lit 0x00, lit 0x00, lit 0x00]
                ]

