{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^),(+))
import ReWire.Bits
import ReWire

--
-- Monads
-- 

type Storage s   = StateT s Identity 
type Mealy i s o = ReacT i o (Storage s)

--
-- Register File model
-- 

data RegFile = RegFile { ra , rb , rc , a_and_b , a_and_c , b_and_c :: W 8 }
data Reg     = RA | RB | RC | A_and_B | A_and_C | B_and_C

readReg :: Reg -> Storage RegFile (W 8)
readReg RA      = do RegFile {ra = w} <- get
                     return w
readReg RB      = do RegFile {rb = w} <- get
                     return w
readReg RC      = do RegFile {rc = w} <- get
                     return w
readReg A_and_B = do RegFile {a_and_b = w} <- get
                     return w
readReg A_and_C = do RegFile {a_and_c = w} <- get
                     return w
readReg B_and_C = do RegFile {b_and_c = w} <- get
                     return w

setReg :: Reg -> W 8 -> Storage RegFile ()
setReg r w = do
               rf <- get
               case r of
                 ----
                 RA      -> put $ rf { ra = w }
                 RB      -> put $ rf { rb = w }
                 RC      -> put $ rf { rc = w } 
                 A_and_B -> put $ rf { a_and_b = w } 
                 A_and_C -> put $ rf { a_and_c = w }
                 B_and_C -> put $ rf { b_and_c = w } 
               
regfile0 :: RegFile
regfile0 = RegFile { ra      = lit 0
                   , rb      = lit 0
                   , rc      = lit 0
                   , a_and_b = lit 0
                   , a_and_c = lit 0
                   , b_and_c = lit 0
                   }

regfile1 :: RegFile
regfile1 = RegFile { ra      = lit 1
                   , rb      = lit 2
                   , rc      = lit 3
                   , a_and_b = lit 4
                   , a_and_c = lit 5
                   , b_and_c = lit 6
                   }

--
-- Three staging operators
-- 

data Ans a = DC | Val a

{-# INLINE stage #-}
stage :: Storage s a -> Mealy i s (Ans o) a
stage x         = do
                   v <- lift x
                   signal DC
                   return v

{-# INLINE stagei #-}
stagei :: Storage s a -> Mealy i s (Ans o) i
stagei x        = do
                   lift x
                   i <- signal DC
                   return i

{-# INLINE stage_ #-}
stage_ :: Storage s a -> Mealy i s (Ans o) ()
stage_ x        = do
                   lift x
                   signal DC
                   return ()

pcsa :: Mealy (W 8) RegFile (Ans (W 8, W 8)) ()
pcsa = do
  a <- signal DC
  b <- stagei $ setReg RA a
  c <- stagei $ setReg RB b
  stage_ $ setReg RC c
  stage_ $ do a <- readReg RA
              b <- readReg RB
              setReg A_and_B (a .&. b)
  stage_ $ do a <- readReg RA
              c <- readReg RC
              setReg A_and_C (a .&. c)
  stage_ $ do b <- readReg RB
              c <- readReg RC
              setReg B_and_C (b .&. c)
  tmp1 <- stage $ do anb <- readReg A_and_B
                     anc <- readReg A_and_C
                     bnc <- readReg B_and_C
                     return (anb .|. anb .|. bnc <<. lit 1)
  tmp2 <- stage $ do a <- readReg RA
                     b <- readReg RB
                     c <- readReg RC
                     return ((a ^ b) ^ c)
  signal (Val (tmp1 , tmp2))
  pcsa

start :: ReacT (W 8) (Ans (W 8, W 8)) Identity ()
start = extrude pcsa regfile0
