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

trReg :: Reg -> RegFile -> W 8
trReg r = case r of       
               RA      -> ra
               RB      -> rb
               RC      -> rc
               A_and_B -> a_and_b
               A_and_C -> a_and_c
               B_and_C -> b_and_c

readReg :: Reg -> Storage RegFile (W 8)
readReg v = do
             rf <- get
             return (trReg v rf)

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

--
-- Asynchronous CSA
--

data In w  = A w | B w | C w | Nop | Go

acsa :: In (W 8) -> Mealy (In (W 8)) RegFile (Ans (W 8, W 8)) ()
acsa Nop   = do i <- signal DC
                acsa i
acsa (A a) = do i <- stagei $ setReg RA a
                acsa i
acsa (B b) = do i <- stagei $ setReg RB b
                acsa i
acsa (C c) = do i <- stagei $ setReg RC c
                acsa i
acsa Go    = do
     stagei $ do a <- readReg RA
                 b <- readReg RB
                 setReg A_and_B (a .&. b)
     stagei $ do a <- readReg RA
                 c <- readReg RC
                 setReg A_and_C (a .&. c)
     stagei $ do b <- readReg RB
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
     i <- signal (Val (tmp1 , tmp2))
     acsa i

start :: ReacT (In (W 8)) (Ans (W 8, W 8)) Identity ()
start = extrude (acsa Nop) regfile0
