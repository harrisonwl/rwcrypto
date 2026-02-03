{-# LANGUAGE DataKinds #-}
module Test256 where

import Prelude as P hiding ((-) , (*) , (<) , (^) , (/) , tail , round , reads)
import ReWire hiding (put , get , signal , lift)
import ReWire.Bits as RB hiding ((<) , (*))
import ReWire.Finite
import ReWire.Vectors hiding (update)

import Aes.ExtensionalSemantics
import Aes.KeyExp.Reference256(Key, KeySchedule, ks0, keyexpand) 
import Aes.KeyExp.Hardware256(I(..), hdl)

import Aes.KeyExp.KATs 
import ReWire.Interactive(Pretty , pretty , pp)

instance Pretty a => Show (I a) where
  show (KB a)   = "Key " P.++ pp a
  show Round    = "Round"
  show Cont     = "Cont"
  show (Read i) = "Read " P.++ show i

instance Pretty Int where
  pp i = show i

instance Pretty (I a) where
  pp (KB _)   = "Key ?"
  pp Round    = "Round"
  pp Cont     = "Cont"
  pp (Read i) = "Read " P.++ show i


------
-- Reference Semantics test functions
------

go :: Integer -> IO ()
go i = pretty $ ref i

ref :: Integer -> W 32
ref i = fst $ runST (keyexpand i k0) (undefined, undefined)

------
-- Hardware Semantics test functions
------

-- | This is the main semantic function.
exec :: Re (I i) s o () -> (I i, s, o) -> [I i] -> Stream (I i, s, o)
exec x w is  = re_inf x w (istr is)

  where

    istr :: [I a] -> Stream (I a)
    istr is = appendStr is (rep Cont)

    appendStr :: [a] -> Stream a -> Stream a
    appendStr [] str       = str
    appendStr (a : as) str = a :< appendStr as str

hw :: Integer -> Maybe (W 32)
hw i = P.head $ P.reverse $ transcript i

run :: Integer -> IO ()
run i = pretty $ transcript i

transcript :: Integer -> [Maybe (W 32)]
transcript i = P.map outs $ takeStr (P.length ins0 P.+ 1) (exec (hdl Cont) w0 ins0)

  where

     outs :: (a , b , c) -> c
     outs = (\ (_ , _ , x) -> x)

     w0 :: (I (W 32) , (KeySchedule, W 6) , Maybe (W 32))
     w0 = (Cont , (ks0 , lit 7) , Nothing)
     
     ins0 :: [I (W 32)]
     ins0 = mkcall k0 i -- Uses example from above (k0)

key_expansion :: Vec 8 (W 32) -> [(Int , Maybe (W 32))]
key_expansion k = P.zip [0..59] (P.drop 23 $ P.map outs $ takeStr (P.length ins0 P.+ 1) (exec (hdl Cont) w0 ins0))

  where

     outs :: (a , b , c) -> c
     outs = (\ (_ , _ , x) -> x)

     w0 :: (I (W 32) , (KeySchedule, W 6) , Maybe (W 32))
     w0 = (Cont , (ks0 , lit 7) , Nothing)
     
     ins0 :: [I (W 32)]
     ins0 = mkcalls k -- Uses example from above (k0)

runkats = ans P.== (P.map key_expansion (P.map tuple2vec keys))
  where
    keys = P.map P.fst kats
    ans  = P.map justify (P.map P.snd kats)

justify =  P.map (\ (i,w32) -> (i,Just w32))
    
tuple2vec :: (W 32 , W 32 , W 32 , W 32 , W 32 , W 32 , W 32 , W 32) -> Vec 8 (W 32)
tuple2vec (w0 , w1 , w2 , w3 , w4 , w5 , w6 , w7)
                  = fromList [w0 , w1 , w2 , w3 , w4 , w5 , w6 , w7]

-- | this encodes a calling protocol for the hdl hardware semantics
mkcalls :: Vec 8 (W 32) -> [I (W 32)]
mkcalls k   = [ KB $ k `index` finite 0
              , KB $ k `index` finite 1
              , KB $ k `index` finite 2
              , KB $ k `index` finite 3
              , KB $ k `index` finite 4
              , KB $ k `index` finite 5
              , KB $ k `index` finite 6
              , KB $ k `index` finite 7 ]
                  P.++ rounds P.++ reads
       where
         rounds = P.take 13 (repeat Round)

mkcall :: Vec 8 (W 32) -> Integer -> [I (W 32)]
mkcall k ix = [ KB $ k `index` finite 0
              , KB $ k `index` finite 1
              , KB $ k `index` finite 2
              , KB $ k `index` finite 3
              , KB $ k `index` finite 4
              , KB $ k `index` finite 5
              , KB $ k `index` finite 6
              , KB $ k `index` finite 7 ]
                  P.++ rounds P.++ reads
                      -- [ Read ix , Cont]
       where
         rounds = P.take 13 (repeat Round)

key0 :: Key
key0 = fromList [ lit 0 , lit 0 , lit 0 , lit 0
                , lit 0 , lit 0 , lit 0 , lit 0 ]


reads = [ Read ix | ix <- [0..59] ] P.++ [ Cont ]

-- From both nist.fips.197-upd1 (Appendix A3) and AES_Core256
keyex :: W 256
keyex = lit 0x603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4

-- k0 is keyex ^^^ split into W 32.
k0 :: Vec 8 (W 32)
k0 = fromList
        [ lit 0x603deb10 , lit 0x15ca71be
        , lit 0x2b73aef0 , lit 0x857d7781
        , lit 0x1f352c07 , lit 0x3b6108d7
        , lit 0x2d9810a3 , lit 0x0914dff4 ]


------
-- Below is the junkyard
------

{-
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
-}

-- mkcallKB :: a -> Integer -> [I a]
-- mkcallKB k ix = [KB k] P.++ rounds P.++ [Read ix , Cont]
--    where
--      rounds = P.take 13 (repeat Round)
{-

before :: List String
before = ["Just 0x603DEB10","Just 0x15CA71BE","Just 0x2B73AEF0","Just 0x857D7781","Just 0x1F352C07","Just 0x3B6108D7","Just 0x2D9810A3","Just 0x0914DFF4","Just 0x9BA35411","Just 0x8E6925AF","Just 0xA51A8B5F","Just 0x2067FCDE","Just 0xA8B09C1A","Just 0x93D194CD","Just 0xBE49846E","Just 0xB75D5B9A","Just 0xD59AECB8","Just 0x5BF3C917","Just 0xFEE94248","Just 0xDE8EBE96","Just 0xB5A9328A","Just 0x2678A647","Just 0x98312229","Just 0x2F6C79B3","Just 0x812C81AD","Just 0xDADF48BA","Just 0x24360AF2","Just 0xFAB8B464","Just 0x98C5BFC9","Just 0xBEBD198E","Just 0x268C3BA7","Just 0x09E04214","Just 0x68007BAC","Just 0xB2DF3316","Just 0x96E939E4","Just 0x6C518D80","Just 0xC814E204","Just 0x76A9FB8A","Just 0x5025C02D","Just 0x59C58239","Just 0xDE136967","Just 0x6CCC5A71","Just 0xFA256395","Just 0x9674EE15","Just 0x5886CA5D","Just 0x2E2F31D7","Just 0x7E0AF1FA","Just 0x27CF73C3","Just 0x749C47AB","Just 0x18501DDA","Just 0xE2757E4F","Just 0x7401905A","Just 0xCAFAAAE3","Just 0xE4D59B34","Just 0x9ADF6ACE","Just 0xBD10190D","Just 0xFE4890D1","Just 0xE6188D0B","Just 0x046DF344","Just 0x706C631E"]

cg :: P.List String
cg = ["0x603DEB10","0x15CA71BE","0x2B73AEF0","0x857D7781","0x1F352C07","0x3B6108D7","0x2D9810A3","0x0914DFF4","0x9BA35411","0x8E6925AF","0xA51A8B5F","0x2067FCDE","0xA8B09C1A","0x93D194CD","0xBE49846E","0xB75D5B9A","0xD59AECB8","0x5BF3C917","0xFEE94248","0xDE8EBE96","0xB5A9328A","0x2678A647","0x98312229","0x2F6C79B3","0x812C81AD","0xDADF48BA","0x24360AF2","0xFAB8B464","0x98C5BFC9","0xBEBD198E","0x268C3BA7","0x09E04214","0x68007BAC","0xB2DF3316","0x96E939E4","0x6C518D80","0xC814E204","0x76A9FB8A","0x5025C02D","0x59C58239","0xDE136967","0x6CCC5A71","0xFA256395","0x9674EE15","0x5886CA5D","0x2E2F31D7","0x7E0AF1FA","0x27CF73C3","0x749C47AB","0x18501DDA","0xE2757E4F","0x7401905A","0xCAFAAAE3","0xE4D59B34","0x9ADF6ACE","0xBD10190D","0xFE4890D1","0xE6188D0B","0x046DF344","0x706C631E","0x9BAA5191","0x7F7FCAA5","0xE5A0A06B","0x58B0B966"]
-}
