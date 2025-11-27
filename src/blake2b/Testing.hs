  
-- |
-- | Testing
-- |

-- |
-- | This is the function from: Fig. 10: Reference Algorithms & Conformance Checking
-- |
-- λ> _BLAKE2b_512 "abc"
--
-- 	BA 80 A5 3F 98 1C 4D 0D 6A 27 97 B6 9F 12 F6 E9 
-- 	4C 21 2F 14 68 5A C4 B7 4B 12 BB 6F DB FF A2 D1 
-- 	7D 87 C5 39 2A AB 79 2D C2 52 D5 DE 45 33 CC 95 
-- 	18 D3 8A A8 DB F1 92 5A B9 23 86 ED D4 00 99 23

_BLAKE2b_512 msg = putStrLn $ urp $ testBLAKE2b_512 (encode msg)
  where
   urp :: T8 (W 64) -> String
   urp (T8 (w0 , w1 , w2 , w3 , w4 , w5 , w6 , w7)) = "\n\t" Prelude.++ revgo w0 Prelude.++ " " Prelude.++ revgo w1 Prelude.++
                                                      " \n\t" Prelude.++ revgo w2 Prelude.++ " " Prelude.++ revgo w3 Prelude.++
                                                      " \n\t" Prelude.++ revgo w4 Prelude.++ " " Prelude.++ revgo w5 Prelude.++
                                                      " \n\t" Prelude.++ revgo w6 Prelude.++ " " Prelude.++ revgo w7 Prelude.++"\n"

---
--- The rest of this testing and formatting code is really, really, REALLY hideous.
--- 

testBlake2b :: W 128 -> W 64 -> W 64 -> T16 (W 64) -> T8 (W 64)
testBlake2b k d x (T16 msg) = T8 $ fst $ runIdentity (runStateT (_BLAKE2b k d x msg) undef0)

testBLAKE2b_512 :: T16 (W 64) -> T8 (W 64)
testBLAKE2b_512 msg = testBlake2b  (lit 3) (lit 0) (lit 64) msg

ex1 :: T8 W64
ex1 = testBlake2b (lit 3) (lit 0) (lit 64) testMessage

testMessage :: T16 W64
testMessage = T16 ( lit 0x636261 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
                  , lit 0        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 )

ex2 :: T8 W64
ex2 =  T8 $ fst $ runIdentity (runStateT (_BLAKE2b (lit 3) (lit 999) (lit 64) testMessage) undef0)
   where
     testMessage :: W64x16
     testMessage = ( lit 0x636261 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
                   , lit 0        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 )

-- Want : (0x0d4d1c983fa580ba , 0xe9f6129fb697276a , 0xb7c45a68142f214c , 0xd1a2ffdb6fbb124b , 0x2d79ab2a39c5877d , 0x95cc3345ded552c2 , 0x5a92f1dba88ad318 , 0x239900d4ed8623b9)
-- Now :  (0x0d4d1c983fa580ba , 0xe9f6129fb697276a , 0xb7c45a68142f214c , 0xd1a2ffdb6fbb124b , 0x2d79ab2a39c5877d , 0x95cc3345ded552c2 , 0x5a92f1dba88ad318 , 0x239900d4ed8623b9)

newtype T8 a  = T8 (a , a , a , a , a , a , a , a )
newtype T16 a = T16 ( a , a , a , a , a , a , a , a
                    , a , a , a , a , a , a , a , a )


instance ShowHex a => Show (T8 a) where
  show (T8 (a0 , a1 , a2 , a3 , a4 , a5 , a6 , a7 )) = "(" Prelude.++ xshow a0 Prelude.++ " , " Prelude.++
                                                              xshow a1 Prelude.++ " , " Prelude.++
                                                              xshow a2 Prelude.++ " , " Prelude.++
                                                              xshow a3 Prelude.++ " , " Prelude.++
                                                              xshow a4 Prelude.++ " , " Prelude.++
                                                              xshow a5 Prelude.++ " , " Prelude.++
                                                              xshow a6 Prelude.++ " , " Prelude.++
                                                              xshow a7 Prelude.++ ")"

instance ShowHex a => Show (T16 a) where
  show (T16 (a0 , a1 , a2 , a3 , a4 , a5 , a6 , a7
            , a8 , a9 , a10 , a11 , a12 , a13 , a14 , a15 ))
      = "\n\t" Prelude.++ xshow a0 Prelude.++ " " Prelude.++ xshow a1 Prelude.++ " " Prelude.++ xshow a2 Prelude.++ "\n\t"
            Prelude.++ xshow a3 Prelude.++ " " Prelude.++ xshow a4 Prelude.++ " " Prelude.++ xshow a5 Prelude.++ "\n\t"
            Prelude.++ xshow a6 Prelude.++ " " Prelude.++ xshow a7 Prelude.++ " " Prelude.++  xshow a8 Prelude.++ "\n\t"
            Prelude.++ xshow a9 Prelude.++ " " Prelude.++ xshow a10 Prelude.++ " " Prelude.++ xshow a11 Prelude.++ "\n\t"
            Prelude.++ xshow a12 Prelude.++ " " Prelude.++ xshow a13 Prelude.++ " " Prelude.++ xshow a14 Prelude.++ "\n\t"
            Prelude.++ xshow a15 Prelude.++ "\n"
                                                 
{-
                                = "(" Prelude.++ xshow a0 Prelude.++ " , " Prelude.++
                                                 xshow a1 Prelude.++ " , " Prelude.++
                                                 xshow a2 Prelude.++ " , " Prelude.++
                                                 xshow a3 Prelude.++ " , " Prelude.++
                                                 xshow a4 Prelude.++ " , " Prelude.++
                                                 xshow a5 Prelude.++ " , " Prelude.++
                                                 xshow a6 Prelude.++ " , " Prelude.++
                                                 xshow a7 Prelude.++ " , " Prelude.++
                                                 xshow a8 Prelude.++ " , " Prelude.++
                                                 xshow a9 Prelude.++ " , " Prelude.++
                                                 xshow a10 Prelude.++ " , " Prelude.++
                                                 xshow a11 Prelude.++ " , " Prelude.++
                                                 xshow a12 Prelude.++ " , " Prelude.++
                                                 xshow a13 Prelude.++ " , " Prelude.++
                                                 xshow a14 Prelude.++ " , " Prelude.++
                                                 xshow a15 Prelude.++ ")"
-}

--------------
-- Infrastructure for RFC 7369 Appendix A example
--------------
encode :: String -> T16 (W 64)
encode x = T16 ( w0 , w1 ,  w2 ,  w3 ,  w4 ,  w5 ,  w6 ,  w7 ,
                 w8 , w9 , w10 , w11 , w12 , w13 , w14 , w15 )
  where
    (w0 , k0)   = explode64 . Prelude.reverse $ x
    (w1 , k1)   = explode64 . Prelude.reverse $ k0      
    (w2 , k2)   = explode64 . Prelude.reverse $ k1      
    (w3 , k3)   = explode64 . Prelude.reverse $ k2      
    (w4 , k4)   = explode64 . Prelude.reverse $ k3      
    (w5 , k5)   = explode64 . Prelude.reverse $ k4      
    (w6 , k6)   = explode64 . Prelude.reverse $ k5      
    (w7 , k7)   = explode64 . Prelude.reverse $ k6      
    (w8 , k8)   = explode64 . Prelude.reverse $ k7      
    (w9 , k9)   = explode64 . Prelude.reverse $ k8      
    (w10 , k10) = explode64 . Prelude.reverse $ k9      
    (w11 , k11) = explode64 . Prelude.reverse $ k10      
    (w12 , k12) = explode64 . Prelude.reverse $ k11      
    (w13 , k13) = explode64 . Prelude.reverse $ k12      
    (w14 , k14) = explode64 . Prelude.reverse $ k13      
    (w15 , _)   = explode64 . Prelude.reverse $ k14      

-- encode :: String -> T16 (W 64)
-- encode x = T16 ( w0 , w1 ,  w2 ,  w3 ,  w4 ,  w5 ,  w6 ,  w7 ,
--                  w8 , w9 , w10 , w11 , w12 , w13 , w14 , w15 )
--   where
--     (w0 , k0)   = explode64 x
--     (w1 , k1)   = explode64 k0      
--     (w2 , k2)   = explode64 k1      
--     (w3 , k3)   = explode64 k2      
--     (w4 , k4)   = explode64 k3      
--     (w5 , k5)   = explode64 k4      
--     (w6 , k6)   = explode64 k5      
--     (w7 , k7)   = explode64 k6      
--     (w8 , k8)   = explode64 k7      
--     (w9 , k9)   = explode64 k8      
--     (w10 , k10) = explode64 k9      
--     (w11 , k11) = explode64 k10      
--     (w12 , k12) = explode64 k11      
--     (w13 , k13) = explode64 k12      
--     (w14 , k14) = explode64 k13      
--     (w15 , _)   = explode64 k14      

so :: Int -> Char -> W 64
so n c = lit (Prelude.toInteger ((power 2 (n Prelude.* 8)) Prelude.* ord c))
   where
     power :: Int -> Int -> Int
     power b 0 = 1
     power b n = b Prelude.* power b (n Prelude.- 1)

explode64 :: String -> ( W 64 , String )
explode64 []                  = ( lit 0          , [] )
explode64 (c0 : [])           = (  so 0 c0      , [] )
explode64 (c1 : c0 : [])      = ( so 1 c1 + so 0 c0      , [] )
explode64 (c2 : c1 : c0 : []) = ( so 2 c2 + so 1 c1 + so 0 c0      , [] )
explode64 (c3 : c2 : c1 : c0 : []) = ( so 3 c3 + so 2 c2 + so 1 c1 + so 0 c0      , [] )
explode64 (c4 : c3 : c2 : c1 : c0 : []) = ( so 4 c4 + so 3 c3 + so 2 c2 + so 1 c1 + so 0 c0      , [] )
explode64 (c5 : c4 : c3 : c2 : c1 : c0 : []) = ( so 5 c5 + so 4 c4 + so 3 c3 + so 2 c2 + so 1 c1 + so 0 c0      , [] )
explode64 (c6 : c5 : c4 : c3 : c2 : c1 : c0 : []) = ( so 6 c6 + so 5 c5 + so 4 c4 + so 3 c3 + so 2 c2 + so 1 c1 + so 0 c0      , [] )
explode64 (c7 : c6 : c5 : c4 : c3 : c2 : c1 : c0 : cs) = ( so 7 c7 + so 6 c6 + so 5 c5 + so 4 c4 + so 3 c3 + so 2 c2 + so 1 c1 + so 0 c0      , cs )

w64 :: W 64
w64 = lit 0xD4D1C983FA580BA
--         0D4D1C983FA580BA
--      "0x0d4d1c983fa580ba"

go :: W 64 -> ( W 8 , W 8 , W 8 , W 8 , W 8 , W 8 , W 8 , W 8 )
go w = ( word0 w , word1 w , word2 w , word3 w , word4 w , word5 w , word6 w , word7 w )
  where
    word0 :: W 64 -> W 8
    word0 w = take w

    word1 :: W 64 -> W 8
    word1 w = take (drop8 w)
       where
         drop8 :: W 64 -> W 56
         drop8 = drop

    word2 :: W 64 -> W 8
    word2 w = take (drop16 w)
       where
         drop16 :: W 64 -> W 48
         drop16 = drop

    word3 :: W 64 -> W 8
    word3 w = take (drop24 w)
       where
         drop24 :: W 64 -> W 40
         drop24 = drop

    word4 :: W 64 -> W 8
    word4 w = take (drop32 w)
       where
         drop32 :: W 64 -> W 32
         drop32 = drop

    word5 :: W 64 -> W 8
    word5 w = take (drop40 w)
       where
         drop40 :: W 64 -> W 24
         drop40 = drop

    word6 :: W 64 -> W 8
    word6 w = take (drop48 w)
       where
         drop48 :: W 64 -> W 16
         drop48 = drop

    word7 :: W 64 -> W 8
    word7 w = drop56 w
       where
         drop56 :: W 64 -> W 8
         drop56 = drop



revgo :: W 64 -> String
revgo w = xshow (word7 w) Prelude.++ " " Prelude.++ xshow (word6 w) Prelude.++ " " Prelude.++ xshow (word5 w) Prelude.++ " " Prelude.++ xshow (word4 w) Prelude.++ " " Prelude.++ xshow (word3 w) Prelude.++ " " Prelude.++ xshow (word2 w) Prelude.++ " " Prelude.++ xshow (word1 w) Prelude.++ " " Prelude.++ xshow (word0 w)
  where
    word0 :: W 64 -> W 8
    word0 w = take w

    word1 :: W 64 -> W 8
    word1 w = take (drop8 w)
       where
         drop8 :: W 64 -> W 56
         drop8 = drop

    word2 :: W 64 -> W 8
    word2 w = take (drop16 w)
       where
         drop16 :: W 64 -> W 48
         drop16 = drop

    word3 :: W 64 -> W 8
    word3 w = take (drop24 w)
       where
         drop24 :: W 64 -> W 40
         drop24 = drop

    word4 :: W 64 -> W 8
    word4 w = take (drop32 w)
       where
         drop32 :: W 64 -> W 32
         drop32 = drop

    word5 :: W 64 -> W 8
    word5 w = take (drop40 w)
       where
         drop40 :: W 64 -> W 24
         drop40 = drop

    word6 :: W 64 -> W 8
    word6 w = take (drop48 w)
       where
         drop48 :: W 64 -> W 16
         drop48 = drop

    word7 :: W 64 -> W 8
    word7 w = drop56 w
       where
         drop56 :: W 64 -> W 8
         drop56 = drop


undef0 :: RegFile
undef0   = RegFile {  v0 = lit $ error "undefined v0"
                   ,  v1 = lit $ error "undefined v1" 
                   ,  v2 = lit $ error "undefined v2" 
                   ,  v3 = lit $ error "undefined v3" 
                   ,  v4 = lit $ error "undefined v4" 
                   ,  v5 = lit $ error "undefined v5" 
                   ,  v6 = lit $ error "undefined v6"
                   ,  v7 = lit $ error "undefined v7"
                   ,  v8 = lit $ error "undefined v8"
                   ,  v9 = lit $ error "undefined v9"
                   , v10 = lit $ error "undefined v10" 
                   , v11 = lit $ error "undefined v11"
                   , v12 = lit $ error "undefined v12"
                   , v13 = lit $ error "undefined v13"
                   , v14 = lit $ error "undefined v14"
                   , v15 = lit $ error "undefined v15"
                   ,  m0 = lit $ error "undefined m0"
                   ,  m1 = lit $ error "undefined m1" 
                   ,  m2 = lit $ error "undefined m2" 
                   ,  m3 = lit $ error "undefined m3" 
                   ,  m4 = lit $ error "undefined m4" 
                   ,  m5 = lit $ error "undefined m5" 
                   ,  m6 = lit $ error "undefined m6"
                   ,  m7 = lit $ error "undefined m7"
                   ,  m8 = lit $ error "undefined m8"
                   ,  m9 = lit $ error "undefined m9"
                   , m10 = lit $ error "undefined m10" 
                   , m11 = lit $ error "undefined m11"
                   , m12 = lit $ error "undefined m12"
                   , m13 = lit $ error "undefined m13"
                   , m14 = lit $ error "undefined m14"
                   , m15 = lit $ error "undefined m15"
                   ,  h0 = lit $ error "undefined h0"
                   ,  h1 = lit $ error "undefined h1" 
                   ,  h2 = lit $ error "undefined h2" 
                   ,  h3 = lit $ error "undefined h3" 
                   ,  h4 = lit $ error "undefined h4" 
                   ,  h5 = lit $ error "undefined h5" 
                   ,  h6 = lit $ error "undefined h6"
                   ,  h7 = lit $ error "undefined h7" }
