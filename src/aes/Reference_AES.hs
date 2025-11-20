{-# LANGUAGE DataKinds #-}
module Aes.Reference_AES where

import Prelude hiding ((*) , (+) , (==))
import ReWire
import ReWire.Bits
import ReWire.Vectors
import ReWire.Interactive

-- | Converting the code from https://unconceived.net/blog/2015/01/29/aes-reference-haskell.html 

type Word8  = W 8
type Word32 = W 32

-- |
-- | Note to Self. [Word32] won't do for KeySchedule. Need to figure out
-- | what the actual, fixed size is and if it must be generated beforehand.
-- |
newtype KeySchedule = KeySchedule [Word32]
    deriving (Eq, Show)

-- | Assume that the number of blocks (_Nb) and rounds (_Nr) are set at 4 and 10, resp.
-- _Nb     Number of columns (32-bit words) comprising the State. For this
--         standard, Nb = 4. (Also see Sec. 6.3.)
-- _Nk     Number of 32-bit words comprising the Cipher Key. For this
--         standard, Nk = 4, 6, or 8. (Also see Sec. 6.3.)
_Nb , _Nk, _Nr :: Int
_Nb = 4
_Nk = 4
_Nr = 10

-- defaultKeySchedule :: KeySchedule
-- defaultKeySchedule = KeySchedule $ repeat 0x00000000

data State = State {
                w0 :: Word32,
                w1 :: Word32,
                w2 :: Word32,
                w3 :: Word32,
                schedule :: KeySchedule }
    deriving (Eq, Show)

{-
rotWord :: Word32 -> Word32
rotWord w = 
        rotate $ octets w
    where
        rotate [w0, w1, w2, w3] = fromOctets [w1, w2, w3, w0]

fromOctets :: [Word8] -> Word32
fromOctets = foldl' shiftOp 0
    where shiftOp l b = (l `shiftL` 8) .|. (fromIntegral b)

octets :: Word32 -> [Word8]
octets i =
    [ fromIntegral $ i `shiftR` 24
    , fromIntegral $ i `shiftR` 16
    , fromIntegral $ i `shiftR` 8
    , fromIntegral i
    ]
-}


