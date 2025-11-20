{-# LANGUAGE OverloadedStrings #-}
-- |
-- AES MixColumns function implementation according to NIST FIPS 197
-- 
-- This module implements the MixColumns transformation used in the AES
-- encryption algorithm. The MixColumns function operates on the State
-- array by treating each column as a four-term polynomial over GF(2^8).
--
-- Reference: NIST FIPS 197 - Advanced Encryption Standard (AES)
-- Section 5.1.3: MixColumns() Transformation

module Aes.AESMixColumns where

import Data.Word (Word8)
import Data.Bits (xor, shiftL, shiftR, (.&.))
import Data.List (zipWith4)

-- | Type alias for a column (4 bytes) in the AES state
type Column = [Word8]

-- | Type alias for the AES state matrix (4x4 bytes)
-- The state is represented as a list of columns, where each column
-- contains 4 bytes representing a column of the 4x4 state matrix
type State = [Column]

-- | Fixed polynomial used in MixColumns: x^4 + 1
-- The irreducible polynomial for GF(2^8) is x^8 + x^4 + x^3 + x + 1
-- This is represented as 0x1B in hexadecimal

-- | Multiplication by 2 in GF(2^8)
-- This is equivalent to left shift by 1, with conditional XOR by 0x1B
mul2 :: Word8 -> Word8
mul2 x = if (x .&. 0x80) /= 0
         then ((x `shiftL` 1) `xor` 0x1B) .&. 0xFF
         else (x `shiftL` 1) .&. 0xFF

-- | Multiplication by 3 in GF(2^8)
-- This is equivalent to x * 2 + x = x * 3
mul3 :: Word8 -> Word8
mul3 x = mul2 x `xor` x

-- | MixColumns transformation matrix for encryption:
-- | 02 03 01 01 |
-- | 01 02 03 01 |
-- | 01 01 02 03 |
-- | 03 01 01 02 |
--
-- Each column is transformed by multiplying it with this matrix
-- over GF(2^8)

-- | Apply MixColumns transformation to a single column
mixColumn :: Column -> Column
mixColumn [s0, s1, s2, s3] = 
    [ mul2 s0 `xor` mul3 s1 `xor` s2 `xor` s3,  -- First row: 02*s0 + 03*s1 + 01*s2 + 01*s3
      s0 `xor` mul2 s1 `xor` mul3 s2 `xor` s3,  -- Second row: 01*s0 + 02*s1 + 03*s2 + 01*s3
      s0 `xor` s1 `xor` mul2 s2 `xor` mul3 s3,  -- Third row: 01*s0 + 01*s1 + 02*s2 + 03*s3
      mul3 s0 `xor` s1 `xor` s2 `xor` mul2 s3   -- Fourth row: 03*s0 + 01*s1 + 01*s2 + 02*s3
    ]
mixColumn _ = error "mixColumn: Column must have exactly 4 elements"

-- | Apply MixColumns transformation to the entire state
-- The state is represented as a list of columns, where each column
-- contains 4 bytes
mixColumns :: State -> State
mixColumns = map mixColumn

-- | Inverse MixColumns transformation matrix for decryption:
-- | 0E 0B 0D 09 |
-- | 09 0E 0B 0D |
-- | 0D 09 0E 0B |
-- | 0B 0D 09 0E |

-- | Multiplication by 9 in GF(2^8)
mul9 :: Word8 -> Word8
mul9 x = mul2 (mul2 (mul2 x)) `xor` x

-- | Multiplication by 11 (0x0B) in GF(2^8)
mul11 :: Word8 -> Word8
mul11 x = mul2 (mul2 (mul2 x)) `xor` mul2 x `xor` x

-- | Multiplication by 13 (0x0D) in GF(2^8)
mul13 :: Word8 -> Word8
mul13 x = mul2 (mul2 (mul2 x)) `xor` mul2 (mul2 x) `xor` x

-- | Multiplication by 14 (0x0E) in GF(2^8)
mul14 :: Word8 -> Word8
mul14 x = mul2 (mul2 (mul2 x)) `xor` mul2 (mul2 x) `xor` mul2 x

-- | Apply inverse MixColumns transformation to a single column
invMixColumn :: Column -> Column
invMixColumn [s0, s1, s2, s3] = 
    [ mul14 s0 `xor` mul11 s1 `xor` mul13 s2 `xor` mul9 s3,   -- First row: 0E*s0 + 0B*s1 + 0D*s2 + 09*s3
      mul9 s0 `xor` mul14 s1 `xor` mul11 s2 `xor` mul13 s3,   -- Second row: 09*s0 + 0E*s1 + 0B*s2 + 0D*s3
      mul13 s0 `xor` mul9 s1 `xor` mul14 s2 `xor` mul11 s3,   -- Third row: 0D*s0 + 09*s1 + 0E*s2 + 0B*s3
      mul11 s0 `xor` mul13 s1 `xor` mul9 s2 `xor` mul14 s3    -- Fourth row: 0B*s0 + 0D*s1 + 09*s2 + 0E*s3
    ]
invMixColumn _ = error "invMixColumn: Column must have exactly 4 elements"

-- | Apply inverse MixColumns transformation to the entire state
invMixColumns :: State -> State
invMixColumns = map invMixColumn

-- | Helper function to convert a list of bytes to a state matrix
-- The input should be 16 bytes representing a 4x4 matrix in column-major order
-- This means the first 4 bytes form the first column, next 4 bytes form the second column, etc.
bytesToState :: [Word8] -> State
bytesToState bytes
    | length bytes /= 16 = error "bytesToState: Input must be exactly 16 bytes"
    | otherwise = [ [bytes !! (i + 4 * c) | i <- [0..3]] | c <- [0..3] ]

-- | Helper function to convert a state matrix back to a list of bytes
-- Returns 16 bytes in column-major order
stateToBytes :: State -> [Word8]
stateToBytes state
    | length state /= 4 = error "stateToBytes: State must have exactly 4 columns"
    | any ((/= 4) . length) state = error "stateToBytes: Each column must have exactly 4 elements"
    | otherwise = concat state

-- | Apply MixColumns to a 16-byte block
mixColumnsBlock :: [Word8] -> [Word8]
mixColumnsBlock = stateToBytes . mixColumns . bytesToState

-- | Apply inverse MixColumns to a 16-byte block
invMixColumnsBlock :: [Word8] -> [Word8]
invMixColumnsBlock = stateToBytes . invMixColumns . bytesToState

-- | Test function to verify the implementation
-- This test uses the example from NIST FIPS 197 Section 5.1.3
testMixColumns :: IO ()
testMixColumns = do
    putStrLn "Testing AES MixColumns function..."
    
    -- Test input from NIST FIPS 197 example
    -- The input is in column-major order: [col0, col1, col2, col3]
    let input = [0xd4, 0xe0, 0xb8, 0x1e,  -- Column 0
                 0xbf, 0xb4, 0x41, 0x27,  -- Column 1
                 0x5d, 0x52, 0x11, 0x98,  -- Column 2
                 0x30, 0xae, 0xf1, 0xe5]  -- Column 3
    
    let expected = [0x04, 0xe0, 0x48, 0x28,  -- Column 0
                    0x66, 0xcb, 0xf8, 0x06,  -- Column 1
                    0x81, 0x19, 0xd3, 0x26,  -- Column 2
                    0xe5, 0x9a, 0x7a, 0x4c]  -- Column 3
    
    let result = mixColumnsBlock input
    
    putStrLn $ "Input:  " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Test " ++ if result == expected then "PASSED" else "FAILED"
    
    -- Test inverse transformation
    let invResult = invMixColumnsBlock result
    putStrLn $ "Inverse result: " ++ show invResult
    putStrLn $ "Inverse test " ++ if invResult == input then "PASSED" else "FAILED"

-- | Example usage
example :: IO ()
example = do
    putStrLn "AES MixColumns Function Example"
    putStrLn "=============================="
    
    let testData = [0x01, 0x02, 0x03, 0x04,
                    0x05, 0x06, 0x07, 0x08,
                    0x09, 0x0a, 0x0b, 0x0c,
                    0x0d, 0x0e, 0x0f, 0x10]
    
    putStrLn $ "Original data: " ++ show testData
    
    let mixed = mixColumnsBlock testData
    putStrLn $ "After MixColumns: " ++ show mixed
    
    let unmixed = invMixColumnsBlock mixed
    putStrLn $ "After InvMixColumns: " ++ show unmixed
    
    putStrLn $ "Round-trip test: " ++ if unmixed == testData then "PASSED" else "FAILED" 
