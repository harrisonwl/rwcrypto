module Iterator where

-- import ReWire
-- import ReWire.Bits 
-- import ReWire.Verilog

import Prelude hiding ((<>), cycle)

import BinaryArithmetic
import W8
import W32
import W64
import Idioms

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Resumption.Reactive

refactor :: W64 -> Oct W8
refactor (W64 b63 b62 b61 b60 b59 b58 b57 b56
              b55 b54 b53 b52 b51 b50 b49 b48
              b47 b46 b45 b44 b43 b42 b41 b40
              b39 b38 b37 b36 b35 b34 b33 b32
              b31 b30 b29 b28 b27 b26 b25 b24
              b23 b22 b21 b20 b19 b18 b17 b16
              b15 b14 b13 b12 b11 b10 b09 b08
              b07 b06 b05 b04 b03 b02 b01 b00)
               = ( W8 b07 b06 b05 b04 b03 b02 b01 b00
                 , W8 b15 b14 b13 b12 b11 b10 b09 b08
                 , W8 b23 b22 b21 b20 b19 b18 b17 b16
                 , W8 b31 b30 b29 b28 b27 b26 b25 b24
                 , W8 b39 b38 b37 b36 b35 b34 b33 b32
                 , W8 b47 b46 b45 b44 b43 b42 b41 b40
                 , W8 b55 b54 b53 b52 b51 b50 b49 b48
                 , W8 b63 b62 b61 b60 b59 b58 b57 b56)

data Z64 = Z00 | Z01 | Z02 | Z03 | Z04 | Z05 | Z06 | Z07 | Z08 | Z09 | Z0a | Z0b | Z0c | Z0d | Z0e | Z0f
         | Z10 | Z11 | Z12 | Z13 | Z14 | Z15 | Z16 | Z17 | Z18 | Z19 | Z1a | Z1b | Z1c | Z1d | Z1e | Z1f
         | Z20 | Z21 | Z22 | Z23 | Z24 | Z25 | Z26 | Z27 | Z28 | Z29 | Z2a | Z2b | Z2c | Z2d | Z2e | Z2f
         | Z30 | Z31 | Z32 | Z33 | Z34 | Z35 | Z36 | Z37 | Z38 | Z39 | Z3a | Z3b | Z3c | Z3d | Z3e | Z3f
           deriving (Eq, Show)

v :: Z64 -> Integer
v Z00 = 0
v Z01 = 1
v Z02 = 2 
v Z03 = 3
v Z04 = 4
v Z05 = 5
v Z06 = 6
v Z07 = 7
v Z08 = 8
v Z09 = 9
v Z0a = 10
v Z0b = 11
v Z0c = 12
v Z0d = 13
v Z0e = 14
v Z0f = 15
v Z10 = 16
v Z11 = 17
v Z12 = 18
v Z13 = 19
v Z14 = 20
v Z15 = 21
v Z16 = 22
v Z17 = 23
v Z18 = 24
v Z19 = 25
v Z1a = 26
v Z1b = 27
v Z1c = 28
v Z1d = 29
v Z1e = 30
v Z1f = 31
v Z20 = 32
v Z21 = 33
v Z22 = 34
v Z23 = 35
v Z24 = 36
v Z25 = 37
v Z26 = 38
v Z27 = 39
v Z28 = 40
v Z29 = 41
v Z2a = 42
v Z2b = 43
v Z2c = 44
v Z2d = 45
v Z2e = 46
v Z2f = 47
v Z30 = 48
v Z31 = 49
v Z32 = 50
v Z33 = 51
v Z34 = 52
v Z35 = 53
v Z36 = 54
v Z37 = 55
v Z38 = 56
v Z39 = 57
v Z3a = 58
v Z3b = 59
v Z3c = 60
v Z3d = 61
v Z3e = 62
v Z3f = 63

         

unroll :: ReacT i o (StateT s Identity) () -> (i, s, o) -> [i] -> [(i, s, o)]
unroll (ReacT x) (i , s , o) (i' : is) = case runIdentity (runStateT x s) of
                                              (Right (o' , k) , s') -> (i , s , o) : unroll (k i') (i', s', o') is

-- working = Work : working
    
tv = X64 0xa0 0xa1 0xa2 0xa3 0xa5 0xa5 0xa6 0xa7 0xa8 0xa9 0xaa 0xab 0xac 0xad 0xae 0xaf
         0xb0 0xb1 0xb2 0xb3 0xb5 0xb5 0xb6 0xb7 0xb8 0xb9 0xba 0xbb 0xbc 0xbd 0xbe 0xbf
         0xc0 0xc1 0xc2 0xc3 0xc5 0xc5 0xc6 0xc7 0xc8 0xc9 0xca 0xcb 0xcc 0xcd 0xce 0xcf
         0xd0 0xd1 0xd2 0xd3 0xd5 0xd5 0xd6 0xd7 0xd8 0xd9 0xda 0xdb 0xdc 0xdd 0xde 0xdf

-- v = W8x64 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
--           31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
--           61 62 63


-- i_mod_64 :: W8x64 -> Integer -> W8
ref :: X64 W8 -> Z64 -> W8
ref (X64 x00 x01 x02 x03 x04 x05 x06 x07 x08 x09 x0a x0b x0c x0d x0e x0f
         x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x1a x1b x1c x1d x1e x1f
         x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x2a x2b x2c x2d x2e x2f
         x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x3a x3b x3c x3d x3e x3f)
    j
      =   case j of
               Z00 -> x00
               Z01 -> x01
               Z02 -> x02
               Z03 -> x03
               Z04 -> x04
               Z05 -> x05
               Z06 -> x06
               Z07 -> x07
               Z08 -> x08
               Z09 -> x09
               Z0a -> x0a
               Z0b -> x0b
               Z0c -> x0c
               Z0d -> x0d
               Z0e -> x0e
               Z0f -> x0f
               Z10 -> x10
               Z11 -> x11
               Z12 -> x12
               Z13 -> x13
               Z14 -> x14
               Z15 -> x15
               Z16 -> x16
               Z17 -> x17
               Z18 -> x18
               Z19 -> x19
               Z1a -> x1a
               Z1b -> x1b
               Z1c -> x1c
               Z1d -> x1d
               Z1e -> x1e
               Z1f -> x1f
               Z20 -> x20
               Z21 -> x21
               Z22 -> x22
               Z23 -> x23
               Z24 -> x24
               Z25 -> x25
               Z26 -> x26
               Z27 -> x27
               Z28 -> x28
               Z29 -> x29
               Z2a -> x2a
               Z2b -> x2b
               Z2c -> x2c
               Z2d -> x2d
               Z2e -> x2e
               Z2f -> x2f
               Z30 -> x30
               Z31 -> x31
               Z32 -> x32
               Z33 -> x33
               Z34 -> x34
               Z35 -> x35
               Z36 -> x36
               Z37 -> x37
               Z38 -> x38
               Z39 -> x39
               Z3a -> x3a
               Z3b -> x3b
               Z3c -> x3c
               Z3d -> x3d
               Z3e -> x3e
               Z3f -> x3f

incZ64 :: Z64 -> Z64
incZ64 Z00 = Z01 
incZ64 Z01 = Z02
incZ64 Z02 = Z03
incZ64 Z03 = Z04
incZ64 Z04 = Z05
incZ64 Z05 = Z06
incZ64 Z06 = Z07
incZ64 Z07 = Z08
incZ64 Z08 = Z09
incZ64 Z09 = Z0a
incZ64 Z0a = Z0b
incZ64 Z0b = Z0c
incZ64 Z0c = Z0d
incZ64 Z0d = Z0e
incZ64 Z0e = Z0f
incZ64 Z0f = Z10
incZ64 Z10 = Z11
incZ64 Z11 = Z12
incZ64 Z12 = Z13
incZ64 Z13 = Z14
incZ64 Z14 = Z15
incZ64 Z15 = Z16
incZ64 Z16 = Z17
incZ64 Z17 = Z18
incZ64 Z18 = Z19
incZ64 Z19 = Z1a
incZ64 Z1a = Z1b
incZ64 Z1b = Z1c
incZ64 Z1c = Z1d
incZ64 Z1d = Z1e
incZ64 Z1e = Z1f
incZ64 Z1f = Z20
incZ64 Z20 = Z21
incZ64 Z21 = Z22
incZ64 Z22 = Z23
incZ64 Z23 = Z24
incZ64 Z24 = Z25
incZ64 Z25 = Z26
incZ64 Z26 = Z27
incZ64 Z27 = Z28
incZ64 Z28 = Z29
incZ64 Z29 = Z2a
incZ64 Z2a = Z2b
incZ64 Z2b = Z2c
incZ64 Z2c = Z2d
incZ64 Z2d = Z2e
incZ64 Z2e = Z2f
incZ64 Z2f = Z30
incZ64 Z30 = Z31
incZ64 Z31 = Z32
incZ64 Z32 = Z33
incZ64 Z33 = Z34
incZ64 Z34 = Z35
incZ64 Z35 = Z36
incZ64 Z36 = Z37
incZ64 Z37 = Z38
incZ64 Z38 = Z39
incZ64 Z39 = Z3a
incZ64 Z3a = Z3b
incZ64 Z3b = Z3c
incZ64 Z3c = Z3d
incZ64 Z3d = Z3e
incZ64 Z3e = Z3f
incZ64 Z3f = Z00
