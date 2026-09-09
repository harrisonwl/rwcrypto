-- | KeeLoq key-derivation ("learning") schemes.
--
-- The base model in KeeLoq.hs deliberately hand-waves *how* a fob and its car
-- come to share a device key: `learn` is simply handed the fob. This file fills
-- that gap. It models the three classic KeeLoq learning schemes used by the
-- Microchip HCS encoders, all of which turn a single 64-bit *manufacturer key*
-- (held by the carmaker / receiver) into a *per-fob device key*:
--
--   * Simple learning : device key = manufacturer key (every fob identical)
--   * Normal learning : device key derived from the fob's (public) serial
--   * Secure learning : device key derived from a per-fob random *seed*
--
-- The point of the exercise: with Normal/Secure learning a receiver can
-- regenerate any fob's device key on demand from public info (the serial) plus
-- its own manufacturer key -- so it never has to store a key per fob and can
-- learn new fobs in the field -- while an attacker who cracks *one* fob does not
-- automatically own every other fob (which is exactly what Simple learning
-- fails at).
--
-- Runs standalone:  runghc KeeLoqLearn.hs   (imports the cipher from KeeLoq.hs)
--
-- NB: exact input framing for Normal/Secure learning varies across encoder
-- parts (HCS200/300/301/360/...). The 0x2/0x6 discrimination prefixes and the
-- "two KeeLoq decryptions, concatenated" structure modelled here match the
-- widely published derivations; treat the precise bit-packing as illustrative.

module KeeLoqLearn where

import Data.Bits
import Data.Word (Word32, Word64)
import Text.Printf (printf)

import KeeLoq
  ( Serial, Counter, Button
  , Fob(..), Car(..), Frame(..), Decision(..)
  , decrypt, press, receive, learn, opened
  , hopDisc, hopCounter
  )

-- ---------------------------------------------------------------------------
-- Key derivation
-- ---------------------------------------------------------------------------

type ManufacturerKey = Word64
type Seed            = Word64

data Learning
  = SimpleLearn          -- key = manufacturer key
  | NormalLearn          -- key from serial number
  | SecureLearn          -- key from a per-device seed
  deriving (Show, Eq)

-- | Simple learning: the device key *is* the manufacturer key. Trivial to
-- learn, but extracting one fob's key exposes the whole product line.
deriveSimple :: ManufacturerKey -> Word64
deriveSimple mk = mk

-- | Normal learning: build two 32-bit blocks from the 28-bit serial, tagged
-- with the fixed discrimination nibbles 0x2 and 0x6, and KeeLoq-decrypt each
-- under the manufacturer key. Concatenate for the 64-bit device key. The serial
-- is public, so the receiver reproduces this from the serial alone.
deriveNormal :: ManufacturerKey -> Serial -> Word64
deriveNormal mk serial =
    (fromIntegral hi `shiftL` 32) .|. fromIntegral lo
  where
    sn = serial .&. 0x0FFFFFFF                 -- 28-bit serial
    lo = decrypt mk (0x20000000 .|. sn)
    hi = decrypt mk (0x60000000 .|. sn)

-- | Secure learning: same construction, but the two blocks come from a per-fob
-- random seed instead of the (guessable) serial. The receiver can only
-- reproduce the key if the fob transmits the seed once, during a deliberate
-- learning mode -- so knowing the serial is not enough to forge.
deriveSecure :: ManufacturerKey -> Seed -> Word64
deriveSecure mk seed =
    (fromIntegral hi `shiftL` 32) .|. fromIntegral lo
  where
    lo = decrypt mk (fromIntegral (seed .&. 0xFFFFFFFF))
    hi = decrypt mk (fromIntegral ((seed `shiftR` 32) .&. 0xFFFFFFFF))

-- | Unified entry point. Secure learning requires the seed.
deriveKey :: ManufacturerKey -> Learning -> Serial -> Maybe Seed -> Word64
deriveKey mk SimpleLearn _      _         = deriveSimple mk
deriveKey mk NormalLearn serial _         = deriveNormal mk serial
deriveKey mk SecureLearn _      (Just sd) = deriveSecure mk sd
deriveKey _  SecureLearn _      Nothing   =
  error "secure learning needs the device seed"

-- ---------------------------------------------------------------------------
-- Manufacturing a fob and learning it into a car
-- ---------------------------------------------------------------------------

-- | Program a fresh fob at the factory with the derived device key.
manufacture :: ManufacturerKey -> Learning -> Serial -> Maybe Seed
            -> Word32 -> Counter -> Fob
manufacture mk lrn serial mseed disc ctr =
  Fob { fobSerial  = serial
      , fobKey     = deriveKey mk lrn serial mseed
      , fobDisc    = disc
      , fobCounter = ctr
      }

-- | What the car receives during a learning press. For Secure learning the fob
-- also emits its seed (only in learning mode); Normal/Simple need only the
-- ordinary frame, because the serial travels in the clear inside it.
data LearnMsg = LearnMsg
  { lmFrame :: Frame
  , lmSeed  :: Maybe Seed
  } deriving (Show)

-- | Car-side learning: regenerate the device key from the manufacturer key
-- (plus serial, plus seed if Secure), then decrypt the presented hop to record
-- the fob's current counter/discrimination as the baseline. This mirrors the
-- receiver-side of `learn` from KeeLoq.hs, but *computes* the key instead of
-- being handed it.
carLearn :: ManufacturerKey -> Learning -> LearnMsg -> Car -> Car
carLearn mk lrn (LearnMsg frame mseed) car =
    learn car virtualFob
  where
    serial     = frSerial frame
    key        = deriveKey mk lrn serial mseed
    pt         = decrypt key (frHop frame)
    virtualFob = Fob { fobSerial  = serial
                     , fobKey     = key
                     , fobDisc    = hopDisc pt
                     , fobCounter = hopCounter pt
                     }

-- Build the learning message a fob sends when put into learning mode. Returns
-- the advanced fob too: a real fob's counter is monotonic, so the learn press
-- consumes a counter value like any other press.
learnMsg :: Learning -> Fob -> Maybe Seed -> (Fob, LearnMsg)
learnMsg _ fob mseed = (fob', LearnMsg frame mseed)
  where (fob', frame) = press fob 0x0    -- a "learn" button press

-- ---------------------------------------------------------------------------
-- Demonstration
-- ---------------------------------------------------------------------------

hex64 :: Word64 -> String
hex64 = printf "0x%016X"

-- Pair a fob to a fresh car under a scheme, then run a normal press.
demoScheme :: ManufacturerKey -> Learning -> Fob -> Maybe Seed -> IO ()
demoScheme mk lrn fob mseed = do
  printf "\n== %s ==\n" (show lrn)
  printf "  manufacturer key : %s\n" (hex64 mk)
  printf "  fob serial       : 0x%07X\n" (fobSerial fob)
  printf "  derived dev key  : %s\n" (hex64 (fobKey fob))
  -- Learn the fob into a car that only knows the manufacturer key + scheme.
  let (fobLearned, lm) = learnMsg lrn fob mseed
      car0             = carLearn mk lrn lm (Car [])
      (_, fr)          = press fobLearned 0x1     -- next press after learning
      (_, d)           = receive car0 fr
  printf "  normal press     -> %-16s %s\n" (show d)
         (if opened d then "[CAR OPENS]" else "[ignored]")

main :: IO ()
main = do
  let mk = 0x5cec6701b79fd949 :: ManufacturerKey

  putStrLn "KeeLoq learning schemes: one manufacturer key -> many device keys"
  putStrLn "-----------------------------------------------------------------"

  -- Two fobs with different serials, same manufacturer key.
  let fobN1 = manufacture mk NormalLearn 0x0A10001 Nothing 0x0A1 100
      fobN2 = manufacture mk NormalLearn 0x0B20002 Nothing 0x0B2 500

  demoScheme mk NormalLearn fobN1 Nothing
  demoScheme mk NormalLearn fobN2 Nothing

  -- Normal learning gives *distinct* device keys from the *same* manufacturer
  -- key -- the receiver regenerates each from the public serial.
  printf "\nNormal learning: distinct device keys from one manufacturer key? %s\n"
         (show (fobKey fobN1 /= fobKey fobN2))
  printf "  fob1 %s\n  fob2 %s\n" (hex64 (fobKey fobN1)) (hex64 (fobKey fobN2))

  -- Simple learning: both fobs share the manufacturer key as their device key.
  let fobS1 = manufacture mk SimpleLearn 0x0C30003 Nothing 0x0C3 10
      fobS2 = manufacture mk SimpleLearn 0x0D40004 Nothing 0x0D4 20
  demoScheme mk SimpleLearn fobS1 Nothing
  printf "\nSimple learning weakness: every fob's key == manufacturer key? %s\n"
         (show (fobKey fobS1 == mk && fobKey fobS2 == mk))

  -- ...and here is why that matters. An attacker who recovered the key from ONE
  -- Simple-learned fob (== the manufacturer key) can forge a fob for ANY serial
  -- and it will validate. We forge a brand-new serial and open its car.
  let victim      = manufacture mk SimpleLearn 0x0E5BEEF Nothing 0x0E5 0
      (_, vlm)    = learnMsg SimpleLearn victim Nothing        -- baseline counter = 1
      carVictim   = carLearn mk SimpleLearn vlm (Car [])
      forgedFob   = Fob { fobSerial = 0x0E5BEEF, fobKey = mk   -- key learned from a different fob!
                        , fobDisc = 0x0E5, fobCounter = 5 }     -- guess a counter ahead of baseline
      (_, forged) = press forgedFob 0x1
      (_, dForge) = receive carVictim forged
  printf "\nForged Simple-learned fob (attacker knows only the shared key): %s %s\n"
         (show dForge) (if opened dForge then "[CAR OPENS - broken!]" else "[ignored]")

  -- Secure learning: the device key comes from a per-fob seed, not the serial.
  let seed  = 0x0123456789ABCDEF :: Seed
      fobSc = manufacture mk SecureLearn 0x0F60006 (Just seed) 0x0F6 42
  demoScheme mk SecureLearn fobSc (Just seed)

  -- An attacker who knows the manufacturer key AND the serial, but NOT the
  -- seed, cannot regenerate the key. Trying Normal-style derivation fails.
  let (_, sclm)   = learnMsg SecureLearn fobSc (Just seed)
      carSecure   = carLearn mk SecureLearn sclm (Car [])
      guesser     = Fob { fobSerial = fobSerial fobSc
                        , fobKey     = deriveNormal mk (fobSerial fobSc) -- wrong: no seed
                        , fobDisc    = 0x0F6, fobCounter = 42 }
      (_, guess)  = press guesser 0x1
      (_, dGuess) = receive carSecure guess
  printf "\nSecure learning: attacker with serial but no seed -> %s %s\n"
         (show dGuess) (if opened dGuess then "[CAR OPENS - broken!]" else "[ignored - safe]")

  putStrLn "\nTakeaway: learning schemes decide how the shared key is *provisioned*."
  putStrLn "Normal/Secure let the receiver regenerate keys without a per-fob table;"
  putStrLn "Secure additionally keeps a cracked fob from compromising the fleet."
