-- | A KeeLoq-style rolling-code ("hopping code") authenticator.
--
-- This is a plain-Haskell reference model (runs with @runghc KeeLoq.hs@, no
-- extra dependencies) that answers the question: how does a car remote work so
-- that one clicker opens exactly one car, and a recorded button press can't be
-- replayed?
--
-- Two layers:
--
--   1. The KeeLoq block cipher (32-bit block, 64-bit key, 528-round NLFSR).
--      This is the shared secret. A clicker and its paired car hold the same
--      device key; no other car does, so no other car can validate the clicker.
--
--   2. The rolling-code protocol. Every press encrypts an incrementing counter.
--      The car accepts a press only if its counter is *ahead* of the last one
--      it saw (within a window), so an eavesdropper's recording is stale the
--      moment it's used.
--
-- See KeeLoq.cry for the same cipher with machine-checked correctness proofs.

module KeeLoq where

import Data.Bits
import Data.Word (Word16, Word32, Word64)
import Data.List (foldl')
import Text.Printf (printf)

-- ---------------------------------------------------------------------------
-- Core cipher (bit numbering is LSB-first: bit 0 is least significant)
-- ---------------------------------------------------------------------------

-- | The non-linear function: a fixed 5-input lookup table. NLF(a,b,c,d,e) is
-- bit (16e+8d+4c+2b+a) of this constant.
nlfTable :: Word32
nlfTable = 0x3A5C742E

bit32 :: Word32 -> Int -> Word32
bit32 x n = (x `shiftR` n) .&. 1

-- | Assemble the 5-bit NLF index from the given tap positions (high..low).
nlfIndex :: Word32 -> Int -> Int -> Int -> Int -> Int -> Int
nlfIndex x b4 b3 b2 b1 b0 =
      fromIntegral (bit32 x b4) `shiftL` 4
  .|. fromIntegral (bit32 x b3) `shiftL` 3
  .|. fromIntegral (bit32 x b2) `shiftL` 2
  .|. fromIntegral (bit32 x b1) `shiftL` 1
  .|. fromIntegral (bit32 x b0)

nlf :: Int -> Word32
nlf i = bit32 nlfTable i

keyBit :: Word64 -> Int -> Word32
keyBit k n = fromIntegral ((k `shiftR` n) .&. 1)

-- | One forward round: shift right, inject a new most-significant bit.
encRound :: Word64 -> Int -> Word32 -> Word32
encRound key i y = (y `shiftR` 1) .|. (f `shiftL` 31)
  where
    f = nlf (nlfIndex y 31 26 20 9 1)
          `xor` bit32 y 0
          `xor` bit32 y 16
          `xor` keyBit key (i `mod` 64)

-- | One inverse round: shift left, recover the bit that was shifted out.
decRound :: Word64 -> Int -> Word32 -> Word32
decRound key i y = ((y `shiftL` 1) .|. b0) .&. 0xFFFFFFFF
  where
    b0 = bit32 y 31
          `xor` nlf (nlfIndex y 30 25 19 8 0)
          `xor` bit32 y 15
          `xor` keyBit key ((15 - i) `mod` 64)

encrypt :: Word64 -> Word32 -> Word32
encrypt key pt = foldl' (flip (encRound key)) pt [0 .. 527]

decrypt :: Word64 -> Word32 -> Word32
decrypt key ct = foldl' (flip (decRound key)) ct [0 .. 527]

-- ---------------------------------------------------------------------------
-- Rolling-code layer
-- ---------------------------------------------------------------------------
--
-- The encrypted 32-bit "hop" packs a button code, discrimination bits, and the
-- counter:  [ button : 4 | disc : 12 | counter : 16 ].

type Serial  = Word32   -- identifies which fob (sent in clear)
type Button  = Word32   -- which button was pressed (4 bits)
type Counter = Word16

packHop :: Button -> Word32 -> Counter -> Word32
packHop btn disc ctr =
      ((btn  .&. 0xF)   `shiftL` 28)
  .|. ((disc .&. 0xFFF) `shiftL` 16)
  .|.  fromIntegral ctr

hopButton :: Word32 -> Button
hopButton w = (w `shiftR` 28) .&. 0xF

hopDisc :: Word32 -> Word32
hopDisc w = (w `shiftR` 16) .&. 0xFFF

hopCounter :: Word32 -> Counter
hopCounter w = fromIntegral (w .&. 0xFFFF)

-- | What a clicker actually broadcasts on a button press.
data Frame = Frame
  { frSerial :: Serial   -- clear-text identity
  , frButton :: Button   -- clear-text button
  , frHop    :: Word32   -- encrypted counter+button+disc
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- The fob (transmitter)
-- ---------------------------------------------------------------------------

data Fob = Fob
  { fobSerial  :: Serial
  , fobKey     :: Word64
  , fobDisc    :: Word32
  , fobCounter :: Counter
  } deriving (Show)

-- | Press a button: bump the counter, encrypt, emit a frame. Returns the new
-- fob state so counters actually advance across presses.
press :: Fob -> Button -> (Fob, Frame)
press fob btn = (fob', frame)
  where
    ctr'  = fobCounter fob + 1
    fob'  = fob { fobCounter = ctr' }
    hop   = encrypt (fobKey fob) (packHop btn (fobDisc fob) ctr')
    frame = Frame (fobSerial fob) btn hop

-- ---------------------------------------------------------------------------
-- The car (receiver) with a forward window + two-press resync
-- ---------------------------------------------------------------------------

-- Window sizes (typical KeeLoq values): counters within SINGLE ahead are
-- accepted immediately; counters further ahead but within DOUBLE are accepted
-- only after a second, consecutive press (resynchronisation); anything else is
-- rejected. Counters never move backward, which is what kills replays.
singleWindow, doubleWindow :: Word16
singleWindow = 16
doubleWindow = 32768

-- | One learned fob, as remembered by the car.
data Learned = Learned
  { lnKey      :: Word64
  , lnDisc     :: Word32
  , lnCounter  :: Counter          -- last accepted counter
  , lnPending  :: Maybe Counter    -- first press seen inside the resync window
  } deriving (Show)

-- | The car knows about zero or more fobs, keyed by serial.
newtype Car = Car { carFobs :: [(Serial, Learned)] }

data Decision
  = Unlock                 -- valid, fresh press: open the car
  | Replay                 -- counter not ahead of last accepted -> reject
  | NeedSecondPress        -- inside resync window: wait for one more press
  | Resynced               -- second consecutive press confirmed -> open + resync
  | BadDiscrimination      -- decrypted, but sanity bits wrong (usually wrong key)
  | UnknownFob             -- serial not paired to this car
  deriving (Show, Eq)

opened :: Decision -> Bool
opened d = d == Unlock || d == Resynced

-- forward distance from a to b, modulo 2^16
forwardDelta :: Counter -> Counter -> Word16
forwardDelta a b = b - a

-- | Pair a fob to a car ("learning"). The car stores the shared key, the
-- discrimination value, and the fob's current counter as the baseline.
learn :: Car -> Fob -> Car
learn (Car fobs) fob = Car ((fobSerial fob, ln) : filter ((/= fobSerial fob) . fst) fobs)
  where ln = Learned (fobKey fob) (fobDisc fob) (fobCounter fob) Nothing

-- | Core acceptance logic for a single learned fob.
step :: Learned -> Frame -> (Learned, Decision)
step ln frame
  -- Sanity check: the decrypted button and discrimination must match. With the
  -- wrong key this is essentially random and fails, so a stranger's clicker (or
  -- a bit-flipped capture) is rejected here.
  | hopButton pt /= frButton frame || hopDisc pt /= lnDisc ln
      = (ln { lnPending = Nothing }, BadDiscrimination)

  -- Immediate replay / stale capture: not ahead of the last accepted counter.
  | delta == 0
      = (ln { lnPending = Nothing }, Replay)

  -- Normal case: a small step forward. Accept and advance.
  | delta <= singleWindow
      = (ln { lnCounter = rcvd, lnPending = Nothing }, Unlock)

  -- Resync window: too far ahead to trust a single press (fob pressed many
  -- times out of range). Require a *second, consecutive* press to confirm.
  | delta <= doubleWindow
      = case lnPending ln of
          Just p | rcvd == p + 1 ->
            (ln { lnCounter = rcvd, lnPending = Nothing }, Resynced)
          _ ->
            (ln { lnPending = Just rcvd }, NeedSecondPress)

  -- Far outside any window (e.g. a counter from the past): reject.
  | otherwise
      = (ln { lnPending = Nothing }, Replay)
  where
    pt    = decrypt (lnKey ln) (frHop frame)
    rcvd  = hopCounter pt
    delta = forwardDelta (lnCounter ln) rcvd

-- | Feed a frame to the car: look up the fob by serial, run the step, and
-- write back the updated per-fob state.
receive :: Car -> Frame -> (Car, Decision)
receive car@(Car fobs) frame =
  case lookup (frSerial frame) fobs of
    Nothing -> (car, UnknownFob)
    Just ln ->
      let (ln', d) = step ln frame
          fobs'    = (frSerial frame, ln') : filter ((/= frSerial frame) . fst) fobs
      in (Car fobs', d)

-- ---------------------------------------------------------------------------
-- Demonstration
-- ---------------------------------------------------------------------------

-- Run a sequence of frames through a car, printing each decision.
runFrames :: String -> Car -> [(String, Frame)] -> IO Car
runFrames title car0 items = do
  putStrLn ("\n== " ++ title ++ " ==")
  foldM_step car0 items
  where
    foldM_step :: Car -> [(String, Frame)] -> IO Car
    foldM_step car [] = return car
    foldM_step car ((label, fr) : rest) = do
      let (car', d) = receive car fr
      printf "  %-38s -> %-16s %s\n" label (show d)
             (if opened d then "[CAR OPENS]" else "[ignored]")
      foldM_step car' rest

main :: IO ()
main = do
  -- Self-test: the cipher round-trips for a spread of key/plaintext pairs.
  let vectors = [ (0x0000000000000000, 0x00000000)
                , (0xFFFFFFFFFFFFFFFF, 0xFFFFFFFF)
                , (0x5cec6701b79fd949, 0xf741e2db)
                , (0x0123456789abcdef, 0xdeadbeef) ]
      ok = and [ decrypt k (encrypt k p) == p && encrypt k (decrypt k c) == c
               | (k, p) <- vectors, let c = p ]
  printf "cipher self-test (encrypt/decrypt inverse): %s\n"
         (if ok then "PASS" else "FAIL")

  -- Two fobs paired to two different cars. Same manufacturer, different keys.
  let fobA = Fob { fobSerial = 0x00A1, fobKey = 0x5cec6701b79fd949
                 , fobDisc = 0x0A1,   fobCounter = 100 }
      fobB = Fob { fobSerial = 0x00B2, fobKey = 0x0123456789abcdef
                 , fobDisc = 0x0B2,   fobCounter = 500 }
      carA = learn (Car []) fobA
      carB = learn (Car []) fobB

  -- 1) Fob A opens car A on a normal press.
  let (fobA1, f1) = press fobA 0x1
  _ <- runFrames "Fob A -> Car A (normal press)" carA
         [("press unlock", f1)]

  -- 2) The SAME frame replayed (eavesdropper recording) is rejected: the
  --    counter is no longer ahead of what the car last accepted.
  let (carA', _) = receive carA f1
  _ <- runFrames "Replay attack on Car A" carA'
         [("replay the recorded press", f1)]

  -- 3) Fob A's frame at car B: car B was never paired with A's serial, so it
  --    is ignored outright. (The everyday "wrong car" case.)
  _ <- runFrames "Fob A -> Car B (never paired)" carB
         [("press near the wrong car", f1)]

  -- 3b) A cloned/forged fob that knows A's *serial* but not its *key*. Car A
  --     decrypts with the real key, gets garbage, and the discrimination bits
  --     don't match -> rejected. The secret key, not the serial, is what counts.
  let forger      = fobA { fobKey = 0xBADBADBADBADBAD1 }
      (_, forged) = press forger 0x1
  _ <- runFrames "Cloned serial, wrong key -> Car A" carA
         [("forged press (right serial)", forged)]

  -- 4) Out-of-sync resync: fob A is pressed many times out of range so its
  --    counter jumps past the single-press window. The car asks for a second
  --    press, then resynchronises.
  let fobFar         = fobA { fobCounter = 100 + 200 }  -- 200 presses out of range
      (fobFar1, g1)  = press fobFar 0x1                 -- counter 301
      (_,       g2)  = press fobFar1 0x1                -- counter 302
  _ <- runFrames "Resync after many out-of-range presses" carA
         [ ("first press (far ahead)",  g1)
         , ("second consecutive press", g2) ]

  putStrLn "\nBottom line: the shared key decides *which* car; the counter"
  putStrLn "window decides *whether this press is fresh*. Together they give"
  putStrLn "\"one clicker, one car, no replays\"."
