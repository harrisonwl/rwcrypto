{-# LANGUAGE DataKinds #-}
-- | The KeeLoq rolling-code receiver as a ReWire Mealy machine (synthesisable).
--
-- This is the hardware sibling of the software model in KeeLoq.hs: same cipher,
-- same counter-window / two-press resync logic, but expressed as a clocked
-- state machine that rwc compiles to SystemVerilog/VHDL.
--
-- Design choice: rather than unroll all 528 KeeLoq rounds combinationally, the
-- receiver decrypts *iteratively*, one round per clock cycle. That matches how
-- KeeLoq hardware actually works (a non-linear feedback shift register clocked
-- 528 times) and keeps the datapath tiny -- a 32-bit register, a 64-bit key,
-- and a round counter -- at the cost of latency.
--
-- Protocol (one command per cycle on the input port; one response per cycle on
-- the output port):
--
--   * Learn k d c : pair a fob -- store its device key k, expected
--                   discrimination d, and baseline counter c. (In a real
--                   receiver k is derived per KeeLoqLearn.hs; here it is loaded
--                   directly.) Emits `Idle`.
--   * Frame hop   : a received 32-bit encrypted hop. The machine spends the
--                   next ~528 cycles emitting `Busy`, then emits `Decided v`.
--   * NoOp        : idle; emits `Idle`.
--
-- Verdicts mirror the software model: Unlock / Replay / NeedSecond / Resynced /
-- BadDisc / Unlearned.
--
-- Build (from src/keeloq):
--   rwc --start=RW_KeeLoqReceiver.start rewire/RW_KeeLoqReceiver.hs -o rewire/RW_KeeLoqReceiver.sv
-- or a pure model for verification (imported by RW_Drive.cry):
--   rwc --cryptol --start=RW_KeeLoqReceiver.start rewire/RW_KeeLoqReceiver.hs -o top_level.cry

module RW_KeeLoqReceiver where

import Prelude hiding
  ( (^), (<>), (+), (-), (*), (==), (/=), (<), (<=), (>), (>=) )
import ReWire
import ReWire.Bits

-- ---------------------------------------------------------------------------
-- I/O alphabets
-- ---------------------------------------------------------------------------

data Cmd = NoOp
         | Learn (W 64) (W 12) (W 16)   -- device key, expected disc, baseline counter
         | Frame (W 32)                 -- a received encrypted hop

data Verdict = Unlock       -- valid, fresh press
             | Replay       -- counter not ahead -> stale / replayed
             | NeedSecond   -- inside resync window: awaiting a second press
             | Resynced     -- second consecutive press confirmed
             | BadDisc      -- discrimination wrong (usually wrong key)
             | Unlearned    -- no fob paired yet

data Resp = Idle
          | Busy
          | Decided Verdict

-- ---------------------------------------------------------------------------
-- One inverse KeeLoq round (LSB-first bit numbering)
-- ---------------------------------------------------------------------------

nlfConst :: W 32
nlfConst = lit 0x3A5C742E

-- bit n of a 32-bit word, returned as a 0/1 word (n supplied as a literal word)
tap :: W 32 -> W 32 -> W 32
tap y n = (y >>. n) .&. lit 1

-- Inverse round r of decryption: shift left, recover the shifted-out bit.
--   bit0 = msb XOR NLF(taps 30,25,19,8,0) XOR bit15 XOR keybit((15-r) mod 64)
decRound :: W 64 -> W 16 -> W 32 -> W 32
decRound key r y = (y <<. lit 1) .|. b0
  where
    ki  = (lit 15 - r) % lit 64                       -- key-bit index this round
    kb  = resize ((key >>. resize ki) .&. lit 1)      -- that key bit, as W 32
    idx =  tap y (lit 0)
       .|. (tap y (lit 8)  <<. lit 1)
       .|. (tap y (lit 19) <<. lit 2)
       .|. (tap y (lit 25) <<. lit 3)
       .|. (tap y (lit 30) <<. lit 4)
    nlf = (nlfConst >>. idx) .&. lit 1
    b0  = tap y (lit 31) ^ nlf ^ tap y (lit 15) ^ kb

-- ---------------------------------------------------------------------------
-- Acceptance logic (pure): mirrors `step` in KeeLoq.hs
-- ---------------------------------------------------------------------------

singleWindow, doubleWindow :: W 16
singleWindow = lit 16
doubleWindow = lit 32768

-- Given the discrimination check, the forward delta, the received counter, the
-- last accepted counter, and the pending-resync state, decide the verdict and
-- the next (lastCounter, havePending, pending).
decide :: Bool -> W 16 -> W 16 -> W 16 -> Bool -> W 16
       -> (Verdict, W 16, Bool, W 16)
decide discOK delta ctr lastC have pend =
  if discOK
    then if delta == lit 0
           then (Replay, lastC, False, lit 0)                    -- stale / replay
           else if delta <= singleWindow
                  then (Unlock, ctr, False, lit 0)               -- normal press
                  else if delta <= doubleWindow
                         then if have
                                then if ctr == (pend + lit 1)
                                       then (Resynced, ctr, False, lit 0)
                                       else (NeedSecond, lastC, True, ctr)
                                else (NeedSecond, lastC, True, ctr)
                         else (Replay, lastC, False, lit 0)      -- far in the past
    else (BadDisc, lastC, False, lit 0)                          -- wrong key

-- ---------------------------------------------------------------------------
-- State + Mealy machine
-- ---------------------------------------------------------------------------

--          key     disc    lastC   havePend pend    learned work    round   busy
data St = St (W 64) (W 12) (W 16)  Bool    (W 16)  Bool    (W 32) (W 16)  Bool

type Dev = ReacT Cmd Resp (StateT St Identity)

initSt :: St
initSt = St (lit 0) (lit 0) (lit 0) False (lit 0) False (lit 0) (lit 0) False

-- The whole next-state / output decision is a *pure* function; the reactive
-- wrapper below is deliberately straight-line (get; put; signal) so that all
-- branching lives in combinational logic. While `busy`, the machine grinds
-- through the 528 decrypt rounds one per clock, ignoring the input; on the last
-- round it applies the acceptance logic and emits a verdict.
stepPure :: Cmd -> St -> (St, Resp)
stepPure cmd (St k d c hp pv lrn work r busy) =
  if busy
    then if r == lit 528
           then let discF  = (work >>. lit 16) .&. lit 0xFFF
                    discOK = discF == resize d
                    ctr    = resize work
                    delta  = ctr - c
                    (v, lastC, have, pend) = decide discOK delta ctr c hp pv
                in (St k d lastC have pend lrn (lit 0) (lit 0) False, Decided v)
           else (St k d c hp pv lrn (decRound k r work) (r + lit 1) True, Busy)
    else case cmd of
           NoOp           -> (St k d c hp pv lrn work r False, Idle)
           Learn k2 d2 c2 -> (St k2 d2 c2 False (lit 0) True (lit 0) (lit 0) False, Idle)
           Frame hop      -> if lrn
                               then (St k d c hp pv lrn hop (lit 0) True, Busy)
                               else (St k d c hp pv lrn work r False, Decided Unlearned)

action :: Cmd -> Dev Cmd
action cmd = do
    st <- lift get
    let (st', out) = stepPure cmd st
    lift (put st')
    signal out

loop :: Cmd -> Dev ()
loop inp = action inp >>= loop

start :: ReacT Cmd Resp Identity ()
start = extrude (signal Idle >>= loop) initSt
