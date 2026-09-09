# KeeLoq rolling-code authenticator

A small, self-contained model of a KeeLoq-style car-remote ("clicker")
authenticator: the thing that lets **one clicker open exactly one car**, and
stops a recorded button press from being replayed.

It exists in three layers, each with a software reference model **and** a Cryptol
spec, plus a synthesisable ReWire hardware version of the receiver:

1. **The cipher** — KeeLoq itself: a 32-bit block, 64-bit key, 528-round
   non-linear feedback shift register.
2. **The rolling-code protocol** — an incrementing counter, encrypted each press,
   checked against a forward window with two-press resynchronisation.
3. **Key derivation ("learning")** — how one manufacturer key becomes a per-fob
   device key (Simple / Normal / Secure schemes).

## The two guarantees, in one sentence

The **shared secret key** decides *which* car (a stranger's clicker decrypts to
garbage and is rejected); the **counter window** decides *whether this press is
fresh* (a replayed recording is stale the moment it's used). Together:
"one clicker, one car, no replays."

## Files

| File | What it is | Run / build |
|------|------------|-------------|
| `KeeLoq.hs` | Software reference: cipher + fob/car protocol (window + resync) with a demo `main` | `runghc KeeLoq.hs` |
| `KeeLoq.cry` | Cryptol spec of the cipher + rolling code, with correctness properties | `cryptol KeeLoq.cry` |
| `KeeLoqLearn.hs` | Simple/Normal/Secure key-derivation schemes + demo `main` (imports `KeeLoq.hs`) | `runghc KeeLoqLearn.hs` |
| `KeeLoqLearn.cry` | Cryptol spec of the derivations, with properties | `cryptol KeeLoqLearn.cry` |
| `rewire/RW_KeeLoqReceiver.hs` | The receiver recast as a **ReWire Mealy machine** (synthesisable) | see below |
| `rewire/RW_KeeLoqReceiver.sv` | Generated SystemVerilog (artifact) | — |
| `top_level.cry` | Pure Cryptol model of the device emitted by `rwc --cryptol` (artifact) | — |
| `RW_Drive.cry` | Drives the compiled device against the reference cipher; four scenario checks | `cryptol RW_Drive.cry` |

The ReWire source lives in `rewire/` (outside the GHC source path) because it is
compiled by `rwc`, not by GHC — the same way this repo keeps its other `RW_*`
device sources out of the cabal library. Only `KeeLoq` and `KeeLoqLearn` (plain
Haskell) are exposed library modules.

The base "simple" model (`KeeLoq.hs`/`KeeLoq.cry`) deliberately hand-waves *how*
the fob and car come to share a key — `learn` is just handed the fob. The
`*Learn*` files fill that in.

## Running things

### Software demos (plain GHC, no dependencies)

```
runghc KeeLoq.hs        # normal press, replay, wrong car, cloned-serial, resync
runghc KeeLoqLearn.hs   # Simple vs Normal vs Secure learning, incl. the attacks
```

### Cryptol proofs / checks

```
cryptol KeeLoq.cry
  KeeLoq> :prove roundInverse      # one round is invertible          (Q.E.D.)
  KeeLoq> :prove replayRejected    # an immediate replay is rejected  (Q.E.D.)
  KeeLoq> :check encDecInverse     # decrypt . encrypt == id
  KeeLoq> :check hopRoundTrip      # receiver recovers the counter

cryptol KeeLoqLearn.cry
  KeeLoqLearn> :prove simpleKeyIgnoresSerial   # Simple learning's flaw (Q.E.D.)
  KeeLoqLearn> :check normalHopRoundTrip
  KeeLoqLearn> :check normalDependsOnSerial
```

### The ReWire receiver (hardware)

Compile to SystemVerilog (or VHDL), or emit a pure Cryptol model (run from
`src/keeloq`):

```
rwc --start=RW_KeeLoqReceiver.start rewire/RW_KeeLoqReceiver.hs -o rewire/RW_KeeLoqReceiver.sv
rwc --cryptol --start=RW_KeeLoqReceiver.start rewire/RW_KeeLoqReceiver.hs -o top_level.cry
```

Then verify the compiled device against the reference cipher end-to-end:

```
cryptol RW_Drive.cry
  RW_Drive> :check scenarioUnlock       # counter +1  -> Unlock       (Q.E.D.)
  RW_Drive> :check scenarioBadDisc      # wrong key   -> BadDisc      (Q.E.D.)
  RW_Drive> :check scenarioReplay       # far ahead   -> Replay       (Q.E.D.)
  RW_Drive> :check scenarioNeedSecond   # resync gap  -> NeedSecond   (Q.E.D.)
```

(`RW_Drive.cry` imports `top_level.cry`, so regenerate that first if you change
the ReWire source.)

## How the ReWire receiver works

`rewire/RW_KeeLoqReceiver.hs` is a clocked Mealy machine (`ReacT Cmd Resp ...`) that
decrypts **one KeeLoq round per clock cycle** — exactly how KeeLoq hardware
clocks its shift register — instead of unrolling all 528 rounds combinationally.
This keeps the datapath tiny (a 32-bit work register, the 64-bit key, and a round
counter) at the cost of latency.

Interface (one command in / one response out per cycle):

- `Learn k d c` — pair a fob: store device key `k`, expected discrimination `d`,
  baseline counter `c`. Emits `Idle`.
- `Frame hop` — a received 32-bit encrypted hop. The machine emits `Busy` for
  ~528 cycles, then a `Decided v` verdict.
- `NoOp` — idle.

Verdicts: `Unlock`, `Replay`, `NeedSecond`, `Resynced`, `BadDisc`, `Unlearned`
— the same set the software model produces.

All next-state/output logic lives in the pure function `stepPure`; the reactive
wrapper is deliberately straight-line (`get; put; signal`) so ReWire's Procify
pass accepts it.

## Fidelity notes (what's real vs illustrative)

- **Real:** the NLFSR structure, tap positions, NLF constant `0x3A5C742E`, 528
  rounds, and the encrypt/decrypt key schedules (`i mod 64` forward,
  `(15 - i) mod 64` reverse).
- **Illustrative / simplified:**
  - The hop's field layout `[ button:4 | disc:12 | counter:16 ]` is a plausible
    but generic packing.
  - Window sizes (`singleWindow = 16`, `doubleWindow = 32768`) are typical but
    configurable.
  - Learning-scheme input framing (the `0x2`/`0x6` discrimination prefixes,
    "two decryptions concatenated") matches the widely published derivations, but
    exact bit-packing varies across encoder parts (HCS200/300/301/360…).
  - The ReWire receiver models a single learned fob and is loaded with the device
    key directly (in a real receiver it would be derived per `KeeLoqLearn.hs`).

## Verification status

- Cipher: `roundInverse` and `replayRejected` **proved**; `encDecInverse`,
  `decEncInverse`, `hopRoundTrip` pass testing.
- Learning: `simpleKeyIgnoresSerial` **proved**; round-trip and
  serial-dependence properties pass testing.
- Software model: `runghc` demos show Unlock / Replay / UnknownFob /
  BadDiscrimination / resync all behaving correctly, plus the Simple-learning
  forgery and the Secure-learning rejection.
- Hardware: `RW_KeeLoqReceiver.hs` compiles with `rwc` to SystemVerilog; the
  emitted device model passes all four `RW_Drive.cry` scenario checks against the
  reference cipher.
