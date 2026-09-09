# Instructions for using this code

I'll assume you have stack.

First, install ReWire from the public github:

- git clone https://github.com/twosixlabs/ReWire
- cd ReWire; stack install
- Move rsp24_codebase into this ReWire top directory
- Change directory to rsp24_codebase.

Here's the contents of this part of the codebase:
rsp24_codebase

├── AsynchronousCSA.hs
├── Blake2b-reference.hs
├── PipelinedCSA.hs
├── README.md
├── RW_Blake2b.hs
└── SimpleCSA.hs

To load code into GHCi, use stack repl:
- stack repl SimpleCSA.hs

To compile with the ReWire compiler (rwc):
- rwc SimpleCSA.hs --verilog

All of the *.hs files compile with rwc except Blake2b-reference.hs (which uses the ReWire definitions of words, etc., but doesn't create a Mealy machine). The file RW_Blake2b.hs may take around 15-20 minutes to compile, depending on your computer. The others should be quick.

## KeeLoq (`keeloq/`)

A KeeLoq-style rolling-code car-remote authenticator: the shared secret key
decides *which* car, and an incrementing counter checked against a forward
window decides *whether a press is fresh* — so one clicker opens one car and
recorded presses can't be replayed. It comes in three layers, each with a
software reference model and a Cryptol spec, plus a synthesisable ReWire version
of the receiver:

- `keeloq/KeeLoq.{hs,cry}` — the KeeLoq cipher + rolling-code protocol (window +
  two-press resync). `runghc keeloq/KeeLoq.hs`; proofs via `cryptol keeloq/KeeLoq.cry`.
- `keeloq/KeeLoqLearn.{hs,cry}` — Simple/Normal/Secure key-derivation
  ("learning") schemes. `runghc keeloq/KeeLoqLearn.hs`.
- `keeloq/rewire/RW_KeeLoqReceiver.hs` — the receiver as a ReWire Mealy machine
  (decrypts one round per clock), compiled with `rwc`; verified end-to-end
  against the reference cipher by `keeloq/RW_Drive.cry`.

`KeeLoq` and `KeeLoqLearn` are plain Haskell and are exposed library modules; the
ReWire receiver lives under `keeloq/rewire/` and is built with `rwc`. See
`keeloq/README.md` for full details and commands.
