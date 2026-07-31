# Cryptographic Algorithms in GaloisInc/cryptol-specs

A comprehensive list of the cryptographic algorithms defined in
[GaloisInc/cryptol-specs](https://github.com/GaloisInc/cryptol-specs),
grouped by category, each with the top-level Cryptol file that defines it.

The repo follows a convention where an algorithm's core definition lives in a
`Specification.cry` (or a single named `.cry`/literate `.md` file), with
`Instantiations/` providing concrete parameter sets and `Tests/` holding
known-answer tests. The tables below list the top-level definition file for each
algorithm.

## Symmetric — Block Ciphers

| Algorithm | Top-level definition file |
| --- | --- |
| AES (128/192/256) | `Primitive/Symmetric/Cipher/Block/AES/Specification.cry` |
| DES | `Primitive/Symmetric/Cipher/Block/DES.md` |
| Triple DES (3DES) | `Primitive/Symmetric/Cipher/Block/TripleDES.md` |
| GOST 28147-89 (Magma) | `Primitive/Symmetric/Cipher/Block/GOST.cry` |
| KATAN | `Primitive/Symmetric/Cipher/Block/KATAN.cry` |
| PRESENT | `Primitive/Symmetric/Cipher/Block/PRESENT.cry` |
| PRINCE | `Primitive/Symmetric/Cipher/Block/PRINCE.md` |
| LED | `Primitive/Symmetric/Cipher/Block/LED.md` |
| SHACAL | `Primitive/Symmetric/Cipher/Block/SHACAL.cry` |
| Simon | `Primitive/Symmetric/Cipher/Block/Simon/Specification.cry` |
| Speck | `Primitive/Symmetric/Cipher/Block/Speck/Specification.cry` |
| TEA | `Primitive/Symmetric/Cipher/Block/TEA.cry` |
| Threefish | `Primitive/Symmetric/Cipher/Block/Threefish.cry` |
| McMambo | `Primitive/Symmetric/Cipher/Block/McMambo.cry` |

## Symmetric — Block Cipher Modes of Operation

| Algorithm | Top-level definition file |
| --- | --- |
| CBC | `Primitive/Symmetric/Cipher/Block/Modes/CBC.cry` |
| CFB | `Primitive/Symmetric/Cipher/Block/Modes/CFB.cry` |
| CTR | `Primitive/Symmetric/Cipher/Block/Modes/CTR.cry` |
| XTS | `Primitive/Symmetric/Cipher/Block/Modes/XTS.cry` |
| AES Key Wrap (KW/KWP) | `Primitive/Symmetric/Cipher/Block/Modes/AESKeyWrap.cry` |
| Triple DES CBC | `Primitive/Symmetric/Cipher/Block/Modes/TDES_CBC.cry` |
| Triple DES CFB | `Primitive/Symmetric/Cipher/Block/Modes/TDES_CFB.cry` |

## Symmetric — Authenticated Encryption (AEAD)

| Algorithm | Top-level definition file |
| --- | --- |
| GCM (Galois/Counter Mode) | `Primitive/Symmetric/Cipher/Authenticated/GCM/Specification.cry` |
| AES-GCM-SIV | `Primitive/Symmetric/Cipher/Authenticated/AES_GCM_SIV.cry` |
| ChaCha20-Poly1305 | `Primitive/Symmetric/Cipher/Authenticated/ChaChaPolyCryptolIETF.md` |
| AES-SIV (RFC 5297) | `Primitive/Symmetric/Cipher/Authenticated/SIV_rfc5297.md` |
| MEE-CBC (MAC-Encode-Encrypt) | `Primitive/Symmetric/Cipher/Authenticated/MEE_CBC/Specification.cry` |

## Symmetric — Stream Ciphers

| Algorithm | Top-level definition file |
| --- | --- |
| ChaCha20 | `Primitive/Symmetric/Cipher/Stream/chacha20.cry` |
| Salsa20 | `Primitive/Symmetric/Cipher/Stream/Salsa20.md` |
| ZUC | `Primitive/Symmetric/Cipher/Stream/ZUC.cry` (also `ZUC1_6.md`) |
| Trivium | `Primitive/Symmetric/Cipher/Stream/trivium.cry` |
| Bivium | `Primitive/Symmetric/Cipher/Stream/bivium.cry` |

## Symmetric — MAC / KDF

| Algorithm | Top-level definition file |
| --- | --- |
| HMAC | `Primitive/Symmetric/MAC/HMAC/Specification.cry` |
| HKDF (HMAC-based KDF) | `Primitive/Symmetric/KDF/HKDF.md` (+ `HKDF256.cry`) |

## Keyless — Hash Functions

| Algorithm | Top-level definition file |
| --- | --- |
| SHA-1 | `Primitive/Keyless/Hash/SHA1/Specification.cry` |
| SHA-2 (224/256/384/512/512-224/512-256) | `Primitive/Keyless/Hash/SHA2/Specification.cry` |
| SHA-3 (224/256/384/512) + SHAKE128/256 | `Primitive/Keyless/Hash/SHA3/Specification.cry` |
| BLAKE2b | `Primitive/Keyless/Hash/Blake2b.cry` |
| BLAKE2s | `Primitive/Keyless/Hash/Blake2s.cry` |
| MD5 | `Primitive/Keyless/Hash/MD5.md` |
| FNV | `Primitive/Keyless/Hash/FNV.cry` |

## Keyless — Random Bit Generator

| Algorithm | Top-level definition file |
| --- | --- |
| DRBG (deterministic RBG) | `Primitive/Keyless/Generator/DRBG.cry` |

## Asymmetric — Ciphers / Encryption Schemes

| Algorithm | Top-level definition file |
| --- | --- |
| RSA (raw cipher) | `Primitive/Asymmetric/Cipher/RSA.cry` |
| RSAES-OAEP | `Primitive/Asymmetric/Scheme/RSAES_OAEP.cry` |
| RSAES-PKCS1-v1_5 | `Primitive/Asymmetric/Scheme/RSAES_PKCS1_v1_5.cry` |

## Asymmetric — Key Establishment (KEM / Key Agreement)

| Algorithm | Top-level definition file |
| --- | --- |
| ECDH | `Primitive/Asymmetric/KEM/ECDH/Specification.cry` |
| ML-KEM (CRYSTALS-Kyber, FIPS-203) | `Primitive/Asymmetric/KEM/ML_KEM/Specification.cry` |

## Asymmetric — Signature Schemes

| Algorithm | Top-level definition file |
| --- | --- |
| RSA signature | `Primitive/Asymmetric/Signature/RSA.cry` |
| RSA-PSS | `Primitive/Asymmetric/Signature/RSA_PSS.cry` |
| ECDSA | `Primitive/Asymmetric/Signature/ECDSA/Specification.cry` |
| ML-DSA (CRYSTALS-Dilithium, FIPS-204) | `Primitive/Asymmetric/Signature/ML_DSA/Specification.cry` |
| FN-DSA / FALCON (Round 1.2) | `Primitive/Asymmetric/Signature/FALCON/1.2/falcon_parameterized.cry` |
| SLH-DSA / SPHINCS+ (Round 3.1) | `Primitive/Asymmetric/Signature/SphincsPlus/3.1/` (per-parameter files, e.g. `sphincsplus128f.cry`) |
| WOTS+ (Winternitz OTS) | `Primitive/Asymmetric/Signature/WOTS/Specification.cry` |
| XMSS | `Primitive/Asymmetric/Signature/XMSS/Specification.cry` |

## Common — Shared Cryptographic Primitives

These aren't standalone "algorithms" in the scheme sense, but are the underlying
mathematical building blocks defined for reuse:

| Component | Top-level definition file |
| --- | --- |
| Prime-field elliptic curves (P-192/224/256/384/521) | `Common/EC/PrimeField/PFEC.cry` |
| Curve25519 | `Common/EC/Curve25519.cry` |
| NTT (number-theoretic transform) | `Common/ntt.cry` |
| GF(2^4), GF(2^8) field arithmetic | `Common/GF24.cry`, `Common/GF28.cry` |

## Notes

- Files under `Instantiations/` are concrete parameter sets (e.g. `AES256.cry`,
  `ML_KEM1024.cry`, `SHA384.cry`) that import the `Specification.cry`; files
  under `Tests/` are known-answer tests. Each entry above points at the
  abstract/top-level definition rather than these.
- A handful of specs are written as **literate Cryptol in Markdown** (`.md`) —
  DES, TripleDES, LED, PRINCE, MD5, Salsa20, ChaCha20-Poly1305, AES-SIV, ZUC,
  HKDF — the executable Cryptol lives inside code fences in those files.
- SPHINCS+ (Round 3.1) doesn't have a single parameterized `Specification.cry`;
  each parameter set (`128f/128s/192f/192s/256f/256s`) is its own top-level file.
