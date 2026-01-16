Configurable AES Plan.

This work is based on both the original 2001 release of the AES standard and its 2023 update:
  * https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197.pdf
  * https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf.
Apologies in advance as this may seem a bit confusing. I'll refer here to the original as _FIPS.197_ and the update as _FIPS.197-upd1_. 

AES has three modes corresponding to the size of the key: 128, 192, and 256 bits. Here, "configurable" means that the mode can be set dynamically. That is, the mode is part of the input to the computation, along with the key and text to be encrypted/decrypted.

The key expansion routine for AES is itself configurable, which is nice because it's smaller and simpler to demonstrate how configurable algorithms are developed and verified with ReWire.

In the FIPS-197, the word size is 32 bits and the following constants are defined:
 * _Nk_ is the _key length_ in words,
 * _Nb_ is the _block size_ in words, and
 * _Nr_ is the _number of rounds_.

These parameters are defined by mode:
|         | Nk | Nb | Nr | KeyExpansion(byte key[4*nk] , word w[nb*(nr+1)])
| -------------------------------------------------------------------------
| AES-128 |  4 |  4 | 10 | KeyExpansion(byte key[16] , word w[44])
| -------------------------------------------------------------------------
| AES-192 |  6 |  4 | 12 | KeyExpansion(byte key[24] , word w[52])
| -------------------------------------------------------------------------
| AES-256 |  8 |  4 | 14 | KeyExpansion(byte key[32] , word w[60])

The KeyExpansion pseudocode from Fig. 11, page 20, of FIPS.197 and defines the size of the _key_ and _key schedule_ parameters, _key_ and _w_, respectively, as in the fifth column above.

KeyExpansion is configurable in hardware because we will compute each mode assuming that _key_ and _w_ have fixed size, resp., of 32 bytes and 60 words. The smaller modes just won't use all of the space. Configurability in _software_ implementations is relatively straightforward because one can simply allocate (e.g., with malloc) the required space as needed.

The development plan will create four distinct instances of AES KeyExpansion:
1. KeyExpansion128, KeyExpansion192, and KeyExpansion256. These are  with the key and key schedule sizes fixed to those in the above table; and
2. KeyExpansionCfg. This design takes an additional mode input signal (e.g., M128, M192, and M256) to signify the mode.

It must be the case
