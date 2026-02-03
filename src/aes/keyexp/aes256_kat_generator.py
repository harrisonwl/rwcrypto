#!/usr/bin/env python3
"""
Known Answer Test (KAT) generator for AES-256 key expansion.
Generates random 256-bit keys, computes key expansion, and writes
Haskell KAT definitions to KATs.hs.
"""

import sys
import secrets

from aes256_key_expansion import key_expansion


def key_bytes_to_words(key: list[int]) -> list[int]:
    """
    Convert 32-byte key to 8 words (each word is 4 bytes).
    Returns list of 8 integers, each representing a 32-bit word.
    """
    words = []
    for i in range(8):
        word_bytes = key[4 * i : 4 * i + 4]
        word_int = (word_bytes[0] << 24) | (word_bytes[1] << 16) | (word_bytes[2] << 8) | word_bytes[3]
        words.append(word_int)
    return words


def word_to_integer(word: list[int]) -> int:
    """Convert 4-byte word (big-endian) to Integer (32-bit value)."""
    return (word[0] << 24) | (word[1] << 16) | (word[2] << 8) | word[3]


def format_kat_for_haskell(key_words: list[int], w: list[list[int]]) -> str:
    """
    Format a single KAT as Haskell: ((lit 0xw0, lit 0xw1, ..., lit 0xw7), [(0, lit 0xw0), (1, lit 0xw1), ...]).
    Key is an 8-tuple of 32-bit words, word values in hex (0x...) with 'lit' prefix; indices in decimal.
    """
    key_tuple = ", ".join(f"lit 0x{word:08x}" for word in key_words)
    w_pairs = ", ".join(
        f"({i}, lit 0x{word_to_integer(word):08x})"
        for i, word in enumerate(w)
    )
    return f"    (({key_tuple}), [{w_pairs}])"


def generate_kats(n: int) -> list[tuple[list[int], list[list[int]]]]:
    """Generate n KATs: list of (key_as_8_words, w)."""
    kats = []
    for _ in range(n):
        key = list(secrets.token_bytes(32))
        w = key_expansion(key)
        key_words = key_bytes_to_words(key)
        kats.append((key_words, w))
    return kats


def write_kats_hs(kats: list[tuple[list[int], list[list[int]]]], path: str = "KATs.hs") -> None:
    """Write KATs as Haskell definition to path."""
    lines = [
        "{-# LANGUAGE DataKinds #-}",
        "module Aes.KeyExp.KATs where",
        "",
        "import ReWire",
        "import ReWire.Bits",
        "",
        "kats :: [((W 32, W 32, W 32, W 32, W 32, W 32, W 32, W 32), [(Int, W 32)])]",
        "kats = [",
    ]
    kat_strs = [format_kat_for_haskell(key_words, w) for key_words, w in kats]
    lines.append(",\n".join(kat_strs) + "]")
    with open(path, "w") as f:
        f.write("\n".join(lines))


def main() -> None:
    if len(sys.argv) < 2:
        print("Usage: python aes256_kat_generator.py <number_of_KATs>", file=sys.stderr)
        sys.exit(1)
    try:
        n = int(sys.argv[1])
    except ValueError:
        print("Error: number of KATs must be an integer", file=sys.stderr)
        sys.exit(1)
    if n < 1:
        print("Error: number of KATs must be at least 1", file=sys.stderr)
        sys.exit(1)
    kats = generate_kats(n)
    write_kats_hs(kats)
    print(f"Wrote {n} KAT(s) to KATs.hs", file=sys.stderr)


if __name__ == "__main__":
    main()
