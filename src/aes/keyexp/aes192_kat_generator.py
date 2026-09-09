#!/usr/bin/env python3
"""
Known Answer Test (KAT) generator for AES-192 key expansion.
Generates random 192-bit keys, computes key expansion, and writes
Haskell KAT definitions to KATs192.hs.
"""

import sys
import secrets

from aes192_key_expansion import key_expansion


def key_bytes_to_integer(key: list[int]) -> int:
    """Convert 24-byte key (big-endian) to Integer."""
    n = 0
    for b in key:
        n = (n << 8) | b
    return n


def word_to_integer(word: list[int]) -> int:
    """Convert 4-byte word (big-endian) to Integer (32-bit value)."""
    return (word[0] << 24) | (word[1] << 16) | (word[2] << 8) | word[3]


def format_kat_for_haskell(key_int: int, w: list[list[int]]) -> str:
    """
    Format a single KAT as Haskell: (keyInteger, [(0, 0xw0), (1, 0xw1), ...]).
    Key and word values in hex (0x...); indices in decimal.
    """
    key_hex = f"0x{key_int:048x}"  # 192 bits = 48 hex digits
    w_pairs = ", ".join(
        f"({i}, 0x{word_to_integer(word):08x})"
        for i, word in enumerate(w)
    )
    return f"    ({key_hex}, [{w_pairs}])"


def generate_kats(n: int) -> list[tuple[int, list[list[int]]]]:
    """Generate n KATs: list of (key_as_int, w)."""
    kats = []
    for _ in range(n):
        key = list(secrets.token_bytes(24))
        w = key_expansion(key)
        key_int = key_bytes_to_integer(key)
        kats.append((key_int, w))
    return kats


def write_kats_hs(
    kats: list[tuple[int, list[list[int]]]], path: str = "KATs192.hs"
) -> None:
    """Write KATs as Haskell definition to path."""
    lines = [
        "kats :: [(Integer, [(Int, Integer)])]",
        "kats = [",
    ]
    kat_strs = [format_kat_for_haskell(key_int, w) for key_int, w in kats]
    lines.append(",\n".join(kat_strs) + "]")
    with open(path, "w") as f:
        f.write("\n".join(lines))


def main() -> None:
    if len(sys.argv) < 2:
        print("Usage: python aes192_kat_generator.py <number_of_KATs>", file=sys.stderr)
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
    print(f"Wrote {n} KAT(s) to KATs192.hs", file=sys.stderr)


if __name__ == "__main__":
    main()
