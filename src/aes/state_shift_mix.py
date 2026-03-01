#!/usr/bin/env python3
"""
AES State, ShiftRows, and MixColumns (FIPS 197).

References:
  - FIPS 197 Section 3.4 (State): https://csrc.nist.gov/pubs/fips/197/final
  - FIPS 197 Section 5.1.2 (ShiftRows)
  - FIPS 197 Section 5.1.3 (MixColumns)
  - NIST FIPS 197-upd1 (May 2023): https://doi.org/10.6028/NIST.FIPS.197-upd1
"""

from typing import List

# -----------------------------------------------------------------------------
# 1. State (FIPS 197 Section 3.4, Eq. 3.6 and 3.7)
# -----------------------------------------------------------------------------

# State is a 4x4 array of bytes: state[r][c] with row index r and column index c
# in range 0..3. Input block is 16 bytes in0..in15; output block is out0..out15.
#
#   s[r, c] = in[r + 4*c]   for 0 <= r < 4 and 0 <= c < 4
#   out[r + 4*c] = s[r, c]
#
# Figure 1 (FIPS 197): columns of the state correspond to in0,in4,in8,in12 (col 0);
# in1,in5,in9,in13 (col 1); etc.


def bytes_to_state(block: List[int]) -> List[List[int]]:
    """
    Map a 128-bit (16-byte) input block to the AES State array (FIPS 197, Section 3.4).

    Eq. 3.6: s[r, c] = in[r + 4*c] for 0 <= r < 4 and 0 <= c < 4.

    Args:
        block: List of 16 bytes (integers 0..255), in0, in1, ..., in15.

    Returns:
        State as a 4x4 list: state[r][c] = block[r + 4*c].
    """
    assert len(block) == 16, "Block must be 16 bytes"
    state = [[0] * 4 for _ in range(4)]
    for r in range(4):
        for c in range(4):
            state[r][c] = block[r + 4 * c]
    return state


def state_to_bytes(state: List[List[int]]) -> List[int]:
    """
    Map the AES State array to a 128-bit output block (FIPS 197, Section 3.4).

    Eq. 3.7: out[r + 4*c] = s[r, c] for 0 <= r < 4 and 0 <= c < 4.

    Args:
        state: 4x4 state array (state[r][c]).

    Returns:
        List of 16 bytes: out0, out1, ..., out15.
    """
    block = [0] * 16
    for r in range(4):
        for c in range(4):
            block[r + 4 * c] = state[r][c]
    return block


# -----------------------------------------------------------------------------
# 2. ShiftRows (FIPS 197 Section 5.1.2, Eq. 5.5)
# -----------------------------------------------------------------------------
#
# s'[r,c] = s[r, (c+r) mod 4]
# "Each byte is moved by r positions to the left in the row, cycling the left-most
#  r bytes around to the right end of the row. The first row (r=0) is unchanged."


def shift_rows(state: List[List[int]]) -> List[List[int]]:
    """
    ShiftRows transformation (FIPS 197, Section 5.1.2).

    Eq. 5.5: s'[r,c] = s[r, (c+r) mod 4].
    Row 0 unchanged; row 1 shifted left by 1; row 2 by 2; row 3 by 3.

    Args:
        state: 4x4 state array.

    Returns:
        New 4x4 state after ShiftRows.
    """
    new_state = [[0] * 4 for _ in range(4)]
    for r in range(4):
        for c in range(4):
            new_state[r][c] = state[r][(c + r) % 4]
    return new_state


# -----------------------------------------------------------------------------
# 3. MixColumns (FIPS 197 Section 5.1.3 and Section 4.3)
# -----------------------------------------------------------------------------
#
# Each column of the state is multiplied by the fixed matrix (Section 5.6):
#   [a0, a1, a2, a3] = [02, 01, 01, 03]
# So the matrix (Section 4.3, Eq. 4.8/4.9) gives:
#   s'[0,c] = 02*s[0,c] + 03*s[1,c] + 01*s[2,c] + 01*s[3,c]
#   s'[1,c] = 01*s[0,c] + 02*s[1,c] + 03*s[2,c] + 01*s[3,c]
#   s'[2,c] = 01*s[0,c] + 01*s[1,c] + 02*s[2,c] + 03*s[3,c]
#   s'[3,c] = 03*s[0,c] + 01*s[1,c] + 01*s[2,c] + 02*s[3,c]
# All addition is XOR; multiplication is in GF(2^8) with m(x) = x^8 + x^4 + x^3 + x + 1.
# XTIMES(b) = (b<<1) if b7=0 else (b<<1)^0x1B (FIPS 197 Eq. 4.5).


def _xtimes(b: int) -> int:
    """Multiplication by {02} in GF(2^8). FIPS 197 Eq. 4.5 (XTIMES)."""
    b = b & 0xFF
    if (b >> 7) & 1:
        return ((b << 1) ^ 0x1B) & 0xFF
    return (b << 1) & 0xFF


def _mul2(b: int) -> int:
    """Multiply by 02 in GF(2^8)."""
    return _xtimes(b)


def _mul3(b: int) -> int:
    """Multiply by 03 in GF(2^8). 03*x = 02*x + x."""
    return _xtimes(b) ^ b


def _mix_column(col: List[int]) -> List[int]:
    """
    Mix a single column (4 bytes) using the fixed matrix (FIPS 197 Section 5.1.3).
    col = [s[0,c], s[1,c], s[2,c], s[3,c]].
    """
    s0, s1, s2, s3 = col[0], col[1], col[2], col[3]
    return [
        _mul2(s0) ^ _mul3(s1) ^ s2 ^ s3,
        s0 ^ _mul2(s1) ^ _mul3(s2) ^ s3,
        s0 ^ s1 ^ _mul2(s2) ^ _mul3(s3),
        _mul3(s0) ^ s1 ^ s2 ^ _mul2(s3),
    ]


def mix_columns(state: List[List[int]]) -> List[List[int]]:
    """
    MixColumns transformation (FIPS 197, Section 5.1.3).

    Each column of the state is multiplied by the fixed matrix (Section 4.3).
    Operates on columns: column c is [state[0][c], state[1][c], state[2][c], state[3][c]].

    Args:
        state: 4x4 state array.

    Returns:
        New 4x4 state after MixColumns.
    """
    new_state = [[0] * 4 for _ in range(4)]
    for c in range(4):
        col = [state[r][c] for r in range(4)]
        mixed = _mix_column(col)
        for r in range(4):
            new_state[r][c] = mixed[r]
    return new_state


# -----------------------------------------------------------------------------
# Helpers and tests (match Haskell ShiftRows.hs / MixColumns.hs test vectors)
# -----------------------------------------------------------------------------


def state_hex(state: List[List[int]]) -> str:
    """Format state as 4 lines of hex bytes (row per line)."""
    lines = []
    for r in range(4):
        line = " ".join(f"0x{state[r][c]:02x}" for c in range(4))
        lines.append(line)
    return "\n".join(lines)


def main() -> None:
    # Test state layout: 16 bytes -> state -> 16 bytes
    block = list(range(16))
    st = bytes_to_state(block)
    assert st[0][0] == 0 and st[1][0] == 1 and st[0][1] == 4
    assert state_to_bytes(st) == block
    print("State layout: OK")

    # _mix_column tests (MixColumns.hs: pc (column 0 rst) -> [10, 169, 13, 116])
    rst = [
        [0x1B, 0x75, 0x4A, 0xC0],
        [0xF3, 0xF8, 0xD2, 0x2C],
        [0xB3, 0x0F, 0xCB, 0x79],
        [0x81, 0x78, 0xB9, 0x8D],
    ]
    expected_mc = [
        [0x0A, 0x8E, 0x8B, 0x1B],
        [0xA9, 0xF7, 0x0A, 0x9E],
        [0x0D, 0x1B, 0xC5, 0x92],
        [0x74, 0x98, 0xAE, 0x0F],
    ]
    # Column 0: [0x1b, 0xf3, 0xb3, 0x81] -> [0x0a, 0xa9, 0x0d, 0x74] (Haskell: [27,243,179,129] -> [10,169,13,116])
    col0 = [rst[r][0] for r in range(4)]
    col1 = [0x0 , 0x1 , 0x2 , 0x3 ]
    col2 = [0xD4 , 0xBF , 0x5d , 0x30 ]
    print("col0 = ", col0)
    print("col1 = ", col1)
    print("col2 = ", col2)
    mixed0 = _mix_column(col0)
    mixed1 = _mix_column(col1)
    mixed2 = _mix_column(col2)
    print("_mix_column(col0) = ", mixed0)
    print("_mix_column(col1) = ", mixed1)
    print("_mix_column(col2) = ", mixed2)
    assert mixed0 == [expected_mc[r][0] for r in range(4)], f"_mix_column(col0): got {mixed0}"
    print("_mix_column(col0): OK")
    # All four columns
    for c in range(4):
        col = [rst[r][c] for r in range(4)]
        mixed = _mix_column(col)
        expected = [expected_mc[r][c] for r in range(4)]
        assert mixed == expected, f"_mix_column(col{c}): got {mixed}, expected {expected}"
    print("_mix_column(col 0..3): OK")

    # ShiftRows test (from ShiftRows.hs: rst -> shiftrows(rst))
    sr = shift_rows(rst)
    expected_sr = [
        [0x1B, 0x75, 0x4A, 0xC0],
        [0xF8, 0xD2, 0x2C, 0xF3],
        [0xCB, 0x79, 0xB3, 0x0F],
        [0x8D, 0x81, 0x78, 0xB9],
    ]
    assert sr == expected_sr, f"ShiftRows mismatch:\n{state_hex(sr)}\nexpected:\n{state_hex(expected_sr)}"
    print("ShiftRows: OK")

    # MixColumns test (from MixColumns.hs: rst -> mc)
    mc = mix_columns(rst)
    assert mc == expected_mc, f"MixColumns mismatch:\n{state_hex(mc)}\nexpected:\n{state_hex(expected_mc)}"
    print("MixColumns: OK")

    print("\nAll tests passed.")
    print("\nShiftRows(rst):")
    print(state_hex(sr))
    print("\nMixColumns(rst):")
    print(state_hex(mc))


if __name__ == "__main__":
    main()
