# type: ignore
# From https://github.com/aahmed-se/pymmh3/blob/c18b0ffed586b4cb158a9aa4832d5a4603db19a4/pymmh3/pymmh3.py
# Vendored since brew releases need pypi source which this project did not have
# and funcitonality is self contained anyway
"""
pymmh3 was written by Fredrik Kihlander and enhanced by Swapnil Gusani, and is placed in the public
domain. The authors hereby disclaim copyright to this source code.
pure python implementation of the murmur3 hash algorithm
https://code.google.com/p/smhasher/wiki/MurmurHash3
This was written for the times when you do not want to compile c-code and install modules,
and you only want a drop-in murmur3 implementation.
As this is purely python it is FAR from performant and if performance is anything that is needed
a proper c-module is suggested!
This module is written to have the same format as mmh3 python package found here for simple conversions:
https://pypi.python.org/pypi/mmh3/2.3.1
"""


def xrange(a, b, c):
    return list(range(a, b, c))


def xencode(x):
    if isinstance(x, bytes) or isinstance(x, bytearray):
        return x
    else:
        return x.encode()


def hash(key, seed=0x0):
    """Implements 32bit murmur3 hash."""

    key = bytearray(xencode(key))

    def fmix(h):
        h ^= h >> 16
        h = (h * 0x85EBCA6B) & 0xFFFFFFFF
        h ^= h >> 13
        h = (h * 0xC2B2AE35) & 0xFFFFFFFF
        h ^= h >> 16
        return h

    length = len(key)
    nblocks = int(length / 4)

    h1 = seed

    c1 = 0xCC9E2D51
    c2 = 0x1B873593

    # body
    for block_start in range(0, nblocks * 4, 4):
        # ??? big endian?
        k1 = (
            key[block_start + 3] << 24
            | key[block_start + 2] << 16
            | key[block_start + 1] << 8
            | key[block_start + 0]
        )

        k1 = (c1 * k1) & 0xFFFFFFFF
        k1 = (k1 << 15 | k1 >> 17) & 0xFFFFFFFF  # inlined ROTL32
        k1 = (c2 * k1) & 0xFFFFFFFF

        h1 ^= k1
        h1 = (h1 << 13 | h1 >> 19) & 0xFFFFFFFF  # inlined ROTL32
        h1 = (h1 * 5 + 0xE6546B64) & 0xFFFFFFFF

    # tail
    tail_index = nblocks * 4
    k1 = 0
    tail_size = length & 3

    if tail_size >= 3:
        k1 ^= key[tail_index + 2] << 16
    if tail_size >= 2:
        k1 ^= key[tail_index + 1] << 8
    if tail_size >= 1:
        k1 ^= key[tail_index + 0]

    if tail_size > 0:
        k1 = (k1 * c1) & 0xFFFFFFFF
        k1 = (k1 << 15 | k1 >> 17) & 0xFFFFFFFF  # inlined ROTL32
        k1 = (k1 * c2) & 0xFFFFFFFF
        h1 ^= k1

    # finalization
    unsigned_val = fmix(h1 ^ length)
    if unsigned_val & 0x80000000 == 0:
        return unsigned_val
    else:
        return -((unsigned_val ^ 0xFFFFFFFF) + 1)


def hash128(key, seed=0x0, x64arch=True):
    """Implements 128bit murmur3 hash."""

    def hash128_x64(key, seed):
        """Implements 128bit murmur3 hash for x64."""

        def fmix(k):
            k ^= k >> 33
            k = (k * 0xFF51AFD7ED558CCD) & 0xFFFFFFFFFFFFFFFF
            k ^= k >> 33
            k = (k * 0xC4CEB9FE1A85EC53) & 0xFFFFFFFFFFFFFFFF
            k ^= k >> 33
            return k

        length = len(key)
        nblocks = int(length / 16)

        h1 = seed
        h2 = seed

        c1 = 0x87C37B91114253D5
        c2 = 0x4CF5AD432745937F

        # body
        for block_start in range(0, nblocks * 8, 8):
            # ??? big endian?
            k1 = (
                key[2 * block_start + 7] << 56
                | key[2 * block_start + 6] << 48
                | key[2 * block_start + 5] << 40
                | key[2 * block_start + 4] << 32
                | key[2 * block_start + 3] << 24
                | key[2 * block_start + 2] << 16
                | key[2 * block_start + 1] << 8
                | key[2 * block_start + 0]
            )

            k2 = (
                key[2 * block_start + 15] << 56
                | key[2 * block_start + 14] << 48
                | key[2 * block_start + 13] << 40
                | key[2 * block_start + 12] << 32
                | key[2 * block_start + 11] << 24
                | key[2 * block_start + 10] << 16
                | key[2 * block_start + 9] << 8
                | key[2 * block_start + 8]
            )

            k1 = (c1 * k1) & 0xFFFFFFFFFFFFFFFF
            k1 = (k1 << 31 | k1 >> 33) & 0xFFFFFFFFFFFFFFFF  # inlined ROTL64
            k1 = (c2 * k1) & 0xFFFFFFFFFFFFFFFF
            h1 ^= k1

            h1 = (h1 << 27 | h1 >> 37) & 0xFFFFFFFFFFFFFFFF  # inlined ROTL64
            h1 = (h1 + h2) & 0xFFFFFFFFFFFFFFFF
            h1 = (h1 * 5 + 0x52DCE729) & 0xFFFFFFFFFFFFFFFF

            k2 = (c2 * k2) & 0xFFFFFFFFFFFFFFFF
            k2 = (k2 << 33 | k2 >> 31) & 0xFFFFFFFFFFFFFFFF  # inlined ROTL64
            k2 = (c1 * k2) & 0xFFFFFFFFFFFFFFFF
            h2 ^= k2

            h2 = (h2 << 31 | h2 >> 33) & 0xFFFFFFFFFFFFFFFF  # inlined ROTL64
            h2 = (h1 + h2) & 0xFFFFFFFFFFFFFFFF
            h2 = (h2 * 5 + 0x38495AB5) & 0xFFFFFFFFFFFFFFFF

        # tail
        tail_index = nblocks * 16
        k1 = 0
        k2 = 0
        tail_size = length & 15

        if tail_size >= 15:
            k2 ^= key[tail_index + 14] << 48
        if tail_size >= 14:
            k2 ^= key[tail_index + 13] << 40
        if tail_size >= 13:
            k2 ^= key[tail_index + 12] << 32
        if tail_size >= 12:
            k2 ^= key[tail_index + 11] << 24
        if tail_size >= 11:
            k2 ^= key[tail_index + 10] << 16
        if tail_size >= 10:
            k2 ^= key[tail_index + 9] << 8
        if tail_size >= 9:
            k2 ^= key[tail_index + 8]

        if tail_size > 8:
            k2 = (k2 * c2) & 0xFFFFFFFFFFFFFFFF
            k2 = (k2 << 33 | k2 >> 31) & 0xFFFFFFFFFFFFFFFF  # inlined ROTL64
            k2 = (k2 * c1) & 0xFFFFFFFFFFFFFFFF
            h2 ^= k2

        if tail_size >= 8:
            k1 ^= key[tail_index + 7] << 56
        if tail_size >= 7:
            k1 ^= key[tail_index + 6] << 48
        if tail_size >= 6:
            k1 ^= key[tail_index + 5] << 40
        if tail_size >= 5:
            k1 ^= key[tail_index + 4] << 32
        if tail_size >= 4:
            k1 ^= key[tail_index + 3] << 24
        if tail_size >= 3:
            k1 ^= key[tail_index + 2] << 16
        if tail_size >= 2:
            k1 ^= key[tail_index + 1] << 8
        if tail_size >= 1:
            k1 ^= key[tail_index + 0]

        if tail_size > 0:
            k1 = (k1 * c1) & 0xFFFFFFFFFFFFFFFF
            k1 = (k1 << 31 | k1 >> 33) & 0xFFFFFFFFFFFFFFFF  # inlined ROTL64
            k1 = (k1 * c2) & 0xFFFFFFFFFFFFFFFF
            h1 ^= k1

        # finalization
        h1 ^= length
        h2 ^= length

        h1 = (h1 + h2) & 0xFFFFFFFFFFFFFFFF
        h2 = (h1 + h2) & 0xFFFFFFFFFFFFFFFF

        h1 = fmix(h1)
        h2 = fmix(h2)

        h1 = (h1 + h2) & 0xFFFFFFFFFFFFFFFF
        h2 = (h1 + h2) & 0xFFFFFFFFFFFFFFFF

        return h2 << 64 | h1

    def hash128_x86(key, seed):
        """Implements 128bit murmur3 hash for x86."""

        def fmix(h):
            h ^= h >> 16
            h = (h * 0x85EBCA6B) & 0xFFFFFFFF
            h ^= h >> 13
            h = (h * 0xC2B2AE35) & 0xFFFFFFFF
            h ^= h >> 16
            return h

        length = len(key)
        nblocks = int(length / 16)

        h1 = seed
        h2 = seed
        h3 = seed
        h4 = seed

        c1 = 0x239B961B
        c2 = 0xAB0E9789
        c3 = 0x38B34AE5
        c4 = 0xA1E38B93

        # body
        for block_start in range(0, nblocks * 16, 16):
            k1 = (
                key[block_start + 3] << 24
                | key[block_start + 2] << 16
                | key[block_start + 1] << 8
                | key[block_start + 0]
            )

            k2 = (
                key[block_start + 7] << 24
                | key[block_start + 6] << 16
                | key[block_start + 5] << 8
                | key[block_start + 4]
            )

            k3 = (
                key[block_start + 11] << 24
                | key[block_start + 10] << 16
                | key[block_start + 9] << 8
                | key[block_start + 8]
            )

            k4 = (
                key[block_start + 15] << 24
                | key[block_start + 14] << 16
                | key[block_start + 13] << 8
                | key[block_start + 12]
            )

            k1 = (c1 * k1) & 0xFFFFFFFF
            k1 = (k1 << 15 | k1 >> 17) & 0xFFFFFFFF  # inlined ROTL32
            k1 = (c2 * k1) & 0xFFFFFFFF
            h1 ^= k1

            h1 = (h1 << 19 | h1 >> 13) & 0xFFFFFFFF  # inlined ROTL32
            h1 = (h1 + h2) & 0xFFFFFFFF
            h1 = (h1 * 5 + 0x561CCD1B) & 0xFFFFFFFF

            k2 = (c2 * k2) & 0xFFFFFFFF
            k2 = (k2 << 16 | k2 >> 16) & 0xFFFFFFFF  # inlined ROTL32
            k2 = (c3 * k2) & 0xFFFFFFFF
            h2 ^= k2

            h2 = (h2 << 17 | h2 >> 15) & 0xFFFFFFFF  # inlined ROTL32
            h2 = (h2 + h3) & 0xFFFFFFFF
            h2 = (h2 * 5 + 0x0BCAA747) & 0xFFFFFFFF

            k3 = (c3 * k3) & 0xFFFFFFFF
            k3 = (k3 << 17 | k3 >> 15) & 0xFFFFFFFF  # inlined ROTL32
            k3 = (c4 * k3) & 0xFFFFFFFF
            h3 ^= k3

            h3 = (h3 << 15 | h3 >> 17) & 0xFFFFFFFF  # inlined ROTL32
            h3 = (h3 + h4) & 0xFFFFFFFF
            h3 = (h3 * 5 + 0x96CD1C35) & 0xFFFFFFFF

            k4 = (c4 * k4) & 0xFFFFFFFF
            k4 = (k4 << 18 | k4 >> 14) & 0xFFFFFFFF  # inlined ROTL32
            k4 = (c1 * k4) & 0xFFFFFFFF
            h4 ^= k4

            h4 = (h4 << 13 | h4 >> 19) & 0xFFFFFFFF  # inlined ROTL32
            h4 = (h1 + h4) & 0xFFFFFFFF
            h4 = (h4 * 5 + 0x32AC3B17) & 0xFFFFFFFF

        # tail
        tail_index = nblocks * 16
        k1 = 0
        k2 = 0
        k3 = 0
        k4 = 0
        tail_size = length & 15

        if tail_size >= 15:
            k4 ^= key[tail_index + 14] << 16
        if tail_size >= 14:
            k4 ^= key[tail_index + 13] << 8
        if tail_size >= 13:
            k4 ^= key[tail_index + 12]

        if tail_size > 12:
            k4 = (k4 * c4) & 0xFFFFFFFF
            k4 = (k4 << 18 | k4 >> 14) & 0xFFFFFFFF  # inlined ROTL32
            k4 = (k4 * c1) & 0xFFFFFFFF
            h4 ^= k4

        if tail_size >= 12:
            k3 ^= key[tail_index + 11] << 24
        if tail_size >= 11:
            k3 ^= key[tail_index + 10] << 16
        if tail_size >= 10:
            k3 ^= key[tail_index + 9] << 8
        if tail_size >= 9:
            k3 ^= key[tail_index + 8]

        if tail_size > 8:
            k3 = (k3 * c3) & 0xFFFFFFFF
            k3 = (k3 << 17 | k3 >> 15) & 0xFFFFFFFF  # inlined ROTL32
            k3 = (k3 * c4) & 0xFFFFFFFF
            h3 ^= k3

        if tail_size >= 8:
            k2 ^= key[tail_index + 7] << 24
        if tail_size >= 7:
            k2 ^= key[tail_index + 6] << 16
        if tail_size >= 6:
            k2 ^= key[tail_index + 5] << 8
        if tail_size >= 5:
            k2 ^= key[tail_index + 4]

        if tail_size > 4:
            k2 = (k2 * c2) & 0xFFFFFFFF
            k2 = (k2 << 16 | k2 >> 16) & 0xFFFFFFFF  # inlined ROTL32
            k2 = (k2 * c3) & 0xFFFFFFFF
            h2 ^= k2

        if tail_size >= 4:
            k1 ^= key[tail_index + 3] << 24
        if tail_size >= 3:
            k1 ^= key[tail_index + 2] << 16
        if tail_size >= 2:
            k1 ^= key[tail_index + 1] << 8
        if tail_size >= 1:
            k1 ^= key[tail_index + 0]

        if tail_size > 0:
            k1 = (k1 * c1) & 0xFFFFFFFF
            k1 = (k1 << 15 | k1 >> 17) & 0xFFFFFFFF  # inlined ROTL32
            k1 = (k1 * c2) & 0xFFFFFFFF
            h1 ^= k1

        # finalization
        h1 ^= length
        h2 ^= length
        h3 ^= length
        h4 ^= length

        h1 = (h1 + h2) & 0xFFFFFFFF
        h1 = (h1 + h3) & 0xFFFFFFFF
        h1 = (h1 + h4) & 0xFFFFFFFF
        h2 = (h1 + h2) & 0xFFFFFFFF
        h3 = (h1 + h3) & 0xFFFFFFFF
        h4 = (h1 + h4) & 0xFFFFFFFF

        h1 = fmix(h1)
        h2 = fmix(h2)
        h3 = fmix(h3)
        h4 = fmix(h4)

        h1 = (h1 + h2) & 0xFFFFFFFF
        h1 = (h1 + h3) & 0xFFFFFFFF
        h1 = (h1 + h4) & 0xFFFFFFFF
        h2 = (h1 + h2) & 0xFFFFFFFF
        h3 = (h1 + h3) & 0xFFFFFFFF
        h4 = (h1 + h4) & 0xFFFFFFFF

        return h4 << 96 | h3 << 64 | h2 << 32 | h1

    key = bytearray(xencode(key))

    if x64arch:
        return hash128_x64(key, seed)
    else:
        return hash128_x86(key, seed)


def hash64(key, seed=0x0, x64arch=True):
    """Implements 64bit murmur3 hash. Returns a tuple."""

    hash_128 = hash128(key, seed, x64arch)

    unsigned_val1 = hash_128 & 0xFFFFFFFFFFFFFFFF
    if unsigned_val1 & 0x8000000000000000 == 0:
        signed_val1 = unsigned_val1
    else:
        signed_val1 = -((unsigned_val1 ^ 0xFFFFFFFFFFFFFFFF) + 1)

    unsigned_val2 = (hash_128 >> 64) & 0xFFFFFFFFFFFFFFFF
    if unsigned_val2 & 0x8000000000000000 == 0:
        signed_val2 = unsigned_val2
    else:
        signed_val2 = -((unsigned_val2 ^ 0xFFFFFFFFFFFFFFFF) + 1)

    return (int(signed_val1), int(signed_val2))


def hash_bytes(key, seed=0x0, x64arch=True):
    """Implements 128bit murmur3 hash. Returns a byte string."""

    hash_128 = hash128(key, seed, x64arch)

    bytestring = ""

    for _i in range(0, 16, 1):
        lsbyte = hash_128 & 0xFF
        bytestring = bytestring + str(chr(lsbyte))
        hash_128 = hash_128 >> 8

    return bytestring


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser("pymurmur3", 'pymurmur [options] "string to hash"')
    parser.add_argument("--seed", type=int, default=0)
    parser.add_argument("strings", default=[], nargs="+")

    opts = parser.parse_args()

    for str_to_hash in opts.strings:
        print('"%s" = 0x%08X\n' % (str_to_hash, hash(str_to_hash)))
