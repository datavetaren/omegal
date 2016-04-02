#include "Hash.hpp"

// Class implementation of Murmur3

namespace PROJECT {

#ifdef __GNUC__
#define FORCE_INLINE __attribute__((always_inline)) inline
#else
#define FORCE_INLINE inline
#endif

static FORCE_INLINE uint32_t rotl32 ( uint32_t x, int8_t r )
{
    return (x << r) | (x >> (32 - r));
}

static FORCE_INLINE uint32_t fmix32 ( uint32_t h )
{
    h ^= h >> 16;
    h *= 0x85ebca6b;
    h ^= h >> 13;
    h *= 0xc2b2ae35;
    h ^= h >> 16;

    return h;
}

Hash::Hash()
{
    reset();
}

void Hash::reset()
{
    thisState = HASH_SEED;
    thisCount = 0;
}

void Hash::update(uint32_t value)
{
    uint32_t k1 = value;
    uint32_t h1 = thisState;

    k1 *= C1;
    k1 = rotl32(k1, 15);
    k1 *= C2;

    h1 ^= k1;
    h1 = rotl32(h1, 13);
    h1 = h1*5+0xe6546b64;

    thisState = h1;
    thisCount += 4;
}

void Hash::update(const Char *str, size_t len)
{
    for (size_t i = 0; i < len; i++) {
	update((uint32_t)str[i]);
    }
}

uint32_t Hash::finalize()
{
    uint32_t h1 = thisState;
    h1 ^= thisCount;
    h1 = fmix32(h1);
    return h1;
}

}

