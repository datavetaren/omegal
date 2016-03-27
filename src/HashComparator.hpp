#ifndef _HashComparator_hpp
#define _HashComparator_hpp

#include "basic.hpp"
#include "Murmur3.hpp"

namespace PROJECT {

static const uint32_t HASH_SEED = 38136192;

template<typename _K> struct HashOf {
    static uint32_t value(const _K &other);
};

template<> struct HashOf<int32_t> {
    static uint32_t value(const int32_t &k) { return Murmur3(&k, sizeof(k), HASH_SEED); }
};

template<> struct HashOf<int64_t> {
    static uint32_t value(const int64_t &k) { return Murmur3(&k, sizeof(k), HASH_SEED); }
};

template<> struct HashOf<void *> {
    static uint32_t value(const int64_t &k) { return Murmur3(&k, sizeof(k), HASH_SEED); }
};

template<typename _K> class HashComparator {
public:
    typedef _K key_type;
    typedef uint32_t hash_t;

    friend bool operator == (const _K &a, const _K &b)
    {
	return a == b;
    }

    static hash_t hash(const _K &k)
    {
	return HashOf<_K>::value(k);
    }
};

}

#endif
