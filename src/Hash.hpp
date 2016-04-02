#ifndef _Hash_hpp
#define _Hash_hpp

#include "basic.hpp"
#include "Murmur3.hpp"

namespace PROJECT {

class Hash {
public:
    Hash();

    void reset();
    void update(uint32_t value);
    void update(const Char *str, size_t len);
    uint32_t finalize();

private:
    static const uint32_t HASH_SEED = 38136192;
    static const uint32_t C1 = 0xcc9e2d51;
    static const uint32_t C2 = 0x1b873593;

    uint32_t thisState;
    size_t thisCount;
};

}

#endif
