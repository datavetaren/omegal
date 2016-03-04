#ifndef _BitMap_hpp
#define _BitMap_hpp

#include <string>
#include "basic.hpp"
#include "Growing.hpp"

namespace PROJECT {

/*
 * BitMap. This class implements a bit set, but with additional
 * useful utilities for the garbage collector.
 * 
 */
class BitMap : public GrowingAllocator<NativeType> {
public:
    BitMap(size_t capacity = 65536);
    ~BitMap();

    void setSize(size_t size);
    size_t getSize() const;

    bool hasBit(size_t index) const;
    void setBit(size_t index, bool value);

    void print(std::ostream &out) const;
    std::string toString() const;

private:
    NativeType *thisData;
    size_t thisSize;
};

}

#endif
