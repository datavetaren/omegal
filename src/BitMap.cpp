#include <iostream>
#include <sstream>
#include "BitMap.hpp"

namespace PROJECT {

BitMap::BitMap(size_t size) : GrowingAllocator<NativeType>((size+NativeTypeBits-1) / NativeTypeBits)
{
    thisData = NULL;
    thisSize = 0;
    setSize(size);
}

BitMap::~BitMap()
{
}

void BitMap::print(std::ostream &out) const
{
    out << "[";
    size_t n = getSize();
    bool first = true;
    for (size_t i = 0; i < n; i++) {
	if (hasBit(i)) {
	    if (!first) out << ", ";
	    out << i;
	    first = false;
	}
    }
    out << "]";
}

std::string BitMap::toString() const
{
    std::stringstream ss;
    print(ss);
    return ss.str();
}

void BitMap::setSize(size_t size)
{
    if (thisData != NULL) deallocate(thisData, sizeof(NativeType));
    size_t numWords = (size+NativeTypeBits-1)/NativeTypeBits;
    thisSize = size;
    thisData = allocate(numWords);
    memset(thisData, 0, numWords*NativeTypeBits/8);
}

size_t BitMap::getSize() const
{
    return thisSize;
}

bool BitMap::hasBit(size_t index) const
{
    return (thisData[index/NativeTypeBits] & (1 << (index % NativeTypeBits))) != 0;
}

void BitMap::setBit(size_t index, bool value)
{
    if (value) {
	thisData[index/NativeTypeBits] |= (1 << (index % NativeTypeBits));
    } else {
	thisData[index/NativeTypeBits] &= ~(1 << (index % NativeTypeBits));
    }
}

}
