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

void BitMap::clear()
{
    memset(thisData, 0, ((getSize()+NativeTypeBits-1)/NativeTypeBits)*sizeof(NativeType));
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

void BitMap::setBits(size_t fromIndex, size_t toIndex, bool value)
{
    if (value) {
	for (size_t i = fromIndex; i < toIndex;) {
	    bool chunkProcessed = false;
	    if (i % NativeTypeBits == 0) {
		size_t chunk = i + NativeTypeBits;
		if (chunk < toIndex) {
		    thisData[i/NativeTypeBits] = (NativeType)-1;
		    i += NativeTypeBits;
		    chunkProcessed = true;
		}
	    }
	    if (!chunkProcessed) {
		thisData[i/NativeTypeBits] |= (1 << (i % NativeTypeBits));
		i++;
	    }
	}
    } else {
	for (size_t i = fromIndex; i < toIndex;) {
	    bool chunkProcessed = false;
	    if (i % NativeTypeBits == 0) {
		size_t chunk = i + NativeTypeBits;
		if (chunk < toIndex) {
		    thisData[i/NativeTypeBits] = 0;
		    i += NativeTypeBits;
		    chunkProcessed = true;
		}
	    }
	    if (!chunkProcessed) {
		thisData[i/NativeTypeBits] &= ~(1 << (i % NativeTypeBits));
		i++;
	    }
	}
    }
}

size_t BitMap::findBit(size_t fromIndex, size_t toIndex, bool value) const
{
    size_t fromWord = fromIndex / NativeTypeBits;
    size_t toWord = (toIndex + NativeTypeBits - 1) / NativeTypeBits;

    if (value) {
	while (fromWord < toWord && thisData[fromWord] == 0) {
	    fromWord++;
	    fromIndex = fromWord * NativeTypeBits;
	}
	for (size_t i = fromIndex; i < toIndex; i++) {
	    if (hasBit(i)) {
		return i;
	    }
	}
	return toIndex;
    } else {
	while (fromWord < toWord && thisData[fromWord] == (NativeType)-1) {
	    fromWord++;
	    fromIndex = fromWord * NativeTypeBits;
	}
	for (size_t i = fromIndex; i < toIndex; i++) {
	    if (!hasBit(i)) {
		return i;
	    }
	}
	return toIndex;	
    }
}

}
