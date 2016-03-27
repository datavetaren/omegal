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

void BitMap::clearBits(size_t fromIndex, size_t toIndex)
{
    setBits(fromIndex, toIndex, false);
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
    size_t toWord = toIndex / NativeTypeBits;

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

bool BitMap::isAllBits(size_t fromIndex, size_t toIndex, bool value) const
{
    size_t fromWord = fromIndex / NativeTypeBits;
    size_t toWord = toIndex / NativeTypeBits;
    if (fromWord + 1 < toWord) {
	NativeType checkVal = value ? (NativeType)-1 : 0;
	for (size_t i = fromWord + 1; i < toWord; i++) {
	    if (thisData[i] != checkVal) {
		return false;
	    }
	}
    }
    size_t firstPart = (fromWord + 1) * NativeTypeBits;
    if (firstPart > toIndex) firstPart = toIndex;

    for (size_t i = fromIndex; i < firstPart; i++) {
	if (hasBit(i) != value) {
	    return false;
	}
    }
    size_t lastPart = toWord * NativeTypeBits;
    for (size_t i = lastPart; i < toIndex; i++) {
	if (hasBit(i) != value) {
	    return false;
	}
    }
    return true;
}

size_t BitMap::findBits(size_t fromIndex, size_t toIndex, size_t len, bool value) const
{
    // This is a Boyer-Moore inspired search. Because its a binary alphabet
    // it becomes much simpler.
    for (size_t i = fromIndex; i < toIndex-len;) {
	int j;
	for (j = len - 1; j >= 0; j--) {
	    if (hasBit(i+j) != value) {
		// Mismatch found. Now jump past this point.
		i += j+1;
		break;
	    }
	}
	if (j == -1) {
	    // We've found a match!
	    return i;
	}
    }

    // We ran out of luck.
    return toIndex;
}

}
