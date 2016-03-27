#ifndef _BitMap_hpp
#define _BitMap_hpp

#include <string>
#include "basic.hpp"
#include "Growing.hpp"

namespace PROJECT {

class BitMap;

class BitMapIterator : std::iterator<std::forward_iterator_tag, size_t>
{
public:
    size_t operator * () { return thisIndex; }
    size_t operator -> () { return thisIndex; }

    BitMapIterator & operator ++ ()
    { advance(); return *this; }

    BitMapIterator operator ++ (int)
    { advance(); return *this; }

    BitMapIterator operator + (size_t offset);

    friend bool operator == (const BitMapIterator &a, const BitMapIterator &b)
    {
	return a.thisIndex == b.thisIndex;
    }

    friend bool operator != (const BitMapIterator &a, const BitMapIterator &b)
    {
	return a.thisIndex < b.thisIndex;
    }

private:
    void advance();

    inline BitMapIterator(const BitMap *bm, bool enabled, bool atEnd, size_t offset);

    friend class BitMap;

    const BitMap *thisBitMap;
    bool thisEnabled;
    size_t thisIndex;
};

/*
 * BitMap. This class implements a bit set, but with additional
 * useful utilities for the garbage collector.
 * 
 */
class BitMap : public GrowingAllocator<NativeType> {
public:
    BitMap(size_t capacity = 65536);
    ~BitMap();

    void clear();

    void setSize(size_t size);
    size_t getSize() const;

    bool hasBit(size_t index) const;
    void setBit(size_t index, bool value = true);

    // Set bits in the range fromIndex <= x < toIndex
    void clearBits(size_t fromIndex, size_t toIndex);
    void setBits(size_t fromIndex, size_t toIndex, bool value = true);

    bool isAllBits(size_t fromIndex, size_t toIndex, bool value = true) const;
    
    // Find next bit that is of value in the given interval
    size_t findBit(size_t fromIndex, size_t toIndex, bool value = true) const;

    size_t findBits(size_t fromIndex, size_t toIndex, size_t len,
		    bool value = true) const;

    // Iterate over set or cleared bits
    typedef BitMapIterator iterator;
    iterator begin(bool enabled = true, size_t offset = 0) {
	return iterator(this, enabled, false, offset);
    }

    iterator end(bool enabled = true) {
	return iterator(this, enabled, true, 0);
    }

    void print(std::ostream &out) const;
    std::string toString() const;

private:
    NativeType *thisData;
    size_t thisSize;
};

inline BitMapIterator::BitMapIterator(const BitMap *bitmap,
				      bool enabled,
				      bool atEnd,
				      size_t offset)
    : thisBitMap(bitmap),
      thisEnabled(enabled),
      thisIndex( atEnd ? bitmap->getSize() : offset)
{
    if (!atEnd) {
	if (thisBitMap->hasBit(thisIndex) != enabled) {
	    advance();
	}
    }
}

inline BitMapIterator BitMapIterator::operator + (size_t offset)
{
    BitMapIterator it = *this;
    it.thisIndex += offset;
    size_t n = thisBitMap->getSize();
    if (it.thisIndex > n) {
	it.thisIndex = n;
    }
    return it;
}

inline void BitMapIterator::advance()
{
    thisIndex = thisBitMap->findBit(thisIndex+1,
				    thisBitMap->getSize(),
				    thisEnabled);
}

}

#endif
