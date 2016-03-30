#ifndef _OpenHashMap_hpp
#define _OpenHashMap_hpp

#include <iostream>
#include <iterator>
#include <type_traits>
#include <limits>
#include <assert.h>
#include "basic.hpp"
#include "Murmur3.hpp"
#include "Growing.hpp"
#include "Primes.hpp"

#include "HashComparator.hpp"
#include "AbstractHashMapEntry.hpp"

namespace PROJECT {

template<typename _K, typename _V, typename _Alloc> class OpenHashMap;

template<typename _K, typename _V> class OpenHashMapEntry : public AbstractHashMapEntry<_K,_V>
{
public:
    OpenHashMapEntry() : AbstractHashMapEntry<_K,_V>(), thisUsed(false) { }

    OpenHashMapEntry(const OpenHashMapEntry<_K,_V> &other)
	: AbstractHashMapEntry<_K,_V>(other.thisKey),
	  thisUsed(other.thisUsed) { }
    OpenHashMapEntry(const _K &k) : AbstractHashMapEntry<_K,_V>(k) { thisUsed = true; }
    OpenHashMapEntry(const _K &k, const _V &v) : AbstractHashMapEntry<_K,_V>(k,v) { thisUsed = true; }

    bool isUsed() const { return thisUsed; }
    void setUsed(bool used) { thisUsed = used; }

private:
    bool thisUsed;
};

template<typename _K, typename _V, class _Alloc> class OpenHashMapIterator
: std::iterator<std::forward_iterator_tag, OpenHashMapEntry<typename remove_cv<_K>::type,typename remove_cv<_V>::type > >
{
public:
    typedef typename remove_cv<_K>::type _KK;
    typedef typename remove_cv<_V>::type _VV;

    typedef OpenHashMapEntry<_KK, _VV> Entry;

    Entry & operator * () { return thisMap->getBucket(thisBucket); }
    Entry * operator -> () { return &thisMap->getBucket(thisBucket); }

    OpenHashMapIterator & operator ++ ()
    { advance(); return *this; }

    OpenHashMapIterator operator ++ (int)
    { advance(); return *this; }

    friend bool operator == (const OpenHashMapIterator &a,
			     const OpenHashMapIterator &b)
    {
	return a.thisMap == b.thisMap && a.thisBucket == b.thisBucket;
    }

    friend bool operator != (const OpenHashMapIterator &a,
			     const OpenHashMapIterator &b)
    {
	return a.thisMap != b.thisMap || a.thisBucket != b.thisBucket;
    }

    // One way conversion to const iterator
    operator OpenHashMapIterator<const _K, const _V, _Alloc> () const
    {
	return OpenHashMapIterator<const _K, const _V, _Alloc>(thisMap, thisBucket);
    }

    size_t getBucket() const
    {
	return thisBucket;
    }

private:
    void init();
    void advance();

    OpenHashMapIterator(OpenHashMap<_KK, _VV, _Alloc> *map, bool atEnd);

    OpenHashMapIterator(OpenHashMap<_KK, _VV, _Alloc> *map, size_t bucket)
	: thisMap(map), thisStartBucket(bucket), thisBucket(bucket)
    {
    }

    friend class OpenHashMap<_KK, _VV, _Alloc>;

    OpenHashMap<_KK, _VV, _Alloc> *thisMap;
    size_t thisStartBucket;
    size_t thisBucket;
};

template<typename _K, typename _V, class _Alloc = GrowingAllocator<OpenHashMapEntry<_K,_V> > > class OpenHashMap {
public:
    typedef OpenHashMapEntry<_K,_V> _E;
    typedef _K key_type;
    typedef _V value_type;
    typedef OpenHashMapIterator<_K,_V,_Alloc> iterator_type;

    friend class OpenHashMapIterator<_K, _V, _Alloc>;

    OpenHashMap(size_t initialCapacity = 17, _Alloc allocator = _Alloc())
    {
	thisAllocator = allocator;
	init(initialCapacity);
    }

    void init(size_t initialCapacity)
    {
	thisAutoRehash = true;
	thisNeedRehash = false;
	thisNumRehash = 0;
	thisNumCollisions = 0;
	thisNumEntries = 0;
	thisNumBuckets = initialCapacity;
	thisBuckets = thisAllocator.allocate(thisNumBuckets);
	thisAllocator.confirmRebased();
	for (int i = 0; i < thisNumBuckets; i++) {
	    thisBuckets[i] = OpenHashMapEntry<_K,_V>();
	}
    }

    void checkRehash()
    {
	if (thisNeedRehash) {
	    rehash(Primes::nextPrime(thisNumBuckets*4+1));
	}
    }

    bool needRehash() const
    {
	return thisNeedRehash;
    }

    // Assumes that 'k' does not exist in hash table.
    // (This means that first unused slot available is used.)
    HashMapLValue<_K,_V> operator [] (const _K &k)
    {
	checkRehash();

	size_t startIndex = HashOf<_K>::value(k) % thisNumBuckets;
	size_t bucketIndex = startIndex;
	do {
	    if (thisBuckets[bucketIndex].isUsed() &&
		thisBuckets[bucketIndex].getKey() == k) {
		return HashMapLValue<_K,_V>(&thisBuckets[bucketIndex]);
	    }
	    if (!thisBuckets[bucketIndex].isUsed()) {
		thisBuckets[bucketIndex] = _E(k);
		thisBuckets[bucketIndex].setUsed(true);
		thisNumEntries++;
		if (thisAutoRehash && thisNumEntries > thisNumBuckets / 4) {
		    thisNeedRehash = true;
		}
		return HashMapLValue<_K,_V>(&thisBuckets[bucketIndex]);
	    }
	    thisNumCollisions++;
	    bucketIndex++;
	    if (bucketIndex >= thisNumBuckets) bucketIndex = 0;
	} while (bucketIndex != startIndex);
	assert("Open hash table is full." == NULL);
	return HashMapLValue<_K,_V>(NULL);
    }

    void put(const _K &k, const _V &v)
    {
	operator [] (k) = v;
    }

    const _V & operator [] (const _K &k) const
    {
	size_t startIndex = HashOf<_K>::value(k) % thisNumBuckets;
	size_t bucketIndex = startIndex;
	do {
	    if (thisBuckets[bucketIndex].isUsed() &&
		thisBuckets[bucketIndex].getKey() == k) {
		return thisBuckets[bucketIndex].getValue();
	    }
	    bucketIndex++;
	    if (bucketIndex >= thisNumBuckets) bucketIndex = 0;
	} while (bucketIndex != startIndex);
	assert("Couldn't find entry in open hash table." == NULL);
	return thisBuckets[bucketIndex].getValue();
    }

    const _V * get(const _K &k) const
    {
	return & operator [] (k);
    }

    void remove(const _K &k)
    {
	size_t startIndex = HashOf<_K>::value(k) % thisNumBuckets;
	size_t bucketIndex = startIndex;
	do {
	    if (thisBuckets[bucketIndex].isUsed() &&
		thisBuckets[bucketIndex].getKey() == k) {
		thisBuckets[bucketIndex].setUsed(false);
		--thisNumEntries;
		return;
	    }
	    bucketIndex++;
	    if (bucketIndex >= thisNumBuckets) bucketIndex = 0;
	} while (bucketIndex != startIndex);
	assert("Couldn't find entry in open hash table." == NULL);
    }

    void rehash(size_t newNumBuckets)
    {
	++thisNumRehash;

	thisAutoRehash = false;
	thisNeedRehash = false;
	_E *oldBuckets = thisBuckets;
	size_t oldNumBuckets = thisNumBuckets;
	_E *newBuckets = thisAllocator.allocate(newNumBuckets);
	if (thisAllocator.hasRebased()) {
	    oldBuckets = thisAllocator.rebase(oldBuckets);
	    thisBuckets = oldBuckets;
	    thisAllocator.confirmRebased();
	}
	thisNumEntries = 0;

	iterator_type itEnd = end();
	for (iterator_type it = begin(); it != itEnd; ++it) {
	    _K key = it->getKey();
	    _V value = it->getValue();
	    thisBuckets = newBuckets;
	    thisNumBuckets = newNumBuckets;
	    put(key, value);
	    thisBuckets = oldBuckets;
	    thisNumBuckets = oldNumBuckets;
	}

	thisAllocator.deallocate(thisBuckets, thisNumBuckets);
	thisBuckets = newBuckets;
	thisNumBuckets = newNumBuckets;

	thisAutoRehash = true;
    }

    size_t numRehash() const {
	return thisNumRehash;
    }

    size_t numCollisions() const {
	return thisNumCollisions;
    }

    size_t numEntries() const {
	return thisNumEntries;
    }

    typedef OpenHashMapIterator<_K, _V, _Alloc> iterator;
    typedef OpenHashMapIterator<const _K, const _V, _Alloc> const_iterator;

    iterator begin() {
	return iterator(this, false);
    }

    iterator end() {
	return iterator(this, true);
    }

    const_iterator begin() const {
	return const_iterator(this, false);
    }

    const_iterator end() const {
	return const_iterator(this, true);
    }

    void clear() {
	for (int i = 0; i < thisNumBuckets; i++) {
	    thisBuckets[i].setUsed(false);
	}
	thisNumEntries = 0;
    }

    friend std::ostream & operator << (std::ostream &out, OpenHashMap<_K, _V> &map)
    {
	map.print(out);
	return out;
    }

    void print(std::ostream &out) const
    {
	out << "[";
	bool first = true;
	for (int i = 0; i < thisNumBuckets; i++) {
	    const _E &entry = thisBuckets[i];
	    if (entry.isUsed()) {
		if (!first) out << ", ";
		out << entry.getKey() << ":" << entry.getValue();
		first = false;
	    }
	}
	out << "]";
    }

    void printInternal(std::ostream &out) const
    {
	for (size_t i = 0; i < thisNumBuckets; i++) {
	    out << "(" << i << "): ";
	    const _E &entry = thisBuckets[i];
	    bool first = true;
	    out << "[";
	    if (entry.isUsed()) {
		if (!first) out << ", ";
		out << "(" << entry.getKey() << ":" << entry.getValue() << ")";
	    } else {
		if (!first) out << ", ";
		out << "(NULL)";
	    }
	    first = false;
	    out << "]\n";
	}
    }

    _Alloc & getAllocator() {
	return thisAllocator;
    }

private:

    const _E & getBucket(size_t bucketIndex) const
    {
	return thisBuckets[bucketIndex];
    }

    _E & getBucket(size_t bucketIndex)
    {
	return thisBuckets[bucketIndex];
    }

    _Alloc thisAllocator;
    size_t thisNumBuckets;
    OpenHashMapEntry<_K,_V> *thisBuckets;
    size_t thisNumEntries;
    size_t thisNumRehash;
    size_t thisNumCollisions;
    bool thisAutoRehash;
    bool thisNeedRehash;
};

template<typename _K, typename _V, typename _Alloc> OpenHashMapIterator<_K, _V, _Alloc>::OpenHashMapIterator(OpenHashMap<typename remove_cv<_K>::type, typename remove_cv<_V>::type, _Alloc > *map, bool atEnd) : thisMap(map)
{
    thisBucket = 0;
    thisStartBucket = 0;
    if (atEnd) {
	thisMap = NULL;
	thisBucket = std::numeric_limits<size_t>::max();
    } else {
	init();
    }
}

template<typename _K, typename _V, typename _Alloc> void OpenHashMapIterator<_K,_V, _Alloc>::advance()
{
    do {
	thisBucket++;
	if (thisBucket >= thisMap->thisNumBuckets) {
	    thisBucket = 0;
	}
	if (thisBucket == thisStartBucket) {
	    thisMap = NULL;
	    thisBucket = std::numeric_limits<size_t>::max();
	    thisStartBucket = 0;
	    return;
	}
    } while (!thisMap->getBucket(thisBucket).isUsed());
}

template<typename _K, typename _V, typename _Alloc> void OpenHashMapIterator<_K,_V, _Alloc>::init()
{
    if (!thisMap->thisBuckets[thisBucket].isUsed()) {
	advance();
    }
}

}

#endif

