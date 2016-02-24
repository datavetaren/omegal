#ifndef _HashMap_hpp
#define _HashMap_hpp

#include <iostream>
#include <iterator>
#include <type_traits>
#include <assert.h>
#include "basic.hpp"
#include "Murmur3.hpp"
#include "Growing.hpp"
#include "Primes.hpp"

namespace PROJECT {

static const uint32_t HASHMAP_SEED = 38136192;

template<typename _K> struct HashOf {
    static uint32_t value(const _K &other);
};

template<> struct HashOf<int32_t> {
    static uint32_t value(const int32_t &k) { return Murmur3(&k, sizeof(k), HASHMAP_SEED); }
};

template<> struct HashOf<int64_t> {
    static uint32_t value(const int64_t &k) { return Murmur3(&k, sizeof(k), HASHMAP_SEED); }
};

template<> struct HashOf<void *> {
    static uint32_t value(const int64_t &k) { return Murmur3(&k, sizeof(k), HASHMAP_SEED); }
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


template<typename _K, typename _V> class HashMapEntry {
public:
    typedef _K key_type;
    typedef _V value_type;
    typedef HashMapEntry<_K,_V> entry_type;

    HashMapEntry(const _K &k)
	: thisRest(0), thisKey(k) {
    }

    HashMapEntry(const _K &k, const _V &v) 
       : thisRest(0), thisKey(k), thisValue(v) {
    }

    void init(const _K &k)
    {
	thisRest = 0;
	thisKey = k;
    }

    void init(const _K &k, const _V &v)
    {
	thisRest = 0;
	thisKey = k;
	thisValue = v;
    }

    const _K & getKey() const {
	return thisKey;
    }


    const _V & getValue() const {
	return thisValue;
    }

    _V & getValueRef() {
	return thisValue;
    }

    void setValue(const _V &value) {
	thisValue = value;
    }

    NativeType getRest() {
	return thisRest;
    }

    void setRest(NativeType rest) {
	thisRest = rest;
    }

private:
    NativeType thisRest;
    _K thisKey;
    _V thisValue;
};

template<typename _K, typename _V> class HashMapLValue {
public:
    typedef _K key_type;
    typedef _V value_type;

    HashMapLValue(HashMapEntry<_K, _V> *entry)
	: thisEntry(entry) { }

    void operator = (const _V &v)
    {
	thisEntry->setValue(v);
    }

    operator _V () const
    {
	return thisEntry->getValue();
    }

private:
    HashMapEntry<_K, _V> *thisEntry;
};

template<typename _K, typename _V, typename _Alloc> class HashMap;

template<typename _K, typename _V, class _Alloc> class HashMapIterator
: std::iterator<std::forward_iterator_tag, HashMapEntry<typename remove_cv<_K>::type,typename remove_cv<_V>::type > >
{
public:
    typedef typename remove_cv<_K>::type _KK;
    typedef typename remove_cv<_V>::type _VV;

    typedef HashMapEntry<_KK, _VV> Entry;

    Entry & operator * () { return *thisCurrent; }
    Entry * operator -> () { return thisCurrent; }

    HashMapIterator & operator ++ ()
    { advance(); return *this; }

    HashMapIterator operator ++ (int)
    { advance(); return *this; }

    friend bool operator == (const HashMapIterator &a, const HashMapIterator &b)
    {
	return a.thisCurrent == b.thisCurrent;
    }

    friend bool operator != (const HashMapIterator &a, const HashMapIterator &b)
    {
	return a.thisCurrent != b.thisCurrent;
    }

    // One way conversion to const iterator
    operator HashMapIterator<const _K, const _V, _Alloc> () const
    {
	return HashMapIterator<const _K, const _V, _Alloc>(thisMap, thisBucket, thisCurrent);
    }

private:
    void advance();

    HashMapIterator(HashMap<_KK, _VV, _Alloc> *map, bool atEnd);

    HashMapIterator(HashMap<_KK, _VV, _Alloc> *map, size_t bucket, Entry *current)
	: thisMap(map), thisBucket(bucket), thisCurrent(current)
    {
    }

    friend class HashMap<_KK, _VV, _Alloc>;

    HashMap<_KK, _VV, _Alloc> *thisMap;
    size_t thisBucket;
    Entry *thisCurrent;
};

template<typename _K, typename _V, class _Alloc = GrowingAllocator<NativeType> > class HashMap {
public:
    typedef HashMapEntry<_K,_V> _E;
    typedef _K key_type;
    typedef _V value_type;
    typedef HashMapIterator<_K,_V,_Alloc> iterator_type;

    friend class HashMapIterator<_K, _V, _Alloc>;

    HashMap(size_t initialCapacity = 17, _Alloc allocator = _Alloc())
    {
	thisAutoRehash = true;
	thisNeedRehash = false;
	thisNumRehash = 0;
	thisNumCollisions = 0;
	thisAllocator = allocator;
	thisNumEntries = 0;
	thisNumBuckets = initialCapacity;
	thisBuckets = thisAllocator.allocate(thisNumBuckets);
	thisAllocator.confirmRebased();
	for (int i = 0; i < thisNumBuckets; i++) {
	    thisBuckets[i] = 0;
	}
    }

    void checkRehash()
    {
	if (thisNeedRehash) {
	    rehash(Primes::nextPrime(thisNumBuckets*2+1));
	}
    }

    bool needRehash() const
    {
	return thisNeedRehash;
    }

    const _V operator [] (const _K &k) const {
	_E *prev = NULL;
	size_t bucketIndex = 0;
	bool doNew = false;
	_E *entry = const_cast<HashMap<_K,_V,_Alloc> *>(this)->findRef(k, doNew, prev, bucketIndex);
	assert(entry != NULL);
	return entry->getValue();
    }

    HashMapLValue<_K,_V> operator [] (const _K &k) {
	checkRehash();
	_E *prev = NULL;
	size_t bucketIndex = 0;
	bool doNew = true;
	HashMapLValue<_K,_V> r(findRef(k, doNew, prev, bucketIndex));
	return r;
    }

    bool put(const _K &k, const _V &v)
    {
	checkRehash();
	int before = thisNumEntries;
	_E *prev = NULL;
	size_t bucketIndex = 0;
	bool doNew = true;
	_E *entry = findRef(k, doNew, prev, bucketIndex);
	entry->setValue(v);
	return before != thisNumEntries;
    }

    const _V * get(const _K &k) const
    {
	_E *prev = NULL;
	size_t bucketIndex = 0;
	bool doNew = false;
	_E *entry = const_cast<HashMap<_K,_V,_Alloc> *>(this)->findRef(k, doNew, prev, bucketIndex);
	if (entry == NULL) {
	    return NULL;
	}
	return &entry->getValue();
    }

    _V & getRef(const _K &k, const _V &defaultVal)
    {
	checkRehash();
	_E *prev = NULL;
	size_t bucketIndex = 0;
	bool doNew = true;
	_E *entry = findRef(k, doNew, prev, bucketIndex);
	if (doNew) {
	    entry->setValue(defaultVal);
	}
	return entry->getValueRef();
    }

    bool remove(const _K &k)
    {
	checkRehash();
	_E *prev = NULL;
	size_t bucketIndex = 0;
	bool doNew = false;
	_E *entry = findRef(k, doNew, prev, bucketIndex);
	if (entry == NULL) {
	    return false;
	}
	_E *follow = getEntry(entry->getRest());

	thisAllocator.deallocate(reinterpret_cast<NativeType *>(follow),
			 (sizeof(_E)+sizeof(NativeType)+1)/sizeof(NativeType));

	if (prev != NULL) {
    	    NativeType followRel =
	      thisAllocator.toRelative(reinterpret_cast<NativeType *>(follow));
	    prev->setRest(followRel);
	} else {
	    setBucket(bucketIndex, follow);
	}
	--thisNumEntries;
	return true;
    }

    void rehash(size_t newNumBuckets)
    {
	++thisNumRehash;

	thisAutoRehash = false;
	thisNeedRehash = false;
	NativeType *oldBuckets = thisBuckets;
	size_t oldNumBuckets = thisNumBuckets;
	NativeType *newBuckets = thisAllocator.allocate(newNumBuckets);
	for (int i = 0; i < newNumBuckets; i++) {
	    newBuckets[i] = 0;
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

	for (int i = 0; i < thisNumBuckets; i++) {
	    _E *entry = getBucket(i);
	    _E *follow = NULL;
	    while (entry != NULL) {
		follow = getEntry(entry->getRest());
		thisAllocator.deallocate(
			 reinterpret_cast<NativeType *>(entry),
			 (sizeof(_E)+sizeof(NativeType)+1)/sizeof(NativeType));
		entry = follow;
	    }
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

    typedef HashMapIterator<_K, _V, _Alloc> iterator;
    typedef HashMapIterator<const _K, const _V, _Alloc> const_iterator;

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


    friend std::ostream & operator << (std::ostream &out, HashMap<_K, _V> &map)
    {
	map.print(out);
	return out;
    }

    void print(std::ostream &out) const
    {
	out << "[";
	bool first = true;
	for (int i = 0; i < thisNumBuckets; i++) {
	    _E *entry = getBucket(i);
	    while (entry != NULL) {
		if (!first) out << ", ";
		out << entry->getKey() << ":" << entry->getValue();
		first = false;
		entry = getEntry(entry->getRest());
	    }
	}
	out << "]";
    }

    void printInternal(std::ostream &out) const
    {
	for (size_t i = 0; i < thisNumBuckets; i++) {
	    out << "(" << i << "): ";
	    _E *entry = getBucket(i);
	    bool first = true;
	    out << "[";
	    while (entry != NULL) {
		if (!first) out << ", ";
		out << "(" << entry->getKey() << ":" << entry->getValue() << ")";
		first = false;
		entry = getEntry(entry->getRest());
	    }
	    out << "]\n";
	}
    }

    size_t getBucketLength(size_t bucketIndex) const
    {
	_E *entry = getBucket(bucketIndex);
	size_t cnt = 0;
	while (entry != NULL) {
	    cnt++;
	    entry = getEntry(entry->getRest());
	}
	return cnt;
    }

private:

    _E * getBucket(size_t bucketIndex) const
    {
	return getEntry(thisBuckets[bucketIndex]);
    }

    void setBucket(size_t bucketIndex, _E *e)
    {
	thisBuckets[bucketIndex] = thisAllocator.toRelative(reinterpret_cast<NativeType *>(e));
    }

    _E * getEntry(size_t rel) const
    {
	return reinterpret_cast<_E *>(thisAllocator.toAbsolute(rel));
    }

    _E * findRef(const _K &k, bool &doNew, _E *&prev, size_t &bucketIndex) {
	uint32_t index = HashOf<_K>::value(k) % thisNumBuckets;
	bucketIndex = index;
	_E *entry = getBucket(index);
        _E *pe = NULL;

	while (entry != NULL) {
	    if (entry->getKey() == k) {
		prev = pe;
		doNew = false;
		return entry;
	    }
	    pe = entry;
	    entry = getEntry(entry->getRest());
	}
	if (!doNew) {
	    return NULL;
	}
	_E *newE = newEntry(k);
	if (pe != NULL) {
	    thisNumCollisions++;
	    pe->setRest(thisAllocator.toRelative(reinterpret_cast<NativeType *>(newE)));
	} else {
	    setBucket(index, newE);
	}
	prev = pe;
	thisNumEntries++;

	if (thisAutoRehash && thisNumEntries > thisNumBuckets / 2) {
	    thisNeedRehash = true;
	}
	return newE;
    }

    _E * newEntry(const _K &key)
    {
	bool oldRebased = thisAllocator.hasRebased();
	(void)oldRebased;
	_E *newEntry = reinterpret_cast<_E *>(thisAllocator.allocate(thisAllocator.getNum(sizeof(_E))));
        newEntry->init(key);
	if (thisAllocator.hasRebased()) {
	    thisBuckets = thisAllocator.rebase(thisBuckets);
	    thisAllocator.confirmRebased();
	}
        return newEntry;
    }

    _Alloc thisAllocator;
    size_t thisNumBuckets;
    NativeType *thisBuckets;
    size_t thisNumEntries;
    size_t thisNumRehash;
    size_t thisNumCollisions;
    bool thisAutoRehash;
    bool thisNeedRehash;
};

template<typename _K, typename _V, typename _Alloc> HashMapIterator<_K, _V, _Alloc>::HashMapIterator(HashMap<typename remove_cv<_K>::type, typename remove_cv<_V>::type, _Alloc > *map, bool atEnd) : thisMap(map)
{
    if (atEnd) {
	thisCurrent = NULL;
	thisBucket = map->thisNumBuckets - 1;
    } else {
	thisBucket = 0;
	thisCurrent = map->getBucket(thisBucket);
	while (thisCurrent == NULL && thisBucket < map->thisNumBuckets - 1) {
	    thisBucket++;
	    thisCurrent = map->getBucket(thisBucket);
	}
    }
}

template<typename _K, typename _V, typename _Alloc> void HashMapIterator<_K,_V, _Alloc>::advance()
{
    if (thisCurrent == NULL) {
	return;
    }
    thisCurrent = thisMap->getEntry(thisCurrent->getRest());
    size_t n = thisMap->thisNumBuckets;
    while (thisCurrent == NULL && thisBucket < n - 1) {
	thisBucket++;
	thisCurrent = thisMap->getBucket(thisBucket);
    }
}

}

#endif
