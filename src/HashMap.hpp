#ifndef _HashMap_hpp
#define _HashMap_hpp

#include <iostream>
#include <iterator>
#include <type_traits>
#include "basic.hpp"
#include "Murmur3.hpp"

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

    HashMapEntry(const _K &k)
	: thisRest(NULL), thisKey(k) {
    }

    HashMapEntry(const _K &k, const _V &v) 
       : thisRest(NULL), thisKey(k), thisValue(v) {
    }

    const _K & getKey() const {
	return thisKey;
    }

    const _V & getValue() const {
	return thisValue;
    }

    void setValue(const _V &value) {
	thisValue = value;
    }

    HashMapEntry<_K,_V> * getRest() {
	return thisRest;
    }

    void setRest(HashMapEntry<_K,_V> *rest) {
	thisRest = rest;
    }

private:
    HashMapEntry<_K,_V> *thisRest;
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

template<typename _K, typename _V> class HashMap;

template<typename _K, typename _V> class HashMapIterator
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
    operator HashMapIterator<const _K, const _V> () const
    {
	return HashMapIterator<const _K, const _V>(thisMap, thisBucket, thisCurrent);
    }

private:
    void advance();

    HashMapIterator(HashMap<_KK, _VV> *map, bool atEnd);

    HashMapIterator(HashMap<_KK, _VV> *map, size_t bucket, Entry *current)
	: thisMap(map), thisBucket(bucket), thisCurrent(current)
    {
    }

    friend class HashMap<_KK, _VV>;

    HashMap<_KK, _VV> *thisMap;
    size_t thisBucket;
    Entry *thisCurrent;
};

template<typename _K, typename _V> class HashMap {
public:
    typedef _K key_type;
    typedef _V value_type;

    friend class HashMapIterator<_K, _V>;

    HashMap(size_t initialCapacity = 17)
    {
	thisNumEntries = 0;
	thisNumBuckets = initialCapacity;
	thisBuckets = new HashMapEntry<_K,_V> * [thisNumBuckets];
	for (int i = 0; i < thisNumBuckets; i++) {
	    thisBuckets[i] = NULL;
	}
    }

    const _V operator [] (const _K &k) const {
	HashMapEntry<_K,_V> *entry = findRef(k, false);
	assert(entry != NULL);
	return entry->getValue();
    }

    HashMapLValue<_K,_V> operator [] (const _K &k) {
	return HashMapLValue<_K,_V>(findRef(k, true));
    }

    bool put(const _K &k, const _V &v)
    {
	int before = thisNumEntries;
	HashMapEntry<_K,_V> *entry = findRef(k, true);
	entry->setValue(v);
	return before != thisNumEntries;
    }

    const _V * get(const _K &k)
    {
	HashMapEntry<_K,_V> *entry = findRef(k, false);
	if (entry == NULL) {
	    return NULL;
	}
	return &entry->getValue();
    }

    size_t numEntries() const {
	return thisNumEntries;
    }

    typedef HashMapIterator<_K, _V> iterator;
    typedef HashMapIterator<const _K, const _V> const_iterator;

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


    friend std::ostream & operator << (std::ostream &out, HashMap<_K, _V> &map) {
	map.print(out);
	return out;
    }

    void print(std::ostream &out) const
    {
	out << "[";
	bool first = true;
	for (int i = 0; i < thisNumBuckets; i++) {
	    HashMapEntry<_K,_V> *entry = thisBuckets[i];
	    while (entry != NULL) {
		if (!first) out << ", ";
		out << entry->getKey() << ":" << entry->getValue();
		first = false;
		entry = entry->getRest();
	    }
	}
	out << "]\n";
    }

    void printInternal(std::ostream &out) const
    {
	for (int i = 0; i < thisNumBuckets; i++) {
	    out << "(" << i << "): ";
	    HashMapEntry<_K,_V> *entry = thisBuckets[i];
	    bool first = true;
	    out << "[";
	    while (entry != NULL) {
		if (!first) out << ", ";
		out << "(" << entry->getKey() << ":" << entry->getValue() << ")";
		first = false;
		entry = entry->getRest();
	    }
	    out << "]\n";
	}
    }

private:
    HashMapEntry<_K,_V> * findRef(const _K &k, bool doNew) {
	uint32_t index = HashOf<_K>::value(k) % thisNumBuckets;
	HashMapEntry<_K,_V> *entry = thisBuckets[index];
        HashMapEntry<_K,_V> *prev = NULL;
	while (entry != NULL) {
	    if (entry->getKey() == k) {
		return entry;
	    }
	    prev = entry;
	    entry = entry->getRest();
	}
	if (!doNew) {
	    return NULL;
	}
	HashMapEntry<_K,_V> *newEntry = new HashMapEntry<_K,_V>(k);
	if (prev != NULL) {
	    prev->setRest(newEntry);
	} else {
	    thisBuckets[index] = newEntry;
	}
	thisNumEntries++;
	return newEntry;
    }

    size_t thisNumBuckets;
    HashMapEntry<_K,_V> **thisBuckets;
    size_t thisNumEntries;
};

template<typename _K, typename _V> HashMapIterator<_K, _V>::HashMapIterator(HashMap<typename remove_cv<_K>::type, typename remove_cv<_V>::type > *map, bool atEnd) : thisMap(map)
{
    if (atEnd) {
	thisCurrent = NULL;
	thisBucket = map->thisNumBuckets - 1;
    } else {
	thisBucket = 0;
	thisCurrent = map->thisBuckets[thisBucket];
	while (thisCurrent == NULL && thisBucket < map->thisNumBuckets - 1) {
	    thisBucket++;
	    thisCurrent = map->thisBuckets[thisBucket];
	}
    }
}

template<typename _K, typename _V> void HashMapIterator<_K,_V>::advance()
{
    if (thisCurrent == NULL) {
	return;
    }
    thisCurrent = thisCurrent->getRest();
    size_t n = thisMap->thisNumBuckets;
    while (thisCurrent == NULL && thisBucket < n - 1) {
	thisBucket++;
	thisCurrent = thisMap->thisBuckets[thisBucket];
    }
}

}

#endif
