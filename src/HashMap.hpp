#ifndef _HashMap_hpp
#define _HashMap_hpp

#include <iostream>
#include "basic.hpp"
#include "Murmur3.hpp"

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

    HashMapEntry(const _K &k, const _V &v) : thisRest(NULL), thisKey(k), thisValue(v) {
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

    friend class MyHashMap;
};

template<typename _K, typename _V> class HashMap {
public:
    typedef _K key_type;
    typedef _V value_type;

    HashMap(size_t initialCapacity = 17)
    {
	thisNumEntries = 0;
	thisNumBuckets = initialCapacity;
	thisBuckets = new HashMapEntry<_K,_V> * [thisNumBuckets];
	for (int i = 0; i < thisNumBuckets; i++) {
	    thisBuckets[i] = NULL;
	}
    }

    bool put(const _K &k, const _V &v)
    {
	size_t index = HashOf<_K>::value(k) % thisNumBuckets;
	HashMapEntry<_K,_V> *entry = thisBuckets[index];
	HashMapEntry<_K,_V> *prev = NULL;
	while (entry != NULL) {
	    if (entry->getKey() == k) {
		entry->setValue(v);
		return false;
	    }
	    prev = entry;
	    entry = entry->getRest();
	}
	HashMapEntry<_K,_V> *newEntry = new HashMapEntry<_K,_V>(k,v);
	if (prev != NULL) {
	    prev->setRest(newEntry);
	} else {
	    thisBuckets[index] = newEntry;
	}
	thisNumEntries++;
	return true;
    }

    void printInternal(std::ostream &out)
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
    size_t thisNumBuckets;
    HashMapEntry<_K,_V> **thisBuckets;
    size_t thisNumEntries;
};

#endif
