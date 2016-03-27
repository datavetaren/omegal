#ifndef _HashMapEntry_hpp
#define _HashMapEntry_hpp

#include "basic.hpp"
#include "Murmur3.hpp"

namespace PROJECT {

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

    _K & getKey() {
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

}

#endif
