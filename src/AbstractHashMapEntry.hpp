#ifndef _AbstractHashMapEntry_hpp
#define _AbstractHashMapEntry_hpp

#include "basic.hpp"
#include "Murmur3.hpp"

namespace PROJECT {

template<typename _K, typename _V> class AbstractHashMapEntry {
public:
    typedef _K key_type;
    typedef _V value_type;
    typedef AbstractHashMapEntry<_K,_V> entry_type;

    AbstractHashMapEntry() : thisKey(), thisValue()
    {
    }

    static void * operator new (size_t sz, AbstractHashMapEntry<_K,_V> *p) throw()
    {
	(void)sz;
	return reinterpret_cast<void *>(p);
    }

    static void operator delete(void *, AbstractHashMapEntry<_K,_V> *) throw()
    {
        // Don't do anything. Silence warnings.
    }

    AbstractHashMapEntry(const _K &k)
	: thisKey(k), thisValue() {
    }

    AbstractHashMapEntry(const _K &k, const _V &v) 
       : thisKey(k), thisValue(v) {
    }

    void init(const _K &k)
    {
	thisKey = k;
    }

    void init(const _K &k, const _V &v)
    {
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

private:
    _K thisKey;
    _V thisValue;
};

template<typename _K, typename _V> class HashMapLValue {
public:
    typedef _K key_type;
    typedef _V value_type;

    HashMapLValue(AbstractHashMapEntry<_K, _V> *entry)
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
    AbstractHashMapEntry<_K, _V> *thisEntry;
};

}

#endif
