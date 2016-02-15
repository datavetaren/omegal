#ifndef _Growing_hpp
#define _Growing_hpp

#include "basic.hpp"

namespace PROJECT {

template<typename T> class GrowingAllocator : public std::allocator<T> {
public:
    typedef size_t size_type;
    typedef T * pointer;
    typedef const T * const_pointer;

    class Ptr {
    public:
	Ptr(GrowingAllocator<T> &ga, T *ptr)
	    : thisGrowingAllocator(ga), thisRelative(ga.toRelative(ptr)) { }
	Ptr(GrowingAllocator<T> &ga, size_t relative)
	    : thisGrowingAllocator(ga), thisRelative(relative) { }

	T * operator * () const {
	    return thisGrowingAllocator.toAbsolute(thisRelative);
	}

	size_t toRelative() const {
	    return thisRelative;
	}

    private:
	GrowingAllocator<T> &thisGrowingAllocator;
	size_t thisRelative;
    };

    GrowingAllocator(size_t initialCapacity = 4096) throw() : std::allocator<T>()
    {
	thisCapacity = initialCapacity;
	thisSize = 0;
	thisBase = new T [thisCapacity+1];
	thisData = &thisBase[1];
    }
    
    inline size_t getNum(size_t bytes)
    {
	return bytes / sizeof(T);
    }

    inline pointer allocate(size_type num, const void *hint=0)
    {
	ensureCapacity(num);
	T *r = &thisData[thisSize];
	thisSize += num;
	return r;
    }

    inline void deallocate(pointer p, size_type)
    {
	// We don't do anything on deallocate. The intention
	// is to do garbage collection.
    }

    inline size_t toRelative(const T *p) const
    {
	if (p == NULL) {
	    return 0;
	} else {
	    return static_cast<size_t>(p - thisBase);
	}
    }

    inline T * toAbsolute(size_t rel) const
    {
	if (rel == 0) {
	    return NULL;
	} else {
	    return &thisBase[rel];
	}
    }

    inline void ensureCapacity(size_t num)
    {
	if (thisSize + num > thisCapacity) {
	    grow(num);
	}
    }

    inline size_t getSize() const
    {
	return thisSize;
    }

    void grow(size_t num)
    {
	while (thisSize + num > thisCapacity) {
	    thisCapacity *= 2;
	}
	T *newBase = new T [thisCapacity+1];
	T *newData = &newBase[1];
	memcpy(newData, thisData, sizeof(T)*thisSize);
	delete [] thisBase;
	thisBase = newBase;
	thisData = newData;
    }

private:
    size_t thisCapacity;
    size_t thisSize;
    T *thisBase;
    T *thisData;
};

}
#endif
