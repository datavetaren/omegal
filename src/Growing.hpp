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
	thisBase = new T [thisCapacity];
	thisData = &thisBase[1];
	thisOldBase = NULL;
    }

    GrowingAllocator(const GrowingAllocator &other)
    {
	moveFrom(const_cast<GrowingAllocator &>(other));
    }

    ~GrowingAllocator()
    {
	delete [] thisBase;
    }

    void operator = (const GrowingAllocator &other)
    {
	moveFrom(const_cast<GrowingAllocator &>(other));
    }

    bool hasRebased() const
    {
	return thisOldBase != NULL;
    }

    void confirmRebased()
    {
	thisOldBase = NULL;
    }

    T* rebase(T *oldAddress) const
    {
	assert(thisOldBase != NULL);
	return (oldAddress - thisOldBase) + thisBase;
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

    inline void trim(size_type num)
    {
	thisSize = num;
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
	if (thisSize + num > thisCapacity - 1) {
	    grow(num);
	}
    }

    inline size_t getSize() const
    {
	return thisSize;
    }

    void grow(size_t num)
    {
	size_t oldCapacity = thisCapacity;
	while (thisSize + num > thisCapacity-1) {
	    thisCapacity *= 2;
	}
	T *newBase = new T [thisCapacity];
	T *newData = &newBase[1];
	memcpy(newData, thisData, sizeof(T)*thisSize);
	memset(thisBase, 0, sizeof(T)*oldCapacity);
	delete [] thisBase;
	thisOldBase = thisBase;
	thisBase = newBase;
	thisData = newData;
    }

private:
    void moveFrom(GrowingAllocator &other)
    {
	if (thisBase != NULL) {
	    delete [] thisBase;
	}
	thisCapacity = other.thisCapacity;
	thisSize = other.thisSize;
	thisBase = other.thisBase;
	thisData = other.thisData;
	thisOldBase = other.thisOldBase;
	other.thisCapacity = 0;
	other.thisSize = 0;
	other.thisBase = NULL;
	other.thisData = NULL;
	other.thisOldBase = NULL;
    }

    size_t thisCapacity;
    size_t thisSize;
    T *thisBase;
    T *thisData;
    T *thisOldBase;
};

}
#endif
