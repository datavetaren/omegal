#ifndef _Stack_hpp
#define _Stack_hpp

#include "Growing.hpp"
#include <algorithm>

namespace PROJECT {

template<typename T> class Stack : public GrowingAllocator<T>
{
public:
    Stack(size_t initialCapacity = 1024)
        : GrowingAllocator<T>(initialCapacity) { }

    void push(const T &el)
    {
	T *t = GrowingAllocator<T>::allocate(1);
	*t = el;
    }

    T pop()
    {
	size_t n = GrowingAllocator<T>::getSize();
	T t = *GrowingAllocator<T>::toAbsolute(n);
	GrowingAllocator<T>::trim(n-1);
	return t;
    }

    T peek(size_t rel = 0) const
    {
	size_t n = GrowingAllocator<T>::getSize();
	T t = *GrowingAllocator<T>::toAbsolute(n-rel);
	return t;
    }

    T & peek(size_t rel = 0)
    {
	size_t n = GrowingAllocator<T>::getSize();
	T &t = *GrowingAllocator<T>::toAbsolute(n-rel);
	return t;
    }

    // Reverse n elements on top of stack
    void reverse(size_t n)
    {
	if (n == 0) return;
	size_t top = GrowingAllocator<T>::getSize();
	T *from = GrowingAllocator<T>::toAbsolute(top-n+1);
	size_t half = n / 2;
	for (size_t i = 0; i < half; i++) {
	    std::swap(from[i], from[n-1-i]);
	}
    }

    void trim(size_t newSize)
    {
	GrowingAllocator<T>::trim(newSize);
    }

    size_t getSize() const
    {
	size_t n = GrowingAllocator<T>::getSize();
	return n;
    }

    bool isEmpty() const
    {
	return GrowingAllocator<T>::getSize() == 0;
    }
};

}

#endif
