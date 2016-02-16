#ifndef _Stack_hpp
#define _Stack_hpp

#include "Growing.hpp"

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
