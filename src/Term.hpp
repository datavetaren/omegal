#ifndef _Term_hpp
#define _Term_hpp

#include "basic.hpp"

namespace PROJECT {

//
// Define Term, similar to terms in Prolog. These are extremely light
// weight objects to encode structured data types. The universe consists
// of:
//
// Term ::= Var | Const | Functor(Term, Term, ..., Term) | Ext
//
// We use the lower two bits to encode data type (i.e. REF, CON, STR, EXT):
//
// It should be noted that this representation is suboptimal for stray
// integers, but these are scarce when dealing with intermediate
// representation for compilers.
//
// Bits:
//   <30 bits> [HeapIndex ] 00   REF Variable
//   <30 bits> [ConstIndex] 01   CON Constant
//   <30 bits> [HeapIndex]  10   STR Structure (pointing at functor+arity)
//   <30 bits> [..........] 11   EXT
//                     000  11   EXT INT32 (next word is a 32-bit int)
//                     001  11   EXT INT64 (next 2 words is a 64-bit int)
//                     010  11   EXT FLOAT (next word is a 32-bit float)
//                     011  11   EXT DOUBLE (next 2 words is a 64-bit double)
//                     100  11   EXT INT128 (next 4 words is a 128-bit int)
//                     101  11   EXT ARRAY (next is an array. Immediate word
//                                          is EXT for array type, then
//                                          array size)
//

// Index to heap.

typedef uint32_t NativeType;

class Index {
public:
    Index(NativeType index) { thisIndex = index; }

    inline NativeType getIndex() const { return thisIndex; }
    inline void setIndex(NativeType index) { thisIndex = index; }
private:
   NativeType thisIndex;
};

class HeapRef : public Index {
public:
    inline HeapRef(NativeType index) : Index(index) { }
};

class ConstRef : public Index {
public:
    inline ConstRef(NativeType index) : Index(index) { }
};

class Cell {
public:
    enum Tag { REF = 0, CON = 1, STR = 2, EXT = 3 };
    enum ExtTag { EXT_INT32 = 3, EXT_INT64 = 7, EXT_FLOAT = 11, EXT_DOUBLE = 15, EXT_INT128 = 19, EXT_ARRAY = 23 };

    inline Cell() : thisCell(0) { }
    inline Cell(Tag tag, NativeType value) : thisCell(((NativeType)tag) | value << 2) { }
    inline Cell(ExtTag tag) : thisCell((NativeType)tag) { }
    inline Cell(NativeType rawValue) : thisCell(rawValue) { }
    inline Cell(const Cell &other) : thisCell(other.thisCell) { }
    inline void operator = (const Cell &other) { thisCell = other.thisCell; }

    inline Tag getTag() const { return (Tag)(thisCell & 0x3); }
    inline ExtTag getExtTag() const { return (ExtTag)(thisCell & 0x1f); }

    inline NativeType getValue() const { return thisCell >> 2; }
    inline void setValue(NativeType val) { thisCell = (thisCell & 0x3) | (val << 2); }

private:
    NativeType thisCell;
};

class Ref : protected Cell {
public:
    inline Ref(HeapRef heapRef) : Cell( REF, heapRef.getIndex()) { }
    inline HeapRef getHeapRef() const { return HeapRef(getValue()); }
};

class Con : protected Cell {
    inline Con(ConstRef constRef) : Cell( CON, constRef.getIndex()) { }
    inline ConstRef getConstRef() const { return ConstRef(getValue()); }
};

class Str : protected Cell {
    inline Str(HeapRef heapRef) : Cell( STR, heapRef.getIndex()) { }
};

template<typename T> class Growing {
protected:
    Growing(size_t initialCapacity)
    {
	thisCapacity = initialCapacity;
	thisSize = 0;
	thisData = new T [thisCapacity];
    }

    inline size_t allocate(size_t num)
    {
	ensureCapacity(num);
	size_t r = thisSize;
	thisSize += num;
	return r;
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
	T *newData = new T [thisCapacity];
	memcpy(newData, thisData, sizeof(T)*thisSize);
	delete [] thisData;
	thisData = newData;
    }

    T * getData(size_t atIndex)
    {
	return &thisData[atIndex];
    }

private:
    size_t thisCapacity;
    size_t thisSize;
    T *thisData;
};

class ConstPool : public Growing<Char> {
public:
    ConstPool(size_t capacity) : Growing<Char>(capacity) { }

    size_t addString(const char *asciiStr) {
	size_t n = strlen(asciiStr)+1;
	size_t r = allocate(n);
	Char *data = getData(r);
	for (size_t i = 0; i < n; i++) {
	    data[i] = (Char)asciiStr[i];
	}
	return r;
    }
};

class ConstTable : public Growing<NativeType> {
public:
    static const int INITIAL_CONST_POOL_SIZE = 65536;
    static const int INITIAL_CONST_TABLE_SIZE = 1024;

    ConstTable() : Growing<NativeType>(INITIAL_CONST_TABLE_SIZE),
		   thisPool(INITIAL_CONST_POOL_SIZE)
    {
    }

private:
    ConstPool thisPool;
};

class Heap : public Growing<Cell> {
public:
    Heap(size_t capacity) : Growing<Cell>(capacity) { }

    inline Ref newVar()
    {
	Ref ref((NativeType)allocate(1));
	return ref;
    }
};

}

#endif
