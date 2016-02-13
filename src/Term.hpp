#ifndef _Term_hpp
#define _Term_hpp

#include "basic.hpp"
#include "HashMap.hpp"
#include "Growing.hpp"

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
    inline ConstRef() : Index(0) { }
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

class ConstString {
public:
    ConstString(Char *str) : thisString(str) { }

    const Char * getString() const { return thisString; }
    size_t getStringLength() const {
	size_t cnt = 0;
	Char *c = thisString;
	while (c != 0) {
	    cnt++;
	    c++;
	}
	return cnt;
    }
private:
    Char *thisString;
};

class ConstPool : public GrowingAllocator<Char> {
public:
    ConstPool(size_t capacity) : GrowingAllocator<Char>(capacity) { }

    size_t addString(const char *asciiStr) {
	size_t n = strlen(asciiStr)+1;
	Char *data = allocate(n);
	for (size_t i = 0; i < n; i++) {
	    data[i] = (Char)asciiStr[i];
	}
	return toRelative(data);
    }

    const ConstString getString(ConstRef ref) {
	return ConstString(toAbsolute(ref.getIndex()));
    }
};

class ConstIndex : public GrowingAllocator<HashMapEntry<ConstString, ConstRef> > {
public:
    
};

class ConstTable : public GrowingAllocator<ConstRef> {
public:
    static const int INITIAL_CONST_POOL_SIZE = 65536;
    static const int INITIAL_CONST_TABLE_SIZE = 1024;

    ConstTable() : GrowingAllocator<ConstRef>(INITIAL_CONST_TABLE_SIZE),
		   thisPool(INITIAL_CONST_POOL_SIZE)
    {
    }

    std::string getNameAscii(ConstRef ref);

    ConstRef addConst(const char *name)
    {
	size_t pos = thisPool.addString(name);
	ConstRef *cr = allocate(1);
	*cr = pos;
	return *cr;
    }

    friend std::ostream & operator << (std::ostream &out, ConstTable &table)
    {
	table.print(out);
	return out;
    }

    void print(std::ostream &out);

private:
    ConstPool thisPool;
};

class Heap : public GrowingAllocator<Cell> {
public:
    Heap(size_t capacity) : GrowingAllocator<Cell>(capacity) { }

    inline Ref newVar()
    {
	Ref ref((NativeType)toRelative(allocate(1)));
	return ref;
    }
};

}

#endif
