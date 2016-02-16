#ifndef _Term_hpp
#define _Term_hpp

#include "basic.hpp"
#include "HashMap.hpp"
#include "Growing.hpp"
#include "Stack.hpp"
#include <memory.h>
#include <sstream>

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
//   <24 bits> <3 bits>000  11   EXT INT32 (next word is a 32-bit int)
//   num cells         001  11   EXT INT64 (next 2 words is a 64-bit int)
//   following         010  11   EXT FLOAT (next word is a 32-bit float)
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

    void operator ++ (int) {
	thisIndex++;
    }

    Index & operator ++ () {
	++thisIndex;
	return *this;
    }


    bool operator == (const Index &other) const {
	return getIndex() == other.getIndex();
    }

    bool operator <= (const Index &other) const {
	return getIndex() <= other.getIndex();
    }

    bool operator != (const Index &other) const {
	return getIndex() != other.getIndex();
    }

    bool operator < (const Index &other) const {
	return getIndex() < other.getIndex();
    }

    bool operator > (const Index &other) const {
	return getIndex() > other.getIndex();
    }

    bool operator >= (const Index &other) const {
	return getIndex() >= other.getIndex();
    }


private:
   NativeType thisIndex;
};

class HeapRef : public Index {
public:
    inline HeapRef(NativeType index) : Index(index) { }
    inline HeapRef operator + (int val) const { return HeapRef(getIndex()+val); }
    inline HeapRef operator - (int val) const { return HeapRef(getIndex()-val); }
};

class ConstRef : public Index {
public:
    inline ConstRef() : Index(0) { }
    inline ConstRef(NativeType index) : Index(index) { }

    friend std::ostream & operator << (std::ostream &out, const ConstRef &cr);
};

class Cell {
public:
    enum Tag { REF = 0, CON = 1, STR = 2, EXT = 3 };
    enum ExtTag { EXT_INT32 = 3, EXT_INT64 = 7, EXT_FLOAT = 11, EXT_DOUBLE = 15, EXT_INT128 = 19, EXT_ARRAY = 23, EXT_COMMA = 27, EXT_END = 31 };

    inline Cell() : thisCell(0) { }
    inline Cell(Tag tag, NativeType value) : thisCell(((NativeType)tag) | value << 2) { }
    inline Cell(ExtTag tag, NativeType numCells)
        : thisCell((NativeType)tag | (numCells << 8)) { }
    inline Cell(NativeType rawValue) : thisCell(rawValue) { }
    inline Cell(const Cell &other) : thisCell(other.thisCell) { }
    inline void operator = (const Cell &other) { thisCell = other.thisCell; }

    inline Tag getTag() const { return (Tag)(thisCell & 0x3); }
    inline ExtTag getExtTag() const { return (ExtTag)(thisCell & 0x1f); }

    inline NativeType getValue() const { return thisCell >> 2; }
    inline void setValue(NativeType val) { thisCell = (thisCell & 0x3) | (val << 2); }

    inline NativeType getRawValue() const { return thisCell; }

    inline size_t extNumCells() const { return thisCell >> 8; }

    inline bool operator == (const Cell &other) const { return thisCell == other.thisCell; }
    inline bool operator != (const Cell &other) const { return thisCell != other.thisCell; }


private:
    NativeType thisCell;
};

template<> struct HashOf<Cell> {
    static uint32_t value(const Cell &cell) 
    {
	return cell.getRawValue();
    }
};

class Ref : public Cell {
public:
    inline HeapRef getHeapRef() const { return HeapRef(getValue()); }

private:
    friend class Heap;
    inline Ref(HeapRef heapRef) : Cell( REF, heapRef.getIndex()) { }
    inline Cell toCell() { return *this; }
};

class Con : public Cell {
public:
    inline ConstRef getConstRef() const { return ConstRef(getValue()); }
private:
    friend class Heap;
    inline Con(ConstRef constRef) : Cell( CON, constRef.getIndex()) { }
    inline Cell toCell() { return *this; }
};

class Str : public Cell {
private:
    friend class Heap;
private:
    inline Str(HeapRef heapRef) : Cell( STR, heapRef.getIndex()) { }
    inline Cell toCell() { return *this; }
};

class ConstString {
public:
    ConstString(Char *str, size_t n, size_t arity)
        : thisLength(n), thisArity(arity), thisString(str) { }

    const Char * getString() const { return thisString; }

    size_t getLength() const {
	return thisLength;
    }

    size_t getArity() const {
	return thisArity;
    }

    bool operator == (const ConstString &other) const
    {
	if (getLength() != other.getLength()) {
	    return false;
	}
	if (getArity() != other.getArity()) {
	    return false;
	}
	return memcmp(thisString, other.thisString, getLength()*sizeof(Char)) == 0;
    }

    friend std::ostream & operator << (std::ostream &out, const ConstString &str);

private:
    size_t thisLength;
    size_t thisArity;
    Char *thisString;
};

template<> struct HashOf<ConstString> {
    static uint32_t value(const ConstString &str) 
    {
	return Murmur3(str.getString(), str.getLength()*sizeof(Char), HASHMAP_SEED) + str.getArity();
    }
};

class ConstPool : public GrowingAllocator<Char> {
public:
    ConstPool(size_t capacity) : GrowingAllocator<Char>(capacity) { }

    ConstString addString(const char *asciiStr, size_t arity) {
	size_t n = strlen(asciiStr);
	Char *data = allocate(1+n+1);
	data[0] = (Char)n;
	data[1] = (Char)arity;
	for (size_t i = 0; i <= n; i++) {
	    data[i+2] = (Char)asciiStr[i];
	}
	return ConstString(&data[2], n, arity);
    }

    size_t toRelativePointer(const ConstString &str)
    {
	const Char *data = str.getString();
	return toRelative(data)-2;
    }

    ConstString getString(size_t rel) {
	Char *data = toAbsolute(rel);
	return ConstString(&data[2], (size_t)data[0], (size_t)data[1]);
    }
};

class ConstIndexing : public HashMap<ConstString, ConstRef> {
public:
   ConstIndexing(size_t initialCapacity) : HashMap(initialCapacity) {
   }

   void add(const ConstString &str, ConstRef cr)
   {
       put(str, cr);
   }

   ConstRef find(const ConstString &str) const
   {
       const ConstRef *ref = get(str);
       if (ref == NULL) {
	   return ConstRef();
       } else {
	   return *ref;
       }
   }
};

class ConstTable;

    /*
class Const {
public:
    Const(const ConstTable &table, ConstRef ref)
        : thisTable(table), thisRef(ref) { }

    friend std::ostream & operator << (std::ostream &out, const Const &c);
private:
    const ConstTable &thisTable;
    ConstRef thisRef;
};
    */

class ConstTable : public GrowingAllocator<NativeType> {
public:
    static const int INITIAL_CONST_POOL_SIZE = 65536;
    static const int INITIAL_CONST_TABLE_SIZE = 1024;

    ConstTable() : GrowingAllocator<NativeType>(INITIAL_CONST_TABLE_SIZE),
		   thisPool(INITIAL_CONST_POOL_SIZE),
		   thisIndexing(Primes::nextPrime(INITIAL_CONST_POOL_SIZE))
    {
    }

    // Const getConst(ConstRef ref) const;
    ConstRef getConst(const char *name, size_t arity) const;
    ConstRef getConst(size_t ordinal) const;
    void getConstName(char *name, size_t ordinal) const;

    ConstRef findConst(const char *name, size_t arity) const;
    ConstRef addConst(const char *name, size_t arity);

    friend std::ostream & operator << (std::ostream &out, ConstTable &table)
    {
	table.print(out);
	return out;
    }

    size_t getConstArity(const ConstRef &ref) const;

    void print(std::ostream &out) const;
    void printConst(std::ostream &out, const ConstRef &ref) const;
    void printConstNoArity(std::ostream &out, const ConstRef &ref) const;

private:
    mutable ConstPool thisPool;
    mutable ConstIndexing thisIndexing;
};


class Heap : public GrowingAllocator<Cell> {
public:
    Heap(size_t capacity = 65536) : GrowingAllocator<Cell>(capacity) { }

    ConstRef getConst(size_t ordinal);
    ConstRef getConst(const char *name, size_t arity);

    inline ConstRef addConst(const char *name, size_t arity)
    {
	return thisConstTable.addConst(name, arity);
    }

    inline ConstRef addConst(size_t ordinal);

    inline ConstRef findConst(const char *name, size_t arity)
    {
	return thisConstTable.findConst(name, arity);
    }

    inline Cell deref(Cell cell) const
    {
	while (cell.getTag() == Cell::REF) {
	    Cell nextCell = *toAbsolute(cell.getValue());
	    if (nextCell == cell) {
		return cell;
	    }
	}
	return cell;
    }

    inline Ref newRef()
    {
	Cell *cell = allocate(1);
	Ref ref(toRelative(cell));
	*cell = ref.toCell();
	return ref;
    }

    inline Ref newRef(HeapRef href)
    {
	Cell *cell = allocate(1);
	Ref ref(href);
	*cell = ref.toCell();
	return ref;
    }

    inline Con newCon(ConstRef constRef)
    {
	Cell *cell = allocate(1);
	Con con(constRef);
	*cell = con.toCell();
	return con;
    }

    inline Str newStr(HeapRef ref)
    {
	Cell *cell = allocate(1);
	Str str(ref.getIndex());
	*cell = str.toCell();
	return str;
    }

    inline Cell getCell(HeapRef ref) const
    {
	return *toAbsolute(ref.getIndex());
    }

    void printCell(std::ostream &out, Cell cell) const;

    inline HeapRef first() const
    {
	return HeapRef(1);
    }

    inline HeapRef top() const
    {
	return HeapRef(1+getSize());
    }

    void print( std::ostream &out, HeapRef ref ) const;
    std::string toString( HeapRef ref) const;

    // ---

    void printRaw( std::ostream &out ) const;
    void printRaw( std::ostream &out, HeapRef from, HeapRef to ) const;

    std::string toRawString() const;
    std::string toRawString(HeapRef from, HeapRef to) const;

private:
    void printTag(std::ostream &out, Cell cell) const;
    void printConst(std::ostream &out, Cell cell) const;
    void printStruct(std::ostream &out, HeapRef href) const;
    void printRef(std::ostream &out, Cell cell) const;

    ConstTable thisConstTable;
    mutable Stack<Cell> thisStack;
    mutable HashMap<Cell, ConstRef> thisNameMap;
    
};

}

#endif
