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
//   <24 bits>      000000  11   EXT INT32 (next word is a 32-bit int)
//   num cells      000001  11   EXT INT64 (next 2 words is a 64-bit int)
//   following      000010  11   EXT FLOAT (next word is a 32-bit float)
//                  000011  11   EXT DOUBLE (next 2 words is a 64-bit double)
//                  000100  11   EXT INT128 (next 4 words is a 128-bit int)
//                  000101  11   EXT ARRAY (next is an array. Immediate word
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

    //    inline explicit operator NativeType () const { return thisIndex; }

private:
   NativeType thisIndex;
};

class Heap;

/*
 * HeapRef. This class represent references to the heap. The heap
 * is main area where cells live.
 */
class HeapRef : public Index {
public:
    inline HeapRef() : Index(0), thisHeap((Heap *)NULL) { } 
    inline HeapRef(Heap &heap, NativeType index);
    inline HeapRef(const HeapRef &other);
    inline ~HeapRef();
    inline void operator = (const HeapRef &other);
    inline HeapRef operator + (size_t val) const { return HeapRef(*thisHeap, getIndex()+val); }
    inline HeapRef operator - (size_t val) const { return HeapRef(*thisHeap, getIndex()-val); }

private:
    Heap *thisHeap;
};

class IndexedHeapRef : public Index
{
public:
    IndexedHeapRef(const HeapRef &href) : Index(href.getIndex()) { }

    friend std::ostream & operator << (std::ostream &out, const IndexedHeapRef &r)
    {
	return out << r.getIndex();
    }
};



template<> struct HashOf<IndexedHeapRef> {
    static uint32_t value(const IndexedHeapRef &href) 
    {
	return href.getIndex();
    }
};

/*
 * ConstRef. This class represents references to constants. Constants
 * live in their own area,
 */
class ConstRef : public Index {
public:
    inline ConstRef() : Index(0) { }
    inline ConstRef(NativeType index) : Index(index) { }

    friend std::ostream & operator << (std::ostream &out, const ConstRef &cr);
};

/*
 * Cell. Yhis class is the main entity that lives on the heap.
 * They represent different data structures depending on the tag.
 * For simple cells (the most common ones) the tag are stored in the lower
 * 2 bits. For more advanced cells, the lower 8 bits represent an extended
 * tag, and the upper 24 bits contain auxillary information.
 *
 * This syetm is designed to use 32-bit cells to be space efficient
 * despite the machine is a 64-bit architecture. We don't use absolute
 * pointers, but relative ones. Yes, this limits the heap of being
 * 1 GB Cells.
 */
class Cell {
public:
    enum Tag { REF = 0, CON = 1, STR = 2, EXT = 3 };
    enum ExtTag { EXT_INT32 = 3, EXT_INT64 = 7, EXT_FLOAT = 11, EXT_DOUBLE = 15, EXT_INT128 = 19, EXT_ARRAY = 23, EXT_COMMA = 27, EXT_END = 31 };

    inline Cell() : thisCell(0) { }
    inline Cell(Tag tag, NativeType value) : thisCell(((NativeType)tag) | value << 2) { }
    inline Cell(Tag tag, HeapRef href) : thisCell(((NativeType)tag) | (href.getIndex()) << 2) { }
    inline Cell(ConstRef cref) : thisCell((NativeType)CON | (cref.getIndex() << 2)) { }
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

    inline ConstRef toConstRef() const { return ConstRef(getValue()); }

private:
    NativeType thisCell;
};

template<> struct HashOf<Cell> {
    static uint32_t value(const Cell &cell) 
    {
	return cell.getRawValue();
    }
};

class ConstString {
public:
    ConstString(Char *str, size_t n, size_t arity)
        : thisLength(n), thisArity(arity), thisString(str) { }

    const Char * getString() const { return thisString; }

    std::string asStdString() const;

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

class ConstTable : public GrowingAllocator<NativeType> {
public:
    static const int INITIAL_CONST_POOL_SIZE = 65536;
    static const int INITIAL_CONST_TABLE_SIZE = 1024;
    static const size_t MAX_CONST_LENGTH = 1024;

    static const char RESERVED [];

    ConstTable() : GrowingAllocator<NativeType>(INITIAL_CONST_TABLE_SIZE),
		   thisPool(INITIAL_CONST_POOL_SIZE),
		   thisIndexing(Primes::nextPrime(INITIAL_CONST_POOL_SIZE))
    {
    }

    static bool isReserved(const Char ch) {
	if (ch >= 256) {
	    return false;
	}
	return isReserved(static_cast<const char>(ch));
    }

    static bool isReserved(const char ch) {
	ensureReservedMap();
	return RESERVED_MAP[(uint8_t)ch];
    }

    static void ensureReservedMap() {
	if (!theInitialized) {
	    theInitialized = true;
	    for (size_t i = 0; i < sizeof(RESERVED_MAP); i++) {
		RESERVED_MAP[i] = false;
	    }
	    for (size_t i = 0; RESERVED[i] != '\0'; i++) {
		RESERVED_MAP[(uint8_t)RESERVED[i]] = true;
	    }
	}
    }

    // Const getConst(ConstRef ref) const;
    ConstRef getConst(const char *name, size_t arity) const;
    ConstRef getConst(size_t ordinal) const;
    ConstRef findConst(const char *name, size_t arity) const;
    ConstRef addConst(const char *name, size_t arity);

    ConstRef getConstNoEscape(const char *name, size_t arity) const;
    ConstRef findConstNoEscape(const char *name, size_t arity) const;
    ConstRef addConstNoEscape(const char *name, size_t arity);

    void getConstName(char *name, size_t ordinal) const;

    friend std::ostream & operator << (std::ostream &out, ConstTable &table)
    {
	table.print(out);
	return out;
    }

    size_t getConstArity(const ConstRef &ref) const;
    void print(std::ostream &out) const;
    void printConst(std::ostream &out, const ConstRef &ref) const;
    void printConstNoArity(std::ostream &out, const ConstRef &ref) const;
    size_t getConstLength(const ConstRef &ref) const;
    ConstString getConstNameNoArity(const ConstRef &ref) const;

private:
    static void escapeName(const char *str, char *escaped);
    mutable ConstPool thisPool;
    mutable ConstIndexing thisIndexing;

    static bool RESERVED_MAP[256];
    static bool theInitialized;
};

class PrintParam {
public:
    PrintParam(size_t maxWidth = 78)
        : thisMaxWidth(maxWidth), thisIndentWidth(2), thisStartColumn(0) { }

    size_t getStartColumn() const { return thisStartColumn; }
    void setStartColumn(size_t start) { thisStartColumn = start; }

    size_t getEndColumn() const { return thisStartColumn + thisMaxWidth; }

    size_t getMaxWidth() const { return thisMaxWidth; }
    void setMaxWidth(size_t maxWidth) { thisMaxWidth = maxWidth; }

    size_t getIndentWidth() const { return thisIndentWidth; }
    void setIndentWidth(size_t indentWidth) { thisIndentWidth = indentWidth;}

private:
    size_t thisMaxWidth;
    size_t thisIndentWidth;
    size_t thisStartColumn;
};

class PrintState;
class LocationTracker;

class Heap : public GrowingAllocator<Cell> {
public:
    Heap(size_t capacity = 65536)
        : GrowingAllocator<Cell>(capacity), thisMaxNumRoots(0) { }

    ConstRef getConst(size_t ordinal) const;
    ConstRef getConst(const char *name, size_t arity) const;

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

    inline HeapRef newRef()
    {
	Cell *cellPtr = allocate(1);
	HeapRef href(*this, toRelative(cellPtr));
	Cell cell(Cell::REF, href);
	*cellPtr = cell;
	return href;
    }

    inline HeapRef newRef(HeapRef href)
    {
	Cell *cellPtr = allocate(1);
	HeapRef newHref(*this, toRelative(cellPtr));
	Cell cell(Cell::REF, href);
	*cellPtr = cell;
	return newHref;
    }

    inline HeapRef newCon(ConstRef constRef)
    {
	Cell *cellPtr = allocate(1);
	HeapRef href(*this, toRelative(cellPtr));
	Cell con(constRef);
	*cellPtr = con;
	return href;
    }

    inline HeapRef newStr(HeapRef strRef)
    {
	Cell *cellPtr = allocate(1);
	HeapRef href(*this, toRelative(cellPtr));
	Cell str(Cell::STR, strRef);
	*cellPtr = str;
	return href;
    }

    inline Cell getCell(HeapRef ref) const
    {
	return *toAbsolute(ref.getIndex());
    }

    void printCell(std::ostream &out, Cell cell) const;

    inline HeapRef toHeapRef(Cell cell) const
    {
	return HeapRef(const_cast<Heap &>(*this), cell.getValue());
    }

    inline HeapRef first() const
    {
	return HeapRef(const_cast<Heap &>(*this), 1);
    }

    inline HeapRef top() const
    {
	return HeapRef(const_cast<Heap &>(*this), 1+getSize());
    }

    void print( std::ostream &out, HeapRef href,
		const PrintParam &param = PrintParam(78)) const;

    void print( std::ostream &out, PrintState &state ) const;

    std::string toString(HeapRef href) const;

    // ---

    void printRaw( std::ostream &out ) const;
    void printRaw( std::ostream &out, HeapRef from, HeapRef to ) const;

    std::string toRawString() const;
    std::string toRawString(HeapRef from, HeapRef to) const;

    inline void addRoot(const HeapRef &href)
    {
     size_t &cnt = thisRoots.getRef(IndexedHeapRef(href),0); cnt++;
     if (thisRoots.numEntries() > thisMaxNumRoots) {
	 thisMaxNumRoots = thisRoots.numEntries();
     }
    }

    inline void removeRoot(const HeapRef &href)
    { size_t &cnt = thisRoots.getRef(IndexedHeapRef(href),0);
      if (cnt > 0) {
	  cnt--;
	  if (cnt == 0) thisRoots.remove(IndexedHeapRef(href));
      }
    }

    void printStatus(std::ostream &out) const;
    void printRoots(std::ostream &out) const;

private:
    size_t getStringLength(Cell cell, size_t maximum) const;
    size_t getStringLengthForStruct(Cell cell) const;
    size_t getStringLengthForRef(Cell cell) const;

    void printTag(std::ostream &out, Cell cell) const;
    void printConst(std::ostream &out, Cell cell) const;
    ConstRef pushArgs(HeapRef ref);
    void printRef(std::ostream &out, Cell cell) const;
    ConstRef getRefName(Cell cell) const;
    void printIndent(std::ostream &out, PrintState &state) const;

    char parseSkipWhite( std::istream &in, LocationTracker &loc );
    HeapRef parseConst( std::istream &in, char &lookahead,
		     LocationTracker &loc );
    HeapRef parse( std::istream &in, LocationTracker &loc );
    HeapRef parseError(LocationTracker &loc);

    ConstTable thisConstTable;
    mutable Stack<Cell> thisStack;
    mutable HashMap<Cell, ConstRef> thisNameMap;

    HashMap<IndexedHeapRef, size_t> thisRoots;
    size_t thisMaxNumRoots;
};

HeapRef::HeapRef(Heap &heap, NativeType index)
   : Index(index), thisHeap(&heap)
{
    heap.addRoot(*this);
}

HeapRef::~HeapRef()
{
    if (thisHeap != NULL) {
	thisHeap->removeRoot(*this);
    }
}

HeapRef::HeapRef(const HeapRef &other) : Index(other), thisHeap(other.thisHeap)
{
    if (thisHeap != NULL) thisHeap->addRoot(*this);
}

void HeapRef::operator = (const HeapRef &other)
{
    if (thisHeap != NULL) thisHeap->removeRoot(*this);
    setIndex(other.getIndex());
    thisHeap = other.thisHeap;
    if (thisHeap != NULL) thisHeap->addRoot(*this);
}

}

#endif
