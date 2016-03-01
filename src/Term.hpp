#ifndef _Term_hpp
#define _Term_hpp

#include "basic.hpp"
#include "HashMap.hpp"
#include "Growing.hpp"
#include "Stack.hpp"
#include <memory.h>
#include <sstream>
#include "Flags.hpp"

namespace PROJECT {

DeclareFlags(Token, COMMA, LPAREN, RPAREN, TERM);
typedef Flags<Token> Expect;

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
//   <29 bits>   [HeapIndex]   000   REF Variable
//   <29 bits>   [ConstIndex]  001   CON Constant
//   <29 bits>   [HeapIndex]   010   STR Structure (pointing at functor+arity)
//   <29 bits>   [HeapIndex]   011   INT32 Constant
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
class IHeapRef : public Index {
public:
    inline IHeapRef() : Index(0) { } 
    inline IHeapRef(NativeType index) : Index(index) { }
    inline IHeapRef(const IHeapRef &other) : Index(other) { }
    inline ~IHeapRef() { }

    inline bool isEmpty() const { return getIndex() == 0; }

    inline void operator = (const IHeapRef &other)
    { setIndex(other.getIndex()); }
};

class HeapRef : public IHeapRef
{
public:
    inline HeapRef() : thisHeap((Heap *)NULL) { } 
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
    enum Tag { REF = 0, CON = 1, STR = 2, INT32 = 3, EXT = 7 };
    enum ExtTag { EXT_COMMA = 7 + (1 << 3), 
		  EXT_END = 7 + (2 << 3)
                };

    inline Cell() : thisCell(0) { }
    inline Cell(Tag tag, NativeType value) : thisCell(((NativeType)tag) | value << 3) { }
    inline Cell(Tag tag, IHeapRef href) : thisCell(((NativeType)tag) | (href.getIndex()) << 3) { }
    inline Cell(ConstRef cref) : thisCell((NativeType)CON | (cref.getIndex() << 3)) { }
    inline Cell(ExtTag tag, NativeType numCells)
        : thisCell((NativeType)tag | (numCells << 8)) { }
    inline Cell(NativeType rawValue) : thisCell(rawValue) { }
    inline Cell(const Cell &other) : thisCell(other.thisCell) { }
    inline void operator = (const Cell &other) { thisCell = other.thisCell; }

    inline Tag getTag() const { return (Tag)(thisCell & 0x7); }
    inline ExtTag getExtTag() const { return (ExtTag)(thisCell & 0x1f); }

    inline NativeType getValue() const { return thisCell >> 3; }
    inline void setValue(NativeType val) { thisCell = (thisCell & 0x7) | (val << 3); }

    inline NativeType getRawValue() const { return thisCell; }

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
    ConstString(const Char *str, size_t n, size_t arity)
        : thisLength(n), thisArity(arity), thisString(str), thisNoEscape(false) { }

    bool isNoEscape() const { return thisNoEscape; }
    void setNoEscape(bool noEscape) { thisNoEscape = noEscape; }

    static void convert(const char *src, size_t n, Char *dst);
    static void convert(const Char *src, size_t n, char *dst);

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

private:
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

    template<typename T> static size_t escapeName(const T *str, size_t len, char *escaped);

    size_t thisLength;
    size_t thisArity;
    const Char *thisString;
    bool thisNoEscape;

    static const char RESERVED [];
    static bool RESERVED_MAP[256];
    static bool theInitialized;
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

    ConstString addString(const char *asciiStr, size_t nameLen, size_t arity) {
	Char *data = allocate(1+nameLen+1);
	data[0] = (Char)nameLen;
	data[1] = (Char)arity;
	for (size_t i = 0; i <= nameLen; i++) {
	    data[i+2] = (Char)asciiStr[i];
	}
	return ConstString(&data[2], nameLen, arity);
    }

    ConstString addString(const Char *name, size_t nameLen, size_t arity) {
	Char *data = allocate(1+nameLen+1);
	data[0] = (Char)nameLen;
	data[1] = (Char)arity;
	memcpy(&data[2], name, nameLen*sizeof(Char));
	data[2+nameLen] = (Char)0;
	return ConstString(&data[2], nameLen, arity);
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

class ConstTable : public GrowingAllocator<NativeType> {
public:
    static const int INITIAL_CONST_POOL_SIZE = 65536;
    static const int INITIAL_CONST_TABLE_SIZE = 1024;
    static const size_t MAX_CONST_LENGTH = 1024;

    ConstTable() : GrowingAllocator<NativeType>(INITIAL_CONST_TABLE_SIZE),
		   thisPool(INITIAL_CONST_POOL_SIZE),
		   thisIndexing(Primes::nextPrime(INITIAL_CONST_POOL_SIZE))
    {
    }

    // Const getConst(ConstRef ref) const;
    ConstRef getConst(const char *name, size_t arity) const;
    ConstRef getConst(size_t ordinal) const;
    ConstRef getConst(ConstRef name, size_t newArity) const;

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
    void printConstNoEscape(std::ostream &out, const ConstRef &ref) const;
    size_t getConstLength(const ConstRef &ref) const;
    ConstString getConstName(const ConstRef &ref) const;

private:
    ConstRef findConst(const char *name, size_t len, size_t arity) const;
    ConstRef findConst(const Char *name, size_t len, size_t arity) const;
    ConstRef addConst(const char *name, size_t len, size_t arity);
    ConstRef addConst(const Char *name, size_t len, size_t arity);

    mutable ConstPool thisPool;
    mutable ConstIndexing thisIndexing;
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
    Heap(size_t capacity = 65536);

    ConstRef getConst(size_t ordinal) const;
    ConstRef getConst(const char *name, size_t arity) const;
    ConstRef getConst(ConstRef other, size_t newArity) const;
    ConstString getConstName(ConstRef cref) const;

    inline IHeapRef deref(IHeapRef href) const
    {
	Cell cell = getCell(href);
	while (cell.getTag() == Cell::REF) {
	    IHeapRef nextHref = IHeapRef(cell.getValue());
	    if (nextHref == href) {
		return href;
	    }
	    href = nextHref;
	    cell = getCell(href);
	}
	return href;
    }

    inline HeapRef newArgs(size_t numArgs)
    {
	Cell *cellPtr = allocate(numArgs);
	HeapRef href(*this, toRelative(cellPtr));
	return href;
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

    inline HeapRef newStr(HeapRef strRef, size_t numArgs)
    {
	Cell *cellPtr = allocate(1+numArgs);
	HeapRef href(*this, toRelative(cellPtr));
	Cell str(Cell::STR, strRef);
	*cellPtr = str;
	return href;
    }

    inline HeapRef newCell(Cell cell)
    {
	Cell *cellPtr = allocate(1);
	HeapRef href(*this, toRelative(cellPtr));
	*cellPtr = cell;
	return href;
    }

    inline void setCell(HeapRef ref, Cell cell)
    {
	*toAbsolute(ref.getIndex()) = cell;
    }

    inline HeapRef newInt32(HeapRef ref)
    {
	Cell *cellPtr = allocate(1);
	*cellPtr = Cell(Cell::INT32, ref);
	HeapRef href(*this, toRelative(cellPtr));
	return href;
    }

    inline void newInt32(HeapRef atRef, int32_t value)
    {
	Cell *cellPtr = allocate(1);
	HeapRef href(*this, toRelative(cellPtr));
	*cellPtr = Cell(static_cast<NativeType>(value));
	setCell(atRef, Cell(Cell::INT32, href));
    }

    inline Cell getCell(IHeapRef ref) const
    {
	return *toAbsolute(ref.getIndex());
    }

    inline int32_t toInt32(Cell cell) const
    {
	return static_cast<int32_t>(toAbsolute(cell.getValue())->getRawValue());
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

    HeapRef parse( std::istream &in );

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
    size_t getStringLength(IHeapRef href, size_t maximum) const;
    size_t getStringLengthForStruct(Cell cell) const;
    size_t getStringLengthForRef(Cell cell) const;
    size_t getStringLengthForInt32(int32_t value) const;

    void printTag(std::ostream &out, Cell cell) const;
    void printConst(std::ostream &out, Cell cell) const;
    void printInt32(std::ostream &out, int32_t) const;
    ConstRef pushArgs(HeapRef ref);
    void printRef(std::ostream &out, Cell cell) const;
    ConstRef getRefName(Cell cell) const;
    void printIndent(std::ostream &out, PrintState &state) const;

    HeapRef parse( std::istream &in, LocationTracker &loc );
    HeapRef parseError(LocationTracker &loc, const std::string &msg);
    void parseSkipWhite( std::istream &in, LocationTracker &loc );
    HeapRef parseConst( std::istream &in, LocationTracker &loc );
    HeapRef parseArgs( std::istream &in, LocationTracker &loc );
    HeapRef parseTerm( std::istream &in, LocationTracker &loc );
    bool parseCheck( int lookahead, Expect expect );
    HeapRef expectError( LocationTracker &loc, int lookahead, Expect expect );
    void expectErrorToken(std::ostream &out, const char *token,
			  bool isLast, bool &isFirst );

    ConstTable thisConstTable;
    mutable Stack<IHeapRef> thisStack;
    mutable HashMap<Cell, ConstRef> thisNameMap;
    Stack<size_t> thisParseArity;

    IHeapRef thisExtComma;
    IHeapRef thisExtEnd;

    HashMap<IndexedHeapRef, size_t> thisRoots;
    size_t thisMaxNumRoots;
};

HeapRef::HeapRef(Heap &heap, NativeType index)
    : IHeapRef(index), thisHeap(&heap)
{
    heap.addRoot(*this);
}

HeapRef::~HeapRef()
{
    if (thisHeap != NULL) {
	thisHeap->removeRoot(*this);
    }
}

HeapRef::HeapRef(const HeapRef &other)
  : IHeapRef(other), thisHeap(other.thisHeap)
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
