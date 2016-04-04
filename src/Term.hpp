#ifndef _Term_hpp
#define _Term_hpp

#include "basic.hpp"
#include "HashMap.hpp"
#include "Growing.hpp"
#include "Stack.hpp"
#include <memory.h>
#include <sstream>
#include "Flags.hpp"
#include "BitMap.hpp"
#include "OpenHashMap.hpp"
#include "HashMap.hpp"

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
//   <29 bits>   [HeapIndex]   011   MAP (Hash Array Mapped Trie)
//   <29 bits>   [HeapIndex]   101   INT32 Constant
//   <29 bits>   [HeapIndex]   110   FWD Forward reference for GC
//   <29 bits>   [---------]   111   EXT Extended cell
//
//
// REF: These are same as in WAM. They point at other things, or
//      they point at themselves (to represent an unbound variable.)
// CON: Constants. The upper bits then represent the index to a
//      constant table. This table never shrinks. Only new constants
//      are added over time.
// STR: Structure. Same as in WAM. It points at a CON cell. The CON
//      cell contains information about arity and the arity is used
//      to tell how many arguments follow.
//      For example, [1] STR: 2 [2] CON f/3 [3] <Arg1> [4] <Arg2> [5] <Arg3>
//
// MAP: Hash array mapped trie (HAMT). To represent O(1) non-destructive
//      hash tables. A MAP is a reference that points to a cell:
//      [1] MAP: 2 [2] <meta> [3] <mask> [4] <Arg0> [5] <Arg1> ... [?] <Argn>
//      <meta> contains meta data such as the <depth> of the tree or whether
//      the node is representing a root (see mroe below).
//
//      <mask> is a 32-bit mask with 1:s telling whether
//      there exist a non-null argument. Only the non-null arguments then
//      follow. Thus, counting the bits in <mask> gives you the number of
//      arguments that follow.
//
//      The first cell of a pointing MAP: The 32 bits are encoding:
//                                       [1 bit] = root yes/no
//                               [3 bits]        = depth (0-7)
//                      [28 bits]                = entry count
//      
// INT32: Points at a full cell (with no tag) that contains a raw 32-bit
//      (signed) integer.
// FWD: This is only used during garbage collection. When a thunk is moved
//      during compaction, we write FWD reference at the old location that
//      points to the new location so others (during GC) can find it.
//      Once GC is complete, FWD is no longer used. Thus, from a user
//      standpoint, this is something that is never seen in terms.
// EXT: Extended cell. The remaining bits give remaining interpretation.
//      This enables us to extend the system with new cell types.
//

// Index to heap.

class Index {
public:
    Index(NativeType index) { thisIndex = index; }

    inline NativeType getIndex() const { return thisIndex; }
    inline NativeType getValue() const { return thisIndex; }
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
    inline HeapRef() : Index(0) { } 
    inline HeapRef(NativeType index) : Index(index) { }
    inline HeapRef(Index index) : Index(index) { }
    inline HeapRef(const HeapRef &other) : Index(other) { }
    inline ~HeapRef() { }

    inline bool isEmpty() const { return getIndex() == 0; }

    inline void operator = (const HeapRef &other)
    { setIndex(other.getIndex()); }

    inline HeapRef operator + (size_t val) const { return HeapRef(getIndex()+val); }
    inline HeapRef operator - (size_t val) const { return HeapRef(getIndex()-val); }

};

class Cell;

class IndexedCellPtr
{
public:
    IndexedCellPtr()
	: thisCellPtr(NULL) { }

    IndexedCellPtr(Cell *cellPtr)
        : thisCellPtr(cellPtr) { }

    bool operator == (const IndexedCellPtr &other)
        { return thisCellPtr == other.thisCellPtr; }

    const Cell * getCellPtr() const { return thisCellPtr; }
    Cell * getCellPtr() { return thisCellPtr; }

    friend std::ostream & operator << (std::ostream &out,
				       const IndexedCellPtr &r)
    {
	return out << r.thisCellPtr;
    }

private:
    Cell *thisCellPtr;
};

template<> struct HashOf<IndexedCellPtr> {
    static uint32_t value(const IndexedCellPtr &indexed) 
    {
	return (uint32_t)((size_t)(indexed.getCellPtr()) >> 3);
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

    inline bool isNull() const { return getIndex() == 0; }
};

template<> struct HashOf<ConstRef> {
    static uint32_t value(const ConstRef &cref)
    {
	return cref.getIndex();
    }
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
    enum Tag { REF = 0, CON = 1, STR = 2, MAP = 3, INT32 = 5, FWD = 6, EXT = 7 };
    enum ExtTag { };

    inline Cell() : thisCell(0) { }
    inline Cell(Tag tag, NativeType value) : thisCell(((NativeType)tag) | value << 3) { }
    inline Cell(Tag tag, HeapRef href) : thisCell(((NativeType)tag) | (href.getIndex()) << 3) { }
    inline Cell(ConstRef cref) : thisCell((NativeType)CON | (cref.getIndex() << 3)) { }
    inline Cell(ExtTag tag, NativeType numCells)
        : thisCell((NativeType)tag | (numCells << 8)) { }
    inline Cell(NativeType rawValue) : thisCell(rawValue) { }
    inline Cell(const Cell &other) : thisCell(other.thisCell) { }
    inline void operator = (const Cell &other) { thisCell = other.thisCell; }

    inline bool isNull() const { return thisCell == 0; }

    inline Tag getTag() const { return (Tag)(thisCell & 0x7); }
    inline ExtTag getExtTag() const { return (ExtTag)(thisCell & 0x1f); }

    inline NativeType getValue() const { return thisCell >> 3; }
    inline void setValue(NativeType val) { thisCell = (thisCell & 0x7) | (val << 3); }
    inline void setRawValue(NativeType val) { thisCell = val; }
    inline NativeType getRawValue() const { return thisCell; }

    inline bool operator == (const Cell &other) const { return thisCell == other.thisCell; }
    inline bool operator != (const Cell &other) const { return thisCell != other.thisCell; }

    inline ConstRef toConstRef() const { return ConstRef(getValue()); }

private:
    NativeType thisCell;
};

class CellRef {
public:
    inline CellRef();
    inline CellRef(const CellRef &other);
    inline CellRef(const Heap &heap, Cell cell);
    inline ~CellRef();

    inline const Heap & getHeap() const { return *thisHeap; }

    inline bool isEmpty() const { return thisHeap == NULL; }

    void operator = (const CellRef &other);
    inline const Cell & operator * () const { return thisCell; }
    inline const Cell * operator -> () const { return &thisCell; }

private:
    const Heap *thisHeap;
    Cell thisCell;
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
        : thisLength(n), thisArity(arity), thisString(str), thisNoEscape(arity == MAX_ARITY) { }

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

    int compare(const ConstString &other) const;

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
    
    static size_t const MAX_ARITY = 0xffffff;

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
	return Murmur3(str.getString(), str.getLength()*sizeof(Char), HASH_SEED) + str.getArity();
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

class Heap {
public:
    Heap(size_t capacity = 65536);

    void setStrict(bool strict);
    bool isStrict() const;

    ConstRef getConst(size_t ordinal) const;
    ConstRef getConst(const char *name, size_t arity) const;
    ConstRef getConst(ConstRef other, size_t newArity) const;
    ConstString getConstName(ConstRef cref) const;

    inline HeapRef deref(HeapRef href) const
    {
	Cell cell = getCell0(href);
	while (cell.getTag() == Cell::REF) {
	    HeapRef nextHref = HeapRef(cell.getValue());
	    if (nextHref == href) {
		return href;
	    }
	    href = nextHref;
	    cell = getCell0(href);
	}
	return href;
    }

    inline CellRef deref(CellRef cellRef) const
    {
	return CellRef(const_cast<Heap &>(*this), deref(*cellRef));
    }

    inline Cell::Tag getTag(CellRef cellRef) const
    {
	return (*deref(cellRef)).getTag();
    }

    inline CellRef newRef()
    {
	Cell *cellPtr = allocate(1);
	HeapRef href(toRelative(cellPtr));
	Cell cell(Cell::REF, href);
	*cellPtr = cell;
	return CellRef(*this, cell);
    }

    inline CellRef newRef(HeapRef href)
    {
	Cell *cellPtr = allocate(1);
	Cell cell(Cell::REF, href);
	*cellPtr = cell;
	return CellRef(*this, cell);
    }

    inline CellRef newConOnHeap(ConstRef constRef)
    {
	Cell *cellPtr = allocate(1);
	Cell con(constRef);
	*cellPtr = con;
        return CellRef(*this, con);
    }

    inline CellRef newConst(ConstRef constRef)
    {
	Cell con(constRef);
	return CellRef(*this, con);
    }

    inline CellRef newConst(const char *name, size_t arity)
    {
	return newConst(getConst(name, arity));
    }

    inline CellRef newConst(const char *name)
    {
	return newConst(name, 0);
    }

    inline CellRef newList()
    {
	return CellRef(*this, Cell(Cell::CON, thisFunctorEmpty));
    }

    inline CellRef newList(CellRef first, CellRef rest)
    {
	CellRef lst = newStr(thisFunctorDot);
	setArg(lst, 0, first);
	setArg(lst, 1, rest);
	return lst;
    }

    inline CellRef newPair(CellRef first, CellRef second)
    {
	CellRef pair = newStr(thisFunctorColon);
	setArg(pair, 0, first);
	setArg(pair, 1, second);
	return pair;
    }

    inline CellRef assocListNew()
    {
	return newList();
    }

    inline CellRef assocListAdd(CellRef list, CellRef key, CellRef value)
    {
	return newList(newPair(key, value), list);
    }

    inline CellRef assocListRemove(CellRef list, CellRef key)
    {
	return assocListReplace(list, key, CellRef());
    }

    CellRef assocListReplace(CellRef list, CellRef key, CellRef value);

    inline CellRef newStr(HeapRef strRef)
    {
	Cell *cellPtr = allocate(1);
	Cell str(Cell::STR, strRef);
	*cellPtr = str;
	return CellRef(*this, str);
    }

    inline CellRef newStr(ConstRef functor)
    {
	size_t numArgs = thisConstTable.getConstArity(functor);
	Cell *cellPtr = allocate(1+numArgs);
	HeapRef href(toRelative(cellPtr));
	*cellPtr = Cell(functor);
	Cell strCell(Cell::STR, toRelative(cellPtr));
	return CellRef(*this, strCell);
    }

    inline void setArg(const CellRef &cellRef, size_t index, CellRef arg)
    {
	setArg(*cellRef, index, *arg);
    }

    inline bool isFunctor(const CellRef &cell, ConstRef functor) const
    {
	return isFunctor(*cell, functor);
    }

    inline bool isEmpty(const CellRef &cell) const
    {
	return isEmpty(*cell);
    }

    inline bool isDot(const CellRef &cell) const
    {
	return isDot(*cell);
    }

    inline bool isPair(const CellRef &cell) const
    {
	return isPair(*cell);
    }

    CellRef newMap(size_t depth);
    CellRef putMap(CellRef mapCell, CellRef key, CellRef value);
    CellRef mapAsList(CellRef mapCell);
    CellRef qsortList(CellRef list);
    size_t lengthList(CellRef list);

private:
    inline void setArg(Cell cell, size_t index, Cell arg)
    {
	HeapRef href = getArgRef(cell, index);
	setCell(href, arg);
	checkForwardPointer(href, arg);
    }

    static int qsortCmp(void *heap, const void *a, const void *b);

    // Helper functions for maps (or associative arrays)
    CellRef mapCreateNewTree(size_t startDepth, size_t endDepth,
			     uint32_t hash, CellRef key, CellRef value);
    size_t mapGetSpine(CellRef map, CellRef key, uint32_t hash);
    CellRef wrapSpine(CellRef tree, uint32_t hash);
    CellRef setArg32(CellRef mapCell, size_t index, CellRef arg);
    CellRef getArg32(CellRef mapCell, size_t index);

public:
    uint32_t hashOf(CellRef term);

    inline CellRef newInt32(int32_t value)
    {
	Cell *cellPtr = allocate(1);
	*reinterpret_cast<int32_t *>(cellPtr) = value;
	Cell int32Ref(Cell::INT32, toRelative(cellPtr));
	return CellRef(*this, int32Ref);
    }

    inline ConstRef getConst(HeapRef atRef) const
    {
	HeapRef hr = deref(atRef);
	Cell c = getCell0(hr);
	if (c.getTag() == Cell::STR) {
	    c = getCell0(toHeapRef(c));
	}
	return c.toConstRef();
    }

    inline ConstRef getFunctorConst(CellRef strCell) const
    {
	return (*getFunctor(strCell)).toConstRef();
    }

    inline CellRef getFunctor(CellRef strCell) const
    {
	return CellRef(*this, getFunctor(*strCell));
    }

    inline size_t getArity(const CellRef &cellRef) const
    {
	return getArity(*cellRef);
    }

    inline CellRef getArg(const CellRef &strRef, size_t index) const
    {
	Cell cell = getArg(*strRef, index);
	return CellRef(*this, cell);
    }

    inline HeapRef getArgRef(const CellRef &strRef, size_t index) const
    {
	return getArgRef(*strRef, index);
    }

    // Unification
    // Try to unify the two terms. Returns true if successful.
    bool unify(CellRef a, CellRef b);

    inline bool equal(CellRef a, CellRef b) const
    {
	return equal(*a, *b);
    }

    inline bool compare(CellRef a, CellRef b) const
    {
	return compare(*a, *b);
    }

    size_t getStackSize() const;

    inline CellRef getCell(HeapRef ref) const
    {
	return CellRef(const_cast<Heap &>(*this), *toAbsolute(ref.getIndex()));
    }

    inline int32_t toInt32(Cell cell) const
    {
	return static_cast<int32_t>(toAbsolute(cell.getValue())->getRawValue());
    }

    void printCell(std::ostream &out, Cell cell) const;

    inline bool hasHeapRef(Cell cell) const
    {
	switch (cell.getTag()) {
	case Cell::STR:
	case Cell::INT32:
	case Cell::REF:
	case Cell::FWD:
	    return true;
	case Cell::CON:
	case Cell::EXT:
	    return false;
	default: // In case tag is invalid
	    assert("Heap::hasHeapRef(): Invalid TAG"==NULL);
	    return false;
	}
    }

    inline HeapRef toHeapRef(Cell cell) const
    {
	return HeapRef(cell.getValue());
    }

    inline HeapRef firstHeapRef() const
    {
	return HeapRef(1);
    }

    inline size_t getHeapSize() const
    {
	return thisHeap.getSize();
    }

    inline HeapRef topHeapRef() const
    {
	return HeapRef(1+getHeapSize());
    }

    inline void push(CellRef cell)
    {
	thisStack.push(*cell);
    }

    inline CellRef pop()
    {
	return CellRef(*this, thisStack.pop());
    }

    void print( std::ostream &out, CellRef cellRef,
		const PrintParam &param = PrintParam(78)) const;

    void print( std::ostream &out, PrintState &state ) const;

    std::string toString(CellRef cellRef) const;

    CellRef parse( std::istream &in );

    // ----------------------------------------------------------

    void gc(size_t topSize, int verbosity = 0);
    void gc(double percentage, int verbosity = 0);

private:
    inline size_t bitCount(uint32_t w)
    {
	w = w - ((w >> 1) & 0x55555555);
	w = (w & 0x33333333) + ((w >> 2) & 0x33333333);
	return (((w + (w >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
    }

    inline void checkForwardPointer(HeapRef href, Cell cell)
    {
	switch (cell.getTag()) {
	case Cell::CON:
	case Cell::FWD:
	    break;
	case Cell::STR:
	case Cell::REF:
	case Cell::INT32:
	case Cell::MAP:
	    {
		HeapRef dst = toHeapRef(cell);
		if (dst > href) {
		    createForwardPointer(href, dst);
		}
	    }
	    break;
	case Cell::EXT:
	    break;
	}
    }

    void createForwardPointer(HeapRef from, HeapRef to);

    inline void setCell(HeapRef ref, Cell cell)
    {
	*toAbsolute(ref.getIndex()) = cell;
    }

    void print( std::ostream &out, Cell cell,
		const PrintParam &param = PrintParam(78)) const;

    std::string toString(Cell cell) const;

    bool isInRange(Cell cell, HeapRef atStart, HeapRef atEnd);

    inline Cell deref(Cell cell) const
    {
	if (cell.isNull()) return cell;
	while (cell.getTag() == Cell::REF) {
	    if (cell.isNull()) return cell;
	    Cell nextCell = getCell0(toHeapRef(cell));
	    if (nextCell == cell) {
		return cell;
	    }
	    cell = nextCell;
	}
	return cell;
    }

    inline Cell getCell0(HeapRef ref) const
    {
	return *toAbsolute(ref.getIndex());
    }

    inline Cell getFunctor(Cell cell) const
    {
	return getCell0(toHeapRef(deref(cell)));
    }

    inline size_t getArity(Cell cell) const
    {
	cell = deref(cell);
	switch (cell.getTag()) {
	case Cell::CON:
	    return thisConstTable.getConstArity(cell.toConstRef());
	case Cell::STR:
	    {
		Cell fun = getFunctor(cell);
		if (!fun.toConstRef().isNull()) {
		    return thisConstTable.getConstArity(fun.toConstRef());
		} else {
		    return 0;
		}
	    }
	default:
	    return 0; // Unknown
	}
    }

    inline Cell getArg(Cell strCell, size_t index) const
    {
	return getCell0(getArgRef(deref(strCell), index));
    }

    inline HeapRef getArgRef(Cell strCell, size_t index) const
    {
	return toHeapRef(strCell)+index+1;
    }

    inline HeapRef getArgRef(HeapRef functorRef, size_t index) const
    {
	return functorRef+index+1;
    }

    inline bool equal(Cell a, Cell b) const
    {
	return compare(a, b) == 0;
    }

    inline int compare(Cell a, Cell b) const
    {
	int cmp = compareShallow(a,b);
	if (cmp != 0) {
	    return cmp;
	} else {
	    return compareDeep(a,b);
	}
    }

    int compareConst(Cell a, Cell b) const;
    int compareInt32(Cell a, Cell b) const;
    int compareRef(Cell a, Cell b) const;

    inline int compareShallow(Cell a, Cell b) const
    {
	a = deref(a);
	b = deref(b);
	if (a == b) {
	    return 0;
	} else {
	    if (a.getTag() != b.getTag()) {
		return (a.getTag() < b.getTag()) ? -1 : 1;
	    }
	    switch (a.getTag()) {
	    case Cell::CON: return compareConst(a,b);
	    case Cell::INT32: return compareInt32(a,b);
	    case Cell::STR: return 0; // Continue with compare deep
	    case Cell::MAP: return 0; // Continue with compare deep
	    case Cell::REF: return compareRef(a,b);
	    case Cell::EXT: assert("Cell::EXT is unsupported"); return 0;
	    case Cell::FWD: assert("Cell::FWD should not occur in this context"); return 0;
	    }
	}
    }

    int compareDeep(Cell a, Cell b) const;

    void pushRoots(HeapRef atStart, HeapRef atEnd, bool onGCStack);
    void updateRoots(HeapRef atStart, HeapRef atEnd);
    Cell followFwd(Cell cell);
    void resetFreePointers(HeapRef atStart);
    HeapRef findFreeSlot(size_t numCells) const;
    HeapRef findFreeSlotBound(size_t numCells, HeapRef oldLoc) const;
    bool updateForward(HeapRef cell);

    void initiateGC(size_t topSize);
    void finalizeGC(size_t topSize);
    void findLive(size_t topSize);
    void compactMove(HeapRef from, size_t numCells, HeapRef to, int verbosity);
    void compactMove0(HeapRef from, size_t numCells, HeapRef to,int verbosity);

    void compactLive(size_t topSize, int verbosity);
    void printLive(std::ostream &out, size_t topSize) const;

public:

    // ----------------------------------------------------------

    void printRaw( std::ostream &out ) const;
    void printRaw( std::ostream &out, HeapRef from, HeapRef to ) const;

    std::string toRawString() const;
    std::string toRawString(HeapRef from, HeapRef to) const;

    void addGlobalRoot(Cell *cellRef) const;
    void removeGlobalRoot(Cell *cellRef) const;

    void printStatus(std::ostream &out, int detail = 0) const;
    void printRoots(std::ostream &out) const;

    void integrityCheckGlobalRoots();

private:

    inline Cell * allocate(size_t numCells)
    {
	return thisHeap.allocate(numCells);
    }

    inline size_t toRelative(Cell *cellPtr) const
    {
	return thisHeap.toRelative(cellPtr);
    }

    inline bool isValid(Cell *cellPtr) const
    {
	return thisHeap.isValid(cellPtr);
    }

    inline Cell * toAbsolute(size_t relative) const
    {
	return thisHeap.toAbsolute(relative);
    }

    inline Cell * toAbsolute(HeapRef href) const
    {
	return thisHeap.toAbsolute(href.getIndex());
    }


    inline void trim(HeapRef href)
    {
	thisHeap.trim(href.getIndex()-1);
    }

    bool isSpecialFunctor(ConstRef cref) const;

    inline bool isFunctor(Cell cell, ConstRef functor) const
    {
	cell = deref(cell);
	switch (cell.getTag()) {
	case Cell::CON: return cell.toConstRef() == functor;
	case Cell::STR: cell = deref(getCell0(toHeapRef(cell)));
	                return cell.getTag() == Cell::CON &&
			       cell.toConstRef() == functor;
	default:
	    return false;
	}
    }

    inline bool isEmpty(Cell cell) const
    {
	return isFunctor(cell, thisFunctorEmpty);
    }

    inline bool isDot(Cell cell) const
    {
	return isFunctor(cell, thisFunctorDot);
    }

    inline bool isPair(Cell cell) const
    {
	return isFunctor(cell, thisFunctorColon);
    }

    size_t getStringLength(Cell cell, size_t maximum) const;
    size_t getStringLengthForStruct(Cell cell) const;
    size_t getStringLengthForMap(Cell cell) const;
    size_t getStringLengthForRef(Cell cell) const;
    size_t getStringLengthForInt32(int32_t value) const;

    void printTag(std::ostream &out, Cell cell) const;
    void printConst(std::ostream &out, Cell cell) const;
    void printInt32(std::ostream &out, int32_t) const;
    ConstRef pushPrintStrArgs(Cell strCell);
    void pushPrintMapArgs(Cell mapCell, bool &isRoot);
    void printRef(std::ostream &out, Cell cell) const;
    ConstRef getRefName(Cell cell) const;
    CellRef getRef(ConstRef refName) const;
    void printIndent(std::ostream &out, PrintState &state) const;

    CellRef parse( std::istream &in, LocationTracker &loc );
    CellRef parseError(LocationTracker &loc, const std::string &msg);
    void parseSkipWhite( std::istream &in, LocationTracker &loc );
    CellRef parseConst( std::istream &in, LocationTracker &loc );
    CellRef parseArgs( std::istream &in, LocationTracker &loc );
    CellRef parseTerm( std::istream &in, LocationTracker &loc );
    bool parseCheck( int lookahead, Expect expect );
    CellRef expectError( LocationTracker &loc, int lookahead, Expect expect );
    void expectErrorToken(std::ostream &out, const char *token,
			  bool isLast, bool &isFirst );

    void unbind(Cell cell);

    void bindCheckForward(Cell a, Cell b);
    void bind1(Cell a, Cell b);
    void bind(Cell a, Cell b);

    void pushState();
    void popState();
    void discardState();

    struct State {
	HeapRef thisHeapTop;
	size_t thisStackSize;
	size_t thisTrailSize;
	bool thisUseTrail;
    };

    GrowingAllocator<Cell> thisHeap;
    bool thisIsStrict;
    BitMap thisForwardPointers;
    BitMap thisLive;
    BitMap thisVisited;
    HeapRef thisCompactedEnd;
    static const size_t TRACK_SIZES = 256;
    mutable HeapRef thisFreePtr[TRACK_SIZES];
    ConstTable thisConstTable;
    mutable Stack<Cell> thisStack;
    mutable Stack<Cell*> thisStackGC;
    bool thisUseTrail;
    Stack<Cell> thisTrail;
    mutable HashMap<Cell, ConstRef> thisNameMap;
    mutable HashMap<ConstRef, CellRef> thisRefMap;
    Stack<size_t> thisParseArity;
    Stack<State> thisStateStack;

    Cell thisExtComma;
    Cell thisExtEnd;

    static const size_t MAX_NUM_GLOBAL_ROOTS = 1024;
    typedef OpenHashMap<IndexedCellPtr, Cell *> RootMap;
    mutable RootMap thisGlobalRoots;
    mutable size_t thisMaxNumGlobalRoots;

    ConstRef thisFunctorDot;
    ConstRef thisFunctorEmpty;
    ConstRef thisFunctorComma;
    ConstRef thisFunctorColon;

    ConstRef thisPrintRParen;
    ConstRef thisPrintRBrace;
    ConstRef thisPrintRBracket;
    ConstRef thisPrintComma;
    ConstRef thisPrintDot;
    ConstRef thisPrintEmpty;
};

inline CellRef::CellRef() : thisHeap(NULL), thisCell()
{
}

inline CellRef::CellRef(const Heap &heap, Cell cell) : thisHeap(&heap), thisCell(cell)
{
    heap.addGlobalRoot(&thisCell);
}

inline CellRef::~CellRef()
{
    if (thisHeap != NULL) thisHeap->removeGlobalRoot(&thisCell);
    thisHeap = NULL;
    thisCell = Cell(0);
}

inline CellRef::CellRef(const CellRef &other)
    : thisHeap(other.thisHeap), thisCell(other.thisCell)
{
    if (thisHeap != NULL) thisHeap->addGlobalRoot(&thisCell);
}

inline void CellRef::operator = (const CellRef &other)
{
    bool addRoot = thisHeap == NULL && other.thisHeap != NULL;
    thisCell = other.thisCell;
    thisHeap = other.thisHeap;
    if (addRoot) {
	thisHeap->addGlobalRoot(&thisCell);
    }
}

}

#endif
