#include "Term.hpp"
#include <iostream>
#include <sstream>
#include <memory.h>
#include <assert.h>
#include "Stack.hpp"
#include "Flags.hpp"
#include "Hash.hpp"

namespace PROJECT {

// Map node facade (this is what the MAP cell is pointing at)
// [1 bit]: if root yes/no
// [3 bits]: for depth, but 111 (= 7) is reserved as "super root".
// [28 bits]: count
//
// A super root means that the MAP object represents a family of
// MAP roots. This allows lazy growing the hash array mapped trie
// (and avoids rehashing.) If the MapNode is a meta root, then the
// mask represents whether a map exists for depth 1, 2, 3, ...
//
//
class MapNode
{
public:
    inline MapNode(Cell *cellPtr) : thisCellPtr(cellPtr) { }

    inline uint32_t getMeta() const
        { return thisCellPtr[0].getRawValue(); }

    inline void setMeta(uint32_t meta)
        { thisCellPtr[0].setRawValue(meta); }
    inline void setMeta(bool isRoot, size_t depth, size_t count)
    { thisCellPtr[0].setRawValue((isRoot ? 1 : 0) | ((depth & 0x7) << 1) | (count << 4)); }
    inline bool isSuperRoot() const
    { return isRoot() && (((getMeta() >> 1) & 0x7) == 0x7); }
    inline void setSuperRoot()
    { thisCellPtr[0].setRawValue(1 | (0x7 << 1)); }

    inline bool isRoot() const
    { return (getMeta() & 1) != 0; }
    inline size_t getDepth() const
    { return static_cast<size_t>((getMeta() >> 1) & 0x7); }
    inline size_t getCount() const
    { return static_cast<size_t>(getMeta() >> 4); }
    inline void setCount(size_t count)
    { setMeta(isRoot(), getDepth(), getCount() + count); }
    inline void incrementCount()
    { setCount(getCount()+1); }
    inline void decrementCount()
    { setCount(getCount()-1); }

    inline uint32_t getMask() const
    { return static_cast<uint32_t>(thisCellPtr[1].getRawValue()); }

    inline void setMask(uint32_t mask)
    { thisCellPtr[1].setRawValue(mask); }

    inline Cell getArg(size_t index) const
    { return thisCellPtr[2+index]; }
    inline void setArg(size_t index, Cell cell)
    { thisCellPtr[2+index] = cell; }

private:
    Cell *thisCellPtr;
};

class LocationTracker {
public:
    LocationTracker() : thisLine(0), thisColumn(0) { }

    void advance(const char ch)
    {
	// std::cout << "ADVANCE: " << ch << "\n";
	if (ch == '\n') {
	    newLine();
	} else {
	    nextColumn();
	}
    }

    void newLine() { thisLine++; thisColumn = 0; }
    void nextColumn() { thisColumn++; }

    size_t getLine() const { return thisLine; }
    size_t getColumn() const { return thisColumn; }

private:
    size_t thisLine;
    size_t thisColumn;
};

class PrintState {
public:
    static const size_t MAX_INDENT_DEPTH = 100;

    PrintState(const PrintParam &param)
        : thisParam(param),
	  thisNeedNewLine(false),
	  thisColumn(param.getStartColumn()),
	  thisIndent(0)
    {
	for (size_t i = 0; i < MAX_INDENT_DEPTH; i++) {
	    thisIndentTable[i] = 0;
	}
    }

    const PrintParam & getParam() const { return thisParam; }

    size_t getColumn() const { return thisColumn; }

    void markColumn() {
	thisIndentTable[thisIndent] = thisColumn;
    }

    bool willWrap(size_t len) const {
	bool r = thisColumn + len >= thisParam.getEndColumn();
	return r;
    }

    size_t willWrapOnLength() const {
	if (thisColumn < thisParam.getEndColumn()) {
	    return thisParam.getEndColumn() - thisColumn;
	} else {
	    return 0;
	}
    }

    size_t getIndent() const { return thisIndent; }
    void incrementIndent() { thisIndent++; }
    void decrementIndent() { thisIndent--; }

    bool needNewLine() const {
	return thisNeedNewLine;
    }

    PrintState & addToColumn(size_t len)
    { thisColumn += len;
      if (thisColumn > thisParam.getEndColumn()) {
	  thisNeedNewLine = true;
      }
      return *this;
    }

    void resetToColumn(size_t col)
    {
	thisNeedNewLine = false;
	thisColumn = col;
    }

    void newLine(std::ostream &out)
    {
	out << "\n";
	resetToColumn(0);
	printIndent(out);
    }

    void printIndent(std::ostream &out)
    {
	size_t col = thisColumn;
	size_t start = thisParam.getStartColumn();
	while (col < start) {
	    col++;
	    out << " ";
	}

	size_t iw = thisParam.getIndentWidth();
	for (size_t i = 0; i < thisIndent; i++) {
	    size_t p = thisIndentTable[i] != 0 ? thisIndentTable[i] : col+iw;
	    for (size_t j = col; j < p; j++) {
		out << " ";
	    }
	    col = p;
	}
	thisColumn = col;
    }

private:
    const PrintParam &thisParam;
    bool thisNeedNewLine;
    size_t thisColumn;
    size_t thisIndent;
    size_t thisIndentTable[MAX_INDENT_DEPTH];
};

std::ostream & operator << (std::ostream &out, const ConstRef &ref)
{
    if (ref.getIndex() == 0) {
	out << "ConstRef(NULL)";
    } else {
	out << "ConstRef(" << ref.getIndex() << ")";
    }
    return out;
}

const char ConstString::RESERVED[] = { '[', ']', '(', ')', ',', '.', '\\', '\'', ' ', '\0' };
bool ConstString::RESERVED_MAP[256];
bool ConstString::theInitialized = false;

template<typename T> size_t ConstString::escapeName(const T *name, size_t len, char *escapedName)
{
    size_t i = 0, j = 0;
    bool useQuotes = false;

    if (len < 1) {
	if (escapedName != NULL) escapedName[0] = '\0';
	return 0;
    }

    if (name[0] >= 'A' && name[0] <= 'Z') {
	useQuotes = true;
    }
    for (i = 0; i < len; i++) {
	if (isReserved(name[i])) {
	    useQuotes = true;
	    break;
	}
    }
    if (useQuotes) {
	if (escapedName != NULL) escapedName[j] = '\'';
	j++;
    }
    i = 0;
    while (i < len) {
	if (name[i] == '\\' || name[i] == '\'') {
	    if (escapedName != NULL) escapedName[j] = '\\';
	    j++;
	}
	if (escapedName != NULL) escapedName[j] = name[i];
	j++;
	i++;
    }
    if (useQuotes) {
	if (escapedName != NULL) escapedName[j] = '\'';
	j++;
    }
    if (escapedName != NULL) escapedName[j] = '\0';
    return j;
}

int ConstString::compare(const ConstString &other) const
{
    int min = std::min(getLength(), other.getLength());
    for (size_t i = 0; i < min; i++) {
	if (thisString[i] != other.thisString[i]) {
	    if (thisString[i] < other.thisString[i]) {
		return -1;
	    } else {
		return 1;
	    }
	}
    }
    if (getLength() < other.getLength()) {
	return -1;
    } else if (getLength() > other.getLength()) {
	return 1;
    }
    if (getArity() < other.getArity()) {
	return -1;
    } else if (getArity() > other.getArity()) {
	return 1;
    } else {
	return 0;
    }
}

std::ostream & operator << (std::ostream &out, const ConstString &str)
{
    const Char *ch = str.getString();
    size_t n = str.getLength();
    char cstrStack[ConstTable::MAX_CONST_LENGTH];
    char *cstr = &cstrStack[0];

    size_t len = str.isNoEscape() ? n : ConstString::escapeName(ch, n, NULL);
    bool doAlloc = len > sizeof(cstrStack) - 1;

    if (doAlloc) {
	cstr = new char[len+1];
    }

    if (str.isNoEscape()) {
	ConstString::convert(ch, n, cstr);
    } else {
	ConstString::escapeName(ch, n, cstr);
    }
    
    out << cstr;

    if (doAlloc) {
	delete [] cstr;
    }
    
    return out;
}

void ConstString::convert(const Char *src, size_t n, char *dst)
{
    for (size_t i = 0; i < n; i++) {
	dst[i] = (char)src[i];
    }
    dst[n] = '\0';
}

void ConstString::convert(const char *src, size_t n, Char *dst)
{
    for (size_t i = 0; i < n; i++) {
	dst[i] = (Char)src[i];
    }
}

std::string ConstString::asStdString() const
{
    std::stringstream ss;
    const Char *ch = getString();
    for (size_t i = 0; i < thisLength; i++) {
	ss << (char)ch[i];
    }
    if (thisArity != 0) {
	ss << "/";
	ss << thisArity;
    }
    return ss.str();
}

ConstRef ConstTable::getConst(const char *name, size_t arity) const
{
    assert(strlen(name) < MAX_CONST_LENGTH);

    size_t nameLen = strlen(name);

    ConstRef cref = findConst(name, nameLen, arity);
    if (cref == ConstRef()) {
	return const_cast<ConstTable *>(this)->addConst(name, nameLen, arity);
    } else {
	return cref;
    }
}

ConstRef ConstTable::getConst(ConstRef cref, size_t newArity) const
{
    NativeType *c = toAbsolute(cref.getIndex());
    NativeType name = c[0];
    Char *data = thisPool.toAbsolute(name);

    Char *str = &data[2];
    size_t len = (size_t)data[0];
    // size_t oldArity = (size_t)data[1];

    ConstRef newCref = findConst(str, len, newArity);
    if (newCref != ConstRef()) {
	return newCref;
    }

    return const_cast<ConstTable *>(this)->addConst(str, len, newArity);
}

void ConstTable::getConstName(char *name, size_t ordinal) const
{
    static char ALPHABET1 [26] = 
	{ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
	  'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
	  'U', 'V', 'W', 'X', 'Y', 'Z' };

    if (ordinal == 0) {
	name[0] = 'A';
	name[1] = 0;
	return;
    }

    size_t i = 0;

    size_t index;
    while (ordinal > 0) {
	index = ordinal % sizeof(ALPHABET1);
	name[i] = ALPHABET1[index];
	ordinal /= sizeof(ALPHABET1);
	i++;
    }

    size_t i2 = i / 2;
    for (size_t j = 0; j < i2; j++) {
	char ch = name[j];
	name[j] = name[i - j - 1];
	name[i - j - 1] = ch;
    }
    if (i > 1) {
	name[0]--; 
    }
    name[i] = '\0';
}

ConstRef ConstTable::getConst(size_t ordinal) const
{
    char str[16];
    getConstName(str, ordinal);
    return getConst(str, 0);
}

ConstRef ConstTable::addConst(const char *name, size_t nameLen, size_t arity)
{
    ConstString str = thisPool.addString(name, nameLen, arity);
    NativeType *cr = allocate(1);
    NativeType rel = static_cast<NativeType>(thisPool.toRelativePointer(str));
    *cr = rel;
    ConstRef constRef(toRelative(cr));
    thisIndexing.add(str, constRef);
    return constRef;
}

ConstRef ConstTable::addConst(const Char *name, size_t nameLen, size_t arity)
{
    ConstString str = thisPool.addString(name, nameLen, arity);
    NativeType *cr = allocate(1);
    NativeType rel = static_cast<NativeType>(thisPool.toRelativePointer(str));
    *cr = rel;
    ConstRef constRef(toRelative(cr));
    thisIndexing.add(str, constRef);
    return constRef;
}

ConstRef ConstTable::findConst(const char *name, size_t nameLen,
			      size_t arity) const
{
    assert(nameLen < MAX_CONST_LENGTH);

    Char chs[MAX_CONST_LENGTH];
    for (size_t i = 0; i <= nameLen; i++) {
	chs[i] = (Char)name[i];
    }

    return findConst(chs, nameLen, arity);
}

ConstRef ConstTable::findConst(const Char *name, size_t nameLen,
			      size_t arity) const
{
    ConstString str(name, nameLen, arity);
    return thisIndexing.find(str);
}

void ConstTable::print(std::ostream &out) const
{
    size_t n = getSize();
    for (size_t i = 1; i <= n; i++) {
	ConstRef cr(i);
	NativeType *c = toAbsolute(cr.getIndex());
	NativeType name = c[0];
	Char *data = thisPool.toAbsolute(name);
	ConstString str(&data[2], (size_t)data[0], (size_t)data[1]);
	out << "[" << i << "]: " << str;
	out << "\n";
    }
}

void ConstTable::printConst(std::ostream &out, const ConstRef &ref) const
{
    NativeType *c = toAbsolute(ref.getIndex());
    NativeType name = c[0];
    Char *data = thisPool.toAbsolute(name);
    ConstString str(&data[2], (size_t)data[0], (size_t)data[1]);
    out << str;
}

size_t ConstTable::getConstArity(const ConstRef &ref) const
{
    if (ref.isNull()) {
	return 0;
    }
    NativeType *c = toAbsolute(ref.getIndex());
    NativeType name = c[0];
    Char *data = thisPool.toAbsolute(name);
    if (data == NULL) {
	return 0;
    }
    return (size_t)data[1];
}

void ConstTable::printConstNoArity(std::ostream &out, const ConstRef &ref) const
{
    if (ref.isNull()) {
	out << "null";
	return;
    }
    NativeType *c = toAbsolute(ref.getIndex());
    NativeType name = c[0];
    Char *data = thisPool.toAbsolute(name);
    if (data == NULL) {
	out << "null";
	return;
    }
    ConstString str(&data[2], (size_t)data[0], (data[1] == ConstString::MAX_ARITY) ? ConstString::MAX_ARITY : 0);
    out << str;
}

void ConstTable::printConstNoEscape(std::ostream &out, const ConstRef &ref) const
{
    NativeType *c = toAbsolute(ref.getIndex());
    NativeType name = c[0];
    Char *data = thisPool.toAbsolute(name);
    ConstString str(&data[2], (size_t)data[0], 0);
    str.setNoEscape(true);
    out << str;
}

size_t ConstTable::getConstLength(const ConstRef &ref) const
{
    if (ref.isNull()) {
	return 4; // "null"
    }
    NativeType *c = toAbsolute(ref.getIndex());
    NativeType name = c[0];
    Char *data = thisPool.toAbsolute(name);
    if (data == NULL) {
	return 4; // "null";
    }
    return static_cast<size_t>(data[0]);
}

ConstString ConstTable::getConstName(const ConstRef &ref) const
{
    NativeType *c = toAbsolute(ref.getIndex());
    NativeType name = c[0];
    Char *data = thisPool.toAbsolute(name);
    ConstString str(&data[2], (size_t)data[0], 0);
    return str;
}

Heap::Heap(size_t capacity)
       : thisInGC(false),
         thisHeap(capacity),
         thisIsStrict(false),
	 thisForwardPointers(capacity),
	 thisLive(capacity),
	 thisVisited(capacity),
	 thisNameMap(),
	 thisRefMap(),
	 thisGlobalRoots(capacity),
	 thisMaxNumGlobalRoots(0)
{
    // These are special functors. Not that special, but the
    // pretty printer treats them differently.
    thisFunctorDot = getConst(".", 2);
    thisFunctorEmpty = getConst("[]", 0);
    thisFunctorComma = getConst(",", 2);
    thisFunctorColon = getConst(":", 2);

    thisPrintRParen = getConst(")", ConstString::MAX_ARITY);
    thisPrintRBrace = getConst("}", ConstString::MAX_ARITY);
    thisPrintRBracket = getConst("]", ConstString::MAX_ARITY);
    thisPrintComma = getConst(", ", ConstString::MAX_ARITY);
    thisPrintDot = getConst(" . ", ConstString::MAX_ARITY);
    thisPrintEmpty = getConst("[]", ConstString::MAX_ARITY);

    thisUseTrail = false;
}

// A strict heap clears the cells after compaction.
// This is just for sanity/debugging while developing the (partial) GC.
bool Heap::isStrict() const
{
    return thisIsStrict;
}

void Heap::setStrict(bool isStrict)
{
    thisIsStrict = isStrict;
}

void Heap::addGlobalRoot(Cell *cellPtr) const
{
    thisGlobalRoots.put(IndexedCellPtr(cellPtr), cellPtr);
    if (thisGlobalRoots.numEntries() > thisMaxNumGlobalRoots) {
	thisMaxNumGlobalRoots = thisGlobalRoots.numEntries();
    }
}

void Heap::removeGlobalRoot(Cell *cellPtr) const
{
    thisGlobalRoots.remove(IndexedCellPtr(cellPtr));
}

void Heap::integrityCheckGlobalRoots()
{
    static int cnt = 0;
    cnt++;
    std::cout << "Heap::integrityCheckGlobalRoots(): cnt=" << cnt << "\n";
    HeapRef atStart = firstHeapRef();
    HeapRef atEnd = topHeapRef();
    RootMap::iterator itEnd = thisGlobalRoots.end();
    for (RootMap::iterator it = thisGlobalRoots.begin(); it != itEnd; ++it) {
	Cell *cellPtr = it->getValue();
	if (cellPtr != NULL) {
	    if (!cellPtr->isNull() && hasHeapRef(*cellPtr)) {
		HeapRef href = toHeapRef(*cellPtr);
		if (href < atStart || href >= atEnd+10) {
		    std::cout << "CHECK SLOT " << it.getBucket() << "\n";
		}
		assert(atStart <= href && href < atEnd+10);
	    }
	}
    }
}

ConstRef Heap::getConst(const char *name, size_t arity) const
{
    return thisConstTable.getConst(name, arity);
}

ConstRef Heap::getConst(size_t ordinal) const
{
    return thisConstTable.getConst(ordinal);
}

ConstRef Heap::getConst(ConstRef other, size_t newArity) const
{
    return thisConstTable.getConst(other, newArity);
}

ConstString Heap::getConstName(ConstRef cref) const
{
    return thisConstTable.getConstName(cref);
}

void Heap::printTag(std::ostream &out, Cell cell) const
{
    switch (cell.getTag()) {
    case Cell::REF: out << "REF"; break;
    case Cell::CON: out << "CON"; break;
    case Cell::STR: out << "STR"; break;
    case Cell::MAP: out << "MAP"; break;
    case Cell::INT32: out << "INT32"; break;
    case Cell::FWD: out << "FWD"; break;
    case Cell::EXT:
	default: out << "EXT?"; break;
    }
}

void Heap::printConst(std::ostream &out, Cell cell) const
{
    ConstRef cref = cell.toConstRef();
    thisConstTable.printConst(out, cref);
}

void Heap::printCell(std::ostream &out, Cell cell) const
{
    printTag(out, cell);
    out << ":";
    switch (cell.getTag()) {
    case Cell::REF: out << cell.getValue(); break;
    case Cell::CON: {
	printConst(out, cell);
	size_t arity = thisConstTable.getConstArity(cell.toConstRef());
	if (arity != 0) {
	    out << "/";
	    out << arity;
	}
	break;
    }
    case Cell::STR: out << cell.getValue(); break;
    case Cell::MAP: out << cell.getValue(); break;
    case Cell::INT32: out << toInt32(cell); break;
    case Cell::FWD: {
	out << "$fwd(" << cell.getValue() << ")"; break;
    }
    case Cell::EXT: out << "???"; break;
    }
}

void Heap::printRaw(std::ostream &out) const
{
    printRaw(out, firstHeapRef(), topHeapRef());
}

void Heap::printRaw(std::ostream &out, HeapRef from, HeapRef to) const
{
    for (HeapRef i = from; i < to; i++) {
	out << "[" << i.getIndex() << "]: ";
	Cell cell = getCell0(i);
	if (cell.getTag() == Cell::REFARG && toHeapRef(cell) == i) {
	    out << "REF:" << cell.getValue() << " (REFARG)";
	} else {
	    printCell(out, cell);
	}
	if (thisForwardPointers.hasBit(i.getIndex())) {
	    std::cout << " (FPTR)";
	}
	out << "\n";
    }
}

std::string Heap::toRawString() const
{
    return toRawString(firstHeapRef(), topHeapRef());
}

std::string Heap::toRawString(HeapRef from, HeapRef to) const
{
    std::stringstream ss;
    ss << "[";
    for (HeapRef i = from; i < to; i++) {
	if (i != from) {
	    ss << ", ";
	}
	Cell cell = getCell0(i);
	printCell(ss, cell);
    }
    ss << "]";
    return ss.str();
}

bool Heap::isSpecialFunctor(ConstRef cref) const
{
    return cref == thisFunctorComma ||
	   cref == thisFunctorDot ||
	   cref == thisFunctorEmpty ||
	   cref == thisFunctorColon;
}

size_t Heap::getStringLength(Cell cell, size_t maximum) const
{
    size_t current = thisStack.getSize();

    thisStack.push(cell);

    size_t len = 0;

    while (current != thisStack.getSize()) {
	if (len >= maximum) {
	    thisStack.trim(current);
	    return maximum;
	}

	Cell cell = deref(thisStack.pop());
	if (cell.isNull()) {
	    len += 4;
	    continue;
	}

	switch (cell.getTag()) {
	case Cell::CON:
	    len += thisConstTable.getConstLength(cell.toConstRef()); break;
	case Cell::STR:
	    len += getStringLengthForStruct(cell); break;
	case Cell::MAP:
	    len += getStringLengthForMap(cell); break;
	case Cell::REF:
	    len += getStringLengthForRef(cell); break;
	case Cell::INT32:
	    len += getStringLengthForInt32(toInt32(cell)); break;
	case Cell::FWD:
	    len += getStringLengthForInt32(cell.getValue()) + 6; break;
        case Cell::EXT:
	    break;
	}
    }
    return len;
}

size_t Heap::getStringLengthForStruct(Cell cell) const
{
    size_t len = 0;
    ConstRef cref = const_cast<Heap *>(this)->pushPrintStrArgs(cell);
    len += thisConstTable.getConstLength(cref);
    size_t arity = thisConstTable.getConstArity(cref);
    if (arity > 0) {
	len++;
    }
    return len;
}

size_t Heap::getStringLengthForMap(Cell cell) const
{
    size_t len = 0;
    bool isRoot = false;
    const_cast<Heap *>(this)->pushPrintMapArgs(cell, isRoot);
    if (isRoot) { // This is a root node of a map
	len++; // The '{' character
    }

    return len;
}

size_t Heap::getStringLengthForRef(Cell cell) const
{
    const ConstRef *pcref = thisNameMap.get(cell);
    if (pcref == NULL) {
	ConstRef cref = thisConstTable.getConst(thisNameMap.numEntries());
	thisNameMap.put(cell, cref);
	return thisConstTable.getConstLength(cref);
    } else {
	return thisConstTable.getConstLength(*pcref);
    }
}

size_t Heap::getStringLengthForInt32(int32_t value) const
{
    if (value == 0) {
	return 1;
    }
    size_t cnt = 0;
    if (value < 0) {
	cnt++;
    }
    while (value != 0) {
	value /= 10;
	cnt++;
    }
    return cnt;
}

std::string Heap::toString(CellRef cellRef) const
{
    std::stringstream ss;
    print(ss, cellRef);
    return ss.str();
}

std::string Heap::toString(Cell cell) const
{
    std::stringstream ss;
    print(ss, cell);
    return ss.str();
}

ConstRef Heap::pushPrintStrArgs(Cell strCell)
{
    size_t arity = getArity(strCell);
    ConstRef fun = getFunctor(strCell).toConstRef();
    if (isSpecialFunctor(fun)) {
	// Treat lists specially
	if (fun == thisFunctorDot) {
	    size_t stackMark = thisStack.getSize();
	    while (isDot(strCell)) {
		thisStack.push(getArg(strCell, 0));
		strCell = getArg(strCell, 1);
		if (isDot(strCell)) {
		    thisStack.push(Cell(thisPrintComma));
		}
	    }
	    if (!isEmpty(strCell)) {
		thisStack.push(Cell(thisPrintDot));
		thisStack.push(strCell);
	    }
	    thisStack.push(Cell(thisPrintRBracket));
	    // Now reverse the last items on stack
	    thisStack.reverse(thisStack.getSize() - stackMark);
	} else {
	    // Special functor are always in infix form (with two arguments.)
	    if (fun == thisFunctorComma) {
		thisStack.push(thisPrintRParen);
	    }
	    thisStack.push(getArg(strCell, 1));
	    thisStack.push(Cell(fun));
	    thisStack.push(getArg(strCell, 0));
	}
    } else {
	if (arity > 0) {
	    thisStack.push(thisPrintRParen);
	    for (size_t i = 0; i < arity; i++) {
		if (i > 0) {
		    thisStack.push(thisPrintComma);
		}
		Cell arg = getArg(strCell, arity-i-1);
		thisStack.push(arg);
	        //
		/*
		HeapRef argRef = getArgRef(strCell, arity-i-1);
		char msg[512];
		sprintf(msg, "#%d:", argRef.getIndex());
		thisStack.push(Cell(getConst(msg,0)));
		*/
	    }
	}
    }
    return fun;
}

void Heap::pushPrintMapArgs(Cell mapCell, bool &isRoot)
{
    Cell *cellPtr = toAbsolute(toHeapRef(mapCell));
    MapNode mapNode(cellPtr);
    bool isRoot0 = mapNode.isRoot();
    isRoot = isRoot0;
    if (isRoot0) {
	thisStack.push(thisPrintRBrace);
    }
    uint32_t mask = mapNode.getMask();
    size_t cnt = bitCount(mask);
    bool first = true;
    for (size_t i = 0; i < 32; i++) {
	if ((mask & (1 << (31-i))) != 0) {
	    Cell arg = mapNode.getArg(cnt-1);
	    size_t stackMark = getStackSize();
	    if (isDot(arg) || isEmpty(arg)) {
		while (isDot(arg)) {
		    if (!first) {
			thisStack.push(thisPrintComma);
		    }
		    first = false;
		    thisStack.push(getArg(arg,0));
		    arg = getArg(arg,1);
		}
		thisStack.reverse(getStackSize()-stackMark);
	    } else {
		if (!first) {
		    thisStack.push(thisPrintComma);
		}
	        first = false;
		thisStack.push(arg);
	    }
	    cnt--;
	}
    }
}

ConstRef Heap::getRefName(Cell cell) const
{
    const ConstRef *pcref = thisNameMap.get(cell);
    if (pcref == NULL) {
	ConstRef cref = thisConstTable.getConst(thisNameMap.numEntries());
	thisNameMap.put(cell, cref);
	return cref;
    }
    return *pcref;
}

CellRef Heap::getRef(ConstRef name) const
{
    const CellRef *ref = thisRefMap.get(name);
    if (ref == NULL) {
	Cell cell = *const_cast<Heap *>(this)->newRef();
	CellRef cellRef(const_cast<Heap &>(*this), cell);
	thisRefMap.put(name, cellRef);
	return cellRef;
    } else {
	return *ref;
    }
}

void Heap::printIndent(std::ostream &out, PrintState &state) const
{
    if (state.needNewLine()) {
	out << "\n";
	state.resetToColumn(0);
	state.printIndent(out);
    }
}

void Heap::print(std::ostream &out, CellRef cellRef, const PrintParam &param) const
{
    print(out, *cellRef, param);
}

void Heap::print(std::ostream &out, Cell cell, const PrintParam &param) const
{
    PrintState state(param);

    size_t current = thisStack.getSize();

    thisStack.push(cell);

    while (current != thisStack.getSize()) {
        Cell cell = deref(thisStack.pop());
	if (cell.isNull()) {
	    out << "null";
	    continue;
	}

	switch (cell.getTag()) {
	case Cell::CON: {
	    ConstRef cref = cell.toConstRef();
	    printIndent(out, state.addToColumn(thisConstTable.getConstLength(cref)));
	    if (isSpecialFunctor(cref)) {
		thisConstTable.printConstNoEscape(out, cref);
	    } else {
		thisConstTable.printConstNoArity(out, cref);
	    }
	    if (cref == thisPrintRParen || cref == thisPrintRBracket ||
		cref == thisPrintRBrace) {
		state.decrementIndent();
	    }
	    break;
  	    }
	case Cell::INT32: {
	    printIndent(out, state.addToColumn(getStringLengthForInt32(
					       toInt32(cell))));
	    out << toInt32(cell);
	    break;
	    }
	case Cell::STR:
	    {
	    if (state.getIndent() > 0 &&
		state.willWrap(getStringLength(cell,
					       state.willWrapOnLength()))) {
		
		state.newLine(out);
	    }
	    ConstRef fun = const_cast<Heap *>(this)->pushPrintStrArgs(cell);
	    if (isSpecialFunctor(fun)) {
		printIndent(out, state.addToColumn(1));
		bool incIndent = false;
		if (fun == thisFunctorComma) {
		    out << "(";
		    incIndent = true;
		} else if (fun == thisFunctorDot) {
		    out << "[";
		    incIndent = true;
		}
		if (incIndent) {
		    state.markColumn();
		    state.incrementIndent();
		}
	    } else {
		size_t arity = thisConstTable.getConstArity(fun);
		printIndent(out, state.addToColumn(
			   thisConstTable.getConstLength(fun)
			   + ((arity > 0) ? 1 : 0)));
		thisConstTable.printConstNoArity(out, fun);
		if (arity > 0) {
		    out << "(";
		    state.markColumn();
		    state.incrementIndent();
		}
	    }
	    break;
	    }
	case Cell::MAP:
	    {
	    if (state.getIndent() > 0 &&
		state.willWrap(getStringLength(cell,
					       state.willWrapOnLength()))) {
		state.newLine(out);
	    }
	    bool isRoot = false;
	    const_cast<Heap *>(this)->pushPrintMapArgs(cell, isRoot);
	    if (isRoot) {
		printIndent(out, state.addToColumn(1));
		out << "{";
		state.markColumn();
		state.incrementIndent();
	    }
	    break;
	    }
	case Cell::REF: 
	    {
	     ConstRef cref = getRefName(cell);
	     printIndent(out, state.addToColumn(
		 thisConstTable.getConstLength(cref)));
	     thisConstTable.printConstNoEscape(out, cref);
             break;
    	    }
	case Cell::FWD:
	    {
	     printIndent(out, state.addToColumn(
	      	   getStringLengthForInt32(cell.getValue()) + 6));
	     out << "$fwd(" << cell.getValue() << ")";
	     break;
	    }
        case Cell::EXT:
	    printIndent(out, state.addToColumn(3));
	    out << "???";
	    break;
	}
    }
}

void Heap::printStatus(std::ostream &out, int detail) const
{
    out << "Heap{Size=" << getHeapSize() << ",StackSize=" << getStackSize() << ",GlobalRootsSize=" << thisGlobalRoots.numEntries() << ",MaxNumGlobalRoots=" << thisMaxNumGlobalRoots << "}\n";
    if (detail > 0) {
	bool first = true;
	out << "ForwardPointers: ";
	BitMap::iterator itEnd = const_cast<Heap *>(this)->thisForwardPointers.end();
	for (BitMap::iterator it = const_cast<Heap *>(this)->thisForwardPointers.begin();
	 it != itEnd;
	 ++it) {
	    if (!first) { out << ", "; }
	    out << *it << ":";
	    HeapRef href((NativeType)*it);
	    printCell(out, getCell0(href));
	    out << ":";
	    printCell(out, getCell0(toHeapRef(getCell0(href))));
	    first = false;
	}
	out << "\n";
    }
}

void Heap::printRoots(std::ostream &out) const
{
    thisGlobalRoots.print(out);
}

void Heap::parseSkipWhite(std::istream &in, LocationTracker &loc)
{
    while (isspace(in.peek())) {
	char ch;
	in >> ch;
	loc.advance(ch);
    }
}

CellRef Heap::parseConst(std::istream &in, LocationTracker &loc)
{
    char constName[ConstTable::MAX_CONST_LENGTH];
    size_t i = 0;

    char ch = (char)in.peek();
    if (ch == EOF) {
	return parseError(loc, "Expecting constant but got EOF");
    }

    bool useQuotes = false;
    if (ch == '\'') {
	useQuotes = true;
	constName[i] = ch;
	in >> ch;
	loc.advance(ch);
	i++;
    } else {
	if (ConstString::isReserved(ch)) {
	    std::string msg = "Character '";
	    msg += ch;
	    msg += "' is not allowed for constants. Use quotes to embed such characters.";
	    return parseError(loc, msg);
	}
    }

    bool cont = true;

    while (cont) {
	ch = (char)in.peek();
	if (ch == '\\') {
	    if (useQuotes) {
		in >> ch;
		loc.advance(ch);
		in >> ch;
		loc.advance(ch);
		constName[i] = ch;
		i++;
	    } else {
		cont = false;
	    }
	} else {
	    if (useQuotes) {
		if (ch == '\'') {
		    in >> ch;
		    loc.advance(ch);
		    cont = false;
		}
	    } else {
		if (ConstString::isReserved(ch)) {
		    cont = false;
		} else {
		    in >> ch;
		    loc.advance(ch);
		    constName[i] = ch;
		    i++;
		}
	    }
	}
    }
    constName[i] = '\0';

    return newConst(constName, 0);
}

bool Heap::parseCheck(int lookahead, Expect expect)
{
    if (lookahead == ',' && (expect & COMMA)) {
	return true;
    }
    if (lookahead == '(' && (expect & LPAREN)) {
	return true;
    }
    if (lookahead == ')' && (expect & RPAREN)) {
	return true;
    }
    if (expect & TERM) {
	if (ConstString::isReserved((Char)lookahead)) {
	    return false;
	} else {
	    return true;
	}
    }
    return false;
}

void Heap::expectErrorToken(std::ostream &os,
		            const char *token,
			    bool isLast,
			    bool &isFirst)
{
    if (!isFirst) {
	os << ", ";
    }
    if (!isFirst && isLast) {
	os << "or ";
    }
    isFirst = false;
    os << token;
}

CellRef Heap::expectError(LocationTracker &loc, int lookahead, Expect expect)
{
    Expect processed;

    std::stringstream ss;
    ss << "Expecting ";
    bool first = true;

    if (expect & LPAREN) {
	processed = processed | LPAREN;
	expectErrorToken(ss, "'('", expect == processed, first);
    }

    if (expect & RPAREN) {
	processed = processed | RPAREN;
	expectErrorToken(ss, "')'", expect == processed, first);
    }

    if (expect & COMMA) {
	processed = processed | COMMA;
	expectErrorToken(ss, "','", expect == processed, first);
    }

    if (expect & TERM) {
	processed = processed | TERM;
	expectErrorToken(ss, "TERM", expect == processed, first);
    }

    ss << " but got '" << ((char)lookahead) << "'";

    return parseError(loc, ss.str());
}

CellRef Heap::parseTerm(std::istream &in, LocationTracker &loc)
{
    thisRefMap.clear();

    size_t depth = 0;
    Expect expect;

    parseSkipWhite(in, loc);

    CellRef result;

    CellRef currentFunctor = parseConst(in, loc);
    size_t current = thisStack.getSize();
    thisStack.push(*currentFunctor);

    size_t currentArity = 0;

    expect = LPAREN;

    while (!in.eof() && result.isEmpty() && current != thisStack.getSize()) {
	parseSkipWhite(in, loc);
	int lookahead = in.peek();

	if (!parseCheck(lookahead, expect)) {
	    result = expectError(loc, lookahead, expect);
	    break;
	}

	if (lookahead == '(') {
	    char ch;
	    in >> ch;
	    loc.advance(ch);
	    thisParseArity.push(currentArity);
	    currentArity = 1;
	    depth++;

	    expect = TERM;
	} else if (lookahead == ')') {
	    char ch;
	    in >> ch;
	    loc.advance(ch);

	    // On stack we now have:
	    // [functor]
	    // [arg n]
	    // [arg n-1]
	    // ...
	    // [arg 0]
	    //
	    Cell functorRef = thisStack.peek(currentArity);
	    ConstRef functorCref = functorRef.toConstRef();

	    // std::cout << "Manage functor: " << getConstName(functorCref) << " arity " << currentArity << "\n";

	    ConstRef functor = getConst(functorCref, currentArity);
	    CellRef strCell = newStr(functor);
	    for (size_t i = 0; i < currentArity; i++) {
		Cell arg = thisStack.peek(currentArity-1-i);
		setArg(strCell, i, CellRef(*this,arg));
	    }
	    thisStack.trim(thisStack.getSize()-currentArity-1);

	    // Is this the last item to process?
	    if (current == thisStack.getSize()) {
		result = strCell;
	    } else {
		thisStack.push(*strCell);
	    }

	    expect = COMMA | RPAREN;

	    // currentArity is the number of arguments on stack
	    currentArity = thisParseArity.pop();
	    depth--;

	} else if (lookahead == ',') {
	    char ch;
	    in >> ch;
	    loc.advance(ch);
	    currentArity++;

	    expect = TERM;
	} else if ((lookahead >= 'A' && lookahead <= 'Z')
		   || lookahead == '_') {
	    char varName[ConstTable::MAX_CONST_LENGTH];
	    size_t index = 0;
	    // Variable
	    do {
		char ch;
		in >> ch;
		loc.advance(ch);
		varName[index] = ch;
		index++;
		if (index == ConstTable::MAX_CONST_LENGTH) {
		    thisStack.trim(current);
		    result = parseError(loc, "Variable name too long");
		    return result;
		}
		lookahead = in.peek();
	    } while (isalnum(lookahead) || lookahead == '_');

	    varName[index] = '\0';
	    ConstRef refName = getConst(varName, 0);
	    CellRef ref = getRef(refName);
	    thisStack.push(*ref);

	    expect = LPAREN | COMMA | RPAREN;
	} else {
	    // Expect term
	    CellRef newFunctor = parseConst(in, loc);
	    thisStack.push(*newFunctor);

	    expect = LPAREN | COMMA | RPAREN;
	}
    }

    // Remove any remaining garbage on stack
    thisStack.trim(current);

    if (result.isEmpty()) {
	result = parseError(loc, "TERM ended too early");
    }

    thisRefMap.clear();

    return result;
}

CellRef Heap::parseError(LocationTracker &loc, const std::string &reason)
{
    ConstRef cref = thisConstTable.getConst("$parseError", 3);
    ConstRef creason = thisConstTable.getConst(reason.c_str(), 0);
    newConOnHeap(creason);
    CellRef reasonCon = newConOnHeap(creason);
    CellRef lineArg = newInt32(loc.getLine());
    CellRef colArg = newInt32(loc.getColumn());
    CellRef err = newStr(cref);
    setArg(err, 0, reasonCon);
    setArg(err, 1, lineArg);
    setArg(err, 2, colArg);
    return err;
}

CellRef Heap::parse(std::istream &in, LocationTracker &loc)
{
    parseSkipWhite(in, loc);
    CellRef cellRef = parseTerm(in, loc);
    return cellRef;
}

CellRef Heap::parse(std::istream &in)
{
    LocationTracker loc;
    return parse(in, loc);
}

void Heap::unbind(Cell ref)
{
    HeapRef href = toHeapRef(ref);
    setCell(href, Cell(ref.getTag(), href));
}

void Heap::bind1(Cell a, Cell b)
{
    if (thisUseTrail) thisTrail.push(a);
    setCell(toHeapRef(a), b);
}

void Heap::bind(Cell a, Cell b)
{
    if (a.getTag() == Cell::REF && b.getTag() == Cell::REF) {
	HeapRef ha = toHeapRef(a);
	HeapRef hb = toHeapRef(b);
	if (ha < hb) {
	    bind1(b, a);
	} else {
	    bind1(a, b);
	}
    } else if (a.getTag() != Cell::REF) {
	bind1(b, a);
	checkForwardPointer(toHeapRef(b), a);
    } else {
	bind1(a, b);
	checkForwardPointer(toHeapRef(a), b);
    }
}

void Heap::createForwardPointer(HeapRef from)
{
    // Create a ref cell pointing at 'from'
    // ('from' is the source of the forward pointer.)
    Cell cell(Cell::REF, from);
    HeapRef ref;
    if (thisInGC) {
	/*
	HeapRef top = topHeapRef();
	ref = findFreeSlot(1, top);
	if (ref == top) {
	    Cell *forwardPtr = allocate(1);
	    *forwardPtr = cell;
	    ref = toRelative(forwardPtr);
	    if (ref >= thisCompactedEnd) {
		thisCompactedEnd = topHeapRef();
	    }
	}
	setCell(ref, cell);
	*/
    } else {
	Cell *forwardPtr = allocate(1);
	*forwardPtr = cell;
	ref = toRelative(forwardPtr);
    }

    // Set a bit on this REF cell that tracks the forward pointer.
    thisForwardPointers.setBit(ref.getIndex());
}

void Heap::pushState()
{
    State state;
    state.thisHeapTop = topHeapRef();
    state.thisStackSize = getStackSize();
    state.thisTrailSize = thisTrail.getSize();
    state.thisUseTrail = thisUseTrail;
    thisStateStack.push(state);
}

void Heap::popState(bool forFail)
{
    State state = thisStateStack.pop();
    if (forFail) {
	while (thisTrail.getSize() != state.thisTrailSize) {
	    Cell cell = thisTrail.pop();
	    unbind(cell);
	}
    }
    thisUseTrail = state.thisUseTrail;
    if (forFail) {
	trim(state.thisHeapTop);
	thisStack.trim(state.thisStackSize);
    }
}

bool Heap::unify(CellRef a, CellRef b)
{
    pushState();

    thisUseTrail = true;

    size_t stack0 = thisStack.getSize();

    thisStack.push(*a);
    thisStack.push(*b);

    while (stack0 != thisStack.getSize()) {
	Cell ca = deref(thisStack.pop());
	Cell cb = deref(thisStack.pop());

	if (ca == cb) {
	    continue;
	}

	bool isRefA = ca.getTag() == Cell::REF;
	bool isRefB = cb.getTag() == Cell::REF;
	if (isRefA || isRefB) {
	    bind(ca, cb);
	} else if (ca != cb) {
	    if (ca.getTag() != cb.getTag()) {
		// Different tags? Always fail...
		popState(true);
		return false;
	    }
	    switch (ca.getTag()) {
	    case Cell::CON:
		// Const must be inequal (otherwise ca == cb)
		popState(true);
		return false;
	    case Cell::STR:
		{
		    HeapRef sa = toHeapRef(ca);
		    HeapRef sb = toHeapRef(cb);
		    ConstRef sca = getCell0(sa).toConstRef();
		    ConstRef scb = getCell0(sb).toConstRef();
		    if (sca != scb) {
			popState(true);
			return false;
		    }
		    size_t arity = thisConstTable.getConstArity(sca);
		    for (size_t i = 0; i < arity; i++) {
			Cell argA = getArg(ca, i);
			Cell argB = getArg(cb, i);
			thisStack.push(argA);
			thisStack.push(argB);
		    }
		}
		break;
	    case Cell::INT32:
		{
		    if (toInt32(ca) != toInt32(cb)) {
			popState(true);
			return false;
		    }
		}
		break;
	    default:
		{
		    popState(true);
		    return false;
		}
	    }
	}
    }

    popState(false);

    return true;
}

size_t Heap::getStackSize() const
{
    return thisStack.getSize();
}

//
// Algorithm for garbage collection.
//
// If topSize == 0, then we'll do a full GC, otherwise a partial GC
// by only cosidering the top number of cells.
//
// For full GC we do:
//   Find all live references starting with stack and root.

void Heap::gc(size_t windowSize, int verbosity)
{
    HeapRef atStart = topHeapRef();
    HeapRef atEnd = atStart;
    if (windowSize == 0 || windowSize > getHeapSize()) {
	windowSize = getHeapSize();
    }
    atStart = atEnd - windowSize;

    // This variable only controls on how to allocate forward
    // pointers. Instead of just grabbing the top of the heap
    // it's using the free pointer scan.
    thisInGC = true;
    if (verbosity > 0) {
	std::cout << "Heap::gc(): windowSize=" << windowSize << " verbosity=" << verbosity << " top=" << topHeapRef().getIndex() << "\n";

    }
    HeapRef fromHeapRef = atStart;

    initiateGC(fromHeapRef);
    findLive(fromHeapRef);
    if (verbosity > 0) printLive(std::cout, fromHeapRef);
    if (verbosity > 2) {
	std::cout << "Heap::gc(): State of heap before GC: >>> \n";
	printRaw(std::cout);
	std::cout << "Heap::gc(): State of heap before GC: <<< \n";
    }
    compactLive(Heap::COMPACT_MOVE, fromHeapRef, verbosity);
    compactForwardPointers(fromHeapRef, verbosity);
    if (verbosity > 2) {
	std::cout << "Heap::gc(): State of heap after COMPACT_MOVE: >>> \n";
	printRaw(std::cout);
	std::cout << "Heap::gc(): State of heap after COMPACT_MOVE: <<< \n";
    }
    compactLive(Heap::COMPACT_UPDATE_FWD, fromHeapRef, verbosity);
    if (verbosity > 2) {
	std::cout << "Heap::gc(): State of heap after COMPACT_UPDATE_FWD: >>> \n";
	printRaw(std::cout);
	std::cout << "Heap::gc(): State of heap after COMPACT_UPDATE_FWD: <<< \n";
    }
    finalizeGC(fromHeapRef);
    if (verbosity > 2) {
	std::cout << "Heap::gc(): State of heap after GC: >>> \n";
	printRaw(std::cout);
	std::cout << "Heap::gc(): State of heap after GC: <<< \n";
    }
    HeapRef afterTop = topHeapRef();
    if (verbosity > 0) {
	printLive(std::cout, fromHeapRef);
	int compacted = atEnd.getIndex() - afterTop.getIndex();
	std::cout << "Heap::gc(): top=" << atEnd.getIndex() << " after=" << afterTop.getIndex() << " compacted=" << compacted << "\n";
    }
    thisInGC = false;
}

void Heap::gc(double percentage, int verbosity)
{
    size_t heapSize = getHeapSize();
    size_t windowSize = (size_t)(percentage * heapSize);
    gc(windowSize, verbosity);
}

void Heap::printLive(std::ostream &out, HeapRef fromHeapRef) const
{
    HeapRef atStart = fromHeapRef;
    HeapRef atEnd = topHeapRef();

    out << "Live:[\n";
    size_t col = 0;
    size_t atEndIndex = atEnd.getIndex();
    for (size_t i = atStart.getIndex(); i < atEndIndex; i += NativeTypeBits) {
	size_t cnt = 0;
	for (size_t j = 0; j < NativeTypeBits && i+j < atEndIndex; j++) {
	    if (thisLive.hasBit(i+j)) {
		cnt++;
	    }
	}
	if (cnt == 0) out << " "; else out << "*";
	if (++col == 70) {
	    out << " |\n";
	}
    }
    out << "]\n";
}

void Heap::pushRoots(HeapRef atStart, HeapRef atEnd)
{
    size_t stackSize = thisStack.getSize();

    // std::cout << "AT START " << atStart.getIndex() << " " << atEnd.getIndex() << "\n";

    // Push stack elements
    for (size_t i = 0; i < stackSize; i++) {
	Cell cell = deref(thisStack.peek(i));
	if (isInRange(cell, atStart, atEnd)) {
	    thisStack.push(cell);
	}
    }

    // Push all relevant global roots
    RootMap::iterator itEnd = thisGlobalRoots.end();
    for (RootMap::iterator it = thisGlobalRoots.begin(); it != itEnd; ++it) {
	Cell *cellPtr = it->getValue();
	if (cellPtr != NULL) {
	    Cell cell = deref(*cellPtr);
	    if (isInRange(cell, atStart, atEnd)) {
		thisStack.push(cell);
	    }
	}
    }

    // Push all relevant forward pointers
    BitMap::iterator itEnd2 = thisForwardPointers.begin(true,atEnd.getIndex());
    for (BitMap::iterator it2 = thisForwardPointers.begin(true,atStart.getIndex());
	 it2 != itEnd2;
	 ++it2) {
	HeapRef href((NativeType)*it2);
	Cell cell0 = getCell0(href);
        assert(cell0.getTag() == Cell::REF);
	// Don't add this forward pointer as a root reference
	// if the source of the forward pointer is inside the
	// section we're GCing.
	if (!isInRange(cell0, atStart, atEnd)) {
	    Cell cell = deref(cell0);
	    // Is this pointer pointing into the section we're GCing?
	    if (isInRange(cell, atStart, atEnd)) {
		thisStack.push(cell);
	    }
	}
    }
}

Cell Heap::followFwd(Cell cell)
{
    // If the cell is FWD, then it was a REF cell that
    // got deferenced.
    if (cell.getTag() == Cell::FWD) {
	return Cell(Cell::REF, toHeapRef(cell));
    }
    if (hasHeapRef(cell)) {
	HeapRef href = toHeapRef(cell);
	Cell hrefCell = getCell0(href);
	if (hrefCell.getTag() == Cell::FWD) {
	    return Cell(cell.getTag(), toHeapRef(hrefCell));
	} else {
	    return cell;
	}
    } else {
	return cell;
    }
}

void Heap::updateRoots(HeapRef atStart, HeapRef atEnd)
{
    size_t stackSize = thisStack.getSize();

    // Update stack elements
    for (size_t i = 0; i < stackSize; i++) {
	Cell cell0 = thisStack.peek(i);
	Cell cell = followFwd(deref(cell0));
	if (cell != cell0) {
	    thisStack.peek(i) = cell;
	}
    }

    // Update all global roots
    RootMap::iterator itEnd = thisGlobalRoots.end();
    for (RootMap::iterator it = thisGlobalRoots.begin(); it != itEnd; ++it) {
	Cell *cellPtr = it->getValue();
	Cell cell0 = *cellPtr;
	Cell cell = followFwd(deref(cell0));
	if (cell != cell0) {
	    *cellPtr = cell;
	}
    }

    // Update forward pointers
    BitMap::iterator itEnd2 = thisForwardPointers.begin(true,atEnd.getIndex());
    for (BitMap::iterator it2 = thisForwardPointers.begin(true,atStart.getIndex());
	 it2 != itEnd2;
	 ++it2) {
	HeapRef href((NativeType)*it2);
	Cell cell0 = getCell0(href);
	assert(cell0.getTag() == Cell::REF);
	// Do not deref! We can't to keep the REF!
	Cell cell = followFwd(cell0);
	if (cell != cell0) {
	    setCell(href, cell);
	}
    }
}

bool Heap::isInRange(Cell cell, HeapRef atStart, HeapRef atEnd)
{
    if (cell.isNull()) {
	return false;
    }
    if (!hasHeapRef(cell)) {
	return false;
    }
    HeapRef href = toHeapRef(cell);
    switch (cell.getTag()) {
    case Cell::STR:
	{
	    Cell fun = getCell0(href);
	    size_t arity = getArity(fun);
	    if (atStart <= href + arity && href < atEnd) {
		return true;
	    }
	    return false;
	}
    case Cell::REF:
    case Cell::INT32:
	return atStart <= href && href < atEnd;
    case Cell::CON:
	return false;
    case Cell::FWD:
    case Cell::EXT:
	return false;
    default: // In case tag is invalid
        assert("Heap::isInRange(): Cell invalid TAG"==NULL);
        return false;
    }
}

void Heap::findLive(HeapRef fromHeapRef)
{
    HeapRef atStart = fromHeapRef;
    HeapRef atEnd = topHeapRef();

    size_t stackSize = thisStack.getSize();

    // Push all roots on stack
    pushRoots(atStart, atEnd);
    thisLive.clearBits(atStart.getIndex(), atEnd.getIndex());

    // Mark all back pointers to forward pointers as live
    BitMap::iterator itEnd2 = thisForwardPointers.begin(true,atEnd.getIndex());
    for (BitMap::iterator it2 = thisForwardPointers.begin(true,atStart.getIndex());
	 it2 != itEnd2;
	 ++it2) {
	size_t index = *it2;
	thisLive.setBit(index);
    }

    while (stackSize != thisStack.getSize()) {
	Cell cell = thisStack.pop();

	switch (cell.getTag()) {
	case Cell::CON:
	    // Nothing to follow.
	    break;
	case Cell::STR:
	    {
		if (isInRange(cell, atStart, atEnd)) {
		    HeapRef functorRef = toHeapRef(cell);
		    Cell fun = getCell0(functorRef);
		    size_t arity = getArity(fun);
		    bool isVisited = thisLive.hasBit(functorRef.getIndex());
		    thisLive.setBits(functorRef.getIndex(),
				     functorRef.getIndex()+arity+1);
		    if (!isVisited) {
			for (size_t i = 0; i < arity; i++) {
			    bool doPush = true;
			    HeapRef argRef = getArgRef(cell, i);
			    Cell arg = getCell0(argRef);
			    if (arg.getTag() == Cell::REF) {
				// Check if this is a REF cell that
				// points to itself:
				if (toHeapRef(arg) == argRef) {
				    // Then convert it into REFARG so the
				    // GC can distinguish those as functor
				    // arguments (which shouldn't be moved)
				    setCell(argRef, Cell(Cell::REFARG,argRef));
				    doPush = false;
				}
			    }
			    if (doPush) {
				thisStack.push(getArg(cell, i));
			    }
			}
		    }
		}
		break;
	    }
	case Cell::MAP:
	    {
	        assert("Cell::MAP To be implemented"==NULL);
		break;
	    }
	case Cell::REF:
	    {
		if (isInRange(cell, atStart, atEnd)) {
		    HeapRef ref = toHeapRef(cell);
		    bool isVisited = thisLive.hasBit(ref.getIndex());
		    thisLive.setBit(ref.getIndex());
		    if (!isVisited) {
			thisStack.push(getCell0(ref));
		    }
		}
		// Already derefenced. Nothing to follow
		break;
	    }
	case Cell::INT32:
	    {
		if (isInRange(cell, atStart, atEnd)) {
		    HeapRef int32Ref = toHeapRef(cell);
		    thisLive.setBit(int32Ref.getIndex());
		    // Do not push!
		}
		break;
	    }
        case Cell::FWD:
	    assert("Cell::FWD should not exist in this context."==NULL);
	    break;
	case Cell::EXT:
	    assert("Cell::EXT currently unused"==NULL);
	    break;
	}
    }
}

void Heap::resetFreePointers(HeapRef atStart)
{
    thisFreePtr = atStart;
}

HeapRef Heap::findFreeSlot(size_t numCells, HeapRef bound)
{
    HeapRef atStart = thisFreePtr;
    HeapRef atEnd = topHeapRef() - numCells;

    if (bound.isNull()) {
	bound = atEnd;
    }

    size_t found = thisLive.findBits(atStart.getIndex(), atEnd.getIndex(),
				     numCells, false);

    if (found == atEnd.getIndex() || found >= bound.getIndex()) {
	return HeapRef();
    }
    thisFreePtr = found + numCells;
    thisLive.setBits(found, thisFreePtr.getIndex());

    return HeapRef(found);
}

void Heap::compactMove(HeapRef from, size_t numCells, HeapRef to, int verbosity)
{
    if (from == to) {
	if (verbosity > 1) {
	    std::cout << "UNMOVED-: " << from.getIndex() << " " << to.getIndex() << " numCells=" << numCells << "\n";
	}
	return;
    }
    if (verbosity > 1) {
	std::cout << "MOVE-OK-: " << from.getIndex() << " " << to.getIndex() << " numCells=" << numCells << "\n";
    }

    for (size_t i = 0; i < numCells; i++) {
	if (isRefArg(from+i)) {
	    // This is an unbound variable inside a functor.
	    // We need to recreate it at the new location
	    setCell(to+i, Cell(Cell::REFARG, to+i));
	} else {
	    Cell cell = getCell0(from+i);
	    setCell(to+i, cell);
	}
    }
    for (size_t i = 0; i < numCells; i++) {
	setCell(from+i, Cell(Cell::FWD, HeapRef(to+i)));
    }
}

void Heap::initiateGC(HeapRef fromHeapRef)
{
    thisCompactedEnd = fromHeapRef;
    resetFreePointers(fromHeapRef);
}

void Heap::finalizeGC(HeapRef fromHeapRef)
{
    HeapRef top = topHeapRef();
    updateRoots(fromHeapRef, top);
    thisLive.clearBits(thisCompactedEnd.getIndex(), top.getIndex());

    thisForwardPointers.clearBits(thisCompactedEnd.getIndex(),
				  top.getIndex());
    if (isStrict()) {
	for (HeapRef href = thisCompactedEnd; href < top; ++href) {
	    setCell(href, Cell(0));
	    // char msg[32];
	    // sprintf(msg, "C#%d", href.getIndex());
	    // ConstRef cref = getConst(msg, 0);
	    // setCell(href, Cell(cref));
	}
    }
    trim(thisCompactedEnd.getIndex());
}

bool Heap::isRefArg(HeapRef href) const
{
    Cell cell = getCell0(href);
    if (cell.getTag() == Cell::REFARG) {
	if (toHeapRef(cell) == href) {
	    return true;
	}
    }
    return false;
}

// Move live data to compact it
void Heap::compactLive(Heap::Mode mode, HeapRef fromHeapRef, int verbosity)
{
    HeapRef top = topHeapRef();
    HeapRef atStart = fromHeapRef;
    HeapRef atEnd = top;

    // We need to push the roots again. Unfortunately, we can't
    // use the bitmap for the live data and linearly scan it as
    // some cells are untagged. We need to process the pointer
    // graph with interpretation.
    size_t stackSize = thisStack.getSize();
    pushRoots(fromHeapRef, top);

    thisVisited.clearBits(fromHeapRef.getIndex(), top.getIndex());

    while (stackSize != thisStack.getSize()) {
	Cell cell0 = thisStack.pop();
        Cell cell = followFwd(deref(cell0));
	// std::cout << "COMPACTING: ";
	// printCell(std::cout, cell);
	// std::cout << "\n";

	bool visited = false;
	if (hasHeapRef(cell)) {
	    HeapRef href = toHeapRef(cell);
	    visited = thisVisited.hasBit(href.getIndex());
	    if (!visited) {
		thisVisited.setBit(href.getIndex());
	    }
	}

	switch (cell.getTag()) {
	case Cell::CON:
	    break;
	case Cell::STR:
	    {
		HeapRef dst = toHeapRef(cell);
		Cell dstCell = deref(getCell0(dst));
		size_t arity = getArity(dstCell);

		// If not visited before, then push self followed
		// by all arguments to get post order traversal.
		if (!visited) {
		    // std::cout << "PUSH: ";
		    // printCell(std::cout, cell);
		    // std::cout << "\n";
		    thisStack.push(cell);
		    for (size_t i = 0; i < arity; i++) {
			HeapRef arg = getArgRef(dst, arity-i-1);
			if (atStart <= arg && arg < atEnd) {
			    Cell toPush = followFwd(deref(getCell0(arg)));
			    if (hasHeapRef(toPush)) {
				HeapRef toPushHref = toHeapRef(toPush);
				if (!thisVisited.hasBit(
					toPushHref.getIndex())) {
				    // std::cout << "PUSH: ";
				    // printCell(std::cout, toPush);
				    // std::cout << "\n";
				    thisStack.push(toPush);
				}
			    }
			}
		    }
		} else {
		    switch (mode) {
		    case COMPACT_MOVE: {
			// We're back at the parent after having visited
			// all the children. Let's move the functor block
			// itself.
			size_t numCells = 1 + arity;
			HeapRef newLoc = findFreeSlot(numCells, dst);
			if (!newLoc.isNull()) {
			    compactMove(dst, numCells, newLoc, verbosity);
			} else {
			    if (verbosity > 1) {
				std::cout << "MOVEFAIL: " << dst.getIndex() << " numCells=" << numCells << "\n";
			    }
			    newLoc = dst;
			}
			if (newLoc + numCells > thisCompactedEnd) {
			    thisCompactedEnd = newLoc + numCells;
			}
		    }
		    break;
		    case COMPACT_UPDATE_FWD: {
			for (size_t i = 0; i < arity; i++) {
			    HeapRef arg = getArgRef(dst, arity-i-1);
			    if (atStart <= arg && arg < atEnd) {
				Cell argCell0 = getCell0(arg);
				Cell argCell = followFwd(deref(argCell0));
				if (argCell0 != argCell) {
				    setCell(arg, argCell);
				    checkForwardPointer(arg, argCell);
				}
			    }
			}
			break;
		    }
		    break;
		    }
		}
	    }
	    break;
	case Cell::MAP:
	    {
		// Here we should treat the MAP as STR
		// Just process one node and push its arguments.
		assert("Cell::MAP to be implemented"==NULL);
		break;
	    }
	case Cell::REF:
	    {
		switch (mode) {
		case COMPACT_MOVE: {
		    size_t numCells = 1;
		    HeapRef dst = toHeapRef(cell);
		    // Never move unbound variables as a functor argument
		    if (isRefArg(dst)) {
			break;
		    }
		    HeapRef newLoc = findFreeSlot(numCells, dst);
		    if (!newLoc.isNull()) {
			compactMove(dst, numCells, newLoc, verbosity);
		    } else {
			if (verbosity > 1) {
			    std::cout << "MOVEFAIL: " << dst.getIndex() << " numCells=" << numCells << "\n";
			}
			newLoc = dst;
		    }
		    if (newLoc + numCells > thisCompactedEnd) {
			thisCompactedEnd = newLoc + numCells;
		    }
		    break;
		}
		case COMPACT_UPDATE_FWD: {
		    HeapRef dst = toHeapRef(cell);
		    Cell dstCell0 = getCell0(dst);
		    Cell dstCell = followFwd(deref(dstCell0));
		    if (dstCell0 != dstCell) {
			setCell(dst, dstCell);
			checkForwardPointer(dst, dstCell);
		    }
		    break;
		}
		}
		break;
	    }
	case Cell::INT32:
	    {
		// Move up what INT32 cell is pointing at.
		HeapRef dst = toHeapRef(cell);
		if (thisVisited.hasBit(dst.getIndex())) {
		    break;
		}
		thisVisited.setBit(dst.getIndex());
		size_t numCells = 1;
		HeapRef newLoc = findFreeSlot(numCells, dst);
		if (!newLoc.isNull()) {
		    compactMove(dst, numCells, newLoc, verbosity);
		} else {
		    newLoc = dst;
		}
		if (newLoc + numCells > thisCompactedEnd) {
		    thisCompactedEnd = newLoc + numCells;
		}
		break;
	    }
        case Cell::FWD:
	    {
		assert("Cell::FWD should not occur in this context."==NULL);
		break;
	    }
	case Cell::EXT:
	    assert("Cell::EXT currently unused"==NULL);
	    break;
	}
    }
}

void Heap::compactForwardPointers(HeapRef fromHeapRef, int verbosity)
{
    HeapRef top = topHeapRef();
    HeapRef atStart = fromHeapRef;
    HeapRef atEnd = top;

    // Iterate through all relevant forward pointers
    BitMap::iterator itEnd2 = thisForwardPointers.begin(true,atEnd.getIndex());
    for (BitMap::iterator it2 = thisForwardPointers.begin(true,atStart.getIndex());
	 it2 != itEnd2;
	 ++it2) {
	HeapRef href((NativeType)*it2);
	Cell cell0 = getCell0(href);
        assert(cell0.getTag() == Cell::REF);

	// Source location of forward pointer
	HeapRef from = toHeapRef(followFwd(cell0));
	Cell cell = followFwd(getCell0(from));

	// Don't add this forward pointer as a root reference
	// if the source of the forward pointer is inside the
	// section we're GCing.

	bool doMove = true;
	// contained = the source of the forward pointer is inside
	// the region we're GCing.
	bool contained = atStart <= from && from < atEnd;
	if (contained) {
	    // The entire pointer (source and destination) is inside
	    // the region we're GCing. Let's see if we still need
	    // this forward pointer.
	    if (!isForwardPointer(from, cell)) {
		// It's no longer a forward pointer. Clear the bit.
		thisForwardPointers.setBit(*it2, false);
		doMove = false;
	    }
	}
	if (doMove) {
	    // Let's move this one.
	    size_t numCells = 1;
	    HeapRef newLoc = findFreeSlot(numCells, href);
	    if (!newLoc.isNull()) {
		compactMove(href, numCells, newLoc, verbosity);
		thisForwardPointers.setBit(*it2, false);
		thisForwardPointers.setBit(newLoc.getIndex(), true);
	    } else {
		newLoc = href;
	    }
	    if (newLoc + numCells > thisCompactedEnd) {
		thisCompactedEnd = newLoc + numCells;
	    }
	}
    }
}

CellRef Heap::newMap(size_t depth)
{
    Cell *cellPtr = allocate(2);
    MapNode mapNode(cellPtr);
    mapNode.setMeta(true, depth, 0);
    mapNode.setMask(0);
    Cell strCell(Cell::MAP, toRelative(cellPtr));
    return CellRef(*this, strCell);
}

CellRef Heap::getArg32(CellRef mapCell, size_t index)
{
    Cell mapCell0 = deref(*mapCell);
    assert(mapCell0.getTag() == Cell::MAP);
    HeapRef href = toHeapRef(mapCell0);
    Cell *cellPtr = toAbsolute(href);
    MapNode mapNode(cellPtr);
    uint32_t mask = mapNode.getMask();
    if ((mask & (1 << index)) == 0) {
	return CellRef(); // null
    }
    for (size_t i = 0, cnt = 0; i < 32; i++, mask >>= 1) {
	if ((mask & 1) != 0) {
	    if (index == i) {
		return CellRef(*this, mapNode.getArg(cnt));
	    }
	    cnt++;
	}
    }
    return CellRef();
}

CellRef Heap::setArg32(CellRef mapCell, size_t index, CellRef arg)
{
    // Check if we need to augment the cell chunk
    Cell mapCell0 = deref(*mapCell);
    assert(mapCell0.getTag() == Cell::MAP);
    HeapRef href = toHeapRef(mapCell0);
    Cell *cellPtr = toAbsolute(href);
    MapNode mapNode(cellPtr);
    uint32_t oldMask = mapNode.getMask();
    bool toRemove = arg->isNull();

    uint32_t newMask = 0;
    if (toRemove) {
	// Remove element
	newMask = oldMask & ~(1 << index);
    } else {
	newMask = oldMask | (1 << index);
    }
    size_t numArgs = bitCount(newMask);
    Cell *newCellPtr = allocate(2+numArgs);
    MapNode newMapNode(newCellPtr);
    newMapNode.setMeta(mapNode.getMeta());
    newMapNode.setMask(newMask);
    for (size_t i = 0, cnt = 0, newCnt = 0;
	 i < 32; i++,
	     newMask >>= 1, oldMask >>= 1) {
	if ((newMask & 1) != 0) {
	    if (index == i) {
		newMapNode.setArg(newCnt, *arg);
	    } else {
		newMapNode.setArg(newCnt, mapNode.getArg(cnt));
	    }
	    newCnt++;
	}
	if ((oldMask & 1) != 0) cnt++;
    }
    Cell newCell(Cell::MAP, toRelative(newCellPtr));
    return CellRef(*this, newCell);
}

int Heap::compareConst(Cell a, Cell b) const
{
    ConstRef aCon = a.toConstRef();
    ConstRef bCon = b.toConstRef();
    ConstString aStr = getConstName(aCon);
    ConstString bStr = getConstName(bCon);
    int cmp = aStr.compare(bStr);
    if (cmp != 0) {
	return cmp;
    }
    size_t aArity = getArity(aCon);
    size_t bArity = getArity(bCon);
    if (aArity != bArity) {
	if (aArity < bArity) {
	    return -1;
	} else {
	    return 1;
	}
    }
    return 0;
}

int Heap::compareInt32(Cell a, Cell b) const
{
    int32_t aInt = toInt32(a);
    int32_t bInt = toInt32(b);
    if (aInt < bInt) {
	return -1;
    } else if (aInt > bInt) {
	return 1;
    } else {
	return 0;
    }
}

int Heap::compareRef(Cell a, Cell b) const
{
    HeapRef aRef = toHeapRef(a);
    HeapRef bRef = toHeapRef(b);
    if (aRef < bRef) {
	return -1;
    } else if (aRef > bRef) {
	return 1;
    } else {
	return 0;
    }
}

int Heap::compareDeep(Cell a, Cell b) const
{
    if (a.getTag() != b.getTag()) {
	if (a.getTag() < b.getTag()) {
	    return -1;
	} else {
	    return 1;
	}
    }
    // We don't use recursion, so we can explicitly control stack size.
    // (C++ stack will not be as efficient.)
    size_t current = thisStack.getSize();

    thisStack.push(b);
    thisStack.push(a);

    while (current != thisStack.getSize()) {
	a = deref(thisStack.pop());
	b = deref(thisStack.pop());

	if (a.getTag() != b.getTag()) {
	    thisStack.trim(current);
	    if (a.getTag() < b.getTag()) {
		return -1;
	    } else {
		return 1;
	    }
	}
	
	int cmp = compareShallow(a,b);
	if (cmp != 0) {
	    thisStack.trim(current);
	    return cmp;
	}

	switch (a.getTag()) {
	case Cell::CON: // Must be equal (otherwise compareShallow != 0)
	    break;
	case Cell::STR:
	    {
		ConstString aName = getConstName(getFunctor(a).toConstRef());
		ConstString bName = getConstName(getFunctor(b).toConstRef());
		int cmp = aName.compare(bName);
		if (cmp != 0) {
		    thisStack.trim(current);
		    return cmp;
		}

		size_t a_arity = getArity(a);
		size_t b_arity = getArity(b);
		if (a_arity != b_arity) {
		    thisStack.trim(current);
		    return (a_arity < b_arity) ? -1 : 1;
		}
		for (size_t i = 0; i < a_arity; i++) {
		    Cell aArg = getArg(a, a_arity - i - 1);
		    Cell bArg = getArg(b, a_arity - i - 1);
		    thisStack.push(bArg);
		    thisStack.push(aArg);
		}
	    }
	    break;
	case Cell::MAP:
	    {
		assert("equalDeep(): Cell::MAP: TO BE IMPLEMENTED!"==NULL);
		break;
	    }
	default:
	    thisStack.trim(current);
	    assert("equalDeep(): Cell::???: TO BE IMPLEMENTED!"==NULL);
	    break;
	}
    }
    return 0;
}

uint32_t Heap::hashOf(CellRef term)
{
    Hash hash;

    // Traverse term and compute hash
    Cell cell = *term;

    size_t current = thisStack.getSize();

    thisStack.push(cell);

    while (current != thisStack.getSize()) {
	cell = deref(thisStack.pop());
	switch (cell.getTag()) {
	case Cell::CON:
	    {
		ConstRef cref = cell.toConstRef();
		ConstString cs = getConstName(cref);
		size_t arity = thisConstTable.getConstArity(cref);
		hash.update(cs.getString(), cs.getLength());
		hash.update(static_cast<uint32_t>(arity));
		break;
	    }
	case Cell::INT32:
	    {
		hash.update(static_cast<uint32_t>(toInt32(cell)));
		break;
	    }
	case Cell::STR:
	    {
		break;
	    }
	case Cell::MAP:
	    {
		break;
	    }
	case Cell::REF:
	    {
		assert("Heap::hashOf(): Unbounded variables are not allowed in hash computations"==NULL);
		break;
	    }
	case Cell::FWD:
	    {
		assert("Heap::hashOf(): Cell::FWD not allowed in hash computations"==NULL);
		break;
	    }
	case Cell::EXT:
	    {
		assert("Heap::hashOf(): Cell::EXT unsupported");
		break;
	    }
	}
    }
    return hash.finalize();
}

CellRef Heap::assocListFind(CellRef list, CellRef key)
{
    Cell lst = *list;
    Cell key0 = *key;
    while (isDot(lst)) {
	Cell pair = getArg(lst, 0);
	if (equal(key0, getArg(pair, 0))) {
	    return CellRef(*this, getArg(pair, 1));
	}
	lst = getArg(lst, 1);
    }
    return CellRef();
}

CellRef Heap::assocListReplace(CellRef list, CellRef key, CellRef value)
{
    size_t markStack = getStackSize();
    Cell lst = *list;
    while (isDot(lst)) {
	Cell pair = getArg(lst, 0);
	lst = getArg(lst, 1);
	Cell key1 = getArg(pair, 0);
	if (equal(key1, *key)) {
	    CellRef result = CellRef(*this, lst);
	    // If value is null, we should just drop the found pair.
	    if (!value->isNull()) {
		result = newList(newPair(key,value), result);
	    }
	    while (markStack != getStackSize()) {
		CellRef next(*this, thisStack.pop());
		result = newList(next, result);
	    }
	    return result;
	}
	thisStack.push(pair);
    }
    // We couldn't find it, so discard pushed elements on stack and
    // add it first to list
    thisStack.trim(markStack);
    if (value->isNull()) {
	return list;
    } else {
	return assocListAdd(key, value, list);
    }
}

CellRef Heap::mapCreateNewTree(size_t startDepth, size_t endDepth,
			       uint32_t hash, CellRef key, CellRef value)
{
    // We need to create this bottom up so we avoid forward pointers.

    // First create a pair (key, value) which represents the leaf
    CellRef tree = newPair(key, value);

    // Then build up the tree step by step.
    for (size_t i = startDepth; i < endDepth; i++) {
	size_t index = (hash >> (5 * (i-startDepth))) & 0x1f;
	// std::cout << "  NEWTREE: index=" << index << "\n";
	Cell *cellPtr = allocate(3);
	MapNode mapNode(cellPtr);
	mapNode.setMeta(false, i-startDepth+1, 1);
	mapNode.setMask(1 << index);
	mapNode.setArg(0, *tree);
	tree = CellRef(*this, Cell(Cell::MAP, toRelative(cellPtr)));
    }

    return tree;
}

size_t Heap::mapGetSpine(CellRef map, CellRef key, uint32_t hash)
{
    Cell mapCell = deref(*map);
    MapNode mapNode(toAbsolute(toHeapRef(mapCell)));
    size_t depth = mapNode.getDepth();
    
    for (size_t i = 0; i < depth; i++) {
	thisStack.push(mapCell);
	size_t index = (hash >> (depth-i-1)*5) & 0x1f; // value between 0..31
	// std::cout << "   GET SPINE: INDEX=" << index << "\n";
	mapNode = MapNode(toAbsolute(toHeapRef(mapCell)));
	uint32_t mask = mapNode.getMask();
	if ((mask & (1 << index)) == 0) {
	    // std::cout << "    EMPTY!\n";
	    return i+1;
	} else {
	    // std::cout << "    NON-EMPTY!\n";
	    mapCell = *getArg32(CellRef(*this,mapCell), index);
	}
    }
    thisStack.push(mapCell);
    return depth+1;
}

CellRef Heap::wrapSpine(CellRef tree, uint32_t hash)
{
    bool cont = true;
    while (cont) {
	Cell cell = thisStack.pop();
	CellRef cellRef(*this, cell);
	Cell *cellPtr = toAbsolute(toHeapRef(cell));
	MapNode mapNode(cellPtr);
	size_t depth = mapNode.getDepth();
	cont = !mapNode.isRoot();
	size_t index = (hash >> 5*(depth-1)) & 0x1f;
	// std::cout << "  WRAP SPINE: INDEX=" << index << "\n";

	// Last item to be removed?
	if (tree->isNull() && mapNode.getMask() == 1 << index) {
	    // Do noting
	} else {
	    tree = setArg32(cellRef, index, tree);
	}
    }
    return tree;
}

// Walk down the spine and non-destructively create a new table based
// on the old. If 'value' is null, then this operation will remove the
// key.
CellRef Heap::putMap(CellRef map, CellRef key, CellRef value)
{
    size_t stackMark = getStackSize();

    uint32_t hash = hashOf(key);
    // std::cout << "HASH: " << hash << "\n";

    Cell mapCell = deref(*map);
    MapNode mapNode(toAbsolute(toHeapRef(mapCell)));
    size_t depth = mapNode.getDepth();
    size_t spineDepth = mapGetSpine(map, key, hash);

    if (spineDepth != depth + 1) {
	// We couldn't find it, so create a new subtree (if value != null)
	if (value->isNull()) {
	    thisStack.trim(stackMark);
	    return map;
	}
	CellRef newTree = mapCreateNewTree(spineDepth, depth, hash, key, value);
	return wrapSpine(newTree, hash);
    }

    // We've found the entire spine (= a collision)
    CellRef leaf(*this, thisStack.pop());
    // std::cout << "LEAF IS: " << toString(leaf) << "\n";
   
    if (isPair(leaf)) {
	// Shall we create a new list?
	CellRef key1 = getArg(leaf, 0);
	if (equal(key,key1)) {
	    CellRef newLeaf;
	    if (!value->isNull()) {
		newLeaf = newPair(key,value);
	    }
	    return wrapSpine(newLeaf, hash);
	} else {
	    // Create a list with two elements
	    CellRef newLeaf = newList(newPair(key,value),
				      newList(leaf, newList()));
	    return wrapSpine(newLeaf, hash);
	}
    }

    CellRef newLeaf = assocListReplace(leaf, key, value);
    if (*leaf == *newLeaf) {
	// No change. Then we can discard everything and return
	thisStack.trim(stackMark);
	return map;
    }
    return wrapSpine(newLeaf, hash);
}

CellRef Heap::getMap(CellRef map, CellRef key)
{
    size_t stackMark = getStackSize();

    uint32_t hash = hashOf(key);
    (void)mapGetSpine(map, key, hash);
    Cell cell = thisStack.pop();
    thisStack.trim(stackMark);
    Cell key0 = *key;
    if (isPair(cell)) {
	if (equal(key0, getArg(cell, 0))) {
	    return CellRef(*this, getArg(cell, 1));
	} else {
	    return CellRef();
	}
    } else {
	if (!isList(cell)) {
	    return CellRef();
	} else {
	    return assocListFind(CellRef(*this, cell), key);
	}
    }
}

CellRef Heap::mapAsList(CellRef map)
{
    CellRef lst = newList();

    size_t stackMark = getStackSize();
    thisStack.push(*map);

    while (stackMark != getStackSize()) {
	Cell cell = deref(thisStack.pop());
	if (cell.getTag() == Cell::MAP) {
	    MapNode mapNode(toAbsolute(toHeapRef(cell)));
	    size_t cnt = 0;
	    for (uint32_t mask = mapNode.getMask(); mask != 0; mask >>= 1) {
		if ((mask & 1) != 0) {
		    Cell arg = mapNode.getArg(cnt);
		    thisStack.push(arg);
		    cnt++;
		}
	    }
	} else {
	    // This map element might be a list (if there are hash
	    // collisions.) So we need to append it.
	    if (isDot(cell) || isEmpty(cell)) {
		lst = appendList(CellRef(*this, cell), lst);
	    } else {
		lst = newList(CellRef(*this, cell), lst);
	    }
	}
    }

    return lst;
}

size_t Heap::lengthList(CellRef lst)
{
    Cell l = *lst;
    size_t cnt = 0;
    while (isDot(l)) {
	l = getArg(l, 1);
	cnt++;
    }
    return cnt;
}

CellRef Heap::appendList(CellRef list1, CellRef list2)
{
    size_t stackMark = thisStack.getSize();
    Cell lst = *list1;
    while (isDot(lst)) {
	thisStack.push(getArg(lst,0));
	lst = getArg(lst,1);
    }
    while (stackMark != thisStack.getSize()) {
	list2 = newList(CellRef(*this, thisStack.pop()), list2);
    }
    return list2;
}

int Heap::qsortCmp(void *heap0, const void *a, const void *b)
{
    Heap *heap = reinterpret_cast<Heap *>(heap0);
    const Cell *aCell = reinterpret_cast<const Cell *>(a);
    const Cell *bCell = reinterpret_cast<const Cell *>(b);

    int cmp = heap->compare(*aCell, *bCell);
    return cmp;
}

CellRef Heap::qsortList(CellRef lst)
{
    size_t n = lengthList(lst);
    ConstRef arrayConst = getConst("array", n);
    CellRef arrayRef = newStr(arrayConst);
    Cell lst0 = *lst;
    size_t i = 0;
    while (isDot(lst0)) {
	Cell arg = getArg(lst0, 0);
	setArg(*arrayRef, i, arg);
	lst0 = getArg(lst0, 1);
	i++;
    }
    qsort_r(toAbsolute(toHeapRef(*arrayRef)+1), n, sizeof(Cell), this, qsortCmp);

    CellRef newLst = newList();
    // Build sorted list
    for (size_t i = 0; i < n; i++) {
	CellRef cell = getArg(arrayRef, n-i-1);
	newLst = newList(cell, newLst);
    }

    return newLst;
}

}
