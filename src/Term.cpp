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
// [3 bits]: for depth
// [28 bits]: count
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
       : thisHeap(capacity),
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
    case Cell::CON: printConst(out, cell); break;
    case Cell::STR: out << cell.getValue(); break;
    case Cell::MAP: out << cell.getValue(); break;
    case Cell::INT32: out << toInt32(cell); break;
    case Cell::FWD: out << "$fwd(" << cell.getValue() << ")"; break;
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
	printCell(out, cell);
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
    for (size_t i = 0, cnt = 0; i < 32; i++) {
	if ((mask & (1 << i)) != 0) {
	    if (cnt > 0) {
		thisStack.push(thisPrintComma);
	    }
	    thisStack.push(mapNode.getArg(cnt));
	    cnt++;
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

    ConstRef cref = getConst(constName, 0);
    return newCon(cref);
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
    setCell(href, Cell(Cell::REF, href));
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
	checkForwardPointer(toHeapRef(b), a);
	bind1(b, a);
    } else {
	checkForwardPointer(toHeapRef(a), b);
	bind1(a, b);
    }
}

void Heap::createForwardPointer(HeapRef from, HeapRef to)
{
    // Create a new backpointer to the source of this forward pointer
    Cell *backPtr = allocate(1);
    Cell cell(Cell::REF, from);
    *backPtr = cell;

    // Set a bit that we track a forward pointer at this location
    thisForwardPointers.setBit(toRelative(backPtr));
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

void Heap::popState()
{
    State state = thisStateStack.pop();
    while (thisTrail.getSize() != state.thisTrailSize) {
	Cell cell = thisTrail.pop();
	unbind(cell);
    }
    thisUseTrail = state.thisUseTrail;
    trim(state.thisHeapTop);
    thisStack.trim(state.thisStackSize);
}

void Heap::discardState()
{
    (void)thisStateStack.pop();
}

bool Heap::unify(CellRef a, CellRef b)
{
    pushState();

    // Remember all bidings so we can unbind if
    // unification fails.
    bool oldUseTrail = thisUseTrail;
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
		popState();
		return false;
	    }
	    switch (ca.getTag()) {
	    case Cell::CON:
		// Const must be inequal (otherwise ca == cb)
		popState();
		return false;
	    case Cell::STR:
		{
		    HeapRef sa = toHeapRef(ca);
		    HeapRef sb = toHeapRef(cb);
		    ConstRef sca = getCell0(sa).toConstRef();
		    ConstRef scb = getCell0(sb).toConstRef();
		    if (sca != scb) {
			popState();
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
			popState();
			return false;
		    }
		}
		break;
	    default:
		{
		    popState();
		    return false;
		}
	    }
	}
    }

    discardState();

    thisUseTrail = oldUseTrail;

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
    if (verbosity > 0) {
	std::cout << "Heap::gc(): windowSize=" << windowSize << " verbosity=" << verbosity << " top=" << topHeapRef().getIndex() << "\n";

    }
    initiateGC(windowSize);
    findLive(windowSize);
    if (verbosity > 0) printLive(std::cout, windowSize);
    compactLive(windowSize, verbosity);
    HeapRef currentTop = topHeapRef();
    finalizeGC(windowSize);
    HeapRef afterTop = topHeapRef();
    if (verbosity > 0) {
	printLive(std::cout, windowSize);
	size_t compacted = currentTop.getIndex() - afterTop.getIndex();
	std::cout << "Heap::gc(): top=" << currentTop.getIndex() << " after=" << afterTop.getIndex() << " compacted=" << compacted << "\n";
    }
}

void Heap::gc(double percentage, int verbosity)
{
    size_t heapSize = getHeapSize();
    size_t windowSize = (size_t)(percentage * heapSize);
    gc(windowSize, verbosity);
}

void Heap::printLive(std::ostream &out, size_t topSize) const
{
    HeapRef atStart = topHeapRef();
    HeapRef atEnd = atStart;
    if (topSize == 0 || topSize > getHeapSize()) {
	topSize = getHeapSize();
    }
    atStart = atEnd - topSize;

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

void Heap::pushRoots(HeapRef atStart, HeapRef atEnd, bool onGCStack)
{
    size_t stackSize = thisStack.getSize();

    // std::cout << "AT START " << atStart.getIndex() << " " << atEnd.getIndex() << "\n";

    // Push stack elements
    for (size_t i = 0; i < stackSize; i++) {
	Cell &cell = thisStack.peek(i);
	if (isInRange(cell, atStart, atEnd)) {
	    if (onGCStack) {
		thisStackGC.push(&cell);
	    } else {
		thisStack.push(cell);
	    }
	}
    }

    // Push all relevant global roots
    RootMap::iterator itEnd = thisGlobalRoots.end();
    for (RootMap::iterator it = thisGlobalRoots.begin(); it != itEnd; ++it) {
	Cell *cellPtr = it->getValue();
	if (cellPtr != NULL && isInRange(*cellPtr, atStart, atEnd)) {
	    if (onGCStack) {
		thisStackGC.push(cellPtr);
	    } else {
		thisStack.push(*cellPtr);
	    }
	}
    }

    // Push all relevant forward pointers
    BitMap::iterator itEnd2 = thisForwardPointers.begin(true,atEnd.getIndex());
    for (BitMap::iterator it2 = thisForwardPointers.begin(true,atStart.getIndex());
	 it2 != itEnd2;
	 ++it2) {
	HeapRef href((NativeType)*it2);
	Cell backPtr = getCell0(href);
	assert(backPtr.getTag() == Cell::REF);
	HeapRef forwardPointerSrc = toHeapRef(backPtr);
	Cell fwdSrc = getCell0(forwardPointerSrc);
	if (isInRange(fwdSrc, atStart, atEnd)) {
	    if (onGCStack) {
		thisStackGC.push(toAbsolute(href));
	    } else {
		thisStack.push(backPtr);
		thisStack.push(fwdSrc);
	    }
	}
    }
}

Cell Heap::followFwd(Cell cell)
{
    if (cell.getTag() == Cell::FWD) {
	return getCell0(toHeapRef(cell));
    } else {
	return cell;
    }
}

void Heap::updateRoots(HeapRef atStart, HeapRef atEnd)
{
    size_t stackSize = thisStack.getSize();

    // Update stack elements
    for (size_t i = 0; i < stackSize; i++) {
	Cell cell = thisStack.peek(i);
	Cell cell1 = followFwd(cell);
	if (cell != cell1) {
	    thisStack.peek(i) = cell1;
	}
    }

    // Update all global roots
    RootMap::iterator itEnd = thisGlobalRoots.end();
    for (RootMap::iterator it = thisGlobalRoots.begin(); it != itEnd; ++it) {
	Cell *cellPtr = it->getValue();
	Cell cell = *cellPtr;
	Cell cell1 = followFwd(cell);
	if (cell != cell1) {
	    *cellPtr = cell1;
	}
    }

    // Update forward pointers
    BitMap::iterator itEnd2 = thisForwardPointers.begin(true,atEnd.getIndex());
    for (BitMap::iterator it2 = thisForwardPointers.begin(true,atStart.getIndex());
	 it2 != itEnd2;
	 ++it2) {
	HeapRef href((NativeType)*it2);
	Cell backPtr = getCell0(href);
	assert(backPtr.getTag() == Cell::REF);
	HeapRef forwardPointerSrc = toHeapRef(backPtr);
	Cell cell = getCell0(forwardPointerSrc);
	Cell cell1 = followFwd(cell);
	if (cell != cell1) {
	    *toAbsolute(forwardPointerSrc) = cell1;
	}
    }

    // Todo: scan trail
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
	assert("Heap::isInRange(): Cell::FWD/EXT unexpected here"==NULL);
	return false;
    default: // In case tag is invalid
        assert("Heap::isInRange(): Cell invalid TAG"==NULL);
        return false;
    }
}

void Heap::findLive(size_t topSize)
{
    HeapRef atStart = topHeapRef();
    HeapRef atEnd = atStart;
    if (topSize == 0 || topSize > getHeapSize()) {
	topSize = getHeapSize();
    }
    atStart = atEnd - topSize;

    size_t stackSize = thisStack.getSize();

    // Push all roots on stack
    pushRoots(atStart, atEnd, false);
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
			    thisStack.push(getArg(cell, i));
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
    for (size_t i = 0; i < TRACK_SIZES; i++) {
	thisFreePtr[i] = atStart;
    }
}

HeapRef Heap::findFreeSlot(size_t numCells) const
{
    HeapRef atStart = (numCells > TRACK_SIZES) ?
	thisFreePtr[0] : thisFreePtr[numCells];

    HeapRef atEnd = topHeapRef() - numCells;

    size_t found = thisLive.findBits(atStart.getIndex(), atEnd.getIndex(),
				     numCells, false);

    if (numCells > TRACK_SIZES) {
	thisFreePtr[0] = HeapRef(found);
    } else {
	thisFreePtr[numCells] = HeapRef(found);
    }

    if (found == atEnd.getIndex()) {
	return HeapRef();
    }
    return HeapRef(found);
}

HeapRef Heap::findFreeSlotBound(size_t numCells, HeapRef oldLoc) const
{
    HeapRef href = findFreeSlot(numCells);
    if (href.isEmpty() || href > oldLoc) {
	return oldLoc;
    } else {
	return href;
    }
}

void Heap::compactMove0(HeapRef from, size_t numCells, HeapRef to, int verbosity)
{
    if (verbosity > 1) {
	std::cout << "MOVE-OK-: " << from.getIndex() << " " << to.getIndex() << " numCells=" << numCells << "\n";
    }

    Cell *fromCell = toAbsolute(from);
    Cell *toCell = toAbsolute(to);

    memcpy(toCell, fromCell, numCells*sizeof(Cell));

    // Also make a conservative update on forward references
    // TODO: We're going to change how forward pointers are
    // managed.
    if (thisForwardPointers.hasBit(from.getIndex())) {
	thisForwardPointers.setBit(to.getIndex());
    }

    // Mark this section as live
    thisLive.setBits(to.getIndex(), to.getIndex()+numCells);
}

void Heap::compactMove(HeapRef from, size_t numCells, HeapRef to, int verbosity)
{
    // Update compacted end
    if (to + numCells > thisCompactedEnd) {
	thisCompactedEnd = to + numCells;
    }

    if (from == to) {
	if (verbosity > 2) {
	    std::cout << "MOVEFAIL: " << from.getIndex() << " " << to.getIndex() << " numCells=" << numCells << "\n";
	}
	return;
    }
    compactMove0(from, numCells, to, verbosity);
    setCell(from, Cell(Cell::FWD, to));
}

void Heap::initiateGC(size_t topSize)
{
    HeapRef atStart = topHeapRef();
    HeapRef atEnd = atStart;
    if (topSize == 0 || topSize > getHeapSize()) {
	topSize = getHeapSize();
    }
    atStart = atEnd - topSize;

    thisCompactedEnd = atStart;

    resetFreePointers(atStart);
}

void Heap::finalizeGC(size_t topSize)
{
    HeapRef atStart = topHeapRef();
    HeapRef atEnd = atStart;
    if (topSize == 0 || topSize > getHeapSize()) {
	topSize = getHeapSize();
    }
    atStart = atEnd - topSize;

    updateRoots(atStart, atEnd);
    thisLive.clearBits(thisCompactedEnd.getIndex(), atEnd.getIndex());

    thisForwardPointers.clearBits(thisCompactedEnd.getIndex(),
				  atEnd.getIndex());
    if (isStrict()) {
	for (HeapRef href = thisCompactedEnd; href < atEnd; ++href) {
	    setCell(href, Cell(0));
	    /*
	    char msg[128];
	    sprintf(msg, "#%d", href.getIndex());
	    ConstRef xx = getConst(msg, 0);
	    setCell(href, Cell(Cell::CON, xx));
	    */
	}
    }
    trim(thisCompactedEnd.getIndex());
}

// Move live data to compact it
void Heap::compactLive(size_t topSize, int verbosity)
{
    HeapRef atStart = topHeapRef();
    HeapRef atEnd = atStart;
    if (topSize == 0 || topSize > getHeapSize()) {
	topSize = getHeapSize();
    }
    atStart = atEnd - topSize;

    // We need to push the roots again. Unfortunately, we can't
    // use the bitmap for the live data and linearly scan it as
    // some cells are untagged. We need to process the pointer
    // graph with interpretation.
    size_t stackSize = thisStackGC.getSize();
    pushRoots(atStart, atEnd, true);

    thisVisited.clearBits(atStart.getIndex(), atEnd.getIndex());

    while (stackSize != thisStackGC.getSize()) {
	Cell *cellPtr = thisStackGC.pop();
	Cell cell = *cellPtr;

	bool visited = false;
	if (isValid(cellPtr)) {
	    visited = thisVisited.hasBit(toRelative(cellPtr));
	    thisVisited.setBit(toRelative(cellPtr));
	}

	if (hasHeapRef(cell)) {
	    Cell to = getCell0(toHeapRef(cell));
	    if (to.getTag() == Cell::FWD) {
		// Update FWD reference
		cell = Cell(cell.getTag(), toHeapRef(to));
		*cellPtr = cell;
		continue;
	    }
	}

	switch (cell.getTag()) {
	case Cell::CON:
	    break;
	case Cell::STR:
	    {
		HeapRef dst = toHeapRef(cell);
		Cell dstCell = getCell0(dst);
		size_t arity = getArity(dstCell);

		bool strVisit = thisVisited.hasBit(dst.getIndex());

		if (!strVisit) {
		    thisVisited.setBit(dst.getIndex());

		    // Push outer STR cell again to move itself
		    // last (after arguments.) This way we don't
		    // introduce new forward pointers.
		    thisStackGC.push(cellPtr);

		    for (size_t i = 0; i < arity; i++) {
			HeapRef arg = getArgRef(dst, i);
			if (atStart <= arg && arg < atEnd) {
			    thisStackGC.push(toAbsolute(arg));
			}
		    }
		    // thisVisited.setBit(toRelative(cellPtr));
		} else {
		    size_t numCells = 1 + arity;
		    HeapRef newLoc = findFreeSlotBound(numCells, dst);
		    compactMove(dst, numCells, newLoc, verbosity);
		    *cellPtr = Cell(Cell::STR, newLoc);
		}
		break;
	    }
	case Cell::MAP:
	    {
		// Here we should treat the MAP as STR
		// Just process one node and push its arguments.
		assert("Cell::MAP to be implemented"==NULL);
		break;
	    }
	case Cell::REF:
	    {
		// It's ok to move forward pointer objects (they
		// point backwards to the forward pointer source location),
		// but it's not ok to move the forward pointer source
		// (destination is ok though) as it may reside as a functor
		// arg, and then the outer functor "owns" the memory.
		bool isForwardPtr = isValid(cellPtr) &&
	 	           thisForwardPointers.hasBit(toRelative(cellPtr));
		HeapRef dst = toHeapRef(cell);
		if (!isForwardPtr) {
		    bool refVisit = thisVisited.hasBit(dst.getIndex());
		    bool doMove = true;
		    if (!refVisit) {
			thisVisited.setBit(dst.getIndex());
			// Push itself first followed by contents of REF cell
			// This will process the object the REF cell is
			// pointing to first, so we retain back pointers.
			if (atStart <= dst && dst < atEnd) {
			    thisStackGC.push(cellPtr);
			    thisStackGC.push(toAbsolute(dst));
			    doMove = false;
			}
		    }
		    if (doMove) {
			size_t numCells = 1;
			HeapRef newLoc = findFreeSlotBound(numCells, dst);
			compactMove(dst, numCells, newLoc, verbosity);
			if (dst != newLoc) {
			    *cellPtr = Cell(Cell::REF, newLoc);
			}
		    }
		} else {
		    thisStackGC.push(toAbsolute(dst));
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
		HeapRef newLoc = findFreeSlotBound(numCells, dst);
		compactMove(dst, numCells, newLoc, verbosity);
		if (dst != newLoc) {
		    *cellPtr = Cell(Cell::INT32, newLoc);
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
    uint32_t newMask = mapNode.getMask() | (1 << index);
    size_t numArgs = bitCount(newMask);
    Cell *newCellPtr = allocate(2+numArgs);
    MapNode newMapNode(newCellPtr);
    newMapNode.setMeta(mapNode.getMeta());
    newMapNode.setMask(newMask);
    for (size_t i = 0, cnt = 0, newCnt = 0; i < 32; i++, newMask >>= 1) {
	if ((newMask & 1) != 0) {
	    if (index == i) {
		newMapNode.setArg(newCnt, *arg);
	    } else {
		newMapNode.setArg(newCnt, mapNode.getArg(cnt));
		cnt++;
	    }
	    newCnt++;
	}
    }
    Cell newCell(Cell::MAP, toRelative(newCellPtr));
    return CellRef(*this, newCell);
}

bool Heap::equalDeep(Cell a, Cell b) const
{
    if (a.getTag() != b.getTag()) {
	return false;
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
	    return false;
	}
	
	if (equalShallow(a,b)) {
	    continue;
	}

	switch (a.getTag()) {
	case Cell::STR:
	    {
		size_t a_arity = getArity(a);
		size_t b_arity = getArity(b);
		if (a_arity != b_arity) {
		    thisStack.trim(current);
		    return false;
		}
		for (size_t i = 0; i < a_arity; i++) {
		    Cell aArg = getArg(a, i);
		    Cell bArg = getArg(b, i);
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
	    return false;
	}
    }
    return true;
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

CellRef Heap::createNewSubMap(size_t depth, uint32_t hash,
			      CellRef key, CellRef value)
{
    // We need to create this bottom up so we avoid forward pointers.

    // First create a pair (key, value) which represents the leaf
    CellRef tree = newStr(thisFunctorColon);
    setArg(tree, 0, key);
    setArg(tree, 1, value);

    // Then build up the tree step by step.
    for (size_t i = 0; i < depth; i++) {
	size_t index = (hash >> (5 * (depth - i - 1))) & 0x1f;
	Cell *cellPtr = allocate(3);
	MapNode mapNode(cellPtr);
	mapNode.setMeta(false, i+1, 1);
	mapNode.setMask(1 << index);
	mapNode.setArg(0, *tree);
	tree = CellRef(*this, Cell(Cell::MAP, toRelative(cellPtr)));
    }

    return tree;
}

// Walk down the spine and non-destructively create a new table based
// on the old.
CellRef Heap::putMap(CellRef map, CellRef key, CellRef value)
{
    Cell mapCell = deref(*map);
    Cell *cellPtr = toAbsolute(toHeapRef(mapCell));

    MapNode mapNode(cellPtr);
    size_t depth = mapNode.getDepth();
    uint32_t hash = hashOf(key);

    CellRef spine[8];

    size_t i;
    for (i = 0; i < depth; i++) {
	spine[i] = CellRef(*this, mapCell);
	size_t index = (hash >> (depth-i-1)*5) & 0x1f; // value between 0..31
	uint32_t mask = mapNode.getMask();
	if ((mask & (1 << index)) == 0) {
	    // Bit is 0, so we need to create a new branch
	    size_t subDepth = depth - i - 1;
	    CellRef newTree = createNewSubMap(subDepth, hash, key, value);
	    for (size_t j = 0; j <= i; j++) {
		index = (hash >> 5*(i-j)) & 0x1f;
		const CellRef &arg = spine[i-j];
		newTree = setArg32(arg, index, newTree);
		// We need to increment entry count
		mapNode = MapNode(toAbsolute(toHeapRef(*newTree)));
		mapNode.incrementCount();
	    }
	    return newTree;
	} else {
	    mapCell = *getArg32(CellRef(*this,mapCell), index);
	    mapNode = MapNode(toAbsolute(toHeapRef(mapCell)));
	}
    }

    // Adjust 'i' to depth - 1. Treat it as last iteration.
    i--;
    
    // This means we've found an exact match. We need to check if
    // the key is present. If not, then we need to add it.
    // mapCell points to this leaf.
    Cell lst = mapCell;
    Cell searchKey = *key;
    while (isDot(lst)) {
	// Current cell is a non-empty list.
	Cell keyValuePair = getArg(lst, 0);
	Cell key1 = getArg(keyValuePair, 0);
	if (equal(searchKey, key1)) {
	    // TODO: We need a modified list.
	    assert("TODO"==NULL);
	}
	lst = getArg(lst, 1);
    }
    bool isColon = isFunctor(lst, thisFunctorColon);

    // We couldn't find it. So now we need to create a new entry
    CellRef refMapCell(*this, mapCell);
    CellRef keyValuePair = newStr(thisFunctorColon);
    setArg(keyValuePair, 0, key);
    setArg(keyValuePair, 1, value);

    CellRef newTree;

    if (isColon) {
	// Create a list of two items.
	newTree = newList(refMapCell, newCon(thisFunctorEmpty));
	newTree = newList(keyValuePair, newTree);
    } else {
	// We have an existing list. Just add it to the front of the list.
	newTree = newList(keyValuePair, refMapCell);
    }
    
    // Then create the new spine
    for (size_t j = 0; j <= i; j++) {
	size_t index = (hash >> 5*(i-j)) & 0x1f;
	const CellRef &arg = spine[i-j];
	newTree = setArg32(arg, index, newTree);
	// We need to increment entry count
	mapNode = MapNode(toAbsolute(toHeapRef(*newTree)));
	mapNode.incrementCount();
    }
    return newTree;
}

}
