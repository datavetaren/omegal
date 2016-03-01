#include "Term.hpp"
#include <iostream>
#include <sstream>
#include <memory.h>
#include <assert.h>
#include "Stack.hpp"
#include "Flags.hpp"

namespace PROJECT {

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
    size_t escapedLen = ConstString::escapeName(ch, n, NULL);
    bool doAlloc = escapedLen > sizeof(cstrStack);

    if (doAlloc) {
	cstr = new char[escapedLen];
    }

    ConstString::escapeName(ch, n, cstr);
    
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
    NativeType *c = toAbsolute(ref.getIndex());
    NativeType name = c[0];
    Char *data = thisPool.toAbsolute(name);
    return (size_t)data[1];
}

void ConstTable::printConstNoArity(std::ostream &out, const ConstRef &ref) const
{
    NativeType *c = toAbsolute(ref.getIndex());
    NativeType name = c[0];
    Char *data = thisPool.toAbsolute(name);
    ConstString str(&data[2], (size_t)data[0], 0);
    out << str;
}

size_t ConstTable::getConstLength(const ConstRef &ref) const
{
    NativeType *c = toAbsolute(ref.getIndex());
    NativeType name = c[0];
    Char *data = thisPool.toAbsolute(name);
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
       : GrowingAllocator<Cell>(capacity), thisMaxNumRoots(0)
{
    Cell extComma(Cell::EXT_COMMA, 0);
    Cell extEnd(Cell::EXT_END, 0);

    thisExtComma = newCell(extComma);
    thisExtEnd = newCell(extEnd);

    thisStack.push(thisExtComma);
    thisStack.push(thisExtEnd);
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
    case Cell::INT32: out << "INT32"; break;
    case Cell::EXT:
	switch (cell.getExtTag()) {
	default: out << "???"; break;
	}
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
    case Cell::INT32: out << toInt32(cell); break;
    case Cell::EXT:
	switch (cell.getExtTag()) {
	default:
	    out << "???"; break;
	}
    }
}

void Heap::printRaw(std::ostream &out) const
{
    printRaw(out, first(), top()-1);
}

void Heap::printRaw(std::ostream &out, HeapRef from, HeapRef to) const
{
    for (HeapRef i = from; i <= to; i++) {
	out << "[" << i.getIndex() << "]: ";
	Cell cell = getCell(i);
	printCell(out, cell);
	out << "\n";
    }
}

std::string Heap::toRawString() const
{
    return toRawString(first(), top()-1);
}

std::string Heap::toRawString(HeapRef from, HeapRef to) const
{
    std::stringstream ss;
    ss << "[";
    for (HeapRef i = from; i <= to; i++) {
	if (i != from) {
	    ss << ", ";
	}
	Cell cell = getCell(i);
	printCell(ss, cell);
    }
    ss << "]";
    return ss.str();
}

size_t Heap::getStringLength(HeapRef href, size_t maximum) const
{
    size_t current = thisStack.getSize();

    thisStack.push(href);

    size_t len = 0;

    while (current != thisStack.getSize()) {
	if (len >= maximum) {
	    thisStack.trim(current);
	    return maximum;
	}

	IHeapRef href1 = deref(thisStack.pop());
	Cell cell = getCell(href1);

	switch (cell.getTag()) {
	case Cell::CON:
	    len += thisConstTable.getConstLength(cell.toConstRef()); break;
	case Cell::STR:
	    len += getStringLengthForStruct(cell); break;
	case Cell::REF:
	    len += getStringLengthForRef(cell); break;
	case Cell::INT32:
	    len += getStringLengthForInt32(toInt32(cell)); break;
        case Cell::EXT:
	    switch (cell.getExtTag()) {
	    case Cell::EXT_END:
		len++; break;
	    case Cell::EXT_COMMA:
		len += 2; break;
	    default:
		break;
	    }
	    break;
	}
    }
    return len;
}

size_t Heap::getStringLengthForStruct(Cell cell) const
{
    size_t len = 0;
    HeapRef href = toHeapRef(cell);
    ConstRef cref = const_cast<Heap *>(this)->pushArgs(href);
    len += thisConstTable.getConstLength(cref);
    size_t arity = thisConstTable.getConstArity(cref);
    if (arity > 0) {
	len++;
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

std::string Heap::toString(HeapRef href) const
{
    std::stringstream ss;
    print(ss, href);
    return ss.str();
}

ConstRef Heap::pushArgs(HeapRef ref)
{
    Cell cell = getCell(ref);
    ConstRef cref = cell.toConstRef();
    size_t arity = thisConstTable.getConstArity(cref);
    if (arity > 0) {
	thisStack.push(thisExtEnd);
	for (size_t i = 0; i < arity; i++) {
	    if (i > 0) {
		thisStack.push(thisExtComma);
	    }
	    IHeapRef arg = ref+arity-i;
	    thisStack.push(arg);
	}
    }
    return cref;
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

void Heap::printIndent(std::ostream &out, PrintState &state) const
{
    if (state.needNewLine()) {
	out << "\n";
	state.resetToColumn(0);
	state.printIndent(out);
    }
}

void Heap::print(std::ostream &out, HeapRef href, const PrintParam &param) const
{
    PrintState state(param);

    size_t current = thisStack.getSize();

    thisStack.push(href);

    while (current != thisStack.getSize()) {
	IHeapRef dh = deref(thisStack.pop());
	Cell cell = getCell(dh);

	switch (cell.getTag()) {
	case Cell::CON: {
	    ConstRef cref = cell.toConstRef();
	    printIndent(out, state.addToColumn(thisConstTable.getConstLength(cref)));
	    thisConstTable.printConstNoArity(out, cref);
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
		state.willWrap(getStringLength(href,
					       state.willWrapOnLength()))) {
		
		state.newLine(out);
	    }
 	    HeapRef href = toHeapRef(cell);
	    ConstRef cref = const_cast<Heap *>(this)->pushArgs(href);
	    size_t arity = thisConstTable.getConstArity(cref);
	    printIndent(out, state.addToColumn(
	       thisConstTable.getConstLength(cref)
	       + ((arity > 0) ? 1 : 0)));
	    thisConstTable.printConstNoArity(out, cref);
	    if (arity > 0) {
		out << "(";
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
	     thisConstTable.printConstNoArity(out, cref);
             break;
    	    }
        case Cell::EXT:
	    switch (cell.getExtTag()) {
	    case Cell::EXT_END:
		printIndent(out, state.addToColumn(1));
		out << ")";
		state.decrementIndent();
		break;
	    case Cell::EXT_COMMA:
		printIndent(out, state.addToColumn(2));
		out << ", ";
		break;
	    default:
		printIndent(out, state.addToColumn(3));
		out << "???";
		break;
	    }
	    break;
	}
    }
}

void Heap::printStatus(std::ostream &out) const
{
    out << "Heap{Size=" << getSize() << ",StackSize=" << thisStack.getSize() << ",RootsSize=" << thisRoots.numEntries() << ",MaxRootsSize=" << thisMaxNumRoots << "}";
}

void Heap::printRoots(std::ostream &out) const
{
    thisRoots.print(out);
}

void Heap::parseSkipWhite(std::istream &in, LocationTracker &loc)
{
    while (isspace(in.peek())) {
	char ch;
	in >> ch;
	loc.advance(ch);
    }
}

HeapRef Heap::parseConst(std::istream &in, LocationTracker &loc)
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

HeapRef Heap::expectError(LocationTracker &loc, int lookahead, Expect expect)
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

HeapRef Heap::parseTerm(std::istream &in, LocationTracker &loc)
{
    size_t depth = 0;
    Expect expect;

    parseSkipWhite(in, loc);

    HeapRef result;

    HeapRef currentFunctor = parseConst(in, loc);
    size_t current = thisStack.getSize();
    thisStack.push(currentFunctor);

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
	    IHeapRef functorRef = thisStack.peek(currentArity);
	    ConstRef functorCref = getCell(functorRef).toConstRef();

	    // std::cout << "Manage functor: " << getConstName(functorCref) << " arity " << currentArity << "\n";

	    ConstRef functor = getConst(functorCref, currentArity);
	    HeapRef str = newStr(top()+1);
	    HeapRef f = newCon(functor);
	    for (size_t i = 0; i < currentArity; i++) {
		IHeapRef arg = thisStack.peek(currentArity-1-i);
		newCell(getCell(arg));
	    }
	    thisStack.trim(thisStack.getSize()-currentArity-1);

	    // Is this the last item to process?
	    if (current == thisStack.getSize()) {
		result = str;
	    } else {
		thisStack.push(str);
	    }

	    expect = COMMA;

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
	    // Variable
	    expect = COMMA;
	} else {
	    // Expect term
	    HeapRef newFunctor = parseConst(in, loc);
	    thisStack.push(newFunctor);

	    expect = LPAREN | COMMA | RPAREN;
	}
    }

    // Remove any remaining garbage on stack
    thisStack.trim(current);

    if (result.isEmpty()) {
	result = parseError(loc, "TERM ended too early");
    }

    return result;
}

HeapRef Heap::parseError(LocationTracker &loc, const std::string &reason)
{
    ConstRef cref = thisConstTable.getConst("$parseError", 3);
    ConstRef creason = thisConstTable.getConst(reason.c_str(), 0);
    std::stringstream lineStr;
    HeapRef err = newStr(top() + 1);
    newCon(cref);
    newCon(creason);
    HeapRef args = newArgs(2);
    newInt32(args+0, loc.getLine());
    newInt32(args+1, loc.getColumn());

    return err;
}

HeapRef Heap::parse(std::istream &in, LocationTracker &loc)
{
    parseSkipWhite(in, loc);
    HeapRef href = parseTerm(in, loc);

    return href;
}

HeapRef Heap::parse(std::istream &in)
{
    LocationTracker loc;
    return parse(in, loc);
}

}
