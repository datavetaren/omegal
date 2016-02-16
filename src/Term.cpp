#include "Term.hpp"
#include <iostream>
#include <sstream>
#include <memory.h>
#include "Stack.hpp"

namespace PROJECT {

std::ostream & operator << (std::ostream &out, const ConstRef &ref)
{
    if (ref.getIndex() == 0) {
	out << "ConstRef(NULL)";
    } else {
	out << "ConstRef(" << ref.getIndex() << ")";
    }
    return out;
}

std::ostream & operator << (std::ostream &out, const ConstString &str)
{
    const Char *ch = str.getString();
    size_t n = str.getLength();
    size_t arity = str.getArity();
    for (size_t i = 0; i < n; i++) {
	out << (char)ch[i];
    }
    if (arity > 0) {
	out << "/";
	out << arity;
    }
    
    return out;
}

ConstRef ConstTable::getConst(const char *name, size_t arity) const
{
    ConstRef cref = findConst(name, arity);
    if (cref == ConstRef()) {
	return const_cast<ConstTable *>(this)->addConst(name, arity);
    } else {
	return cref;
    }
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

ConstRef ConstTable::addConst(const char *name, size_t arity)
{
    ConstString str = thisPool.addString(name, arity);
    NativeType *cr = allocate(1);
    NativeType rel = static_cast<NativeType>(thisPool.toRelativePointer(str));
    *cr = rel;
    ConstRef constRef(toRelative(cr));
    thisIndexing.add(str, constRef);
    return constRef;
}

ConstRef ConstTable::findConst(const char *name, size_t arity) const
{
    size_t n = strlen(name);
    if (n < 1023) {
	Char chs[1024];
	for (size_t i = 0; i <= n; i++) {
	    chs[i] = (Char)name[i];
	}
	ConstString str(chs, n, arity);
	return thisIndexing.find(str);
    } else {
	// We should disable this code path once we add disable
	// for new/delete allocation.
	Char *chs = new Char[n+1];
	for (size_t i = 0; i <= n; i++) {
	    chs[i] = (Char)name[i];
	}
	ConstString str(chs, n, arity);
	ConstRef ref = thisIndexing.find(str);
	delete [] chs;
	return ref;
    }
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

void Heap::printTag(std::ostream &out, Cell cell) const
{
    switch (cell.getTag()) {
    case Cell::REF: out << "REF"; break;
    case Cell::CON: out << "CON"; break;
    case Cell::STR: out << "STR"; break;
    case Cell::EXT:
	switch (cell.getExtTag()) {
	case Cell::EXT_INT32: out << "INT32"; break;
	case Cell::EXT_INT64: out << "INT64"; break;
	case Cell::EXT_FLOAT: out << "FLOAT"; break;
	case Cell::EXT_DOUBLE: out << "DOUBLE"; break;
	case Cell::EXT_INT128: out << "INT128"; break;
	case Cell::EXT_ARRAY: out << "ARRAY"; break;
	default: out << "???"; break;
	}
    }
}

void Heap::printConst(std::ostream &out, Cell cell) const
{
    ConstRef cref = reinterpret_cast<Con *>(&cell)->getConstRef();
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
    case Cell::EXT: out << "???"; break;
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

std::string Heap::toString(HeapRef ref) const
{
    std::stringstream ss;
    print(ss, ref);
    return ss.str();
}

void Heap::printStruct(std::ostream &out, HeapRef ref) const
{
    Cell cell = getCell(ref);
    ConstRef cref = reinterpret_cast<Con &>(cell).getConstRef();
    thisConstTable.printConstNoArity(out, cref);
    size_t arity = thisConstTable.getConstArity(cref);
    if (arity > 0) {
	out << "(";
	thisStack.push(Cell(Cell::EXT_END,0));
	Cell listComma(Cell::EXT_COMMA,0);
	for (size_t i = 0; i < arity; i++) {
	    if (i > 0) {
		thisStack.push(listComma);
	    }
	    thisStack.push(getCell(ref+arity-i));
	}
    }
}

void Heap::printRef(std::ostream &out, Cell cell) const
{
    const ConstRef *pcref = thisNameMap.get(cell);
    if (pcref == NULL) {
	ConstRef cref = thisConstTable.getConst(thisNameMap.numEntries());
	thisNameMap.put(cell, cref);
	thisConstTable.printConstNoArity(out, cref);
	return;
    }
    thisConstTable.printConstNoArity(out, *pcref);
}

void Heap::print(std::ostream &out, HeapRef ref) const
{
    size_t current = thisStack.getSize();

    Cell cell = getCell(ref);
    thisStack.push(cell);

    while (current != thisStack.getSize()) {
	Cell cell = deref(thisStack.pop());

	switch (cell.getTag()) {
	case Cell::CON:
	    thisConstTable.printConstNoArity(out,
	       reinterpret_cast<Con &>(cell).getConstRef()); break;
	case Cell::STR:
	    printStruct(out, HeapRef(cell.getValue())); break;
	case Cell::REF:
	    printRef(out, cell); break;
        case Cell::EXT:
	    switch (cell.getExtTag()) {
	    case Cell::EXT_END:
		out << ")";
		break;
	    case Cell::EXT_COMMA:
		out << ", ";
		break;
	    default:
		out << "???";
		break;
	    }
	    break;
	}
    }
}

}
