#include "Term.hpp"
#include <iostream>
#include <memory.h>

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

void ConstTable::print(std::ostream &out)
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

}
