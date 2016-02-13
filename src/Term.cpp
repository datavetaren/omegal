#include "Term.hpp"
#include <iostream>
#include <memory.h>

namespace PROJECT {

std::string ConstTable::getNameAscii(ConstRef ref)
{
    std::string s = "";
    ConstString str = thisPool.getString(ref);
    const Char *p = str.getString();
    while (*p != 0) {
	s += (char)*p;
	p++;
    }
    return s;
}

void ConstTable::print(std::ostream &out)
{
    size_t n = getSize();
    for (size_t i = 0; i < n; i++) {
	out << "[" << i << "]: " << getNameAscii(ConstRef(i)) << "\n";
    }
}

}
