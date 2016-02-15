#include <iostream>
#include <assert.h>
#include "../Term.hpp"

using namespace PROJECT;

void testConst()
{
    std::cout << "testConst() ----------------------------------\n";

    ConstTable table;

    ConstRef c1 = table.addConst("kalle", 3);
    ConstRef c2 = table.addConst("kula", 0);
    ConstRef c3 = table.addConst("testar", 1);

    table.print(std::cout);

    ConstRef f1 = table.findConst("kalle", 3);
    assert(f1 == c1);
    ConstRef f2 = table.findConst("kula", 0);
    assert(f2 == c2);
    ConstRef f3 = table.findConst("testar", 1);
    assert(f3 == c3);

    assert(table.findConst("xyzzy", 0) == ConstRef());
    assert(table.findConst("testar", 2) == ConstRef());
}

int main(int argc, char *argv[] )
{
    testConst();

    return 0;
}
