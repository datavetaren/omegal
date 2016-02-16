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

void testWAMBookFigure21()
{
    std::cout << "testWAMBookFigure21() -----------------------\n";
    std::cout << "(wambook.sourceforge.net, page 11)\n";

    // Ok, I know the offset is +1 (if you compare it with WAM book)
    // but I reserve 0 to be the NULL pointer as I store relative pointers
    // and 0 has this special meaning. (Close enough!)

    Heap heap;

    ConstRef h2 = heap.addConst("h", 2);
    ConstRef f1 = heap.addConst("f", 1);
    ConstRef p3 = heap.addConst("p", 3);

    heap.newStr(heap.top() + 1);
    heap.newCon(h2);
    heap.newRef();
    heap.newRef();
    heap.newStr(heap.top() + 1);
    heap.newCon(f1);
    heap.newRef(heap.first() + 3);
    heap.newStr(heap.top() + 1);
    heap.newCon(p3);
    heap.newRef(heap.first() + 2);
    heap.newStr(heap.first() + 1);
    heap.newStr(heap.first() + 5);

    heap.printRaw(std::cout);

    std::string asList = heap.toRawString();
    std::cout << "AS LIST: " << asList << "\n";

    assert(asList == "[STR:2, CON:h/2, REF:3, REF:4, STR:6, CON:f/1, REF:4, STR:9, CON:p/3, REF:3, STR:2, STR:6]");

    std::cout << "AS TERM: ";
    heap.print(std::cout, heap.first() + 7);
    std::cout << "\n";

    const std::string expected = "p(A, h(A, B), f(B))";
    std::cout << "EXPECTED: "<< expected << "\n";

    std::string got = heap.toString(heap.first() + 7);
    std::cout << "GOT     : " << got << "\n";

    assert(expected == got);
}

int main(int argc, char *argv[] )
{
    testConst();
    testWAMBookFigure21();

    return 0;
}
