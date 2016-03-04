#include <iostream>
#include <assert.h>
#include <vector>
#include "BitMap.hpp"

using namespace PROJECT;

void testBitMap1()
{
    std::cout << "----- testBitMap1() -----------------------\n";

    BitMap bm;
    bm.setBit(100, true);
    bm.setBit(1023, true);
    bm.setBit(2049, true);
    bm.setBit(135, true);

    std::string actual = bm.toString();
    std::string expect = "[100, 135, 1023, 2049]";
    std::cout << "ACTUAL: " << actual << "\n";
    std::cout << "EXPECT: " << expect << "\n";
    assert(actual == expect);
}

void testBitMap2()
{
    std::cout << "----- testBitMap2() -----------------------\n";

    BitMap bm(1000000);

    std::cout << " Size of bitmap: " << bm.getSize() << " bits\n";
    bm.setBit(164642, true);

    std::string actual = bm.toString();
    std::string expect = "[164642]";
    std::cout << "ACTUAL: " << actual << "\n";
    std::cout << "EXPECT: " << expect << "\n";
    assert(actual == expect);

}


int main(int argc, char *argv[])
{
    testBitMap1();
    testBitMap2();

    return 0;
}
