#include <iostream>
#include <assert.h>
#include <vector>
#include <sstream>
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

void testBitMapRange1()
{
    std::cout << "----- testBitMapRange1() ------------------\n";

    BitMap bm(1000000);

    std::cout << " Size of bitmap: " << bm.getSize() << " bits\n";

    bm.setBits(1992,2019,true);
    std::string actual = bm.toString();
    std::string expect = "[1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018]";
    std::cout << "ACTUAL: " << actual << "\n";
    std::cout << "EXPECT: " << expect << "\n";
    assert(actual == expect);

    bm.setBits(2016, 2018, false);
    actual = bm.toString();
    expect = "[1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2018]";
    std::cout << "ACTUAL: " << actual << "\n";
    std::cout << "EXPECT: " << expect << "\n";
    assert(actual == expect);

    bm.setBits(801, 2010, false);
    actual = bm.toString();
    expect = "[2010, 2011, 2012, 2013, 2014, 2015, 2018]";
    std::cout << "ACTUAL: " << actual << "\n";
    std::cout << "EXPECT: " << expect << "\n";
    assert(actual == expect);

    // Test find bits

    std::cout << "Find first bit that is true\n";
    size_t found = bm.findBit(0, 10000, true);
    std::cout << "Found: " << found << "\n";
    assert(found == 2010);

    std::cout << "Find from 2050 (should be nothing)\n";
    found = bm.findBit(2050, 10000, true);
    std::cout << "Found: " << found << "\n";
    assert(found == 10000);

    std::cout << "Find bit not set startoing from 2010\n";
    found = bm.findBit(2010, 10000, false);
    std::cout << "Found: " << found << "\n";
    assert(found == 2016);

    std::cout << "Find cleared bit from 2017 = 2017\n";
    found = bm.findBit(2017, 10000, false);
    std::cout << "Found: " << found << "\n";
    assert(found == 2017);

    std::cout << "Find cleared bit from 2018 = 2019\n";
    found = bm.findBit(2018, 10000, false);
    std::cout << "Found: " << found << "\n";
    assert(found == 2019);

    // Test iterators
    BitMap::iterator itEnd = bm.end();
    std::stringstream ss;
    ss << "[";
    bool first = true;
    for (BitMap::iterator it = bm.begin(); it != itEnd; ++it) {
	if (!first) ss << ", ";
	first = false;
	ss << *it;
    }
    ss << "]";
    std::cout << "Iterate: " << ss.str() << "\n";
    assert(ss.str() == expect);

    // Iterate with offset and limit
    itEnd = bm.begin() + 10000;
    ss.str("");
    ss.clear();
    ss << "[";
    first = true;
    for (BitMap::iterator it = bm.begin(true, 2012); it != itEnd; ++it) {
	if (!first) ss << ", ";
	first = false;
	ss << *it;
    }
    ss << "]";
    std::cout << "Iterate from 2012: " << ss.str() << "\n";
    assert(ss.str() == "[2012, 2013, 2014, 2015, 2018]");

    // For cleared bits
    // Iterate with offset and limit
    itEnd = bm.begin(false, 2025);
    ss.str("");
    ss.clear();
    ss << "[";
    first = true;
    for (BitMap::iterator it = bm.begin(false, 2012); it != itEnd; ++it) {
	if (!first) ss << ", ";
	first = false;
	ss << *it;
    }
    ss << "]";
    std::cout << "Iterate from 2012 cleared bits: " << ss.str() << "\n";
    assert(ss.str() == "[2016, 2017, 2019, 2020, 2021, 2022, 2023, 2024]");

}

int main(int argc, char *argv[])
{
    testBitMap1();
    testBitMap2();
    testBitMapRange1();

    return 0;
}
