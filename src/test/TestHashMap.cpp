#include <iostream>
#include <assert.h>
#include <vector>
#include "HashMap.hpp"

using namespace PROJECT;

void testBasic1()
{
    HashMap<int,int> map;

    map[3] = 100;

    std::cout << "Key 3 has value " << map[3] << "\n";
    assert(map[3] == 100);

    map[23] = 200;
    map[11] = 300;

    map.put(192, 400);
    map.put(77, 500);
    map.put(42, 600);
    map.put(659, 700);

    std::cout << "Key 11 has value " << map[11] << "\n";
    assert(map[11] == *map.get(11));
    assert(map[11] == 300);

    std::cout << "HashMap Internal\n";
    map.printInternal(std::cout);

    std::cout << "HashMap: " << map << "\n";
    std::cout << "HashMap: [";
    for (HashMap<int,int>::iterator it = map.begin(); it != map.end(); ++it) {
	if (it != map.begin()) {
	    std::cout << ", ";
	}
	std::cout << it->getKey() << ":" << it->getValue();
    }
    std::cout << "]\n";

    std::cout << "Remove key 42\n";
    map.remove(42);

    std::cout << "HashMap Internal\n";
    map.printInternal(std::cout);

    std::cout << "Remove key 77\n";
    map.remove(77);

    std::cout << "HashMap Internal\n";
    map.printInternal(std::cout);

    std::cout << "Remove key 659\n";
    map.remove(659);

    std::cout << "HashMap Internal\n";
    map.printInternal(std::cout);
}

typedef std::pair<int,int> RefEntry;
typedef std::vector<RefEntry> RefMap;

void refPut(RefMap &map, int key, int value)
{
    for (RefMap::iterator it = map.begin(); it != map.end(); ++it) {
	if (it->first == key) {
	    it->second = value;
	    return;
	}
    }
    map.push_back(std::make_pair(key,value));
}

int refGet(RefMap &map, int key)
{
    for (RefMap::iterator it = map.begin(); it != map.end(); ++it) {
	if (it->first == key) {
	    return it->second;
	}
    }
    return -1;
}

void testBasic2()
{
    HashMap<int,int> map;
    RefMap refMap;

    for (int i = 0; i < 100; i++) {
	map[i] = i*1000;
	refPut(refMap, i, i*1000);
    }

    map.printInternal(std::cout);

    for (RefMap::iterator it = refMap.begin(); it != refMap.end(); ++it) {
	std::cout << "Check " << it->first << " -> " << it->second << "\n";
	std::cout << "Got   " << it->first << " -> " << map[it->first]<<"\n";
	assert(map[it->first] == it->second);
    }

    assert(map.numRehash() == 4);

    std::cout << "Num rehash: " << map.numRehash() << "\n";
    std::cout << "Num entries: " << map.numEntries() << "\n";
    std::cout << "Num collisions: " << map.numCollisions() << "\n";

    assert(map.numEntries() == 100);

    assert(map.numCollisions() < 40);
}

int main(int argc, char *argv[])
{
    testBasic1();
    testBasic2();

    return 0;
}
