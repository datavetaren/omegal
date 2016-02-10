#include <iostream>
#include <assert.h>
#include "HashMap.hpp"

using namespace PROJECT;

int main(int argc, char *argv[])
{
    HashMap<int,int> map;

    map[3] = 100;
    map[23] = 200;
    map[11] = 300;

    map.put(192, 400);
    map.put(77, 500);
    map.put(42, 600);

    std::cout << "VALUE AT [11] = " << map[11] << "\n";

    assert(map[11] == *map.get(11));

    std::cout << "HashMap Internal\n";
    map.printInternal(std::cout);
    std::cout << "HashMap: " << map << "\n";

    for (HashMap<int,int>::iterator it = map.begin(); it != map.end(); ++it) {
	std::cout << "We have: " << it->getKey() << "\n";
    }


    return 0;
}
