#include <iostream>
#include <assert.h>
#include "HashMap.hpp"

int main(int argc, char *argv[])
{
    HashMap<int,int> map;
    map.put(3, 100);
    map.put(23, 200);
    map.put(11, 300);
    map.put(192, 400);
    map.put(77, 500);
    map.put(42, 600);

    map.printInternal(std::cout);

    return 0;
}
