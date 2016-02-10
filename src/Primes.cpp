#include <memory.h>
#include <math.h>
#include "Primes.hpp"

namespace PROJECT {

int * Primes::thePrimes = NULL;
int Primes::thePrimesSize = 0;
int Primes::thePrimesCapacity = 0;

void Primes::init()
{
    static const int initialPrimes[] =
	{2, 3, 5, 7, 11, 13, 17, 19};

    thePrimesCapacity = 1000;
    thePrimesSize = sizeof(initialPrimes) / sizeof(int);
    thePrimes = new int[thePrimesCapacity];
    memcpy(thePrimes, initialPrimes, sizeof(initialPrimes));
}

void Primes::reset()
{
    if (thePrimes != NULL) {
	delete thePrimes;
    }
    thePrimes = NULL;
    thePrimesSize = 0;
    thePrimesCapacity = 0;
}

int Primes::nextPrime()
{
    if (thePrimes == NULL) {
	init();
    }
    int cand = thePrimes[thePrimesSize-1] + 2;
    for (;;) {
	bool isPrime = true;
	for (int i = 0; thePrimes[i]*thePrimes[i] <= cand; i++) {
	    if (cand % thePrimes[i] == 0) {
		isPrime = false;
		break;
	    }
	}
	if (isPrime) {
	    if (thePrimesSize == thePrimesCapacity) {
		int newCap = 2*thePrimesCapacity;
		int *newPrimes = new int[newCap];
		memcpy(newPrimes, thePrimes, sizeof(thePrimesSize)*sizeof(int));
	        delete thePrimes;
		newPrimes = thePrimes;
	    }
	    thePrimes[thePrimesSize++] = cand;
	    return cand;
	}
	cand += 2;
    }
}

int Primes::nextPrime(int start)
{
    if (thePrimes == NULL) {
	init();
    }
    if (start < 2) { return 2; }
    int cand = start+1;
    if (cand % 2 == 0) cand++;
    for (;;) {
	bool isPrime = true;
	for (int i = 0; thePrimes[i]*thePrimes[i] <= cand; i++) {
	    if (i+1 == thePrimesSize) {
		nextPrime();
	    }
	    if (cand % thePrimes[i] == 0) {
		isPrime = false;
		break;
	    }
	}
	if (isPrime) {
	    return cand;
	}
	cand += 2;
    }
}

void Primes::cleanup()
{
    delete thePrimes;
    thePrimes = NULL;
}

}
