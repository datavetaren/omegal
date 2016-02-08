#ifndef _Primes_hpp
#define _Primes_hpp

class Primes {
public:
    static int nextPrime();
    static int nextPrime(int start);

    static void cleanup();

private:
    Primes() { } // Prevent construction

    static void init();
    static void reset();

    static int *thePrimes;
    static int thePrimesSize;
    static int thePrimesCapacity;
};

#endif
