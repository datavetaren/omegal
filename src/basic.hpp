#ifndef _basic_hpp
#define _basic_hpp

#include <stdlib.h>
#include <stdint.h>
#include <string>

#define PROJECT omegal

namespace PROJECT {

static const size_t WORDSIZEBITS = 3;

template< class T > struct remove_const          { typedef T type; };
template< class T > struct remove_const<const T> { typedef T type; };
 
template< class T > struct remove_volatile             { typedef T type; };
template< class T > struct remove_volatile<volatile T> { typedef T type; };

template< class T >
struct remove_cv {
    typedef typename remove_volatile<typename remove_const<T>::type>::type type;
};

typedef uint32_t Char;
typedef std::basic_string<Char> String;

}

#endif
