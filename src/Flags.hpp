#ifndef _Flags_hpp
#define _Flags_hpp

#include "basic.hpp"

namespace PROJECT {

template<typename EnumType> class Flags {
private:
    typedef EnumType T;
    typedef NativeType StorageType;

public:
    Flags() { thisValue = 0; }
    Flags(const Flags<T> &other) { thisValue = other.thisValue; }
    Flags(T flag) { thisValue = 1 << flag; }
    
    inline StorageType getValue() const { return thisValue; }

    inline void clearAll() { thisValue = 0; }

    inline bool operator == (const Flags<T> &other) const {
	return thisValue == other.thisValue;
    }

    inline bool operator != (const Flags<T> &other) const {
	return thisValue != other.thisValue;
    }
    
    inline bool operator & (T flag) const {
	return (thisValue & ((StorageType)1 << flag)) != 0;
    }

    inline bool operator & (const Flags<T> &flags) const {
	return (thisValue & flags.thisValue) != 0;
    }

    inline Flags<T> operator | (T flag) const {
	return thisValue | (1 << flag);
    }

    inline Flags<T> & operator = (const Flags<T> &rhs) {
	thisValue = rhs.thisValue;
	return *this;
    }

    inline Flags<T> operator | (const Flags<T> &other) {
	return thisValue | other.thisValue;
    }

    inline Flags<T> operator | (const T &other) {
	return *this | Flags<T>(other);
    }

    inline void clear(T flag) {
	thisValue &= ~(1 << flag);
    }

    inline void set(T flag, bool v) {
	if (v) {
	    thisValue |= (1 << flag);
	} else {
	    clear(flag);
	}
    }

    std::string toString() const {
	std::string s;
	bool first = true;
	for (size_t i = 0; i < sizeof(thisValue)*8; i++) {
	    T flag = (T)i;
	    if (*this & flag) {
		char flagName[64];
		Flags_GetFlagName(flag, flagName);
		if (!first) s += "|";
		s += flagName;
		first = false;
	    }
	}
	return s;
    }

    inline Flags(StorageType value) : thisValue(value) { }

private:
    StorageType thisValue;
};

#define DeclareFlags(EnumName, ...)		                    \
    enum EnumName { __VA_ARGS__ };                                  \
    static void Flags_GetFlagName(enum EnumName flag, char *name)   \
    { const char *p = #__VA_ARGS__;                                 \
      const char *pEnd = p + strlen(p);                             \
      int cnt = 1;                                                  \
      while (p < pEnd) {                                            \
          bool hasEq = false;                                       \
	  const char *pNextComma = strchr(p, ',');                  \
	  const char *pNextEq = strchr(p, '=');                     \
          const char *pNext = NULL;                                 \
          if (pNextComma != NULL && pNextEq != NULL) {              \
	      pNext = (pNextComma < pNextEq) ? pNextComma : pNextEq;\
	  } else if (pNextComma == NULL) {                          \
              pNext = pNextEq;                                      \
	  } else {                                                  \
	      pNext = pNextComma;                                   \
          }                                                         \
          if (pNext != NULL && *pNext == '=') {                     \
	      cnt = strtol(pNext+1, NULL, 0);                       \
              hasEq = true;                                         \
          }                                                         \
          if (pNext == NULL) pNext = pEnd;                          \
          if (flag == cnt) {                                        \
	      const char *p1 = pNext;                               \
	      while (isspace(*p)) p++;                              \
	      do { p1--; } while (p1 > p && isspace(*p1));          \
	      strncpy(name, p, p1-p+1);                             \
	      name[p1-p+1] = '\0';                                  \
              return;                                               \
          }                                                         \
	  if (hasEq) {                                              \
               pNext = strchr(p, ',');			            \
	       if (pNext == NULL) pNext = pEnd;                     \
          }                                                         \
          cnt++;                                                    \
          if (*pNext == ',') pNext++;                               \
          p = pNext;                                                \
      }                                                             \
      strcpy(name, "?");                                            \
    }                                                               \
    template class Flags<EnumName>;				    \
    inline Flags<EnumName> Flags_Private(EnumName a, EnumName b)    \
    { return Flags<EnumName>(((NativeType)1 << a) |                 \
			     ((NativeType)1 << b)); }               \
    static inline Flags<EnumName> operator | (EnumName a, EnumName b) \
    { return Flags_Private(a,b); }
}

#endif
