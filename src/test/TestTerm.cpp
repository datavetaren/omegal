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
    heap.print(std::cout, heap.getCell(heap.first() + 7));
    std::cout << "\n";

    const std::string expected = "p(A, h(A, B), f(B))";
    std::cout << "EXPECTED: "<< expected << "\n";

    std::string got = heap.toString(heap.getCell(heap.first() + 7));
    std::cout << "GOT     : " << got << "\n";

    assert(expected == got);
}

static size_t myRand(size_t bound)
{
    static size_t state = 4711;
    
    state = 13*state + 734672631;

    return state % bound;
}

Cell newTerm(Heap &heap, size_t maxDepth, size_t depth = 0)
{
    size_t arity = (depth >= maxDepth) ? 0 : myRand(6);
    char functorName[2];
    functorName[0] = 'a' + (char)arity;
    functorName[1] = '\0';
    ConstRef functor = heap.getConst(functorName, arity);
    Cell str = heap.newStr(heap.top()+1);
    heap.newCon(functor);
    if (arity > 0) {
	Cell *args = heap.allocate(arity);
	for (size_t j = 0; j < arity; j++) {
	    Cell arg = newTerm(heap, maxDepth, depth+1);
	    args[j] = arg;
	}
    }
    return str;
}

const char *BIG_TERM_GOLD =
"e(b(e(b(e(a, a, a, a)), b(e(a, a, a, a)), b(e(a, a, a, a)), b(c(a, a)))), \n"
"  f(\n"
"    e(b(e(a, a, a, a)), f(a, f(a, a, a, a, a), a, f(a, a, a, a, a), a), \n"
"      b(e(a, a, a, a)), b(e(a, a, a, a))), \n"
"    d(c(b(a), c(a, a)), f(a, b(a), a, d(a, a, a), e(a, a, a, a)), \n"
"      f(c(a, a), b(a), c(a, a), f(a, a, a, a, a), a)), \n"
"    b(e(f(a, a, a, a, a), e(a, a, a, a), b(a), c(a, a))), \n"
"    f(e(f(a, a, a, a, a), c(a, a), b(a), e(a, a, a, a)), b(c(a, a)), \n"
"      f(c(a, a), f(a, a, a, a, a), a, b(a), e(a, a, a, a)), \n"
"      d(a, d(a, a, a), e(a, a, a, a)), b(e(a, a, a, a))), \n"
"    d(c(b(a), c(a, a)), b(e(a, a, a, a)), d(e(a, a, a, a), d(a, a, a), a))), \n"
"  b(\n"
"    e(d(e(a, a, a, a), f(a, a, a, a, a), c(a, a)), \n"
"      f(c(a, a), f(a, a, a, a, a), c(a, a), b(a), e(a, a, a, a)), \n"
"      b(e(a, a, a, a)), d(e(a, a, a, a), b(a), c(a, a)))), \n"
"  f(a, \n"
"    d(a, d(e(a, a, a, a), f(a, a, a, a, a), a), \n"
"      f(c(a, a), b(a), e(a, a, a, a), b(a), a)), \n"
"    d(a, d(c(a, a), b(a), c(a, a)), b(e(a, a, a, a))), b(a), \n"
"    d(a, b(a), \n"
"      f(e(a, a, a, a), d(a, a, a), c(a, a), d(a, a, a), e(a, a, a, a)))))\n";

static std::string cut(const char *from, const char *to)
{
    std::string r;
    for (const char *p = from; p < to; p++) {
	r += (char)*p;
    }
    return r;
}

void testBigTerm()
{
    std::cout << "testBigTerm() -------------------------------\n";

    Heap heap;

    const size_t DEPTH = 5;

    Cell term = newTerm(heap, DEPTH);
    PrintParam param;
    param.setMaxWidth(78-param.getStartColumn());

    std::stringstream ss;
    heap.print(ss, term, param);
    ss << "\n";

    std::string str = ss.str();

    const char *goldScan = BIG_TERM_GOLD;
    const char *actScan = str.c_str();

    size_t lineCnt = 0;
    bool err = false;

    while (goldScan[0] != '\0') {
	const char *goldNextLine = strchr(goldScan, '\n');
	const char *actNextLine = strchr(actScan, '\n');
	if (goldNextLine == NULL && actNextLine != NULL) {
	    std::cout << "There was no next line in gold template.\n";
	    std::cout << "Actual: " << cut(actScan, actNextLine) << "\n";
	    err = true;
	    break;
	}
	if (goldNextLine != NULL && actNextLine == NULL) {
	    std::cout << "Actual feed terminated to early.\n";
	    std::cout << "Expect: " << cut(goldScan, goldNextLine) << "\n";
	    err = true;
	    break;
	}
	size_t goldNextLineLen = goldNextLine - goldScan;
	size_t actNextLineLen = actNextLine - actScan;
	if (goldNextLineLen != actNextLineLen) {
	    std::cout << "Difference at line " << lineCnt << ":\n";
	    std::cout << "(Due to different lengths: ExpectLen: " << goldNextLineLen << " ActualLen: "<< actNextLineLen << ")\n";
	    std::cout << "Actual: " << cut(actScan, actNextLine) << "\n";
	    std::cout << "Expect: " << cut(goldScan, goldNextLine) << "\n";
	    err = true;
	    break;
	}
	std::string actual = cut(actScan, actNextLine);
	std::string expect = cut(goldScan, goldNextLine);
	if (actual != expect) {
	    std::cout << "Difference at line " << lineCnt << ":\n";
	    for (size_t i = 0; i < actual.length(); i++) {
		if (actual[i] != expect[i]) {
		    std::cout << "(at column " << i << ")\n";
		    break;
		}
	    }
	    std::cout << "Actual: " << actual << "\n";
	    std::cout << "Expect: " << expect << "\n";
	    err = true;
	    break;
	}
	goldScan = &goldNextLine[1];
	actScan = &actNextLine[1];
	lineCnt++;
    }

    if (!err) {
	std::cout << "OK\n";
    }

    assert(!err);
}

int main(int argc, char *argv[] )
{
    testConst();
    testWAMBookFigure21();
    testBigTerm();

    return 0;
}
