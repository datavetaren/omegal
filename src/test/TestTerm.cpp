#include <iostream>
#include <assert.h>
#include "../Term.hpp"

using namespace PROJECT;

void testConst()
{
    std::cout << "testConst() ----------------------------------\n";

    ConstTable table;

    ConstRef c1 = table.getConst("kalle", 3);
    ConstRef c2 = table.getConst("kula", 0);
    ConstRef c3 = table.getConst("testar", 1);

    ConstRef f1 = table.getConst("kalle", 3);
    assert(f1 == c1);
    ConstRef f2 = table.getConst("kula", 0);
    assert(f2 == c2);
    ConstRef f3 = table.getConst("testar", 1);
    assert(f3 == c3);


    {
	ConstRef f4 = table.getConst("A", 0);
	std::stringstream ss;
	table.printConst(ss, f4);
	assert(ss.str() ==  "'A'");
    }

    {
	ConstRef f5 = table.getConst("x(", 0);
	std::stringstream ss;
	table.printConst(ss, f5);
	assert(ss.str() ==  "'x('");
    }

    {
	ConstRef f6 = table.getConst("x\\'y", 0);
	std::stringstream ss;
	table.printConst(ss, f6);
	std::cout << "HEJ: " << ss.str() << "\n";
	assert(ss.str() ==  "'x\\\\\\'y'");
    }

    table.print(std::cout);
}

void testWAMBookFigure21()
{
    std::cout << "testWAMBookFigure21() -----------------------\n";
    std::cout << "(wambook.sourceforge.net, page 11)\n";

    // Ok, I know the offset is +1 (if you compare it with WAM book)
    // but I reserve 0 to be the NULL pointer as I store relative pointers
    // and 0 has this special meaning. (Close enough!)

    Heap heap;

    ConstRef h2 = heap.getConst("h", 2);
    ConstRef f1 = heap.getConst("f", 1);
    ConstRef p3 = heap.getConst("p", 3);

    HeapRef origin = heap.top();

    heap.newStr(heap.top() + 1);
    heap.newCon(h2);
    heap.newRef();
    heap.newRef();
    heap.newStr(heap.top() + 1);
    heap.newCon(f1);
    heap.newRef(origin + 3);
    heap.newStr(heap.top() + 1);
    heap.newCon(p3);
    heap.newRef(origin + 2);
    heap.newStr(origin + 1);
    heap.newStr(origin + 5);

    heap.printRaw(std::cout);

    std::string asList = heap.toRawString(origin, heap.top()-1);
    std::cout << "AS LIST: " << asList << "\n";

    assert(asList == "[STR:4, CON:h, REF:5, REF:6, STR:8, CON:f, REF:6, STR:11, CON:p, REF:5, STR:4, STR:8]");

    std::cout << "AS TERM : ";
    heap.print(std::cout, origin + 7);
    std::cout << "\n";

    const std::string expected = "p(A, h(A, B), f(B))";
    std::cout << "EXPECTED: "<< expected << "\n";

    std::string got = heap.toString(origin + 7);
    std::cout << "GOT     : " << got << "\n";

    assert(expected == got);
}

static size_t myRand(size_t bound)
{
    static uint64_t state = 4711;

    if (bound == 0) {
	state = 4711;
	return 0;
    }
    
    state = 13*state + 734672631;

    return state % bound;
}

HeapRef newTerm(Heap &heap, size_t maxDepth, size_t depth = 0)
{
    size_t arity = (depth >= maxDepth) ? 0 : myRand(6);
    char functorName[2];
    functorName[0] = 'a' + (char)arity;
    functorName[1] = '\0';
    ConstRef functor = heap.getConst(functorName, arity);
    HeapRef str = heap.newStr(heap.top()+1);
    heap.newCon(functor);
    if (arity > 0) {
	Cell *args = heap.allocate(arity);
	for (size_t j = 0; j < arity; j++) {
	    HeapRef arg = newTerm(heap, maxDepth, depth+1);
	    args[j] = heap.getCell(arg);
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

    HeapRef term = newTerm(heap, DEPTH);
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
	std::cout << str;
	std::cout << "OK\n";
    }
    heap.printStatus(std::cout);
    std::cout << "\n";

    //    heap.printRoots(std::cout);

    assert(!err);
}

static void testParse(Heap &heap,
		      const std::string &input,
		      const std::string &expect)
{
    std::cout << "INPUT : " << input << "\n";
    std::cout << "EXPECT: " << expect << "\n";

    std::istringstream is(input);
    is >> std::noskipws;
    HeapRef hr = heap.parse(is);

    std::string result = heap.toString(hr);

    std::cout << "PARSED: " << result << "\n";

    assert(result == expect);
}

void testParse()
{
    printf("-------- testParse() -----------------------\n");

    Heap heap;

    std::string input, expect;

    std::cout << "Standard...\n";
    input = "foo( bar( a, b), c, baz(blag(dd),xy),ef )";
    expect = "foo(bar(a, b), c, baz(blag(dd), xy), ef)";
    testParse(heap, input, expect);

    std::cout << "Negative...\n";
    input = "foo( bar( a, b), c, ";
    expect = "$parseError('TERM ended too early', 0, 20)";
    testParse(heap, input, expect);

    input = "foo( bar(a, ), c)";
    expect = "$parseError('Expecting TERM but got \\\')\\\'', 0, 12)";
    testParse(heap, input, expect);
}

void testParseBigTerm()
{
    printf("-------- testParseBigTerm() ----------------\n");

    const size_t DEPTH = 5;

    myRand(0); // Reset random generator

    // First create big term (same as before)
    Heap heap;

    std::cout << "Create big term...\n";
    HeapRef term = newTerm(heap, DEPTH);

    // Print it to a string
    std::cout << "Print term to a string...\n";
    PrintParam param;
    param.setMaxWidth(78-param.getStartColumn());
    std::stringstream ss;
    ss << std::noskipws;
    heap.print(ss, term, param);
    ss << "\n";

    // Now parse it
    std::cout << "Parse it...\n";
    std::istringstream is(ss.str());
    is >> std::noskipws;
    HeapRef parsed = heap.parse(is);

    // Print it again to a string
    std::cout << "Print it again...\n";
    std::stringstream ss2;
    ss2 << std::noskipws;
    heap.print(ss2, parsed, param);
    ss2 << "\n";

    // ss and ss2 should be identical
    std::cout << "Compare strings...\n";
    if (ss.str() != ss2.str()) {
	std::cout << "String are inequal!\n";
	std::cout << "GOT:\n";
	std::cout << ss2.str() << "\n";
    }
    assert(ss.str() == ss2.str());
}

void testUnify1()
{
    printf("-------- testUnify1() ----------------------\n");

    Heap heap;

    std::istringstream is1("foo(X, Y, bar(X, Z))");
    is1 >> std::noskipws;
    std::istringstream is2("foo(A, B, bar(q, B))");
    is2 >> std::noskipws;

    HeapRef term1 = heap.parse(is1);
    HeapRef term2 = heap.parse(is2);

    std::cout << "Term 1: " << heap.toString(term1) << "\n";
    std::cout << "Term 2: " << heap.toString(term2) << "\n";

    std::cout << "Unify : " << heap.unify(term1, term2) << "\n";
    std::string result1 = heap.toString(term1);
    std::string result2 = heap.toString(term2);
    std::cout << "Term 1: " << result1 << "\n";
    std::cout << "Term 2: " << result2 << "\n";

    assert(result1 == "foo(q, B, bar(q, B))");
    assert(result1 == result2);
}

void testUnify2()
{
    printf("-------- testUnify2() ----------------------\n");

    Heap heap;

    std::istringstream is1("foo(X, Y, bar(gugga, Z))");
    is1 >> std::noskipws;
    std::istringstream is2("foo(A, B, bar(q, B))");
    is2 >> std::noskipws;

    HeapRef term1 = heap.parse(is1);
    HeapRef term2 = heap.parse(is2);

    std::cout << "Term 1: " << heap.toString(term1) << "\n";
    std::cout << "Term 2: " << heap.toString(term2) << "\n";

    size_t heapSz = heap.getSize();
    size_t stackSz = heap.getStackSize();

    std::cout << "HeapSz: " << heapSz << " StackSz: " << stackSz << "\n";

    bool r = heap.unify(term1, term2);
    std::cout << "Unify : " << r << "\n";

    size_t heapSz1 = heap.getSize();
    size_t stackSz1 = heap.getStackSize();
    std::cout << "HeapSz: " << heapSz1 << " StackSz: " << stackSz1 << "\n";

    assert(heapSz == heapSz1);
    assert(stackSz == stackSz1);

    assert(!r);

    std::string result1 = heap.toString(term1);
    std::string result2 = heap.toString(term2);
    std::cout << "Term 1: " << result1 << "\n";
    std::cout << "Term 2: " << result2 << "\n";

    assert(result1 == "foo(A, B, bar(gugga, C))");
    assert(result2 == "foo(D, E, bar(q, E))");
}

HeapRef generalizeTerm(Heap &heap, HeapRef term, int p)
{
    if (myRand(100) < p) {
	return heap.newRef();
    }

    HeapRef href = heap.deref(term);
    Cell c = heap.getCell(href);
    if (c.getTag() == Cell::STR) {
	HeapRef functorRef = heap.getFunctorRef(href);
	HeapRef newFunctor = heap.newArgs(heap.getCell(functorRef));
	size_t arity = heap.getArity(newFunctor);
	for (size_t i = 0; i < arity; i++) {
	    HeapRef newArg = generalizeTerm(heap,heap.getArgRef(functorRef,i),p);
	    heap.setArg(newFunctor, i, heap.getCell(newArg));
	}
	return heap.newStr(newFunctor);
    } else {
	return term;
    }
}

void testUnifyBig()
{
    printf("-------- testUnifyBig() --------------------\n");

    const size_t DEPTH = 5;

    myRand(0); // Reset random generator

    // First create big term (same as before)
    Heap heap;

    std::cout << "Create big term...\n";
    HeapRef term = newTerm(heap, DEPTH);

    PrintParam param;
    param.setMaxWidth(78-param.getStartColumn());

    std::stringstream ss;
    heap.print(ss, term, param);
    std::cout << " TERM:\n";
    std::cout << ss.str() << "\n";

    std::cout << "GTERM:\n";
    HeapRef gTerm = generalizeTerm(heap, term, 10);
    std::stringstream ss2;
    heap.print(ss2, gTerm, param);
    std::cout << ss2.str() << "\n";

    std::cout << "HTERM:\n";
    HeapRef hTerm = generalizeTerm(heap, term, 10);
    std::stringstream ss3;
    heap.print(ss3, hTerm, param);
    std::cout << ss3.str() << "\n";

    // Let's unify all terms together. It must succeed!
    std::cout << "Unify GTERM and HTERM\n";
    assert(heap.unify(gTerm, hTerm));
    std::cout << "GHTERM:\n";
    std::stringstream ss4;
    heap.print(ss4, gTerm, param);
    std::cout << ss4.str() << "\n";
    std::stringstream ss5;
    heap.print(ss5, hTerm, param);
    std::cout << ss5.str() << "\n";
    assert(ss4.str() == ss5.str());

    // And finally unify with original term
    bool r;
    std::cout << "Unify with TERM: " << (r = heap.unify(term, gTerm)) << "\n";
    assert(r);

    std::stringstream ss6;
    heap.print(ss6, hTerm, param);
    std::cout << ss6.str() << "\n";

    // And we should be back to original term.
    assert(ss.str() == ss6.str());

    // Print heap status
    heap.printStatus(std::cout);
    std::cout << "\n";
}

int main(int argc, char *argv[] )
{
    testConst();
    testWAMBookFigure21();
    testBigTerm();
    testParse();
    testParseBigTerm();
    testUnify1();
    // Run same test again, to ensure that clearing ref hash map works.
    testUnify1();
    testUnify2();
    testUnifyBig();

    return 0;
}
