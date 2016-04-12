#include <iostream>
#include <assert.h>
#include <vector>
#include <map>
#include "../Term.hpp"
#include "../Hash.hpp"

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

    HeapRef origin = heap.topHeapRef();

    heap.newStr(heap.topHeapRef() + 1);
    heap.newConOnHeap(h2);
    heap.newRef();
    heap.newRef();
    heap.newStr(heap.topHeapRef() + 1);
    heap.newConOnHeap(f1);
    heap.newRef(origin + 3);
    heap.newStr(heap.topHeapRef() + 1);
    heap.newConOnHeap(p3);
    heap.newRef(origin + 2);
    heap.newStr(origin + 1);
    heap.newStr(origin + 5);

    heap.printRaw(std::cout);

    std::string asList = heap.toRawString(origin, heap.topHeapRef());
    std::cout << "AS LIST: " << asList << "\n";

    assert(asList == "[STR:2, CON:h/2, REF:3, REF:4, STR:6, CON:f/1, REF:4, STR:9, CON:p/3, REF:3, STR:2, STR:6]");

    std::cout << "AS TERM : ";
    heap.print(std::cout, heap.getCell(origin + 7));
    std::cout << "\n";

    const std::string expected = "p(A, h(A, B), f(B))";
    std::cout << "EXPECTED: "<< expected << "\n";

    std::string got = heap.toString(heap.getCell(origin + 7));
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

CellRef newTerm(Heap &heap, size_t maxDepth, size_t depth = 0)
{
    size_t arity = (depth >= maxDepth) ? 0 : myRand(6);
    char functorName[2];
    functorName[0] = 'a' + (char)arity;
    functorName[1] = '\0';
    ConstRef functor = heap.getConst(functorName, arity);
    std::vector<CellRef> args(arity);
    for (size_t j = 0; j < arity; j++) {
	args[j] = newTerm(heap, maxDepth, depth+1);
    }
    CellRef str = heap.newStr(functor);
    for (size_t j = 0; j < arity; j++) {
	heap.setArg(str, j, args[j]);
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

    CellRef term = newTerm(heap, DEPTH);
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
    CellRef cellRef = heap.parse(is);

    std::string result = heap.toString(cellRef);

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
    CellRef term = newTerm(heap, DEPTH);

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
    CellRef parsed = heap.parse(is);

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

    CellRef term1 = heap.parse(is1);
    CellRef term2 = heap.parse(is2);
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

    CellRef term1 = heap.parse(is1);
    CellRef term2 = heap.parse(is2);

    std::cout << "Term 1: " << heap.toString(term1) << "\n";
    std::cout << "Term 2: " << heap.toString(term2) << "\n";

    size_t heapSz = heap.getHeapSize();
    size_t stackSz = heap.getStackSize();

    std::cout << "HeapSz: " << heapSz << " StackSz: " << stackSz << "\n";

    bool r = heap.unify(term1, term2);
    std::cout << "Unify : " << r << "\n";

    size_t heapSz1 = heap.getHeapSize();
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

CellRef generalizeTerm(Heap &heap, CellRef term, int p, bool lotsOfForward)
{
    if (myRand(100) < p) {
	return heap.newRef();
    }

    CellRef cellRef = heap.deref(term);
    if (heap.getTag(cellRef) == Cell::STR) {
	size_t arity = heap.getArity(cellRef);
	std::vector<CellRef> args(arity);
	CellRef newStr;
	if (lotsOfForward) {
	    newStr = heap.newStr(heap.getFunctorConst(cellRef));
	}
	for (size_t i = 0; i < arity; i++) {
	    CellRef newArg = generalizeTerm(heap,heap.getArg(cellRef,i),
					    p,lotsOfForward);
	    args[i] = newArg;
	}
	if (!lotsOfForward) {
	    newStr = heap.newStr(heap.getFunctorConst(cellRef));
	}
	for (size_t i = 0; i < arity; i++) {
	    heap.setArg(newStr, i, args[i]);
	}
	return newStr;
    } else {
	return term;
    }
}

namespace PROJECT {

class HeapTest {
public:
    HeapTest(Heap &heap) : thisHeap(heap) { }

    BitMap & getForwardPointers() { return thisHeap.thisForwardPointers; }

private:
    Heap &thisHeap;
};

}

void testGC()
{
    // An explicit GC test

    Heap heap;
    heap.setStrict(true); // Will clear top of heap after GC
    
    // Create 11 cells of garbage
    // We put this into a scope so that there are no live references
    // to it (otherwise it will not be garbage.)
    {
	const size_t TRASH_ARITY = 10;
	CellRef trash = heap.newStr(heap.getConst("foo", TRASH_ARITY));
	for (size_t i = 0; i < TRASH_ARITY; i++) {
	    heap.setArg(trash, i, heap.newConst("nothing"));
	}
    }

    CellRef middle; // This is the only reference that will survive
    {
    // Create a small term above1(Q)
    // This will have location [12]: CON:above1/1
    //                         [13]: REF:13
    CellRef above1 = heap.newTerm(heap.getConst("above1", 1));

    // Create another small term above2(R)
    // This will have location [14]: CON:above/1
    //                         [15]: REF:15
    CellRef above2 = heap.newTerm(heap.getConst("above2", 1));

    // Create a term with 3 args
    // This will have location [14]: CON:middle/3
    //                         [15]: REF:15
    //                         [16]: REF:16
    //                         [17]: REF:17
    middle = heap.newTerm(heap.getConst("middle", 3));

    // Create a small term below(W)
    // This will have location [18]: CON:below/1
    //                         [19]: REF:19
    CellRef below = heap.newTerm(heap.getConst("below", 1));

    // Now we'll create a forward pointer from above1(Q) to point at middle/3
    heap.unify(heap.getArg(above1,0), middle);

    // Then we'll create a back pointer to above1(Q) from middle(A,B,C)
    // So that B = above1(Q).
    heap.unify(heap.getArg(middle,1), above1);

    // Create another back pointer to above2(R) from middle(A,B,C)
    // So that C = above2(Q). (above2 will not have a forward pointer
    // to middle)
    heap.unify(heap.getArg(middle,2), above2);

    // Then finally a back pointer from below(W) to middle/3
    heap.unify(heap.getArg(below,0), middle);
    }

    heap.printRaw(std::cout);

    // Now check that we have precisely one forward pointer.
    HeapTest heapTest(heap);
    BitMap &fwd = heapTest.getForwardPointers();
    size_t n = fwd.getSize();
    std::cout << "Number of bits for forward pointers: " << n << "\n";
    int cnt = 0;
    CellRef cell;
    for (size_t index = 0; index < n;) {
	index = fwd.findBit(index, n);
	if (index < n) {
	    std::cout << "Found at : [" << index << "]: ";
	    cell = heap.getCell(HeapRef(index));
	    heap.printCell(std::cout, cell);
	    std::cout << "\n";
	    cnt++;
	}
	index++;
    }
    std::cout << "Expecting 1 forward pointer: got=" << cnt << "\n";
    assert(cnt == 1);
    std::cout << "Forward pointer should be REF:13 and got: ";
    heap.printCell(std::cout, cell);
    std::cout << "\n";
    assert(cell->getTag() == Cell::REF);
    assert(heap.toHeapRef(cell) == HeapRef(13));
    std::cout << ">>> Do a full GC --------------------------------\n";

    // Now let's invoke a full GC
    heap.gc(1.0, 3);

    std::cout << "Another scan for forward pointers:\n";
    cnt = 0;
    for (size_t index = 0; index < n;) {
	index = fwd.findBit(index, n);
	if (index < n) {
	    std::cout << "Found at : [" << index << "]: ";
	    if (index == 0) {
		std::cout << "NULL";
	    } else {
		cell = heap.getCell(HeapRef(index));
		heap.printCell(std::cout, cell);
	    }
	    std::cout << "\n";
	    cnt++;
	}
	index++;
    }    
    std::cout << "<<< Done\n";

    // Print heap again
    heap.printRaw(std::cout);

    // Size of heap should now be 9 cells
    std::cout << "Size of heap: " << heap.getHeapSize() << " (expecting 9)\n";
    assert(heap.getHeapSize() == 9);
}

void testUnifyTerms(double gcFactor, size_t depth, bool moreForwards,
		    int verbosity = 1)
{
    std::cout << "-------- testUnifyTerms(gcFactor=" << gcFactor << ",depth=" << depth << ",moreForwards=" << moreForwards << ") --------------------\n";

    Heap heap;
    heap.setStrict(true);

    CellRef hTerm;

    PrintParam param;
    param.setMaxWidth(78-param.getStartColumn());

    // Declared outside so we can do comparison outside of below scope
    std::stringstream ss6;

    {
    myRand(0); // Reset random generator

    // First create big term (same as before)
    std::cout << "Create term with depth=" << depth << "...\n";
    CellRef term = newTerm(heap, depth);

    std::stringstream ss;
    heap.print(ss, term, param);
    if (verbosity > 1) {
	std::cout << "TERM: " << ss.str() << "\n";
    }

    CellRef gTerm = generalizeTerm(heap, term, 10, false);
    std::stringstream ss2;
    heap.print(ss2, gTerm, param);
    if (verbosity > 1) {
	std::cout << "GTERM: " << ss2.str() << "\n";
    }

    hTerm = generalizeTerm(heap, term, 10, moreForwards);
    std::stringstream ss3;
    heap.print(ss3, hTerm, param);
    if (verbosity > 1) {
	std::cout << "HTERM: " << ss3.str() << "\n";
    }

    // Let's unify all terms together. It must succeed!
    std::cout << "Unify GTERM and HTERM\n";
    assert(heap.unify(gTerm, hTerm));
    std::stringstream ss4;
    heap.print(ss4, gTerm, param);
    if (verbosity > 1) {
	std::cout << "GTERM: " << ss4.str() << "\n";
    }
    std::stringstream ss5;
    heap.print(ss5, hTerm, param);
    if (verbosity > 1) {
	std::cout << "HTERM: " << ss5.str() << "\n";
    }
    assert(ss4.str() == ss5.str());

    // And finally unify with original term
    bool r;
    std::cout << "Unify with TERM: " << (r = heap.unify(term, gTerm)) << "\n";
    assert(r);

    heap.print(ss6, hTerm, param);
    if (verbosity > 1) {
	std::cout << ss6.str() << "\n";
    }

    // And we should be back to original term.
    assert(ss.str() == ss6.str());
    } // Only reference to hTerm should survive

    // Create a new random reference to this term.
    CellRef outer = heap.newStr(heap.toHeapRef(*heap.deref(hTerm)));
    CellRef newVar = heap.newRef();
    heap.unify(newVar, outer);

    // Print heap status
    heap.printStatus(std::cout, verbosity - 1);

    std::cout << "First gc\n";
    heap.gc(gcFactor, verbosity);

    heap.print(std::cout, hTerm, param);
    std::cout << "\n";

    std::cout << "Second gc\n";
    heap.gc(gcFactor, verbosity);

    std::stringstream ss7;
    heap.print(ss7, hTerm, param);
    if (verbosity > 1 || (ss7.str() != ss6.str())) {
	std::cout << "Term after GC\n" << ss7.str() << "\n";
    }

    std::cout << "Compare that they equal after GC: " << (ss7.str() == ss6.str()) << "\n";
    assert(ss7.str() == ss6.str());
}




//
// HAMT thoughts
//
// So how to dynamically grow the tree?
//
// What we have at the leaves is a union that we need to split
// up. How do we find what keys to remove from a leaf?
//
// Should be possible

static void printIt(CellRef obj, const char *label)
{
    const Heap &heap = obj.getHeap();
    PrintParam param;
    param.setMaxWidth(78-param.getStartColumn());
    if (label != NULL) {
	std::cout << label;
    }
    heap.print(std::cout, obj, param);
    std::cout << "\n";
}

void testAssocList()
{
    std::cout << "-------- testAssocList() -------------------\n";

    Heap heap;
    
    std::string s;

    CellRef lst = heap.newList();

    std::cout << "List: " << (s = heap.toString(lst)) << "\n";
    assert(s == "[]");

    lst = heap.assocListAdd(lst, heap.newConst("foo"), heap.newConst("bar"));

    std::cout << "List: " << (s = heap.toString(lst)) << "\n";
    assert(s == "[foo:bar]");

    lst = heap.assocListAdd(lst, heap.newConst("xyz"), heap.newConst("abc"));

    std::cout << "List: " << (s = heap.toString(lst)) << "\n";
    assert(s == "[xyz:abc, foo:bar]");

    lst = heap.assocListReplace(lst, heap.newConst("foo"), heap.newConst("xxx"));

    std::cout << "List: " << (s = heap.toString(lst)) << "\n";
    assert(s == "[xyz:abc, foo:xxx]");

    lst = heap.assocListAdd(lst, heap.newConst("gug"), heap.newConst("yep"));

    std::cout << "List: " << (s = heap.toString(lst)) << "\n";
    assert(s == "[gug:yep, xyz:abc, foo:xxx]");

    lst = heap.assocListRemove(lst, heap.newConst("xyz"));

    std::cout << "List: " << (s = heap.toString(lst)) << "\n";
    assert(s == "[gug:yep, foo:xxx]");
}

static std::string toString(std::map<std::string, std::string> &map)
{
    std::stringstream ss;
    ss << "[";
    bool first = true;
    for (std::map<std::string, std::string>::iterator it = map.begin();
	 it != map.end(); ++it) {
	if (!first) ss << ", ";
	ss << it->first << ":" << it->second;
	first = false;
    }
    ss << "]";
    return ss.str();
}

static void normalizeString(std::string &s)
{
    size_t n = s.length();
    size_t j = 0;
    bool lastSpace = false;
    for (size_t i = 0; i < n; i++) {
	if (isspace(s[i])) {
	    if (lastSpace) {
		continue;
	    }
	    lastSpace = true;
	}
	s[j] = s[i];
	j++;
    }
    s.resize(j);
}

void testMap1()
{
    std::cout << "-------- testMap1() ------------------------\n";

    Heap heap;

    CellRef map = heap.newMap(5);

    std::cout << "Put foo:bar\n";
    map = heap.putMap(map, heap.newConst("foo"), heap.newConst("bar"));

    CellRef r = heap.getMap(map, heap.newConst("foo"));
    std::cout << "Find foo: " << heap.toString(r) << "\n";
    assert(heap.toString(r) == "bar");
    r = heap.getMap(map, heap.newConst("bar"));
    std::cout << "Find bar: " << heap.toString(r) << "\n";
    assert(heap.toString(r) == "null");

    std::cout << "Put xyz:abc\n";
    map = heap.putMap(map, heap.newConst("xyz"), heap.newConst("abc"));

    r = heap.getMap(map, heap.newConst("foo"));
    std::cout << "Find foo: " << heap.toString(r) << "\n";
    assert(heap.toString(r) == "bar");    

    r = heap.getMap(map, heap.newConst("xyz"));
    std::cout << "Find xyz: " << heap.toString(r) << "\n";
    assert(heap.toString(r) == "abc");
}

void testMap2()
{
    std::cout << "-------- testMap2() ------------------------\n";

    std::map<std::string, std::string> refMap;

    Heap heap;

    CellRef map = heap.newMap(2);
    for (size_t i = 0; i < 100; i++) {
	char key[32];
	char val[32];
	sprintf(key, "key%lu", myRand(100));
	sprintf(val, "val%lu", myRand(100));
	ConstRef keyConst = heap.getConst(key, 0);
	ConstRef valConst = heap.getConst(val, 0);

	CellRef keyCell = heap.newConst(keyConst);
	CellRef valCell = heap.newConst(valConst);

	std::cout << "Adding " << key << ":" << val << "\n";

	map = heap.putMap(map, keyCell, valCell);
	refMap[key] = val;

	CellRef mapList = heap.qsortList(heap.mapAsList(map));
	std::string refMapStr = toString(refMap);
	std::string heapMapStr = heap.toString(mapList);
	normalizeString(refMapStr);
	normalizeString(heapMapStr);

	if (refMapStr != heapMapStr) {
	    std::cout << "REF: " << refMapStr << "\n";
	    std::cout << "MAP: " << heapMapStr << "\n";
	}

	assert(refMapStr == heapMapStr);
    }

    std::cout << ">> DONE ---\n";
    printIt(map, "MAP: ");
    printIt(heap.mapAsList(map), "LST: ");
}

int main(int argc, char *argv[] )
{
    std::cout << "TestTerm::main() **************************************\n";

    testConst();
    testWAMBookFigure21();
    testBigTerm();
    testParse();
    testParseBigTerm();
    testUnify1();
    // Run same test again, to ensure that clearing ref hash map works.
    testUnify1();
    testUnify2();

    testGC();

    testUnifyTerms(0.4, 5, true, 1);
    testUnifyTerms(0.8, 5, true, 1);
    testUnifyTerms(1.0, 5, true, 1);
    testUnifyTerms(0.4, 5, false, 1);
    testUnifyTerms(0.8, 5, false, 1);
    testUnifyTerms(1.0, 5, false, 1);
    
    testAssocList();
    testMap1();
    testMap2();

    return 0;
}
