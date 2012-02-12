# Alpha Chen

build: webcrawl1 webcrawl2

webcrawl1: webcrawl1.o
	g++ webcrawl1.o -o webcrawl1

webcrawl2: webcrawl2.o
	g++ webcrawl2.o -o webcrawl2

webcrawl1.o: WebTree.o webcrawl1.cpp
	g++ -c webcrawl1.cpp

webcrawl2.o: WebTree.o webcrawl2.cpp
	g++ -c webcrawl2.cpp

WebTree.o: TreeNode.o MinHeap.o
	g++ -c WebTree.cpp

TreeNode.o: LinkList.o
	g++ -c TreeNode.cpp

MinHeap.o:
	g++ -c MinHeap.cpp

LinkList.o: Node.h LinkList.cpp LinkList.h
	g++ -c LinkList.cpp

clean:
	rm -f *.o
	rm -f .*.swp
	rm -f webcrawl1
	rm -f webcrawl2
