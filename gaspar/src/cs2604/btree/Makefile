# Alpha Chen

build: btree

btree: btree.o
	g++ btree.o -o btree

btree.o: StudentDB.o btree.cpp
	g++ -c btree.cpp

StudentDB.o: BTree.o LinkList.o StudentDB.cpp
	g++ -c StudentDB.cpp

BTree.o: LinkList.o BTree.cpp
	g++ -c BTree.cpp

Linklist.o: Node.o LinkList.cpp
	g++ -c LinkList.cpp

Node.o: Node.h
	g++ -c Node.h

clean:
	rm -f *.o
	rm -f .*.swp
	rm -f btree
