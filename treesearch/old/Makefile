# Alpha Chen

websearch: main.o
	g++ main.o -o websearch

main.o: WebSearch.o main.cpp
	g++ -c main.cpp

WebSearch.o: LinkList.o WebSearch.cpp WebSearch.h
	g++ -c WebSearch.cpp

LinkList.o: Node.h LinkList.cpp LinkList.h
	g++ -c LinkList.cpp

clean:
	rm -f *.o
	rm -f .*.swp
	rm -f websearch
