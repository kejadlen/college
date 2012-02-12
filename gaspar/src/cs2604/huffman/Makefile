# Alpha Chen

build: buildtree encode decode

buildtree: buildtree.o
	g++ buildtree.o -o buildtree

encode: encode.o
	g++ encode.o -o encode

decode: decode.o
	g++ decode.o -o decode

buildtree.o: Huffman.o buildtree.cpp
	g++ -c buildtree.cpp

encode.o: Huffman.o encode.cpp
	g++ -c encode.cpp

decode.o: Huffman.o decode.cpp
	g++ -c decode.cpp

Huffman.o: HuffNode.h Huffman.cpp Huffman.h
	g++ -c Huffman.cpp

clean:
	rm -f *.o
	rm -f .*.swp
	rm -f buildtree
	rm -f encode
	rm -f decode
