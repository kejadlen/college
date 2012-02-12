#ifndef _HUFFMAN_H_
#define _HUFFMAN_H_

#include "HuffNode.h"

class Huffman {
	// Class for managing a huffman tree, for both encoding
	// and decoding, as well as producing a frequency file
	
	private:
		HuffList *huffList; // Holds the huffman linked list/tree
		HuffNode *huffHead;		// Holds the huffman tree (for decoding)
		int charChange[47]; // Useful for the special characters
		
		// Helper functions
		
		// buildTreeFile:
		string buildTreeFile(HuffNode *n);
		// buildCodeFile:
		void getCodes(HuffNode *h, string a[30], string c);
		// encodeFile
		void buildCodeArray(string c, string a[30]);
		// decodeFile
		void buildTreeFromFile(string t);
		string getCode(char c);

	public:
		Huffman();
		
		// Build the huffman tree from a frequency file
		void buildTree(string f);

		// Output functions
		void buildTreeFile(string o);
		void buildCodeFile(string c);

		// Encoding and decoding functions
		void encodeFile(string c, string m, string e);
		void decodeFile(string t, string e, string m);

		void print() { huffList->head->print(); }
		
		~Huffman();
};

#endif
