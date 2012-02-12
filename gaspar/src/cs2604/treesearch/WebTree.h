#ifndef _WEBTREE_H_
#define _WEBTREE_H_

#include <string>

#include "TreeNode.cpp"
#include "MinHeap.cpp"

using namespace std;

class WebTree {
	// WebTree class.  Holds tree of
	// TreeNodes and a heap for the
	// webcrawling.
	
	private:

		// Holdes the tree
		TreeNode *head;

		// Number of elements in the tree
		int size;

		// The heap!
		MinHeap<TreeNode> *heap;

		// Output file
		ofstream output;

		// Initializing functions
		void parseInput(string i);
		void buildHeap(TreeNode *cur);

	public:
		
		// Constructor
		WebTree(string i, string o);

		// Destructor
		~WebTree();

		// Program 1
		void webcrawl();

		// Program 2
		void startSearch(string searchFile);

//		void print();
};

#endif
