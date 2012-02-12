#ifndef _TREENODE_H_
#define _TREENODE_H_

template <typename T> class TreeNode {
	// A TreeNode class for the binTree class 
	// to use.  It has rudimentary capabilities
	// thus far.
	//
	// To add:
	// 	- TreeNode *previous ?
	// 		(getPrevious, setPrevious
	// 		 naturally follow)
	
	private:
		T* data;	// Stores the data of type T
        TreeNode *parent;	// Points at the parent Node
		LinkList<TreeNode *> children;	// The children of the current Node

		void print(int level);	// Helper function for print
		
	public:
		// Constructor
		TreeNode(T *d=0, TreeNode *p=0) { data = d; parent = p; };

		// Information retrieval functions
		T* get() { return data; }
		TreeNode *getParent() { return parent; }

		// Add a child
		void addChild(T *c);
		
		// Does a search in the tree
		void search(string s);

		// Prints entire tree, preorder
		void print();
};

#endif
