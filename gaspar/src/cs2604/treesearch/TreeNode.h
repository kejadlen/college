#ifndef _TREENODE_H_
#define _TREENODE_H_

#include <string>

#include "LinkList.cpp"

using namespace std;

class TreeNode {
	// This class stores information about
	// a webpage in a tree form.
	
	private:
		// Webpage information
		string url;
		int rank;
		int line;
		LinkList<string> keywords;

		// Parent TreeNode
		TreeNode* parent;

		// Add keywords from a string
		void addKeywords(string k);

	public:
		// Constructor
		TreeNode(string u, int r, int l, string k, TreeNode *p);
	
		// Children
		LinkList<TreeNode*> children;

		// Return the parent
		TreeNode* getParent();

		// Add a child
		void add(TreeNode *temp);

		// return the URL;
		string getURL();

		// Since ewbpages "inherit" keywords,
		// this function fixes the keywords
		void updateKeywords();
		
		// Return a string of keywords
		string getKeywords();

		// Searches the keywords for string s
		int search(string s);
		
/*		void print();
		void print(int level);
		void print(string search);
		void print(string search, int level);*/

		// Printing and helping function.
		string print(string search1, string search2, int flag);
		string print(string search1, string search2, int flag, int level);
		
		// Deconstructor
		~TreeNode();

		// Less than operator -- for MinHeap
		bool operator<(const TreeNode& rhs);
};

#endif
