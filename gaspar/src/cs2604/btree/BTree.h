#ifndef _BTREE_H_
#define _BTREE_H_

class BTreeNode {
	// Solely for utility purposes for this
	// specific application, makes it 
	// easier to handle nodes
	
	public:
		int num;	// If internal node, num
					// is the number of children
					// If external node, num
					// is the negative number of
					// keys
		string *data;	// Holds the data
		long *dataPtr;	// Holds the positions of
						// the data
		long *childPtr; // Holds the positions of
						// the children

		BTreeNode() {
			num = 0;
			data = NULL;
			dataPtr = NULL;
			childPtr = NULL;
		}
		
		BTreeNode(int m, int n) {
			// Constructor
			num = n;
			data = new string[m];
			dataPtr = new long[m];
			childPtr = new long[m+1];
		}

		void print() {
			// For checking purposes.  Not used in
			// the actual program
			int i, n = (num > 0) ? num-1 : -num ;
			cout<<"num: "<<num<<endl;
			for(i=0; i<n; i++) {
				cout<<childPtr[i]<<endl;
				cout<<data[i]<<": "<<dataPtr[i]<<endl;
			}
			cout<<childPtr[i]<<endl<<endl;
		}
		
		~BTreeNode() {
			// Destructor
			delete[] data;
			delete[] dataPtr;
			delete[] childPtr;
		}
};

class BTree {
	// BTree class to handle BTrees
	// Uses a file for the index, storing
	// the templated data
	
	private:
		int m;	// Order of the BTree

		int length;	// Length of the data
		
		fstream indexFile;	// The index file

		// Functions to help insert elements
		long addHelp(long lPtr, string data, long dataPos, long rPtr, long place, long splits);
		long split(BTreeNode *data, long place, long splits);
		long findParent(long pos, long place);

		// Reading/writing to the index file
		void getData(BTreeNode *data, long n);
		void writeData(BTreeNode *data, long n=0);

	public:
		// Constructor
		BTree(int mT, int l, string fileName);
		
		// Necessary functions
		long add(string data, long dataPos, long place);
		void find(LinkList<long> &data, string d, long place);
		void print(LinkList<long> &data, long place);

		// Outputs the number of nodes in the tree
		long numNodes();
		
		~BTree();
};

#endif
