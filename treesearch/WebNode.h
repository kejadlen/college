#ifndef _WEBNODE_H_
#define _WEBNODE_H_

#include <string>

#include "LinkList.cpp"

using namespace std;

class WebNode {
	// A WebNode class to store information about
	// a URL.
	
	private:
		string URL;
		int rank;
		LinkList<string> keywords;
		
	public:
		// Constructor
		WebNode(string u, int r, string k);

		// Output
		string getURL() { return URL; };
		int getRank() { return rank; };
		
		// Useful for debugging
		void print();
};

#endif
