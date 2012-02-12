#ifndef _WEBSEARCH_H_
#define _WEBSEARCH_H_

#include <string>
#include <fstream>

#include "LinkList.cpp"

using namespace std;

class KeywordNode {
	// The KeywordNode class is basically a
	// struct to use to keep track of keywords
	// and the respective webpages which are
	// linked to them.

	private:
		string keyword; // Stores the keyword itself
		LinkList<Node<string>*> *webpages; // A pointer to the related webpages
		
	public:
		// Constructor
		KeywordNode(string k="") { keyword = k; webpages = new LinkList<Node<string>*>; };

		// Set the keyword
		void setKeyword(string k) { keyword = k; };
		
		// Add to the linked list
		void add(Node<string> *p) { webpages->add(p); };

		// Return the keyword
		string getKeyword() { return keyword; };
		// Returns the size of the list of pointers
		// (the number of pages related to the keyword)
		int getSize() { return webpages->getSize(); }
		// Return the pointer to the webpages
		LinkList<Node<string>*> *getWebpages() { return webpages; };

		// Destructor
		~KeywordNode() { delete webpages; }
};

class WebSearch {
	// The WebSearch class takes an input of
	// three files: a list of webpages and the
	// keywords which are related to them, a
	// list of keywords to run searches for, 
	// and an output file.
	//
	// The keywords are kept in a sorted
	// array, and the binary search algorithm
	// is used to run searches.
	
	private:
		KeywordNode *keywords[1000]; // Array of pointers
									 // to KeywordNodes
		int numKeywords; // Number of keywords in array
		LinkList<string> *webpages; // List of /all/ the webpages
		string search; // File which holds keywords to
					   // be searched for
		ofstream output; // Output file

		int lookups; // Number of lookups made in the last
					 // call to keywordSearch
		
	public:
		// Constructor: the strings hold the filenames
		WebSearch(string i="", string s="", string o="");

		// Utility functions
		// Parses the input, creates the keyword array
		// and webpage linked list
		void parseInput(string i);
		// Adds a URL into the webpages linked list
		void addURL(string u);
		// Adds a keyword into the keyword array
		int addKeyword(string k);
		// Makes the link between a keyword and the 
		// webpages which should be returned
		void linkKeywordPage(string k, string w);

		// Runs the search terms from the search file
		void runSearch();
		// Outputs the results for a particular keyword
		void printPages(int p);
		
		// Searches for a keyword to see if it exists
		int keywordSearch(string k);

		// Destructor
		~WebSearch();
};

#endif
