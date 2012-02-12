#ifndef _WEBSEARCH_CPP_
#define _WEBSEARCH_CPP_

#include <iostream>
#include <string>
#include <fstream>

#include "WebSearch.h"

using namespace std;

WebSearch::WebSearch(string i, string s, string o) {
	// Constructor
	
	// Set the array to all NULL pointers
	for(int j=0; j<1000 ;j++) keywords[j] = NULL;
	numKeywords = 0; // No keywords in the array yet
	lookups = 0; // No lookups have been made yet
	webpages = new LinkList<string>; // Initialize the linked list
											// of webpages
	search = s; // Remember the search filename
	
	output.open(o.c_str()); // Open the output file
	
	parseInput(i); // Build the array and linked list
}

void WebSearch::parseInput(string i) {
	// Parse the input file and create the 
	// keyword array and linked list of
	// webpages

	string url, keyword,
		   temp;    // Temporary holding location for
		   			// the input line
	int breakpoint; // Where the input line should be split
	
	// Create and open the input filestream
	ifstream input(i.c_str());
	
	if(input.fail()) {
		// If the file was unable to be opened
		cout<<"Unable to open input file "<<i<<endl;
		exit(0);
	}

	// Go through the entire input file, grab the
	// first string on the line, which happens to
	// be the URL
	while(!input.eof() && input>>url) {
		getline(input,temp,'\n'); // Get the rest of
								  // the data

		// Add the URL to the webpages linked list
		addURL(url);
		
		// temp has an extra space in the front;
		// this gets rid of it
		temp = temp.substr(1,temp.length()-1);
		
		// While there are more keywords for the
		// given URL...
		while(temp.find(" ") != string::npos) {

			// Find the breaks in between keywords
			breakpoint = temp.find(" ");

			// Get the first keyword
			keyword = temp.substr(0,breakpoint);

			// Remove the first keyword from the 
			// temp string
			temp = temp.substr(breakpoint+1,temp.length()-breakpoint-1);
			
			// Relate the webpage to the keyword
			linkKeywordPage(keyword, url);
		}
	
		// There is still one more keyword left; it
		// is the only thing in temp now, so finish
		// relating the url with its keywords
		linkKeywordPage(temp, url);
	}

	input.close();
}

void WebSearch::addURL(string u) {
	// Add the given URL into the webpages
	// linked list

	webpages->add(u);
}

int WebSearch::addKeyword(string k) {
	//	Adds a keyword to the array, in proper
	//	sorted order

	int p=0; // Holds the place in the array to
			 // store the keyword
	
	// Get the sorted position of where the keyword should be
	for(p=0; keywords[p] && k>keywords[p]->getKeyword() && p<numKeywords; p++);
	
	if(numKeywords && p!=numKeywords) { // Keyword is not in the beginning or
										// end of the array
		// Shift the old keywords over to make room for the new one
		keywords[numKeywords] = keywords[numKeywords-1];
		for(int i=numKeywords-1; i>p; i--)
			keywords[i] = keywords[i-1];

		// Add the new keyword
		keywords[p] = new KeywordNode(k);
	}
	// If the keyword is in the back of the array or is
	// the first one, a new node will be created for it
	else keywords[p] = new KeywordNode(k);
	
	// A keyword has been added; increment the counter
	numKeywords++;

	return p; // Return the position of the keyword
}

void WebSearch::linkKeywordPage(string k, string w) {
	// Links a page to a keyword
	
	// If there are no keywords, the keyword won't
	// be found, so add it
	if(!numKeywords) addKeyword(k);
	
	// Is the keyword in the array?
	int place = keywordSearch(k);
	if(place==numKeywords) // No, so add it to the array
		place = addKeyword(k);

	// Link the webpage to the keyword
	keywords[place]->add(webpages->find(w));
}

void WebSearch::runSearch() {
	// Prints out the webpages associated with the 
	// keywords in the search file
	
	string keyword;		// Holds keywords read from the file
	int totalLookups=0;	// Holds the total number of lookups
	int pages;			// Temporary holding spot for pages
	int totalPages=0;	// Holds the total number of pages found
	int place;			// Holds a place in the array of keywords
	int totalKeywords=0;// Number of keywords searched for
	
	// Open the search file
	ifstream input(search.c_str());

	if(input.fail()) {
		// If the file cannot be opened, exit gracefully
		cout<<"Unable to open search file.\n";
		exit(0);
	}

	while(!input.eof() && input>>keyword) {
		place = keywordSearch(keyword);
		pages = (place < numKeywords) ? keywords[place]->getSize() : 0 ;

		output<<keyword<<": "<<pages<<" web pages found, "
			<<lookups<<" keyword lookups.\n";

		if(place < numKeywords) printPages(place);

		totalKeywords++;
		totalLookups += lookups;
		totalPages += pages;
	}

	output<<"TOTALS: "<<totalKeywords<<" keywords, "
		<<totalPages<<" web pages returned, "<<totalLookups
		<<" keyword lookups.\n";
}

void WebSearch::printPages(int p) {
	// Prints out the webpages in the array at p
	// Assumes that keywords[p] exists

	LinkList<Node<string>*> *listPtr = keywords[p]->getWebpages();

	output<<"  "<<listPtr->get()->get()<<endl;

	while(listPtr->next())
		output<<"  "<<listPtr->get()->get()<<endl;
}

int WebSearch::keywordSearch(string k) {
	// Uses the binary search to find the position of 
	// keyword k in the array.  If it does not exist,
	// returns numKeywords

	// If there are no elements in the array, the
	// keyword won't be found.

	lookups = 0; // Holds number of comparisons
	if(!numKeywords) return 1;
	
	// The usual binary search algorithm
	int l=0, r=numKeywords-1, x=(l+r)/2;
	for(; k!=keywords[x]->getKeyword() && (r>l) ;x=(l+r)/2) {
		if(k < keywords[x]->getKeyword())
			r = x-1;
		else
			l = x+1;
		lookups++; // One more comparison has been made
	}

	lookups++;
	
	if(k == keywords[x]->getKeyword()) return x;
	else return numKeywords;
}

WebSearch::~WebSearch() {
	// Deconstructor: make sure all of the
	// dynamically created objects are nicely
	// destroyed

	// Destruct all of the pointers in the array
	for(int i=0; i<1000 ;i++) {
		delete keywords[i];
		keywords[i] = NULL;
	}

	// Destruct the linked list
	delete webpages;

	// Close the output file
	output.close();
}

#endif
