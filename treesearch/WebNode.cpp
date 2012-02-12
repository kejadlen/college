#ifndef _WEBNODE_CPP_
#define _WEBNODE_CPP_

#include <iostream>
#include <string>

#include "WebNode.h"

using namespace std;

WebNode::WebNode(string u, int r, string k) {
	// Constructor -- the only complicated part
	// is creating the linked list of keywords

	int breakpoint = 0; // Used to keep track of
						// location of spacers in
						// the keyword string
	string keyword;	// Holds the keyword to insert
					// into the linked list
	
	// Set the URL and the rank of the webpage
	URL = u;
	rank = r;

	// Create the linked list of keywords
	// keyword string format: "k1 k2 ... kn"
	
	while(k.find(" ") != string::npos) {
		// Find the break for the first keyword
		breakpoint = k.find(" ");
		// Save the keyword
		keyword = k.substr(0,breakpoint);
		// Remove the keyword from the string
		k = k.substr(breakpoint+1,k.length()-breakpoint-1);
		// Add the keyword to the linked list
		keywords.add(keyword);
	}

	// There is sitll one more keyword left; it
	// is the only thing remaining in the keyword
	// string.  Add the final keyword.
	keywords.add(k);
}

void WebNode::print() {
	// Useful for debugging purposes
	
	cout<<"URL: "<<URL<<endl;
	cout<<"Rank: "<<rank<<endl;
	cout<<"Keywords: \n";
	keywords.print();
}

#endif
