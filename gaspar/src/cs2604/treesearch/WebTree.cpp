#ifndef _WEBTREE_CPP_
#define _WEBTREE_CPP_

#include <string>
#include <fstream>
#include <iostream>

#include "WebTree.h"

void WebTree::parseInput(string i) {
	// Parses the input
	
	string url, keywords, temp;

	int breakpoint, level, rank, currentLevel=0;

	TreeNode *tN = NULL,
			 *tempTN = NULL;
	
	ifstream input(i.c_str());

	if(input.fail()) {
		cout<<"unable to open input file "<<i<<endl;
		exit(0);
	}
	
	while(!input.eof() && getline(input,temp,'\n')) {
		level = temp.find_first_not_of(' ');
		temp = temp.substr(level,temp.length()-level);
		level /= 3;
		breakpoint = temp.find(' ');
		url = temp.substr(0, breakpoint);
		temp = temp.substr(breakpoint+1,temp.length()-breakpoint-1);
		breakpoint = temp.find(' ');
		rank = atoi(temp.substr(0,breakpoint).c_str());
		if(breakpoint>-1) keywords = temp.substr(breakpoint+1,temp.length()-breakpoint-1);

		if(!head) {
			head = new TreeNode(url, rank, size, keywords, tN);
			tN = head;
		}

		else {
			while(currentLevel >= level) {
				tN = tN->getParent();
				currentLevel--;
			}
	
			tempTN = NULL;
			tempTN = new TreeNode(url, rank, size, keywords, tN);
	
			tN->children.add(tempTN);
			tN->children.goTail();
			tN = tN->children.get();
			currentLevel++;
		}

		keywords = "";

		size++;
	}

	input.close();
}

void WebTree::buildHeap(TreeNode *cur) {
	// Builds the heap
	
	heap->add(cur);

	if(cur->children.getSize()) {
		cur->children.goHead();
		buildHeap(cur->children.get());
		while(cur->children.next())
			buildHeap(cur->children.get());
	}
}

WebTree::WebTree(string i, string o) {
	// Constructor
	
	size = 0;
	head = NULL;
	heap = NULL;
	output.open(o.c_str());

	parseInput(i);
	head->updateKeywords();

	heap = new MinHeap<TreeNode>(size);
	buildHeap(head);
}

WebTree::~WebTree() {
	// Destructor
	
	delete head;
	delete heap;
	output.close();
}

void WebTree::webcrawl() {
	// Program 1
	
	TreeNode* temp = NULL;
	
	while(heap->getCount()) {
		temp = heap->get();
		output<<temp->getURL()<<" "
			<<temp->getKeywords()
			<<endl;
	}
}

void WebTree::startSearch(string searchFile) {
	// Program 2
	
	ifstream input(searchFile.c_str());

	if(input.fail()) {
		cout<<"Unable to open search file "<<searchFile<<endl;
		exit(0);
	}

	string temp = "", s1, s2;
	int breakpoint;

	while(!input.eof() && getline(input, temp, '\n')) {
		output<<temp<<endl;
		breakpoint = temp.find(" OR ");
		if(breakpoint>-1) {
			s1 = temp.substr(0, breakpoint);
			s2 = temp.substr(breakpoint+4, temp.length()-breakpoint-4);
			if(head->search(s1) || head->search(s2))
				output<<head->print(s1,s2,0);
			else output<<"NOT FOUND"<<endl;
		}
		else {
			breakpoint = temp.find(" AND ");
			if(breakpoint>-1) {
				s1 = temp.substr(0, breakpoint);
				s2 = temp.substr(breakpoint+5, temp.length()-breakpoint-5);
				if(head->search(s1) && head->search(s2))
					output<<head->print(s1,s2,1);
				else output<<"NOT FOUND"<<endl;
			}
			else 
				if(head->search(temp)) output<<head->print(temp, temp, 0);
				else output<<"NOT FOUND"<<endl;
		}
	output<<endl;
	}
}

/*void WebTree::print() {
	head->print();
}*/

#endif
