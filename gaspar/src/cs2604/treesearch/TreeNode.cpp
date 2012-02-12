#ifndef _TREENODE_CPP_
#define _TREENODE_CPP_

#include <string>
#include <iostream>

#include "TreeNode.h"

TreeNode::TreeNode(string u, int r, int l, string k, TreeNode *p) {
	// Constructor

	url = u;
	rank = r;
	line = l;
	parent = p;

	addKeywords(k);
}

void TreeNode::addKeywords(string k) {
	// Adds keywords from a string
	
	int breakpoint = 0;
	string keyword;
	
	while(k.find(" ") != string::npos) {
		breakpoint = k.find(" ");
		keyword = k.substr(0,breakpoint);
		k = k.substr(breakpoint+1,k.length()-breakpoint-1);
		if(keyword.length() && !keywords.find(keyword))
			keywords.add(keyword);
	}

	// Still a keyword left...
	if(k.length() && !keywords.find(k))
		keywords.add(k);
}

TreeNode* TreeNode::getParent() {
	// Return the parent

	return parent;
}

string TreeNode::getURL() {
	// Return the URL
	
	return url;
}

/*void TreeNode::print() {
	cout<<url;

	if(keywords.getSize()) {
		keywords.goHead();
		cout<<" "<<keywords.get();
		while(keywords.next())
			cout<<" "<<keywords.get();
	}

	cout<<endl;
	
	if(children.getSize()) {
		children.goHead();
		children.get()->print(1);
		while(children.next())
			children.get()->print(1);
	}
}*/

void TreeNode::updateKeywords() {
	// Update keywords
	
	if(children.getSize()) {

		// Update childrens' keywords --
		// must make sure that childrens'
		// keywords are up to date before
		// updating own keywords
		children.goHead();
		children.get()->updateKeywords();
		while(children.next())
			children.get()->updateKeywords();

		// Now update own keywords.
		children.goHead();
		addKeywords(children.get()->getKeywords());
		while(children.next())
			addKeywords(children.get()->getKeywords());
	}
	
}

string TreeNode::getKeywords() {
	// Get keywords.  Returns string of
	// keywords
	
	string temp = "";
	
	if(keywords.getSize()) {
		keywords.goHead();
		temp += keywords.get();
		while(keywords.next()) {
			temp += " ";
			temp += keywords.get();
		}
	}

	return temp;
}

int TreeNode::search(string s) {
	// Is s a keyword of the TreeNode?
	
	if(keywords.find(s))
		return 1;
	return 0;
}

/*void TreeNode::print(int level) {
	for(int i=0; i<level; i++) cout<<"   ";
	
	cout<<url;

	if(keywords.getSize()) {
		keywords.goHead();
		cout<<" "<<keywords.get();
		while(keywords.next())
			cout<<" "<<keywords.get();
	}

	cout<<endl;

	if(children.getSize()) {
		children.goHead();
		children.get()->print(level+1);
		while(children.next())
			children.get()->print(level+1);
	}
}

void TreeNode::print(string search) {
	cout<<url;

	cout<<endl;
	
	if(children.getSize()) {
		children.goHead();
		if(children.get()->search(search)) children.get()->print(search, 1);
		while(children.next())
			if(children.get()->search(search)) children.get()->print(search, 1);
	}
}

void TreeNode::print(string search, int level) {
	for(int i=0; i<level; i++) cout<<"   ";
	
	cout<<url;

	cout<<endl;

	if(children.getSize()) {
		children.goHead();
		if(children.get()->search(search)) children.get()->print(search, level+1);
		while(children.next())
			if(children.get()->search(search)) children.get()->print(search, level+1);
	}
}*/

string TreeNode::print(string search1, string search2, int flag) {
	// Print function.
	
	int s1, s2;
	string temp = "";
	
	temp += url;

	temp += "\n";
	
	if(children.getSize()) {
		children.goHead();
		s1 = children.get()->search(search1);
		s2 = children.get()->search(search2);
		if((flag && s1 && s2) || (!flag && (s1 || s2)))
			temp += children.get()->print(search1, search2, flag, 1);
		while(children.next()) {
			s1 = children.get()->search(search1);
			s2 = children.get()->search(search2);
			if((flag && s1 && s2) || (!flag && (s1 || s2)))
				temp += children.get()->print(search1, search2, flag, 1);
		}
	}

	return temp;
}

string TreeNode::print(string search1, string search2, int flag, int level) {
	// Helper print function
	
	int s1, s2;
	string temp = "";
	
	for(int i=0; i<level; i++) temp += "   ";
	
	temp += url;

	temp += "\n";

	if(children.getSize()) {
		children.goHead();
		s1 = children.get()->search(search1);
		s2 = children.get()->search(search2);
		if((flag && s1 && s2) || (!flag && (s1 || s2)))
			temp += children.get()->print(search1, search2, flag, level+1);
		while(children.next()) {
			s1 = children.get()->search(search1);
			s2 = children.get()->search(search2);
			if((flag && s1 && s2) || (!flag && (s1 || s2)))
				temp += children.get()->print(search1, search2, flag, level+1);
		}
	}

	return temp;
}

TreeNode::~TreeNode() {
	// Deconstructor
	
	if(children.getSize()) {
		children.goHead();
		delete children.get();
		while(children.next())
			delete children.get();
	}
}

bool TreeNode::operator<(const TreeNode& rhs) {
	// Overloading operator is useful.
	
	return (rank < rhs.rank) || (rank == rhs.rank && line < rhs.line);
}

#endif
