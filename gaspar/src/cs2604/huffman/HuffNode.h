#ifndef _HUFFNODE_H_
#define _HUFFNODE_H_

#include <iostream>
#include <fstream>
#include <string>

using namespace std;

struct HuffNode {
	// Node for holding huffman tree data; doubles
	// as both a linked list node and a tree node
	
	// Basic data the huffman tree needs
	char character;
	int frequency;

	// Pointers for building the Huffman tree
	HuffNode *left;
	HuffNode *right;
	HuffNode *parent;

	// For linked list purposes
	HuffNode *next;
	
	HuffNode(char c, int f, HuffNode *l=NULL, HuffNode *r=NULL, HuffNode *p = NULL, HuffNode *n=NULL) : character(c), frequency(f), left(l), right(r), parent(p), next(n) {}

	bool isChild() { return !(left || right); }

	void print() { cout<<"("<<character<<":"<<frequency<<")"<<endl; if(left) left->print(); if(right) right->print(); }

	bool operator<(const HuffNode& rhs);
	
	~HuffNode() { if(left) delete left; if(right) delete right; if(next) delete next; }
};

bool compare(char a, char b) {
	//

	if(a == 1 && b == 1) return 1;
	
	int i = a, j = b;
	if(a != 1 && a < 'a') i = a+100;
	if(b != 1 && b < 'a') j = b+100;
	return i < j;
}

bool HuffNode::operator<(const HuffNode& rhs) {
	//
	
	return ( frequency < rhs.frequency ||
			(frequency == rhs.frequency && compare(character,rhs.character)));
}

struct HuffList {
		HuffNode *head;
		int length;

		HuffList() : head(NULL), length(0) {}

		void insert(HuffNode *temp);
		void print();
		
		~HuffList();
};

void HuffList::insert(HuffNode *temp) {
	//

	if(!head) {
		head = temp;
	}
	
	else if(*temp < *head) {
		temp->next = head;
		head = temp;
	}
	
	else {
		HuffNode *ptr = NULL;
		for(ptr=head; ptr->next && !(*temp < *ptr->next); ptr=ptr->next);
		
		if(ptr->next) temp->next = ptr->next;
		ptr->next = temp;
	}

	length++;
}

void HuffList::print() {
	// 

	for(HuffNode *ptr=head; ptr; ptr=ptr->next) {
		cout<<ptr->character<<":"<<ptr->frequency<<"~"<<endl;
	}
}

HuffList::~HuffList() {
	for(HuffNode *ptr=head; ptr; ptr=ptr->next) {
		delete ptr;
	}
}

#endif
