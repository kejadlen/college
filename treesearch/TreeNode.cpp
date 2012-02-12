#ifndef _TREENODE_CPP_
#define _TREENODE_CPP_

#include <iostream>

#include "TreeNode.h"

template <typename T> void TreeNode<T>::addChild(T *c) {
	// Adds a child to the node

	TreeNode<T> *temp = new TreeNode<T>(c, this);
	
	children.add(temp);
}

template <typename T> void TreeNode<T>::print() {
	// Prints out the entire tree in a
	// preorder traversal
	
	data->print();

	children.goHead();
	children.get()->print(1);
	while(children.next()) children.get()->print(1);
}

template <typename T> void TreeNode<T>::print(int level) {
	// Helper for print
	
	for(int i=0; i<level ;i++) cout<<"\t";
	data->print();

	children.goHead();
	children.get()->print(level+1);
	while(children.next()) children.get()->print(level+1);
}

#endif
