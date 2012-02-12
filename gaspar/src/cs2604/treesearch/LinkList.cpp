#ifndef _LINKLIST_CPP_
#define _LINKLIST_CPP_

#include <iostream>

#include "LinkList.h"

using namespace std;

template <typename T> LinkList<T>::LinkList(const LinkList& l) {
	// Copy constructor
	
	size = l.size;
	Node<T> *temp = l.head;

	head = NULL;
	tail = NULL;

	while(temp) {
		add(temp->get());
		if(temp == l.current)
			current = tail;
		temp = temp->getNext();
	}
}

template <typename T> void LinkList<T>::add(T d) {
	// Add a Node with data d to the list
	
	if(tail) { // If there is a last node, attach
			   // the new Node to the end
		tail->setNext(d);
		tail = tail->getNext();
	}
	else // There is no last node == there is no
		 // list.  Start the list with this Node
		current = tail = head = new Node<T>(d);

	size++; // A node has been added; increment
			// the size
}

template <typename T> void LinkList<T>::clear() {
	// Clears the list.  Deallocate the memory
	// and set the size back to zero.
	
	for(; head;) {
		current = head;
		head = head->getNext();
		delete current;
	}
	
	head = tail = current = 0; // Reset the pointers
							   // to NULL
	size = 0; // Reset the size to zero
}

template <typename T> T LinkList<T>::get() {
	// Return the data in the current Node.
	// Assumes that current exists.

	return current->get();
}

template <typename T> T* LinkList<T>::getPtr() {
	// Returns the pointer to the current Node
	
	return current->getPtr();
}

template <typename T> int LinkList<T>::getSize() {
	// Returns the size of the linked list

	return size;
}

template <typename T> Node<T>* LinkList<T>::find(T d) {
	// Finds the first Node with data equal to d
	// and returns a pointer to that Node.  If no
	// such Node is found, return NULL.  Assumes 
	// the == operator exists for T.
	
	for(Node<T> *find=head; find; find=find->getNext()) {
		if(find->get() == d) return find; // Found
	}
	
	return 0; // Not found
}

template <typename T> void LinkList<T>::goHead() {
	// Set current to the first Node
	
	current = head;
}

template <typename T> int LinkList<T>::next() {
	// Set current to the next Node, if it exists
	
	if(current->getNext()) {
		current = current->getNext();
		return 1;
	}
	else return 0;
}

template <typename T> void LinkList<T>::goTail() {
	// Set current to the last Node
	
	current = tail;
}

template <typename T> void LinkList<T>::print() {
	// Debugging function: display the contents of
	// all of the Nodes, in order.  Assumes the <<
	// operator exists for T.

	for(Node<T> *temp=head; temp; temp=temp->getNext())
		cout<<"\t"<<temp->get()<<endl;
}

template <typename T> LinkList<T>::~LinkList() {
	// The destructor: make sure all of the 
	// dynamically allocated memory is
	// deallocated.

	clear();
}

template <typename T> LinkList<T>& LinkList<T>::operator=(const LinkList<T>& rhs) {
	// Operator overloading is useful, what can I say?

	size = rhs.size;

	Node<T> *temp = rhs.head;
	
	head = NULL;
	tail = NULL;
	
	while(temp) {
		add(temp->get());
		if(temp == rhs.current)
			current = tail;
		temp = temp->getNext();
	}
	return *this;
}

#endif
