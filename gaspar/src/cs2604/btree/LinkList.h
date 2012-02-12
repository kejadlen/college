#ifndef _LINKLIST_H_
#define _LINKLIST_H_

#include "Node.h"

//using namespace std;

template <typename T> class LinkList {
	// This class allows for easy access
	// and control of a templated linked
	// list.
	
	// Things to add:
	// 	- doubly linked list capability
	// 	- inserting in the front
	// 	- inserting in sorted place
	// 	- [] operator
	// 	- removal of current Node
	// 	- setting/changing current data
	// 	- copy constructor
	// 	- finding the place of a Node
	// 	- isEmpty() ?
	
	private:
        Node<T> *head; // Beginning of linked list
		Node<T> *current; // Current node
		Node<T> *tail; // End of linked list
		int size; // Size of the linked list
	public:
		// Constructor: everything is set to NULL at first
		LinkList() : head(0), current(0), tail(0), size(0){}

		// Useful functions
		void add(T d); // Add a new node with data d
		//void remove(); // Remove the current node
		void clear(); // Returns LinkList to original state

		T get(); // Returns the value in the current node
		int getSize(); // Returns the size of the list
		Node<T> *find(T d); // Returns the first node
							  // with data d
		
		// List movement functions
		void goHead(); // Goes to the first node
		int next(); // Goes to the next node
		void goTail(); // Goes to the last node

		// Debugging purposes
		void displayAll(); // Displays entire linked list
		
		// Destructor
		~LinkList();
};

#endif
