#ifndef _NODE_H_
#define _NODE_H_

template <typename T> class Node {
	// A Node class for the linked
	// list class to use.  It has
	// rudimentary capabilities so
	// far.
	//
	// To add:
	// 	- Node *previous ?
	// 		(getPrevious, setPrevious
	// 		 naturally follow)
	// 	- getCurrent ?
	
	private:
		T data;	// Stores the data of type T
		Node *next;	// Points at the next Node
	public:
		// Constructor
		Node(T d=0, Node *n=0) : data(d), next(n){}

		// Information retrieval functions
		T get() { return data; }
		Node *getNext() { return next; }

		// Node property functions
		void set(T d) { data = d; }
		void setNext(T d) { this->next = new Node<T>(d); }
		void setNext(Node<T> *n) { this->next = n; }
};

#endif
