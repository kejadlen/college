#ifndef _MINHEAP_H_
#define _MINHEAP_H_

template <typename T> class MinHeap {
	// A minheap class
	
	private:
		int size; 	// Total size of the heap
		int count;  // Current number of elements
					// in the heap
		T **heap;	// The heap itself. Stores
					// pointers to the elements

		void swap(int i, int j);
			// Utility function - swaps heap elements

	public:
		// Constructor
		MinHeap(int s);

		// Add element to heap
		void add(T *temp);
		
		// Get pointer to element from heap
		T *get();

		// Get how many elements are in the heap
		int getCount() { return count; };
		
		// Deconstructor
		~MinHeap();
};

#endif
