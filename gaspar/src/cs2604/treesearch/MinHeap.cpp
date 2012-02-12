#ifndef _MINHEAP_CPP_
#define _MINHEAP_CPP_

#include "MinHeap.h"

using namespace std;

template <typename T> void MinHeap<T>::swap(int i, int j) {
	// Swap what is in position i for what is in position j
	
	T *temp = heap[i];
	heap[i] = heap[j];
	heap[j] = temp;
}

template <typename T> MinHeap<T>::MinHeap(int s) {
	// Constructor
	
	size = s;
	count = 0;
	heap = new T*[size];

	for(int i=0; i<size ;i++)
		heap[i] = NULL;
}

template <typename T> void MinHeap<T>::add(T *temp) {
	// add to the heap
	
	int current = count; // This will be the current
						 // element once the new one
						 // is placed in
	
	// Put the new element in
	heap[count] = temp;
	count++;

	// Move the new element to where it should go
	int parent = (current-1)/2;
	while(current) {
		if(*heap[current] < *heap[parent]) {
			swap(current, parent);
			current = parent;
			parent = (current-1)/2;
		}
		else break;
	}
}

template <typename T> T* MinHeap<T>::get() {
	// Spit out the top of the heap

	if(!count) return NULL;

	// Store the element to be returned
	T *temp = heap[0];

	// The element "placed" in the top
	// of the heap and compared to see
	// where it should go
	T *v = heap[--count];

	// Start k at the top of the array
	int j, k = 0;
	
	// Reorder the heap so that it is 
	// correct
	while(k < count/2) {
		j = 2*k+1;
		if(j < count-1)
			if(*heap[j+1] < *heap[j]) j++;
		if(*v < *heap[j]) {
			heap[k] = v;
			return temp;
		}
		heap[k] = heap[j];
		k = j;
	}
	heap[k] = v;
	return temp;
}

template <typename T> MinHeap<T>::~MinHeap() {
	// Deconstructor
	
	for(int i=0; i<size ;i++)
		heap[i] = NULL;

	delete heap;
}

#endif
