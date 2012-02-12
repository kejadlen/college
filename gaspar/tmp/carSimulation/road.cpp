/*
 *  road.cpp
 *  carSimulation
 *
 *  Created by Alpha Chen on Sat Apr 24 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 *  To do:
 *
 *  2004-04-24
 *      road.cpp created
 *      constructor, destructor finished
 *      moveCar, getCar finished
 *      addData finished
 *
 */

#include <string>
#include <vector>
#include "car.h"

/*  Constructor, sets name, lanes, length of
 *  the road and creates the matrix which
 *  holds the road data.
 */
Road::Road(string n, int la, int le) {
    // Seed the random number generator with the time
    srand(time(NULL));
    
    // Write the data
    name = n;
    lanes = la;
    length = le;
    
    // Initialize matrix
    data = new int*[lanes];
    for(int i=0; i<lanes; i++)
	data[i] = new int[length];
    
    // Clear the road
    for(int i=0; i<lanes; i++)
	for(int j=0; j<length; j++)
	    data[i][j] = 0;
    
    carData.clear();
    carLane.clear();
    carPosition.clear();
    
    // Initialize mutual exclusion variable
    pthread_mutex_init(&roadMutex, NULL);
}

/*  Cars can "see" more than one location, checking
 *  to see whether they can continue or not.
 */
int Road::canProgress(int carID) {    
    carID--;
    
    int currentLane = carLane[carID], currentPosition = carPosition[carID];
    int startLane, stopLane;

    if(currentPosition + 1 == length) return 1;
    
    for(startLane=currentLane-1; startLane>-1 && data[startLane][currentPosition]!=-1; startLane--);
    for(stopLane=currentLane+1; stopLane<lanes && data[stopLane][currentPosition]!=-1; stopLane++);
    
    for(int i=startLane+1; i<=stopLane-1; i++)
	if(data[i][currentPosition+1] != -1) return 1;
    
    return 0;
}

/*  Moves the given car in the given direction. If
 *  the car cannot be moved that way, return the carID
 *  of the blocking car or -1 if a barrier is in the
 *  way.
 *
 *  direction:
 *  0   forward
 *  1   left
 *  2   right
 *  3   stop
 */
int Road::moveCar(int carID, int direction) {
    carID--;    // "normalize" the carID
    
    // Get car information
    Car * carPtr = carData[carID];
    int lane = carLane[carID];
    int position = carPosition[carID];
    
    // Stop moving
    if(direction == 3) {
	data[lane][position] = -1;
	return 0;
    }
    
    // Go foward
    if(direction == 0) {
	position++;

	// Car has reached end of road
	if(position == length) {
	    carPtr->incOdometer();
	    carPosition[carID] = -1;    // -1 represents end of road
	    data[lane][position-1] = 0; // Clear old position
	    carPtr->stop();
	    return 0;
	} // end if(newPosition > length)

	// Nothing in front of the car, so
	// move it forward
	if(!data[lane][position]) {
	    carPtr->incOdometer();
	    carPosition[carID]++;	    // Update car's current position
	    data[lane][position-1] = 0;     // Clear old position
	    data[lane][position] = carID+1; // Write car into new position
	    return 0;
	}
	    
	// The car can't move forward; return what
	// is blocking it
	return data[lane][position];
	
    } // end if(direction == 0)

    /*  What the?! g++ compiler doesn't recognize the ternary operation?!
    int newLane = lane + (direction == 1) ? -1 : 1 ;
     */
    
    // Move left/right
    int newLane;
    if(direction == 1)
	newLane = lane - 1;
    else
	newLane = lane + 1;

    // Check for moving off the road
    if(newLane == -1 || newLane == lanes)
	return -1;
    
    // The way is clear, so move the car
    // in the appropriate direction
    if(!data[newLane][position]) {
	carPtr->incOdometer();
	carLane[carID] = newLane;
	data[lane][position] = 0;
	data[newLane][position] = carID+1;
	return 0;
    }
    
    // The car can't move, so return
    // what is blocking it
    return data[newLane][position];

} // end moveCar()

/*  Returns a pointer to the car in the given direction
 *  relative to the carID given.
 */
Car * Road::getCar(int carID, int direction) {
    carID--;    // Normalize carID
    
    // Get car's position on the road
    int lane = carLane[carID];
    int position = carPosition[carID];

    /*  Ternary operator didn't work?
    lane += (direction) ? -1 : 1 ;
     */
    
    // Get the correct lane
    if(direction)
	lane++;
    else
	lane--;
    
    // Return a pointer to the car in
    // that position
    return carData[data[lane][position]-1];
} // end getCar()

/*  Adds a car to the road if carData is given. Otherwise,
 *  place a barrier onto the road.
 */
int Road::addData(int lane, int position, Car * carPtr) {
    // Check if the space is open
    if(data[lane][position] || lane < 0 || position < 0 || lane >= lanes || position >= length)
	return -1;
    
    // Add a car
    if(carPtr) {
	// Modify car data
	carData.push_back(carPtr);
	carLane.push_back(lane);
	carPosition.push_back(position);
	
	// Is this right?
	carPtr->setID(carData.size());
	data[lane][position] = carData.size();
    }
    
    // Add a barrier
    else
	data[lane][position] = -1;
    
    return 0;
}

/*  Writes the status of the cars to cout
 */
int Road::writeCars() {
    int numCars = carData.size();

    for(int i=0; i<numCars; i++) {
	cout<<carData[i]->getLicense()<<" ";

	if(carPosition[i] < 0)
	    cout<<"has reached the end of";
	else
	    cout<<"is idle in";
	
	cout<<" lane "<<carLane[i]<<" at ";
	
	if(carPosition[i] < 0)
	    cout<<length-1;
	else
	    cout<<carPosition[i];
	
	cout<<" with odometer reading "<<carData[i]->getOdometer()<<endl;
    }

    return 0;
}

/*  Writes the status of the road to cout
 */
int Road::writeRoad() {
    cout<<"Time: "<<time(NULL)<<endl;
    
    cout.setf(ios::right);

    for(int i=0; i<length; i++) {
	cout<<setw(2)<<i<<'|';

	for(int j=0; j<lanes; j++) {
	    if(data[j][i] == -1)
		cout<<"XXXXXXXXXX";
	    else if(data[j][i])
		cout<<setw(10)<<carData[data[j][i]-1]->getLicense();
	    else
		cout<<"          ";
	}
	
	cout<<endl;
    }
    
    cout<<endl;
    cout.setf(ios::left);
    return 0;
}

/*  Destructor
 */
Road::~Road() {
    for(int i=0; i<lanes; i++)
	delete data[i];
    delete data;

    pthread_mutex_destroy(&roadMutex);
}
