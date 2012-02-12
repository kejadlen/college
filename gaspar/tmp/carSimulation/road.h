#ifndef _ROAD_H_
#define _ROAD_H_

/*
 *  highway.h
 *  carSimulation
 *
 *  Created by Alpha Chen on Thu Apr 22 2004.
 *
 *  2004-04-25
 *      Finished
 *
 *  2004-04-23
 *      Road declaration finished
 *      checkBarrier() removed
 *
 *  2004-04-22
 *      Road declaration written
 */

class Car;

class Road {
private:
    string name;	// Road name
    int lanes, length;  // Road attributes
    int ** data;	// Status of all the
			// positions on the road
    
    // For mutual exclusion -- only one car can
    // move at a time
    pthread_mutex_t roadMutex;
    
    // Data on each car
    vector<Car *> carData;
    vector<int> carLane;
    vector<int> carPosition;
    
public:
    // Checks to see whether the car can go forward or not eventually.
    int canProgress(int carID);
	
    // Moves a car in the given direction. If the move is invalid,
    // returns what is blocking the car.
    int moveCar(int carID, int direction);

    // Returns a pointer to the car next to the car specified,
    // in the given direction
    Car * getCar(int carID, int direction);
    
    // Insert a car or barrier onto the road
    int addData(int lane, int position, Car * carPtr);
    
    // Write the road to cout, showing the cars and
    // barriers.
    int writeRoad();

    // Write the status of the cars to cout
    int writeCars();

    // Mutual exclusion lock/unlock functions:
    void lock() { pthread_mutex_lock(&roadMutex); }
    void unlock() { pthread_mutex_unlock(&roadMutex); }
    
    // Constructor and destructor
    Road(string n, int lanes, int length);
    ~Road();
};

#include "road.cpp"

#endif
