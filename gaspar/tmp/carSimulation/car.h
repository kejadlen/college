#ifndef _CAR_H_
#define _CAR_H_

/*
 *  car.h
 *  carSimulation
 *
 *  Created by Alpha Chen on Thu Apr 22 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 *  2004-04-25
 *      Finished
 *
 *  2004-04-23
 *      Car and TurboCar declarations finished
 *      StanardCar class removed
 *  2004-04-22
 *      Car declaration written
 */

class Road;

// The base car class
class Car {
private:
    // Self explanatory variables
    int carID;
    string license;
    
    Road * roadPtr;
    int odometer;
    int directionPref;
    int canProgress;
    int speed;
    
public:
    // Start and stop the car
    int start();
    int stop();

    // Modify carID
    int setID(int c) { carID = c; return 0; }
    
    // Get status of car
    int getID() { return carID; }
    string getLicense() { return license; }
    int getDirection() { return directionPref; }
    int getProgress() { return canProgress; }
    int getOdometer() { return odometer; }
    int incOdometer() { odometer++; return odometer; }
    
    // Constructor
    Car(string l, Road * r, int s);
};

/*  The Standard car moves on one second ticks.
 */
class StandardCar : public Car {
public:
    StandardCar(string l, Road * r) : Car(l, r, 1) {}
};

/*  The Turbo car is the same as the Standard car,
 *  except for being twice as fast.
 */
class TurboCar : public Car {
public:
    TurboCar(string l, Road * r) : Car(l, r, 2) {}
};

#include "car.cpp"

#endif
