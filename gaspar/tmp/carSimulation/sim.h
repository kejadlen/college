#ifndef _SIM_H_
#define _SIM_H_

/*
 *  sim.h
 *  carSimulation
 *
 *  Created by Alpha Chen on Thu Apr 22 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

// Helper function so that threading can be used
void * startCar(void *);

class Sim {
private:
    // Simulation data
    Road * road;
    vector<Car *> cars;
    
public:
    // Start the cars moving with threading
    int startCars();
	
    // Constructor, destructor
    Sim();
    ~Sim();
};

/*  Constructor -- create the road, and seed it
 *  with the cars and barriers given
 */
Sim::Sim() {
    string name, data, command, arguments;
    int tabLocation, length;
    int lane = 0, position = 0;
    Car * carPtr;

    //  Read in the road data
    while((!lane || !position) && !cin.eof()) {
	getline(cin, data);
	if(data[0] != ';') {
	    tabLocation = (int) data.find('\t');
	    length = (int) data.length();
	    command = data.substr(0, tabLocation);  // Get what is in front of the first tab
	    arguments = data.substr(tabLocation+1, length);
	    
	    if(command == "Name:")
		name = arguments;
	    else if(command == "Lanes:")
		lane = atoi(arguments.c_str());
	    else if(command == "Length:")
		position = atoi(arguments.c_str());
	}
    }
    
    road = new Road(name, lane, position);
    cars.clear();

    // Read in rest of data
    while(!cin.eof()) {
	getline(cin, data);
	if(data[0] != ';') {
	    
	    // Get whether it is a vehicle or barrier
	    tabLocation = (int) data.find('\t');
	    length = (int) data.length();
	    command = data.substr(0, tabLocation);
	    arguments = data.substr(tabLocation+1, length);
	    
	    if(command == "vehicle") {
		// Get type of vehicle
		tabLocation = (int) arguments.find('\t');
		length = (int) arguments.length();
		data = arguments.substr(0, tabLocation);
		arguments = arguments.substr(tabLocation+1, length);
		
		// Get license plate
		tabLocation = (int) arguments.find('\t');
		length = (int) arguments.length();
		name = arguments.substr(0, tabLocation);
		arguments = arguments.substr(tabLocation+1, length);
	    }
	    
	    if(command == "vehicle" || command == "barrier") {
		// Get lane
		tabLocation = (int) arguments.find('\t');
		length = (int) arguments.length();
		lane = atoi(arguments.substr(0, tabLocation).c_str());
		arguments = arguments.substr(tabLocation+1, length);
		
		// Get position
		tabLocation = (int) arguments.find('\t');
		length = (int) arguments.length();
		position = atoi(arguments.substr(0, tabLocation).c_str());
		
		// Create vehicle
		if(command == "vehicle") {
		    if(data == "turbo")
			carPtr = new TurboCar(name, road);
		    else
			carPtr = new StandardCar(name, road);
		    if(road->addData(lane, position, carPtr) != -1)
			cars.push_back(carPtr);
		    else {
			cout<<"Error placing "<<name<<" in lane "<<lane<<" at "<<position<<endl;
			delete carPtr;
		    }
		}
		
		// Create barrier
		else {
		    if(road->addData(lane, position, NULL) == -1)
			cout<<"Error placing barrier in lane "<<lane<<" at "<<position<<endl;
		}
	    }
	}
    }
}

/*  Start the cars running, threaded
 */
int Sim::startCars() {
    int numCars = cars.size();
    pthread_t* hThread = new pthread_t[numCars];

    // Write the beginning status of the road
    road->writeCars();
    road->writeRoad();
    
    // Start all the threads
    for(int i=0; i<numCars; i++)
	pthread_create(&hThread[i], NULL, startCar, (void *) cars[i]);
    
    // Don't end this process until the previous threads
    // are finished
    for(int i=0; i<numCars; i++)
	pthread_join(hThread[i], NULL);
    
    delete hThread;
    
    // Write the final state of the cars
    road->writeCars();
    
    return 0;
}

/*  Helper function so that threading can be used
 */
void * startCar(void * _tgtObject) {
    Car * tgtObject = (Car *) _tgtObject;
    tgtObject->start();
    return NULL;
}

/*  Deconstructor
 */
Sim::~Sim() {
    delete road;
    int numCars = cars.size();
    for(int i=0; i<numCars; i++)
	delete cars[i];
}

#endif
