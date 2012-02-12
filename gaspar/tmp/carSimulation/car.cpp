/*
 *  car.cpp
 *  carSimulation
 *
 *  Created by Alpha Chen on Fri Apr 23 2004.
 *
 *  2004-04-25
 *      Finished
 *
 *  2004-04-23
 *      car.cpp started
 *
 */

/*  Constructor -- takes license number, pointer
 *  to the road, and the speed of the car
 */
Car::Car(string l, Road * r, int s) {
    carID = 0; // carID will be changed later
    
    // license has a max length of 8
    if(l.size() < 9)
	license = l;
    else
	license = l.substr(0,8);
    
    roadPtr = r;
    directionPref = rand() % 2; // Randomly set initial preferred direction
    
    odometer = 0;
    canProgress = 1;
    speed = s;
} 

/*  start() moves the car until it reaches the end
 *  of the road or stops
 */
int Car::start() {
    // Holds information on what is around the car
    int forward, curDir, otherDir;
//    Car * carPtr = NULL;

//    canProgress = 1;
    
    while(directionPref != -1) {
//	cout<<license<<" moving, preferred direction is: "<<directionPref<<endl;
	
	// Enforce mutual exclusion -- try to
	// get a handle on the road.
	roadPtr->lock();
	
	forward = roadPtr->moveCar(carID, 0);   // Try going forward
	
	if(forward) {   // Couldn't go forward
	    if(!roadPtr->canProgress(carID)) { // Can't go past this position
		roadPtr->moveCar(carID, 3); // Car is now a barrier on the road
		stop(); // Stop the car -- don't waste any more CPU cycles
	    }
	    else {
		curDir = roadPtr->moveCar(carID, directionPref+1);
		if(curDir) {
		    directionPref = !directionPref;
		    otherDir = roadPtr->moveCar(carID, directionPref+1);
		}
	    }
	}
	
	/*  My attempt at not letting the cars see more than one space
	 *
	// Try moving forward
	forward = roadPtr->moveCar(carID, 0);
    
	// If there is not a barrier in front of
	// the car, it can progress eventually.
	if(!canProgress && forward != -1) canProgress = 1;
	
	if(forward) { // The car could not go forward
	    
	    // Try moving in the preferred direction
	    curDir = roadPtr->moveCar(carID, directionPref+1);

	    if(curDir) { // Could not move in preferred direction

		if(curDir == -1) { // A barrier was in the way

		    // Stop the car if it cannot go forward on the road
		    if(!canProgress) {
			roadPtr->moveCar(carID, 3); // Car is now a barrier on the road
			stop(); // Stop the car -- don't waste any more CPU cycles
		    }

		    else if(forward == -1)  // The car has hit a barrier and hasn't
			canProgress = 0;    // found a way to move forward yet
		    
		} // end if(curDir == -1)
		
		else { // A car is blocking
		    // Prepare to talk to the blocking car
		    carPtr = roadPtr->getCar(carID, directionPref);
		    
		    if(carPtr->getProgress())
			canProgress = 1;
		    
		    // Hits another car which hasn't found a way to go forward yet
		    else if(carPtr->getDirection() != directionPref) {
			// No forward progress can be made from either direction...
			// so stop the current car and blocking car
			if(!canProgress) {
			    roadPtr->moveCar(carID, 3);
			    stop();
			    roadPtr->moveCar(carPtr->getID(), 3);
			    carPtr->stop();
			}

			// No forward progress from the other direction...
			else if(forward == -1)
			    canProgress = 0;
			
		    } // end if(carPtr->getDirection()...
		    
		} // end else
		
		directionPref = !directionPref; // Switch preferred direction
		
		// Try the other direction
		otherDir = roadPtr->moveCar(carID, directionPref+1);
		
		if(otherDir) { // Could not move in new preferred direction

		    if(otherDir == -1) {
			// If barriers surround the car
			if(forward == -1 && curDir == -1) {
			    roadPtr->moveCar(carID, 3);
			    stop();
			}
		    }
		    
		    // Do the same as above when blocked by another car...
		    else {
			carPtr = roadPtr->getCar(carID, directionPref);

			if(!carPtr->getProgress() && carPtr->getDirection() != directionPref) {
				if(!canProgress) {
				    roadPtr->moveCar(carID, 3);
				    stop();
				    roadPtr->moveCar(carPtr->getID(), 3);
				    carPtr->stop();
				}
			    
			} // end if(carPtr->getDirection()...
			
		    } // end else if(otherDir > 0)
		    
		} // end if(otherDir)
		
	    } // end if(curDir)
	    
	} // end if(forward)

	if(!canProgress && forward != -1) canProgress = 1;
	 */
	
	// Output the current status of the road
	roadPtr->writeRoad();
	
	// Enforcing mutual exclusion -- other
	// cars can now move on the road.
	roadPtr->unlock();
	
	// Wait depending on speed (from 0.25/speed to 2.5/speed seconds)
	usleep(2500 * (rand() % 1000 + 1) / speed);

    } // end while(directionPref != -1)
    
    return 0;
}

/*  stop() tells the road that the car has been stopped
 *  and also stops the start() function by making
 *  directionPref -1.
 */
int Car::stop() {
    directionPref = -1; // Stops the car from doing anything else
    return 0;
}
