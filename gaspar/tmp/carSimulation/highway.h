/*
 *  highway.h
 *  carSimulation
 *
 *  Created by Alpha Chen on Thu Apr 22 2004.
 *
 *  See included spec (81GrandPrix.pdf)
 *
 *  2004-04-22  -   Road class created
 */

/*  Just in case I need it later...
 struct RoadData {
    int status;
    Car * carPtr;
    
    RoadData(s=0, c=NULL) : status(s), carPtr(c) {}
    ~RoadData() { carPtr = NULL; }
}
*/

class Road {
private:
    // These variables should be self-explanatory
    string name;
    int numLanes, roadLength;
    int ** roadData;
    vector<Car *> carData;
    
public:
    int check(int lane, int position);
    
    int moveCar(int carID);
    
    // Checks whether a car can move forward or not from
    // the position passed in.
    int checkBarrier(int lane, int position);
    
    // Write the road to cout, showing the cars and
    // barriers.
    int output();   // Outputting to a stream passed in by
		    // an argument may be better...
    
    Road(); // Constructor
    ~Road();    // Destructor
};