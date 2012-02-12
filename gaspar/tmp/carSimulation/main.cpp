#include <iostream>
#include <iomanip>
#include <vector>
#include <string>

#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>

using namespace std;

#include "road.h"
#include "car.h"
#include "sim.h"

//extern "C" void * startCar(void *);

int main (int argc, char * const argv[]) {
    
    Sim * newSim = new Sim();
    
    newSim->startCars();
    
    delete newSim;
    
    return 0;
}