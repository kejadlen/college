struct train {
  short slot;         // slot number of train (note: NOT THE TRAIN ID)
  short location;     // location on track
  short sensor_block; // 1 -> in a block, 0 -> in a sensor
  short direction;    // 1 -> forward, 0 -> backwards
  short facing;       // 1 -> clockwise, 0 -> counterclockwise
  short speed;        // not implemented
  struct train *next; // next train (counterclockwise) on the block
};

struct block {
  short id;
  struct train *current_train;  // pointer to most clockwise train on block
};

struct sensor {
  short id;
  struct train *current_train;  // pointer to train obstructing sensor
  struct block *cw_out;         // block where  clockwise-moving        trains will go
  struct block *ccw_out;        // "  "         counterclockwise-moving " "
  struct block *cw_in[2];       // "  "         clockwise-moving        trains come from
  struct block *ccw_in[2];      // "  "         counterclockwise-moving " "
};

/*
 Total number of various items of the state table
 */
int num_trains = 2;
int num_sensors = 24;
int num_blocks = 20;
int num_switches = 12;

/*
 This structure contains all of the trains on the track.
 It can be populated here or in the init_trains() function.
 */
struct train TrainArray[2] =
{
  { 1, -1, -1, 1, 1, 0, 0 },
  { 3, -1, -1, 1, 1, 0, 0 }
};

/*
 This holds all of the blocks on the track.
 */
struct block BlockArray[20] =
{
  { 0, 0 },
  { 1, 0 },
  { 2, 0 },
  { 3, 0 },
  { 4, 0 },
  { 5, 0 },
  { 6, 0 },
  { 7, 0 },
  { 8, 0 },
  { 9, 0 },
  { 10, 0 },
  { 11, 0 },
  { 12, 0 },
  { 13, 0 },
  { 14, 0 },
  { 15, 0 },
  { 16, 0 },
  { 17, 0 },
  { 18, 0 },
  { 19, 0 }
};

/*
 This holds the bulk of the logic of the state table. Sensors have outgoing blocks
 in both directions and incoming blocks from both directions.
 */
struct sensor SensorArray[24] =
{
  { 0, 0, &BlockArray[0], &BlockArray[1], {0, 0}, {&BlockArray[1], 0} },  // Sensor 0
  { 1, 0, &BlockArray[0], &BlockArray[2], {0, 0}, {&BlockArray[2], 0} },
  { 2, 0, &BlockArray[3], 0, {&BlockArray[3], 0}, {0, 0} },
  { 3, 0, &BlockArray[4], 0, {&BlockArray[4], 0}, {0, 0} },
  { 4, 0, &BlockArray[0], &BlockArray[4], {0, 0}, {&BlockArray[4], 0} },  // Sensor 4
  { 5, 0, &BlockArray[2], 0, {&BlockArray[2], 0}, {0, 0} },
  { 6, 0, &BlockArray[0], &BlockArray[3], {&BlockArray[0], 0}, {&BlockArray[0], 0} },
  { 7, 0, &BlockArray[1], 0, {&BlockArray[1], 0}, {0, 0} },
  { 8, 0, 0, &BlockArray[5], {0, 0}, {&BlockArray[5], 0} },
  { 9, 0, &BlockArray[5], &BlockArray[6], {&BlockArray[5], 0}, {&BlockArray[6], 0} }, // Sensor 9
  { 10, 0, &BlockArray[7], &BlockArray[8], {&BlockArray[7], 0}, {&BlockArray[8], 0} },
  { 11, 0, &BlockArray[9], &BlockArray[0], {&BlockArray[9], 0}, {&BlockArray[0], 0} },
  { 12, 0, &BlockArray[10], &BlockArray[9], {0, 0}, {&BlockArray[9], 0} },
  { 13, 0, &BlockArray[6], &BlockArray[11], {&BlockArray[6], &BlockArray[20]}, {0, 0} },
  { 14, 0, &BlockArray[12], &BlockArray[10], {&BlockArray[12], 0}, {&BlockArray[10], &BlockArray[-12]} }, // Sensor 14
  { 15, 0, &BlockArray[13], &BlockArray[12], {&BlockArray[13], 0}, {&BlockArray[12], 0} },
  { 16, 0, &BlockArray[14], &BlockArray[13], {&BlockArray[14], 0}, {&BlockArray[13], 0} },
  { 17, 0, &BlockArray[15], &BlockArray[16], {&BlockArray[15], 0}, {&BlockArray[16], 0} },
  { 18, 0, &BlockArray[8], &BlockArray[17], {&BlockArray[8], 0}, {&BlockArray[17], 0} },
  { 19, 0, &BlockArray[17], &BlockArray[18], {&BlockArray[17], 0}, {&BlockArray[18], 0} },  // Sensor 19
  { 20, 0, &BlockArray[18], &BlockArray[19], {&BlockArray[18], 0}, {&BlockArray[19], 0} },
  { 21, 0, &BlockArray[16], &BlockArray[15], {&BlockArray[16], 0}, {&BlockArray[15], 0} },
  { 22, 0, &BlockArray[19], &BlockArray[7], {&BlockArray[19], 0}, {&BlockArray[7], 0} },
  { 23, 0, &BlockArray[10], &BlockArray[14], {&BlockArray[10], &BlockArray[11]}, {&BlockArray[14], 0} } // Sensor 23 (last)
};

/*
 The state of all of the switches on the track.
 */
char Switches[12] = 
{
  0,0,0,0,0,
  1,1,0,1,0,
  1,1
};

void init_trains( void ) {
  /*
   Train initialization can be set in the state table, or in this function. This
   basically sets up the state table; initial train and switch data should be
   placed here.
   
   To set a train's slot number:
    TrainArray[x].slot = z;
   
   To place a train at a sensor:
    SensorArray[y].current_train = &TrainArray[x];
    TrainArray[x].location = y;
    TrainArray[x].sensor_block = 0;
   
   To place a train at a block:
    BlockArray[y].current_train = &TrainArray[x];
    TrainArray[x].location = y;
    TrainArray[x].sensor_block = 1;

   Don't forget that multiple trains on one block must be linked to each other!
   */
  SensorArray[10].current_train = &TrainArray[0];
  TrainArray[0].location = 10;
  TrainArray[0].slot = 5;
  TrainArray[0].sensor_block = 0;

  BlockArray[10].current_train = &TrainArray[1];
  TrainArray[1].location = 10;
  TrainArray[1].slot = 6;
  TrainArray[1].sensor_block = 1;
}

int going_clockwise(struct train *trainPtr) {
  /*
   Returns:
   trainPtr is going clockwise        -> 1
   trainPtr is going counterclockwise -> 0
   */ 
  return (trainPtr->direction == trainPtr->facing);
}

struct train * get_cw_train(struct block *block_id) {
  /*
   Returns the train on the block given as an argument
   coming from the clockwise direction. This is going
   to be the last train in the linked list of trains.
   */
  struct train *trainPtr, *tempPtr;
  
  if(!block_id) return 0; // Check if the block is valid.
  
  tempPtr = 0;
  
  if((trainPtr = block_id->current_train)) {  // If there are trains at the block,
    while(trainPtr->next) {                   // get the last train.
      tempPtr = trainPtr;
      trainPtr = trainPtr->next;
    }
    if(!going_clockwise(trainPtr)) {    // If the last train is moving in the right direction,
      if(tempPtr) tempPtr->next = 0;    // remove it from the block and return it.
      else block_id->current_train = 0;
      return trainPtr;
    }
  }
  
  return 0;
}

struct train * get_ccw_train(struct block *block_id) {
  /*
   Returns the train on the block given as an argument
   coming from the counterclockwise direction.
   */ 
  struct train *trainPtr;

  if(!block_id) return 0; // Check if the block is valid.
  
  /*
   If the train is going in the right direction, remove it
   from the block and return it.
   */
  if((trainPtr = block_id->current_train) && going_clockwise(trainPtr)) {
    block_id->current_train = trainPtr->next;
    trainPtr->next = 0;
    return trainPtr;
  }

  return 0;
}

struct train * get_train_for_sensor(int sensor_id) {
  /*
   Check all of the input tracks for the given sensor and return
   the first train which is coming to the sensor.
   
   NOTE: This does not check for the possibility of multiple
   trains being able to move towards the given sensor. This is a
   situation for which we don't have enough data. A possible
   solution could be to take the velocities of trains into account
   and using arrival/departure times at sensors to determine which
   train is the most probable.
   */
  struct train *trainPtr = 0;

  if((trainPtr = get_ccw_train(SensorArray[sensor_id].ccw_in[0])));
  else if((trainPtr = get_ccw_train(SensorArray[sensor_id].ccw_in[1])));
  else if((trainPtr = get_cw_train(SensorArray[sensor_id].cw_in[0])));
  else if((trainPtr = get_cw_train(SensorArray[sensor_id].cw_in[1])));

  return trainPtr;
}

void move_train_cw(struct block *block_id, struct train *trainPtr) {
  /*
   Moves the given train clockwise to the given block. The train
   must be placed in the end of the block's linked list of trains.
   */
  struct train * tempPtr;
  
  // Set the metadata.
  trainPtr->location = block_id->id;
  
  if((tempPtr = block_id->current_train)) {       // If there is a train at the block, put the
    while(tempPtr->next) tempPtr = tempPtr->next; // train in the end of the linked list.
    tempPtr->next = trainPtr;
  }
  else {  // There's no train on the block, so just place the train onto the block.
    block_id->current_train = trainPtr;
  }

  //diag_printf("train %d has moved clockwise to block %d\n", trainPtr->id, trainPtr->location);
}

void move_train_ccw(struct block *block_id, struct train *trainPtr) {
  /*
   Moves the given train counterclockwise to the given block.
   */
  
  // Set the metadata.
  trainPtr->location = block_id->id;
  
  trainPtr->next = block_id->current_train; // Put the train in the front
  block_id->current_train = trainPtr;       // of the linked list.

  //diag_printf("train %d has moved counterclockwise to block %d\n", trainPtr->id, trainPtr->location);
}

void move_train_from_sensor(int sensor_id, struct train *trainPtr) {
  /*
   Given a train and a sensor, this places the train into the
   appropriate block.
   */
  if(going_clockwise(trainPtr))
    move_train_cw(SensorArray[sensor_id].cw_out, trainPtr);
  else
    move_train_ccw(SensorArray[sensor_id].ccw_out, trainPtr);

  trainPtr->sensor_block = 1;               // The train is now at a block.
  SensorArray[sensor_id].current_train = 0; // Remove the train from the sensor.
}

void update_sensor(int id, int state) {
  /*
   This is the function that gets called to update the positions
   of the trains in the state table. The id and state of the changed
   sensor is used to find how the state table should be modified.
   */
  struct train *trainPtr;
  
  if(state) { // The sensor has been turned on, so a train is moving into it.
    if(SensorArray[id].current_train)
      diag_printf("Train at sensor %d already!\n", id);
    else if((trainPtr = get_train_for_sensor(id))) {  // Get train from one of
      SensorArray[id].current_train = trainPtr;       // the input blocks and
      trainPtr->location = id;                        // place it in the sensor.
      trainPtr->sensor_block = 0;

      //diag_printf("train %d has moved to sensor %d\n", trainPtr->id, trainPtr->location);
    }
    else
      diag_printf("nothing is coming to sensor %d?!\n", id);
  }
  else // The sensor has turned off, so a train is moving away from it.
    if((trainPtr = SensorArray[id].current_train))
      move_train_from_sensor(id, trainPtr);
    else
      diag_printf("No train is coming from sensor %d!\n", id);
}

void update_switch(int id, int state) {
  /*
   This is called to update the positions of the switches and
   modify the sensor state table's input and output blocks.
   */
  Switches[id] = state;
  
  switch(id) {
    case 0:
      if(!Switches[2]) {
        SensorArray[1].cw_in[0] = (state) ? &BlockArray[0]
                                          : 0;
        SensorArray[6].cw_in[0] = (state) ? 0
                                          : &BlockArray[0];
      }
      break;
    case 1:
      if(Switches[2]) {
        SensorArray[0].cw_in[0] = (state) ? 0
                                          : &BlockArray[0];
        SensorArray[4].cw_in[0] = (state) ? &BlockArray[0]
                                          : 0;
      }
      break;
    case 2:
      SensorArray[0].cw_in[0] = (state && !Switches[1]) ? &BlockArray[0]
                                                        : 0;
      SensorArray[1].cw_in[0] = (!state && Switches[0]) ? &BlockArray[0]
                                                        : 0;
      SensorArray[4].cw_in[0] = (state && Switches[1]) ? &BlockArray[0]
                                                       : 0;
      SensorArray[6].cw_in[0] = (!state && !Switches[0]) ? &BlockArray[0]
                                                         : 0;
      break;
    case 4:
      SensorArray[12].cw_in[0] = (state) ? &BlockArray[10]
                                         : 0;
      SensorArray[23].cw_in[0] = (state) ? 0
                                         : &BlockArray[10];
      break;
    case 5:
      SensorArray[9].ccw_in[0] = (state) ? &BlockArray[6]
                                         : 0;
      SensorArray[13].cw_out = (state) ? &BlockArray[6]
                                       : &BlockArray[20];
      break;
    case 6:
      SensorArray[23].cw_out = (state) ? &BlockArray[10]
                                       : &BlockArray[11];
    case 8:
      SensorArray[20].cw_out = (state) ? &BlockArray[18]
                                       : &BlockArray[13];
      break;
    case 9:
      SensorArray[16].ccw_out = (state) ? &BlockArray[18]
                                        : &BlockArray[13];
      SensorArray[15].cw_in[0] = (state) ? 0
                                         : &BlockArray[13];
      SensorArray[20].cw_in[1] = (state) ? &BlockArray[13]
                                         : 0;
      break;
    case 10:
      SensorArray[19].ccw_out = (state) ? &BlockArray[18]
                                        : &BlockArray[13];
      break;
    case 11:
      SensorArray[15].cw_out = (state) ? &BlockArray[18]
                                       : &BlockArray[13];
      SensorArray[16].ccw_in[0] = (state) ? &BlockArray[13]
                                          : 0;
      SensorArray[19].ccw_in[1] = (state) ? 0
                                          : &BlockArray[13];
      break;
    default: break;
  }
  
  //diag_printf("switch %d -> state %d\n", id, state);
}

void update_train(int slot, int direction) {
  /*
   Updates the train's direction; used to see if the train is
   going clockwise or counterclockwise on the track.
   */
  int i;
  
  for( i=0; i<num_trains; i++)
    if(TrainArray[i].slot == slot)
      TrainArray[i].direction = direction;

  //diag_printf("train %d -> direction %d\n", id, direction);
}
