#include <stdio.h>
#include <cyg/hal/hal_diag.h>
#include <cyg/hal/hal_intr.h>
#include <cyg/infra/diag.h>
#include <cyg/infra/cyg_type.h>
#include <cyg/hal/drv_api.h>
#include <parts/m55800/lib_m55800.h>
#include <parts/m55800/reg_m55800.h>
#include <targets/eb55/eb55.h>

/*
 train_init.c has all of the state table specifics
 
 The following functions are used to interface with
 the state table, for modularity:
 
 update_sensor(int id, int state);
  id:     sensor id
  state:  0 -> off, 1 -> on
 update_switch(int id, int state);
  id:     switch id
  state:  0 -> thrown, 1 -> closed
 update_train(int slot, int direction);
  slot:   train slot id
  state:  0 -> backwards, 1 -> forwards
 */
#include "train_init.c"

static u_int timer_counter;
static u_int clock_select;

volatile read_buffer[0xFF];
u_int x = 0;

cyg_uint32 usart_read( cyg_vector_t vector, cyg_addrword_t data ) {
  /*
   Interrupt service routine for USART receive. This gets the current byte
   on the line and places it into the buffer. Every time a byte is received,
   we check the buffer for a complete packet and modify the state table 
   using the update_X() functions if required.
   */
  UsartDesc *usart = &(USART1_DESC);
  u_int in1, in2, object_id, object_state;
  
  at91_usart_read(usart, &read_buffer[x]);  // Get the byte from the USART.
  x++;                                      // Increment byte counter.

  if (x > 3) {  // All packets we care about must be at least 4 bytes long.
    if (read_buffer[x-4] == 0xB2){  // Check for a sensor information packet.
      in1 = read_buffer[x-3];       // Get other packet info
      in2 = read_buffer[x-2];
      //check_sum is read_buffer[x-1] -- TODO: Add code to check this.

      // IN1 = <0, A6, A5, A4, - A3, A2, A1, A0>
      // IN2 = <0,  X,  I, L, - A10, A9, A8, A7>
      
      /* Code for reading sensor data from Zach Burlingame */
      // Sensor num = <0, 0, 0, 0, - A10, A9, A8, A7, - A6, A5, A4, A3, - A2, A1, A0, I>
      object_id = ( (( ( (in2 & 0x0F)<<7)+(in1 & 0x7F) )<<1) + ( ( in2 & 0x20) >> 5) ) ;
      
      // State = <0, 0, 0, L>
      // L = 0 -> no train, 1 -> train tripped sensor
      object_state = ((in2 & 0x10) >> 4);
      
      update_sensor(object_id, object_state);

      x=0;  // Reset byte counter.
    }
    
    if (read_buffer[x-4] == 0xB0) { // Check for a packet that is setting a switch.
      /*
       Here, we attempted to disable interrupts for a short period because setting
       switches also sets off spurious sensor data.
      
       // Disable interrupts.
       usart->usart_base->US_IER = 0;
       usart->usart_base->US_IDR = US_RXRDY|US_TXRDY|US_RXBRK|US_ENDRX|US_ENDTX|US_OVRE|US_FRAME|US_PARE|US_TIMEOUT|US_TXEMPTY;

       HAL_DISABLE_INTERRUPTS(save);
       */

      // IN1 = <0, A6, A5, A4, - A3, A2, A1, A0>
      // IN2 = <0,  0,DIR, ON, - A10, A9, A8, A7>
      in1 = read_buffer[x-3];
      in2 = read_buffer[x-2];

      // 12 switches, thus highest switch number is 11, only use A3-A0
      object_id = (in1 & 0x0F);

      // state = <0, 0 ,0 , DIR>
      // DIR = 1 -> closed, 0 -> thrown/open
      object_state = ((in2 & 0x20) >> 5);

      update_switch(object_id, object_state);

      /*
       Here is the delay for debouncing should be, if interrupts can
       be disabled:
       
       for (i=0; i<10; i++)
         hal_delay_us(50000);

       // Reset interrupts.
       usart->usart_base->US_IDR = 0;
       usart->usart_base->US_IER = US_RXRDY;

       HAL_RESTORE_INTERRUPTS(save);

       HAL_ENABLE_INTERRUPTS();
       timer->tc_base->TC_IER = TC_CPCS;
       at91_tc_trig_cmd(timer,TC_TRIG_CHANNEL);
       usart->usart_base->US_IER = US_RXRDY;//US_MASK_IRQ_RX;
       */

      x=0;  // Reset byte counter.
    }
    
    if (read_buffer[x-4] == 0xA1) { // Check for a packet changing the direction of a train
      // IN1 = <SLOT#>
      // IN2 = <0, 0, DIR, LIG, F4, F3, F2, F1>
      in1 = read_buffer[x-3];
      in2 = read_buffer[x-2];

      object_id = in1;  // Identify train.

      /*
       According to the spec, 1 -> forward, 0 -> backward
      
       NOTE: The LocoNet spec seems to be wrong, hence the
       negation of the bit for update_train()'s sake.
       */
      object_state = !((in2 & 0x20) >> 5);
      
      update_train(object_id, object_state);

      x=0;  // Reset byte counter.
    }
  }
  
  cyg_drv_interrupt_acknowledge(vector);
  return(CYG_ISR_HANDLED);
}

cyg_uint32 output_train_location( cyg_vector_t vector, cyg_addrword_t data ) {
  /*
   Interrupt service routine for the timer. Every half-second, all
   of the train slots, directions, and locations are outputted.
   */
  TCDesc *timer = &(TC3_DESC);
  int i;
   
  for( i=0; i<num_trains; i++) {  // Print info for each of the trains.
    diag_printf("Train %d is at ", TrainArray[i].slot);
    if(TrainArray[i].sensor_block) diag_printf("block ");
    else diag_printf("sensor ");
    diag_printf("%d going ", TrainArray[i].location);
    if(going_clockwise(&TrainArray[i])) diag_printf("clockwise\n");
    else diag_printf("counterclockwise\n");
  }
  diag_printf("\n");
   
  cyg_drv_interrupt_acknowledge(vector);
  at91_tc_get_status(timer);  // Reset timer.
  return(CYG_ISR_HANDLED);
}

void init_timer( void ) {
  /*
   Initializes the timer to run every half-second.
   */
  TCDesc *timer = &(TC3_DESC);
  u_int timer_regs[4];

  diag_printf("Setting up timer...\n");
  
  timer_counter = 32000000 >> 11;
  if (timer_counter >= (1<<16)) {
    diag_printf("Timer value (%d) too large!\n",timer_counter);
    return;
  }

  clock_select = TC_CLKS_MCK1024;
  timer_regs[RA] = 0;
  timer_regs[RB] = timer_counter;
  timer_regs[RC] = timer_counter;
  
  at91_tc_open(timer,TC_WAVE | TC_CPCTRG,FALSE,FALSE);
  at91_tc_set_mode(timer,clock_select,1);
  at91_tc_write(timer,timer_regs);
    
  diag_printf("Finished setting up timer\n");
}

void init_usart( void ) {
  /*
   Initializes the usart for LocoNet reading.
   */
  UsartDesc *usart = &(USART1_DESC);
  
  diag_printf("Setting up USART...\n");
  
  /*
   set up USART for 1 start, 8 data, and 1 stop bit with no parity check
   clock calculation: 32M / 8 / 16457k = 15
   */
  at91_usart_open(usart, US_CLKS_MCK8 | US_CHRL_8 | US_PAR_NO | US_NBSTOP_1 | US_CHMODE_NORMAL, 15, 0);
  
  diag_printf("Finished setting up USART\n");
}

void init_interrupts( void ) {
  /*
   Initialize the interrupts for the timer and usart.
   */
  TCDesc *timer = &(TC3_DESC);
  cyg_interrupt tc_interrupt_info;
  cyg_handle_t tc_handle;
  
  UsartDesc *usart = &(USART1_DESC);
  cyg_interrupt uc_interrupt_info;
  cyg_handle_t uc_handle;
  
  diag_printf("Setting up interrupts...\n");
  
  // timer
  cyg_drv_interrupt_create(9,7,NULL,output_train_location,NULL,&tc_handle,&tc_interrupt_info);
  cyg_drv_interrupt_attach(tc_handle);
  cyg_drv_interrupt_configure(9,false,false);
  cyg_drv_interrupt_unmask(9);
  
  // usart1
  cyg_drv_interrupt_create(3,7,NULL,usart_read,NULL,&uc_handle,&uc_interrupt_info);
  cyg_drv_interrupt_attach(uc_handle);
  cyg_drv_interrupt_configure(3,false,false);
  cyg_drv_interrupt_unmask(3);
  
  diag_printf("Finished setting up interrupts\n");
  
  HAL_ENABLE_INTERRUPTS();
  
  timer->tc_base->TC_IER = TC_CPCS;
  at91_tc_trig_cmd(timer,TC_TRIG_CHANNEL);

  usart->usart_base->US_IER = US_RXRDY;
}

void cyg_user_start( void ) {
  init_timer();
  init_usart();
  init_trains();
  init_interrupts();
  
  // put the system in idle mode permanently
  for (;;) {
    at91_clock_set_mode(PS_MODE_IDLE);
  }

  return;
}
