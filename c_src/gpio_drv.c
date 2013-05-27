/****** BEGIN COPYRIGHT *******************************************************
 *
 * Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 ****** END COPYRIGHT ********************************************************/

//
// gpio_drv.c
//

#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <errno.h>
#include <unistd.h>
#include <stdbool.h>
#include <poll.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <sys/mman.h>

#if USE_EPOLL 
#define MAX_EPOLL_EVENTS 64 
// if driver_event is not working, maybe kernel events was enabled?
#include <sys/epoll.h>
#endif

#include "erl_driver.h"


#define ATOM(NAME) am_ ## NAME
#define INIT_ATOM(NAME) am_ ## NAME = driver_mk_atom(#NAME)
#define DECL_ATOM(NAME) ErlDrvTermData am_ ## NAME

// Hack to handle R15 driver used with pre R15 driver
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif

#define PORT_CONTROL_BINARY

#define INT_EVENT(e) ((int)((long)(e)))

// Port commands
// MUST BE EQUAL TO DEFINES IN gpio.erl !!!!
#define CMD_INIT          1
#define CMD_SET           2
#define CMD_CLR           3
#define CMD_GET           4
#define CMD_SET_DIRECTION 5
#define CMD_GET_DIRECTION 6
#define CMD_SET_MASK      7
#define CMD_CLR_MASK      8
#define CMD_RELEASE       9
#define CMD_SET_INTERRUPT 10
#define CMD_GET_INTERRUPT 11
#define CMD_DEBUG_LEVEL   12
#define CMD_DUMP          13

// Direct access
#define DIRECT_ACCESS_OFF 0
#define DIRECT_ACCESS_ON  1

#include "gpio_drv.h"

// define possible chipsets here
extern gpio_methods_t gpio_bcm2835_meth;

typedef struct gpio_pin_t
{
    uint8_t  pin_reg;
    uint8_t  pin;
    bool direct; // Not needed ??
    struct gpio_pin_t* next;   // when linked    
    ErlDrvEvent fd;   // To /sys/class/gpio/gpioX/value
    gpio_interrupt_t interrupt;
    ErlDrvTermData target;     // interrupt target
} gpio_pin_t;

typedef struct _gpio_ctx_t
{
    ErlDrvPort port;
    gpio_pin_t *reg0[32]; // Pin data for pins in reg 0
    gpio_pin_t *reg1[32]; // Pin data for pins in reg 1
    gpio_pin_t *first;    // Pins not in reg 0 and 1
    bool auto_create;     // Auto export/create register
    gpio_chipset_t chipset;
    gpio_methods_t* meth;         // pin register mask access
    volatile uint32_t *gpio_reg;  //Pointer to physical gpio memory
    uint32_t reg0_direct_pins; // Mask for pins with direct access
    uint32_t reg1_direct_pins; // Mask for pins with direct access
    ErlDrvEvent epollfd;
} gpio_ctx_t;

//--------------------------------------------------------------------
static int  gpio_drv_init(void);
static void gpio_drv_finish(void);
static void gpio_drv_stop(ErlDrvData);
static void gpio_drv_output(ErlDrvData, char*, ErlDrvSizeT);
static void gpio_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void gpio_drv_ready_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData gpio_drv_start(ErlDrvPort, char* command);
static ErlDrvSSizeT gpio_drv_ctl(ErlDrvData,unsigned int,char*,ErlDrvSizeT,char**, ErlDrvSizeT);
static void gpio_drv_timeout(ErlDrvData);
static void gpio_drv_stop_select(ErlDrvEvent, void*);

#define push_atom(atm) do {			\
	message[i++] = ERL_DRV_ATOM;		\
	message[i++] = (atm);			\
    } while(0)

#define push_nil() do {			\
	message[i++] = ERL_DRV_NIL;	\
    } while(0)

#define push_string(str) do {			\
	message[i++] = ERL_DRV_STRING;		\
	message[i++] = (ErlDrvTermData) (str);	\
	message[i++] = strlen(str);		\
    } while(0)

#define push_int(val) do {			\
	message[i++] = ERL_DRV_INT;		\
	message[i++] = (val);			\
    } while(0)

#define push_tuple(n) do {			\
	message[i++] = ERL_DRV_TUPLE;		\
	message[i++] = (n);			\
    } while(0)

#define push_list(n) do {			\
	message[i++] = ERL_DRV_LIST;		\
	message[i++] = (n);			\
    } while(0)


DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(undefined);
DECL_ATOM(gpio_interrupt);

static int del_interrupt(gpio_ctx_t* ctx, gpio_pin_t* gp);
static int add_interrupt(gpio_ctx_t* ctx, gpio_pin_t* gp);

static ErlDrvEntry gpio_drv_entry;

// keep the last errno here, errno will be destroyed in debug logging
// and other system calls otherwise, this errno should reflect the actual
// error.
static int gpio_errno = 0;

int gpio_debug_level = DLOG_DEFAULT;

void gpio_emit_log(int level, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    if ((level == DLOG_EMERGENCY) ||
	((gpio_debug_level >= 0) && (level <= gpio_debug_level))) {
	va_start(ap, line);
	fmt = va_arg(ap, char*);
	fprintf(stderr, "%s:%d: ", file, line); 
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\r\n");
	va_end(ap);
    }
}

//--------------------------------------------------------------------
// Conversion
//--------------------------------------------------------------------
static inline uint32_t get_uint32(uint8_t* ptr)
{
    uint32_t value = (ptr[0]<<24) | (ptr[1]<<16) | (ptr[2]<<8) | (ptr[3]<<0);
    return value;
}

static inline uint16_t get_uint16(uint8_t* ptr)
{
    uint16_t value = (ptr[0]<<8) | (ptr[1]<<0);
    return value;
}

static inline uint8_t get_uint8(uint8_t* ptr)
{
    uint8_t value = (ptr[0]<<0);
    return value;
}

static inline int8_t get_int8(uint8_t* ptr)
{
    uint8_t value = (ptr[0]<<0);
    return (int8_t) value;
}

static inline void put_uint16(uint8_t* ptr, uint16_t v)
{
    ptr[0] = v>>8;
    ptr[1] = v;
}

static inline void put_uint32(uint8_t* ptr, uint32_t v)
{
    ptr[0] = v>>24;
    ptr[1] = v>>16;
    ptr[2] = v>>8;
    ptr[3] = v;
}

//--------------------------------------------------------------------
// Support functions
//--------------------------------------------------------------------
//--------------------------------------------------------------------
// Create new gpio pin first in list 
//--------------------------------------------------------------------
static gpio_pin_t* create_pin(gpio_ctx_t* ctx, 
			      uint8_t pin_reg, 
			      uint8_t pin)
{
    gpio_pin_t* gp;

    DEBUGF("create_pin: pin %d:%d", pin_reg, pin);

    if ((gp = driver_alloc(sizeof(gpio_pin_t))) == NULL) {
	gpio_errno = ENOMEM;
	return NULL;
    }

    gp->pin_reg = pin_reg;
    gp->pin = pin;
    gp->direct = false;
    gp->interrupt = 0;
    gp->fd = NULL;
    gp->next = NULL;

    // If register 0 or 1 use predefined arrays
    // otherwise put in linked list
    if ((pin_reg == 0) && (pin < 32)) 
	ctx->reg0[pin] = gp;
    else if ((pin_reg == 1) && (pin < 32)) 
	ctx->reg1[pin] = gp;
    else {
	DEBUGF("create_pin: adding pin %d:%d to linked list", 
	       pin_reg, pin);
	gp->next = ctx->first;
	ctx->first = gp;
    }

    return gp;
}

//--------------------------------------------------------------------
// Find gpio pin in list
//--------------------------------------------------------------------
static gpio_pin_t* find_pin(gpio_ctx_t* ctx, 
			    uint8_t pin_reg, 
			    uint8_t pin,
			    gpio_pin_t*** gppp)
{
    gpio_errno = EINVAL; // If not found

    DEBUGF("find_pin: pin %d:%d", pin_reg, pin);

    // If register 0 or 1 use predefined arrays
    // otherwise look in linked list
    if ((pin_reg == 0) && (pin < 32)) {
	if (gppp) *gppp = &ctx->reg0[pin];
	return ctx->reg0[pin];
    }
    else if ((pin_reg == 1) && (pin < 32)) {
	if (gppp) *gppp = &ctx->reg1[pin];
	return ctx->reg1[pin];
    }
    else {
	gpio_pin_t** gpp = &ctx->first;
	while(*gpp) {
	    gpio_pin_t* gp = *gpp;
	    if ((gp->pin_reg == pin_reg) && (gp->pin == pin)) {
		if (gppp) *gppp = gpp; 
		return gp;
	    }
	    gpp = &gp->next;
	}
    }

    return NULL;
}

//--------------------------------------------------------------------
// Utility to write a string to a file and then close it
//--------------------------------------------------------------------
static int write_value(char* fpath, int pin, char* value)
{
    char path[128];
    int n;
    int fd;

    if (snprintf(path, sizeof(path), fpath, pin) >= sizeof(path)) {
        DEBUGF("Failed to format %s: reason , too long", fpath);
	gpio_errno = EINVAL;
	return GPIO_NOK; // format too long
    }
    if ((fd = open(path, O_WRONLY)) < 0) {
	gpio_errno = errno;
        DEBUGF("Failed to open %s: reason, %s", path, strerror(errno));
	return GPIO_NOK;
    }
    n = strlen(value);
    if (write(fd, value, n) != n) {
	gpio_errno = errno;
        DEBUGF("Failed to write %s: reason, %s", value, strerror(errno));
	close(fd);
	return GPIO_NOK;
    }
    close(fd);
    return GPIO_OK;
}


//--------------------------------------------------------------------
// Check if pin already is exported
//--------------------------------------------------------------------
static int is_exported(int pin)
{
    char path[128];
    char *dirname = "/sys/class/gpio/gpio%d";
    struct stat st;

    if (snprintf(path, sizeof(path), dirname, pin) >= sizeof(path))
	return -1;
    if (stat(path, &st) < 0) {
	gpio_errno = errno;
	if (errno == ENOENT)
	    return 0;
	return -1;
    }
    if (st.st_mode & S_IFDIR)
	return 1;
    return -1;
}

//--------------------------------------------------------------------
// Write in the export file that we can use to ask the kernel
// to export control to us. See kernel/Documentation/gpio.txt
//--------------------------------------------------------------------
static int export(int pin)
{
    char value[16];

    sprintf(value, "%d", pin);
    return write_value("/sys/class/gpio/export", 0, value);
}

//--------------------------------------------------------------------
// Write in the unexport file that we can use to ask the kernel
// to retreive control from us. See kernel/Documentation/gpio.txt
//--------------------------------------------------------------------
static int unexport(int pin)
{
    char value[16];

    sprintf(value, "%d", pin);
    return write_value("/sys/class/gpio/unexport", 0, value);
}

//--------------------------------------------------------------------
// Open the value file, used for input/ouptput. 
// See kernel/Documentation/gpio.txt
//--------------------------------------------------------------------
static int open_value_file(int pin)
{
    int fd = -1;
    char *fname = "/sys/class/gpio/gpio%d/value";
    char path[128];

    // Generate a correct path to the file
    if (snprintf(path, sizeof(path), fname, pin) >= sizeof(path)) {
	gpio_errno = EINVAL;
	return -1;
    }
    if ((fd = open(path, O_RDWR)) < 0) {
	gpio_errno = errno;
        DEBUGF("Failed to open %s: %s", path, strerror(errno));
    }
    else {
	gpio_errno = errno;
	DEBUGF("Value file %s has fd %d", path, fd);
    }
    return fd; 
}

//--------------------------------------------------------------------
// Initialize with files always
//--------------------------------------------------------------------
static gpio_pin_t* init_pin(gpio_ctx_t* ctx, 
			    int pin_reg, 
			    int pin) 
{
    gpio_pin_t* gp = NULL;
    int fd = -1;
    gpio_errno = EINVAL;

    DEBUGF("init_pin: pin %d:%d", pin_reg, pin);

    //If pin not already exported, export it
    switch(is_exported(pin)) {
    case -1:
	return NULL;
    case 0:
	// Tell linux we will take over pin
	if (export(pin) != GPIO_OK)
	    return NULL;
	break;
    case 1:
	break;
    }

    // Prepare value file
    if((fd = open_value_file(pin)) < 0)
	return NULL;

    if ((gp=create_pin(ctx, pin_reg, pin)) == NULL)
	close(fd);
    gp->fd = (ErlDrvEvent) fd;

    // Set default interrupt target to process that created the pin struct
    gp->target = driver_caller(ctx->port);

    DEBUGF("init_pin: pin %d:%d initialized", pin_reg, pin);

    return gp;
}

//--------------------------------------------------------------------
// Go through necessary release steps
//--------------------------------------------------------------------
static int release_pin(gpio_ctx_t* ctx, int pin_reg, int pin) 
{
    gpio_pin_t* gp;
    gpio_pin_t** gpp;

    DEBUGF("release_pin: pin %d:%d", pin_reg, pin);
    if (unexport(pin) != GPIO_OK)
	return GPIO_NOK;
    
    if ((gp=find_pin(ctx, pin_reg, pin, &gpp)) == NULL)
	return GPIO_OK; // or badarg ???
    if (gp->interrupt) {
	del_interrupt(ctx, gp);
	gp->interrupt = 0;
    }
    // async close the file
    driver_select(ctx->port, gp->fd, ERL_DRV_USE, 0);
    *gpp = gp->next; // unlink
    driver_free(gp);

    DEBUGF("release_pin: pin %d:%d released", 
	   pin_reg, pin);

    return GPIO_OK;
}
//--------------------------------------------------------------------
// Write pin direction in direction file
//--------------------------------------------------------------------
static int gpio_set_direction_indirect(gpio_pin_t* gp, 
				       gpio_direction_t direction)
{
    char* value = "";

    switch(direction) {
    case gpio_direction_in:  value = "in"; break;
    case gpio_direction_out: value = "out"; break;
    case gpio_direction_low: value = "low"; break;
    case gpio_direction_high: value = "high"; break;
    default:
	gpio_errno = EINVAL;
	return GPIO_NOK;
    }
    if (write_value("/sys/class/gpio/gpio%d/direction", gp->pin, value) < 0)
	return GPIO_NOK;
    DEBUGF("Wrote direction %s", value);

    return GPIO_OK;
}

//--------------------------------------------------------------------
// Write pin direction
//--------------------------------------------------------------------
static int gpio_set_direction(gpio_ctx_t* ctx, 
			      gpio_pin_t* gp, 
			      gpio_direction_t direction)
{
    int result;

    DEBUGF("set direction to %d on pin %d:%d", 
	   direction, gp->pin_reg, gp->pin);

    if (gp->direct)
	result = (*ctx->meth->set_direction)(ctx->gpio_reg,
					     gp->pin_reg, gp->pin, direction);
    else 
	result = gpio_set_direction_indirect(gp, direction);

    return result;
}

//--------------------------------------------------------------------
// Get pin state from value file
//--------------------------------------------------------------------
static int gpio_get_direction_indirect(gpio_pin_t* gp, 
				       gpio_direction_t *dp)
{
    char path[128];
    char *fpath = "/sys/class/gpio/gpio%d/direction";
    int n;
    int fd;
    char direction[11];

    if (snprintf(path, sizeof(path), fpath, gp->pin) >= sizeof(path)) {
        DEBUGF("Failed to format %s: reason , too long", fpath);
	gpio_errno = EINVAL;
	return GPIO_NOK; // format too long
    }

    if ((fd = open(path, O_RDONLY)) < 0) {
	gpio_errno = errno;
        DEBUGF("Failed to open %s: reason, %s", path, strerror(errno));
	return GPIO_NOK;
    }

    lseek(INT_EVENT(fd), 0, SEEK_SET);

    n = read(INT_EVENT(fd), &direction, 10);
    gpio_errno = errno; // If needed
    close(fd);

    if (n < 1) return GPIO_NOK;
    direction[n] = '\0';

    switch(direction[0]) {
    case 'i': 
	if (strcmp(direction,"in\n") == 0) *dp = gpio_direction_in; 
	break;
    case 'o': 
	if  (strcmp(direction,"out\n") == 0) *dp = gpio_direction_out; 
	break;
    case 'l': 
	if  (strcmp(direction,"low\n") == 0) *dp = gpio_direction_low; 
	break;
    case 'h': 
	if  (strcmp(direction,"high\n") == 0) *dp = gpio_direction_high; 
	break;
    default:
        DEBUGF("Illegal direction %s.", direction);
	*dp = gpio_direction_undef;
	gpio_errno = EINVAL;
	return GPIO_NOK;
    }

    DEBUGF("Read direction %d for pin %d:%d from file %d.", 
	   *dp, gp->pin_reg, gp->pin, fd);
    return GPIO_OK;
}

//--------------------------------------------------------------------
// Read pin direction
//--------------------------------------------------------------------
static int gpio_get_direction(gpio_ctx_t* ctx, 
			      gpio_pin_t* gp, 
			      gpio_direction_t* dp)
{
    int result;

    DEBUGF("Get direction for pin %d:%d", gp->pin_reg, gp->pin);

    if (gp->direct)
	result = (*ctx->meth->get_direction)(ctx->gpio_reg,
					     gp->pin_reg, gp->pin, dp);
    else 
	result = gpio_get_direction_indirect(gp, dp);

    return result;
}

//--------------------------------------------------------------------
// Find pin or create it with correct direction
//--------------------------------------------------------------------
static gpio_pin_t* find_or_create_pin(gpio_ctx_t* ctx, 
				      uint8_t pin_reg, 
				      uint8_t pin,
				      gpio_direction_t direction)
{
    gpio_pin_t* gp = NULL;

    if ((gp=find_pin(ctx, pin_reg, pin, NULL)) == NULL) {
	if (!ctx->auto_create) 
	    return NULL;
	// We should create it
	if ((gp=init_pin(ctx, pin_reg, pin)) == NULL)
	    return NULL;
	if (gpio_set_direction(ctx, gp, direction) != GPIO_OK)
	    return NULL;
    }
    return gp;
}

//--------------------------------------------------------------------
// Set pin state by writing to value file
//--------------------------------------------------------------------
static int gpio_set_indirect(gpio_pin_t* gp, gpio_state_t state)
{
    int fd;

    fd = INT_EVENT(gp->fd);
    switch(state) {
    case gpio_state_low:
	DEBUGF("Writing low to value file fd %d", fd);
        if (write(fd, "0", 1) < 1) {
	    gpio_errno = errno;
	    return GPIO_NOK;
	}
        return GPIO_OK;

    case gpio_state_high:
	DEBUGF("Writing high to value file fd %d", fd);
        if (write(fd, "1", 1) < 1) {
	    gpio_errno = errno;
	    return GPIO_NOK;
	}
        return GPIO_OK;

    default:
	gpio_errno = EINVAL;
        break;
    }
    return GPIO_NOK;
}

//--------------------------------------------------------------------
// Set pin state 
//--------------------------------------------------------------------
static int gpio_set_state(gpio_ctx_t* ctx, 
			  gpio_pin_t* gp, 
			  gpio_state_t state)
{
    DEBUGF("Changing state to %d on pin %d:%d", 
	   state, gp->pin_reg, gp->pin);

    if (gp->direct) {
	uint32_t mask = (1 << gp->pin);
	if (state == gpio_state_high)
	    return (*ctx->meth->set_mask)(ctx->gpio_reg, gp->pin_reg, mask);
	else
	    return (*ctx->meth->clr_mask)(ctx->gpio_reg, gp->pin_reg, mask);
    }
    else 
	return gpio_set_indirect(gp, state);
}

//--------------------------------------------------------------------
// Get pin state from value file
//--------------------------------------------------------------------
static int gpio_get_indirect(gpio_pin_t* gp,
			     uint8_t* sp)
{
    char state;
    
    // Read state from value file
    lseek(INT_EVENT(gp->fd), 0, SEEK_SET);
    if (read(INT_EVENT(gp->fd), &state, 1) != 1) {
	gpio_errno = errno;
	return GPIO_NOK;
    }
    
    // Transform from char to uint
    *sp = state - '0';
    if (*sp > 1) {
	// Invalid state
	gpio_errno = EINVAL;
	return GPIO_NOK;
    }
    DEBUGF("Read state %d for pin %d:%d from file %d.", 
	   *sp, gp->pin_reg, gp->pin, gp->fd);
    return GPIO_OK;

}

//--------------------------------------------------------------------
// Get pin state 
//--------------------------------------------------------------------
static int gpio_get_state(gpio_ctx_t* ctx, 
			  gpio_pin_t* gp,
			  uint8_t* sp)
{
    DEBUGF("Get state for pin %d:%d", gp->pin_reg, gp->pin);

    if (gp->direct) {
	uint32_t mask = (*ctx->meth->get_mask)(ctx->gpio_reg,gp->pin_reg);
	*sp = (mask >> gp->pin) & 1;
	return GPIO_OK;
    }
    else 
	return gpio_get_indirect(gp, sp);
}

//--------------------------------------------------------------------
// Add pin to direct access mask
//--------------------------------------------------------------------
static int set_pin_in_mask(gpio_ctx_t* ctx, 
			   uint8_t pin_reg, 
			   uint8_t pin)
{
    if ((pin_reg > 1) || (pin > 31)) {
	gpio_errno = EINVAL;
	return GPIO_NOK;
    }
    if (pin_reg == 0)
	ctx->reg0_direct_pins |= 1 << pin;
    else if (pin_reg == 1)
	ctx->reg1_direct_pins |= 1 << pin;
    else 
	return GPIO_NOK;

    return GPIO_OK;
}
//--------------------------------------------------------------------
// Remove pin from direct access mask
//--------------------------------------------------------------------
static int clr_pin_in_mask(gpio_ctx_t* ctx, 
			   uint8_t pin_reg, 
			   uint8_t pin)
{
    if ((pin_reg > 1) || (pin > 31)) {
	gpio_errno = EINVAL;
	return GPIO_NOK;
    }
    if (pin_reg == 0)
	ctx->reg0_direct_pins &= ~(1 << pin);
    else if (pin_reg == 1)
	ctx->reg1_direct_pins &= ~(1 << pin);
    else 
	return GPIO_NOK;

    return GPIO_OK;
}

//--------------------------------------------------------------------
// Set pin states for pins defined by mask
// Applicable for pins in register 0 and 1
//--------------------------------------------------------------------
static int gpio_set_mask_on_reg(gpio_ctx_t* ctx, 
				uint8_t pin_reg, 
				uint32_t mask, 
				gpio_state_t state)
{
    gpio_pin_t* gp;
    gpio_pin_t** gpp;
    uint32_t direct_mask;
    uint32_t indirect_mask;
    uint8_t pin = 0;
    gpio_errno = EINVAL;

    // First handle direct access pins
    if (pin_reg == 0) {
	direct_mask = mask & ctx->reg0_direct_pins;
	DEBUGF("Set direct mask 0x%x on register %d", direct_mask, pin_reg);
	if (direct_mask) {
	    if (state == gpio_state_high)
		(*ctx->meth->set_mask)(ctx->gpio_reg, 0, direct_mask);
	    else
		(*ctx->meth->clr_mask)(ctx->gpio_reg, 0, direct_mask);
	}
	indirect_mask = mask & ~(ctx->reg0_direct_pins);
	gpp = ctx->reg0;
    }
    else if (pin_reg == 1) {
	direct_mask = mask & ctx->reg1_direct_pins;
	DEBUGF("Set direct mask 0x%x on register %d", direct_mask, pin_reg);
	if (direct_mask) {
	    if (state == gpio_state_high)
		(*ctx->meth->set_mask)(ctx->gpio_reg, 1, direct_mask);
	    else
		(*ctx->meth->clr_mask)(ctx->gpio_reg, 1, direct_mask);
	}
	indirect_mask = mask & ~(ctx->reg1_direct_pins);
	gpp = ctx->reg1;
    }
    else 
	return GPIO_NOK;

    // Take care of rest (if any)
    while (indirect_mask) {
	DEBUGF("Set indirect mask 0x%x on linked list.", indirect_mask);
	if (indirect_mask & 1) {
	    if (gpp[pin] == NULL) {
		if (!ctx->auto_create) 
		    return GPIO_NOK; // ??
		// We should initialize pin
		if ((gp=init_pin(ctx, pin_reg, pin)) == NULL)
		    return GPIO_NOK; // ??
		if (gpio_set_direction(ctx, gp, gpio_direction_out) != GPIO_OK)
		    return GPIO_NOK; // ??
	    }
	    else
		gp = gpp[pin];
	    gpio_set_state(ctx, gp, state);
	}
	indirect_mask >>= 1;
	pin ++;
    }
    return GPIO_OK;
}

//--------------------------------------------------------------------
// Set pin states for pins defined by mask
//--------------------------------------------------------------------
static int gpio_set_mask_on_list(gpio_ctx_t* ctx,
				 uint8_t pin_reg,
				 uint32_t mask, 
				 gpio_state_t state)
{
    uint8_t pin = 0;
    gpio_pin_t* gp;

    while (mask) {
	DEBUGF("Set mask 0x%x on linked list.", mask);
	if (mask & 1) {
	    if ((gp=find_or_create_pin(ctx, pin_reg, pin, 
				       gpio_direction_out)) == NULL) 
		return GPIO_NOK; // ??
	    gpio_set_state(ctx, gp, state);
	}
	mask >>= 1;
	pin ++;
    }
    return GPIO_OK;
}

//--------------------------------------------------------------------
// Set pin states for pins defined by mask
// Applicable for pins without direct access
//--------------------------------------------------------------------
static int gpio_set_interrupt(gpio_pin_t* gp, gpio_interrupt_t interrupt)
{
    char* value = "";
    gpio_errno = EINVAL;

    DEBUGF("set interrupt to %d on pin %d:%d", 
	   interrupt, gp->pin_reg, gp->pin);

    switch(interrupt) {
    case gpio_interrupt_none:   value = "none"; break;
    case gpio_interrupt_rising: value = "rising"; break;
    case gpio_interrupt_falling: value = "falling"; break;
    case gpio_interrupt_both:    value = "both"; break;
    default:
	return GPIO_NOK;
    }
    if (write_value("/sys/class/gpio/gpio%d/edge", gp->pin, value) == GPIO_NOK)
	return GPIO_NOK;
    gp->interrupt = interrupt;
    return GPIO_OK;
}
//--------------------------------------------------------------------
// Activate interrupt for pin
//--------------------------------------------------------------------
static int add_interrupt(gpio_ctx_t* ctx, gpio_pin_t* gp)
{
    struct erl_drv_event_data evd;

#ifdef USE_EPOLL
    if (INT_EVENT(ctx->epollfd) >= 0) {
	struct epoll_event ev;
	
	ev.events  = EPOLLPRI | EPOLLERR;
	ev.data.ptr = (void*) gp;
	
	if (epoll_ctl(INT_EVENT(ctx->epollfd), EPOLL_CTL_ADD, 
		      INT_EVENT(gp->fd), &ev) < 0) {
	    gpio_errno = errno;
	    DEBUGF("Failed epoll_ctl add (%d) reason, %s",
		   INT_EVENT(gp->fd), strerror(errno));
	    return GPIO_NOK;
	}
	return GPIO_OK;
    }
#endif
    // fallback - that may work
    evd.events = POLLPRI | POLLERR;
    evd.revents = 0;
    if (driver_event(ctx->port, gp->fd, &evd) < 0) {
	gpio_errno = errno;
	return GPIO_NOK;
    }
    return GPIO_OK;
}

//--------------------------------------------------------------------
// Deactivate interrupt for pin
//--------------------------------------------------------------------
static int del_interrupt(gpio_ctx_t* ctx, gpio_pin_t* gp)
{
    struct erl_drv_event_data evd;

#ifdef USE_EPOLL
    if (INT_EVENT(ctx->epollfd) >= 0) {
	struct epoll_event ev;
	
	ev.events  = 0;
	ev.data.fd = INT_EVENT(gp->fd);
	
	if (epoll_ctl(INT_EVENT(ctx->epollfd), EPOLL_CTL_DEL, 
		      INT_EVENT(gp->fd), &ev) < 0) {
	    gpio_errno = errno;
	    DEBUGF("Failed epoll_ctl del (%d) reason, %s",
		   INT_EVENT(gp->fd), strerror(errno));
	    return GPIO_NOK;
	}
	return GPIO_OK;
    }
#endif
    // fallback - that may work
    evd.events  = 0;
    evd.revents = 0;
    if (driver_event(ctx->port, gp->fd, &evd) < 0) {
	gpio_errno = errno;
	return GPIO_NOK;
    }
    return GPIO_OK;
}

//--------------------------------------------------------------------
// Inform interrupt activator of triggered interrupt
//--------------------------------------------------------------------
static int send_interrupt(gpio_ctx_t* ctx, gpio_pin_t* gp)
{
    ErlDrvTermData message[16];
    int i = 0;
    uint8_t state;
    
    //if (gpio_get_state(ctx, gp, &state) != GPIO_OK)
    //goto error;
    // File must be read to stop interrupt from coming
    if (gpio_get_indirect(gp, &state) != GPIO_OK)
	goto error;
   
    // Format of info to activator
    // {gpio_interrupt, <reg>, <pin>, <value>}
    push_atom(ATOM(gpio_interrupt));
    push_int(gp->pin_reg);
    push_int(gp->pin);
    push_int(state);
    push_tuple(4);
    driver_send_term(ctx->port, gp->target, message, i); 
    return 0;
error:
    DEBUGF("send_interrupt read error %c for pin %d:%d", 
	   (char)state, gp->pin_reg, gp->pin);
    return -1;
}

//--------------------------------------------------------------------
// map kernel memory
//--------------------------------------------------------------------
static void* map_registers(off_t base, size_t len)
{
    int fd = open("/dev/mem", O_RDWR);
    void * vaddr = MAP_FAILED;

    if (fd < 0) {
	gpio_errno = errno;
	DEBUGF("Failed to open /dev/mem: %m\n");
	return vaddr;
    }
    vaddr = mmap(NULL, len, PROT_READ|PROT_WRITE, MAP_SHARED, fd, base);
    close(fd);
    if (vaddr == MAP_FAILED){
	gpio_errno = errno;
	DEBUGF("Failed to map peripheral at 0x%08x: %m\n", base);
    }
    return vaddr;
}

//--------------------------------------------------------------------
// unmap kernel memory
//--------------------------------------------------------------------
static int unmap_registers(void* addr, size_t len)
{
    return munmap(addr, len);
}

//--------------------------------------------------------------------
// Dump data
//--------------------------------------------------------------------
static void dump(gpio_ctx_t* ctx)
{
    int i;
    gpio_pin_t *gp;

    DEBUGF("Register 0:");
    for (i = 0; i < 32; i++) {
	gp = ctx->reg0[i];
	if (gp) 
	    DEBUGF("Pin: %d, DirectFlag %d, Interrupt %d",
		   gp->pin, gp->direct, gp->interrupt);
    }
    DEBUGF("Register 1:");
    for (i = 0; i < 32; i++) {
	gp = ctx->reg1[i];
	if (gp)
	    DEBUGF("Pin: %d, DirectFlag %d, Interrupt %d",
		   gp->pin, gp->direct, gp->interrupt);
    }

    DEBUGF("Other pins:");
    gp = ctx->first;
    while (gp) {
	DEBUGF("Reg: %d, Pin: %d, DirectFlag %d, Interrupt %d",
	       gp->pin_reg, gp->pin, gp->direct, gp->interrupt);
	gp = gp->next;
    }

    DEBUGF("Direct pin mask for register 0: %x", ctx->reg0_direct_pins);
    DEBUGF("Direct pin mask for register 1: %x", ctx->reg1_direct_pins);
    DEBUGF("Chipset %d", ctx->chipset);
    if (ctx->gpio_reg) 
	DEBUGF("Physical mem: %x", ctx->gpio_reg);
    else
	DEBUGF("No physical memory mapped.");
   
    return;

}
//--------------------------------------------------------------------
// ErlDriver functions
//--------------------------------------------------------------------
//--------------------------------------------------------------------
// Setup global object area
// Load atoms etc.
//--------------------------------------------------------------------
static int gpio_drv_init(void)
{
    gpio_debug_level = DLOG_DEFAULT;
    DEBUGF("gpio_driver_init");
    INIT_ATOM(ok);
    INIT_ATOM(error);
    INIT_ATOM(undefined);
    INIT_ATOM(gpio_interrupt);
    return 0;
}

//--------------------------------------------------------------------
// Clean up global stuff
//--------------------------------------------------------------------
static void gpio_drv_finish(void)
{
}

//--------------------------------------------------------------------
// Initialize memory etc
//--------------------------------------------------------------------
static ErlDrvData gpio_drv_start(ErlDrvPort port, char* command)
{
    (void) command;
    gpio_ctx_t* ctx;
    char *ptr;
    bool auto_create = true; // If true pins are created when needed
    gpio_chipset_t chipset = gpio_chipset_none;
    gpio_methods_t* meth = NULL;
    uint32_t* gpio_reg = NULL;

    DEBUGF("gpio_drv: start (%s)", command);
    ptr = command;
    // Skip program name
    while (*ptr && (*ptr != ' ')) ptr ++;
    // Skip first blank (if any)
    while (*ptr == ' ') ptr ++;
    while (*ptr) {
	switch (*ptr) {
	case 'n': 
	    auto_create = false;
	    DEBUGF("gpio_drv: auto create turned off.");
	    break;
	case 'd':
	    gpio_debug_level = DLOG_DEBUG;
	    DEBUGF("gpio_drv: debug turned on.");
	    break;
	case 'b': {
	    chipset = bcm2835;
	    meth = &gpio_bcm2835_meth;
	    break;
	}
	default:
	    break;
	}
	ptr ++;
    }

    if (meth != NULL) {
	if ((gpio_reg = map_registers(meth->base,  meth->len)) == MAP_FAILED) {
	    gpio_errno = errno;
	    goto error;
	}
	DEBUGF("chipset %s, gpio reg mapped.", meth->name);
    }

    // Init memory
    if ((ctx = (gpio_ctx_t*) driver_alloc(sizeof(gpio_ctx_t))) == NULL) {
	if (gpio_reg != NULL)
	    unmap_registers(gpio_reg, meth->len);
	gpio_errno = ENOMEM;
	goto error;
    }
    memset(ctx, 0, sizeof(gpio_ctx_t));

    ctx->port = port;
    ctx->first = NULL;
    ctx->chipset = chipset;
    ctx->meth = meth;
    ctx->auto_create = auto_create;
    ctx->gpio_reg = gpio_reg;
    ctx->epollfd = (ErlDrvEvent) -1;
#ifdef USE_EPOLL
    {
	ctx->epollfd = (ErlDrvEvent) epoll_create(MAX_EPOLL_EVENTS);
	if (INT_EVENT(ctx->epollfd) < 0) {
	    DEBUGF("Failed epoll_create (%d) reason, %s", MAX_EPOLL_EVENTS,
		   strerror(errno));
	}
	else {
	    driver_select(ctx->port, ctx->epollfd, ERL_DRV_READ, 1);
	}
    }
#endif

#ifdef PORT_CONTROL_BINARY
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
#endif
    return (ErlDrvData) ctx;
error:
    {
        char* err_str = erl_errno_id(gpio_errno);
	DEBUGF("Failed starting %s, reason %s.", command, err_str);
	errno = gpio_errno;
	return ERL_DRV_ERROR_ERRNO;
    }
}

//--------------------------------------------------------------------
// Free memory etc 
//--------------------------------------------------------------------
static void gpio_drv_stop(ErlDrvData d)
{
    gpio_ctx_t* ctx = (gpio_ctx_t*) d;
    gpio_pin_t* gp = ctx->first;
    while(gp) {
	gpio_pin_t* gpn = gp->next;
	if (gp->interrupt) {
	    del_interrupt(ctx, gp);
	    gp->interrupt = 0;
	}
	driver_select(ctx->port, gp->fd, ERL_DRV_USE, 0);
	driver_free(gp);
	gp = gpn;
    }    // add structure cleanup here
#ifdef USE_EPOLL
    if (INT_EVENT(ctx->epollfd) >= 0)
	driver_select(ctx->port, ctx->epollfd, ERL_DRV_USE, 0);
#endif
    if (ctx->meth != NULL)
	unmap_registers((void*)ctx->gpio_reg, ctx->meth->len);
    driver_free(ctx);
}

//--------------------------------------------------------------------
// General control reply function 
//--------------------------------------------------------------------
static ErlDrvSSizeT ctl_reply(int rep, 
			      void* buf, 
			      ErlDrvSizeT len,
			      char** rbuf, 
			      ErlDrvSizeT rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
#ifdef PORT_CONTROL_BINARY
	ErlDrvBinary* bin = driver_alloc_binary(len+1);
	if (bin == NULL) 
	    return -1;
	ptr = bin->orig_bytes;	
	*rbuf = (char*) bin;
#else
	if ((ptr = driver_alloc(len+1)) == NULL)
	    return -1;
	*rbuf = ptr;
#endif
    }
    else
	ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}

//--------------------------------------------------------------------
//--------------------------------------------------------------------
static ErlDrvSSizeT gpio_drv_ctl(ErlDrvData d, 
				 unsigned int cmd, 
				 char* buf0, 
				 ErlDrvSizeT len,
				 char** rbuf, 
				 ErlDrvSizeT rsize)
{
    gpio_ctx_t* ctx = (gpio_ctx_t*) d;
    uint8_t* buf = (uint8_t*) buf0;
    gpio_pin_t* gp;
    uint8_t pin_reg = -1;
    uint8_t pin = -1;

    DEBUGF("gpio_drv: ctl: cmd=%u, len=%d", cmd, len);

    switch(cmd) {
    case CMD_INIT: {
	uint8_t direct_access;
	if (len != 3) goto badarg;
	pin_reg = get_uint8(buf);
	pin = get_uint8(buf+1);
	direct_access = get_uint8(buf+2);

	DEBUGF("gpio_drv: init: pin_reg=%d, pin=%d, direct_access %d", 
	       cmd, len, direct_access);

	// Chip set must be known for direct access
	if (direct_access && (ctx->chipset == gpio_chipset_none))
	    goto badarg;
	// Direct access only OK for register 0 and 1
	if (direct_access && (pin_reg != 0) && (pin_reg != 1))
	    goto badarg;
	// Direct access only OK for pin 0-31
	if (direct_access && (pin_reg > 31))
	    goto badarg;

	if ((gp = find_pin(ctx, pin_reg, pin, NULL)) == NULL)
	    if ((gp = init_pin(ctx, pin_reg, pin)) == NULL)
		goto error;

	// Pins 0-31 in register 0 and 1 can have direct access
	// See checks above.
	if (direct_access && !gp->direct){
	    // Pin changed to direct access
	    if (set_pin_in_mask(ctx, pin_reg, pin) != GPIO_OK)
		goto error;
	    gp->direct = true;
	}
	else if (!direct_access && gp->direct) {
	    // Pin changed from direct access
	    if (clr_pin_in_mask(ctx, pin_reg, pin)!= GPIO_OK)
		goto error;
	    gp->direct = false;
	}
	goto ok;
    }

    case CMD_RELEASE: {
	if (len != 2) goto badarg;
	pin_reg = get_uint8(buf);
	pin = get_uint8(buf+1);

	if (release_pin(ctx, pin_reg, pin) != GPIO_OK)
	    goto error;

	goto ok;
    }

    case CMD_SET: {
	if (len != 2) goto badarg;
	pin_reg = get_uint8(buf);
	pin = get_uint8(buf+1);

	if ((gp = find_or_create_pin(ctx, pin_reg, pin, 
				     gpio_direction_out)) == NULL) 
	    goto error;
	if (gpio_set_state(ctx, gp, gpio_state_high) != GPIO_OK)
	    goto error;

	goto ok;
    }

     case CMD_CLR: {
	if (len != 2) goto badarg;
	pin_reg = get_uint8(buf);
	pin = get_uint8(buf+1);

	if ((gp = find_or_create_pin(ctx, pin_reg, pin, 
				     gpio_direction_out)) == NULL) 
	    goto error;
	if (gpio_set_state(ctx, gp, gpio_state_low) != GPIO_OK)
	    goto error;
	goto ok;
    }

    case CMD_GET: {
	uint8_t state;

	if (len != 2) goto badarg;
	pin_reg = get_uint8(buf);
	pin = get_uint8(buf+1);

	if ((gp = find_or_create_pin(ctx, pin_reg, pin, 
				     gpio_direction_in)) == NULL) 
	    goto error;

	if (gpio_get_state(ctx, gp, &state) != GPIO_OK)
	    goto error;

	return ctl_reply(1, &state, 1, rbuf, rsize);
    }

    case CMD_SET_DIRECTION: {
	uint8_t dir;
	gpio_pin_t* gp;

	if (len != 3) goto badarg;
	pin_reg = get_uint8(buf);
	pin = get_uint8(buf+1);
	dir = get_uint8(buf+2);

	if ((gp=find_pin(ctx, pin_reg, pin, NULL)) == NULL) {
	    if (!ctx->auto_create) 
		goto badarg;
	    if ((gp=init_pin(ctx, pin_reg, pin)) == NULL)
		goto error;
	}
	if (gpio_set_direction(ctx, gp,  (gpio_direction_t) dir) != GPIO_OK)
		goto error;
	goto ok;
    }

    case CMD_GET_DIRECTION: {
	gpio_direction_t direction;
	uint8_t dir;
	if (len != 2) goto badarg;
	pin_reg = get_uint8(buf);
	pin = get_uint8(buf+1);

	if ((gp = find_pin(ctx, pin_reg, pin, NULL)) == NULL) {
	    gpio_errno = ENOENT;
	    goto error;
	}
	
	if (gpio_get_direction(ctx, gp, &direction) == GPIO_NOK)
	    goto error;
	dir = (uint8_t) direction;
	DEBUGF("Read direction %d for pin %d:%d", dir, pin_reg, pin);
	return ctl_reply(1, &dir, sizeof(dir), rbuf, rsize);
    }

    case CMD_SET_MASK: {
	uint32_t mask;
	int result = GPIO_OK;

	if (len != 5) goto badarg;
	pin_reg = get_uint8(buf);
	mask = get_uint32(buf+1);

	if ((pin_reg == 0) || (pin_reg == 1))
	    result = 
		gpio_set_mask_on_reg(ctx, pin_reg, mask, gpio_state_high);
	else
	    result = 
		gpio_set_mask_on_list(ctx, pin_reg, mask, gpio_state_high);

	if (result == GPIO_NOK) goto error;

	goto ok;
    }

    case CMD_CLR_MASK: {
	uint32_t mask;
	if (len != 5) goto badarg;
	pin_reg = get_uint8(buf);
	mask = get_uint32(buf+1);

	if ((pin_reg == 0) || (pin_reg == 1))
	    gpio_set_mask_on_reg(ctx, pin_reg, mask, gpio_state_low);
	else
	    gpio_set_mask_on_list(ctx, pin_reg, mask, gpio_state_low);

	goto ok;
    }

    case CMD_SET_INTERRUPT: {
	gpio_interrupt_t intval;

	if (len != 3) goto badarg;
	pin_reg = get_uint8(buf);
	pin = get_uint8(buf+1);
	intval = (gpio_interrupt_t) get_uint8(buf+2);

	if ((gp = find_or_create_pin(ctx, pin_reg, pin, 
				     gpio_direction_in)) == NULL) 
	    goto error;
	if (gpio_set_interrupt(gp, intval) == GPIO_NOK)
	    goto error;
	if (intval == gpio_interrupt_none) {
	    if (gp->interrupt) {
		del_interrupt(ctx, gp);
		gp->interrupt = gpio_interrupt_none;
	    }
	}
	else {
	    gpio_direction_t direction = gpio_direction_undef;

	    gpio_get_direction(ctx, gp, &direction);
	    if (direction != gpio_direction_in)
		goto badarg;
	    if (add_interrupt(ctx, gp) == GPIO_NOK)
		goto error;
	    gp->interrupt = intval;
	    gp->target = driver_caller(ctx->port);
	}
	goto ok;
    }

    case CMD_GET_INTERRUPT: {
	uint8_t intval;
	if (len != 2) goto badarg;
	pin_reg = get_uint8(buf);
	pin = get_uint8(buf+1);

	if ((gp = find_pin(ctx, pin_reg, pin, NULL)) == NULL) {
	    gpio_errno = ENOENT;
	    goto error;
	}
	intval = (uint8_t) gp->interrupt;
	DEBUGF("Read interrupt %d for pin %d:%d", 
	       intval, pin_reg, pin);
	return ctl_reply(1, &intval, sizeof(intval), rbuf, rsize);
    }
	
    case CMD_DEBUG_LEVEL: {
	if (len != 1) goto badarg;
	gpio_debug_level = get_int8(buf);
	DEBUGF("Debug level set to %d", gpio_debug_level);
	goto oki;
    }

    case CMD_DUMP: {
	if (len != 0) goto badarg;
	dump(ctx);
	goto oki;
    }

    default:
	goto badarg;
    }

ok:
    DEBUGF("Successfully executed %d on pin %d:%d", cmd, pin_reg, pin);
oki:
    return ctl_reply(0, NULL, 0, rbuf, rsize);
badarg:
    gpio_errno = EINVAL;
error:
    {
        char* err_str = erl_errno_id(gpio_errno);
	DEBUGF("Failed executing %d on pin %d:%d, reason %s.", 
	       cmd, pin_reg, pin, err_str);
	return ctl_reply(255, err_str, strlen(err_str), rbuf, rsize);
    }
}


//--------------------------------------------------------------------
//--------------------------------------------------------------------
static void gpio_drv_output(ErlDrvData d, char* buf, ErlDrvSizeT len)
{
    (void) d;
    (void) buf;
    (void) len;
    // gpio_ctx_t*   ctx = (gpio_ctx_t*) d;
    DEBUGF("gpio_drv: output");
}

//--------------------------------------------------------------------
//--------------------------------------------------------------------
static void gpio_drv_outputv(ErlDrvData d, ErlIOVec *ev)
{
    (void) d;
    (void) ev;
//  gpio_ctx_t*   ctx = (gpio_ctx_t*) d;
    DEBUGF("gpio_drv: outputv");
}

//--------------------------------------------------------------------
//--------------------------------------------------------------------
static void gpio_drv_event(ErlDrvData d, ErlDrvEvent e,
				  ErlDrvEventData ed)
{
    gpio_ctx_t* ctx = (gpio_ctx_t*) d;
    gpio_pin_t* gp = ctx->first;

    DEBUGF("gpio_drv: event called fd=%d", INT_EVENT(e));

    while(gp && (gp->fd != e))
	gp = gp->next;
    if (!gp) {
	DEBUGF("gpio_drv: event not found");
	return;
    }
    if (ed->revents & POLLERR)
	goto error;
    if (ed->revents & POLLPRI)
	send_interrupt(ctx, gp);
    return;
error:
    DEBUGF("gpio_drv_event read error (revents=%x) for pin %d:%d", 
	   ed->revents, gp->pin_reg, gp->pin);
}

//--------------------------------------------------------------------
//--------------------------------------------------------------------
static void gpio_drv_ready_input(ErlDrvData d, ErlDrvEvent e)
{
#ifdef USE_EPOLL    
    gpio_ctx_t* ctx = (gpio_ctx_t*) d;
    DEBUGF("gpio_drv: ready_input called");
    if (ctx->epollfd == e) {
	struct epoll_event events[MAX_EPOLL_EVENTS];
	int i,n;
	n = epoll_wait(INT_EVENT(ctx->epollfd), events, MAX_EPOLL_EVENTS, 0);
	for (i = 0; i < n; i++) {
	    if (events[i].events & EPOLLPRI)
		send_interrupt(ctx, (gpio_pin_t*) events[i].data.ptr);
	}
    }
#else
    (void) d;
    (void) e;
#endif
}

//--------------------------------------------------------------------
//--------------------------------------------------------------------
static void gpio_drv_ready_output(ErlDrvData d, ErlDrvEvent e)
{
    (void) d;
    (void) e;
//  gpio_ctx_t* ctx = (gpio_ctx_t*) d;
    DEBUGF("gpio_drv: ready_output called");
}

//--------------------------------------------------------------------
// operation timed out
//--------------------------------------------------------------------
static void gpio_drv_timeout(ErlDrvData d)
{
    (void) d;
    DEBUGF("gpio_drv: timeout");
}

//--------------------------------------------------------------------
// file not in use anymore, see ERL_DRV_USE
//--------------------------------------------------------------------
static void gpio_drv_stop_select(ErlDrvEvent event, void* arg)
{    
    (void) arg;
    DEBUGF("gpio_drv: stop_select event=%d", INT_EVENT(event));
    close(INT_EVENT(event));
}

//--------------------------------------------------------------------
//--------------------------------------------------------------------
DRIVER_INIT(gpio_drv)
{
    ErlDrvEntry* ptr = &gpio_drv_entry;

    DEBUGF("gpio driver_init");

    ptr->driver_name = "gpio_drv";
    ptr->init  = gpio_drv_init;
    ptr->start = gpio_drv_start;
    ptr->stop  = gpio_drv_stop;
    ptr->output = gpio_drv_output;
    ptr->ready_input  = gpio_drv_ready_input;
    ptr->ready_output = gpio_drv_ready_output;
    ptr->finish = gpio_drv_finish;
    ptr->control = gpio_drv_ctl;
    ptr->timeout = gpio_drv_timeout;
    ptr->outputv = gpio_drv_outputv;
    ptr->ready_async = 0;
    ptr->flush = 0;
    ptr->call = 0;
    ptr->event = gpio_drv_event;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
    ptr->process_exit = 0;
    ptr->stop_select = gpio_drv_stop_select;
    return ptr;
}
