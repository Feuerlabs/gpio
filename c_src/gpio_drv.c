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
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>

#include "erl_driver.h"
// #include "dthread.h"

#define ATOM(NAME) am_ ## NAME
#define INIT_ATOM(NAME) am_ ## NAME = driver_mk_atom(#NAME)

// Hack to handle R15 driver used with pre R15 driver
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif

#define PORT_CONTROL_BINARY

#define INT_EVENT(e) ((int)((long)(e)))

// Port commands
// MUST BE EQUAL TO DEFINES IN gpio.erl !!!!
#define CMD_INIT 1
#define CMD_SET  2
#define CMD_CLR 3
#define CMD_GET 4
#define CMD_INPUT 5
#define CMD_OUTPUT 6
#define CMD_SET_MASK 7
#define CMD_CLR_MASK 8
#define CMD_DIRECTION 9
#define CMD_RELEASE 10

#define GPIO_NOK -1
#define GPIO_OK 0

typedef enum {
    gpio_direction_undef = -1,
    gpio_direction_in = 1,
    gpio_direction_out = 2,
    gpio_direction_bi = 3,
}  gpio_direction_t;


typedef enum  {
    gpio_state_undef = -1,
    gpio_state_low = 0,
    gpio_state_high = 1,
} gpio_state_t;

typedef struct gpio_pin_t
{
    struct gpio_pin_t* next;   // when linked    
    uint8_t  pin_register;
    uint8_t  pin;
    int value_fd; // To /sys/class/gpio/gpioX/value
    gpio_state_t state;
    gpio_direction_t direction;
} gpio_pin_t;

typedef struct _gpio_ctx_t
{
    ErlDrvPort port;
    gpio_pin_t *first;

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

ErlDrvTermData am_ok;
ErlDrvTermData am_error;
ErlDrvTermData am_undefined;

static ErlDrvEntry gpio_drv_entry;

//--------------------------------------------------------------------
// Logging
//--------------------------------------------------------------------
#define DLOG_DEBUG     7
#define DLOG_INFO      6
#define DLOG_NOTICE    5
#define DLOG_WARNING   4
#define DLOG_ERROR     3
#define DLOG_CRITICAL  2
#define DLOG_ALERT     1
#define DLOG_EMERGENCY 0
#define DLOG_NONE     -1

#ifndef DLOG_DEFAULT
#define DLOG_DEFAULT DLOG_DEBUG
#endif

#define DLOG(level,file,line,args...) do { \
	if (((level) == DLOG_EMERGENCY) ||				\
	    ((debug_level >= 0) && ((level) <= debug_level))) { \
	    emit_log((level),(file),(line),args);		\
	}								\
    } while(0)

#define DEBUGF(args...) DLOG(DLOG_DEBUG,__FILE__,__LINE__,args)
#define INFOF(args...)  DLOG(DLOG_INFO,__FILE__,__LINE__,args)
#define NOTICEF(args...)  DLOG(DLOG_NOTICE,__FILE__,__LINE__,args)
#define WARNINGF(args...)  DLOG(DLOG_WARNING,__FILE__,__LINE__,args)
#define ERRORF(args...)  DLOG(DLOG_ERROR,__FILE__,__LINE__,args)
#define CRITICALF(args...)  DLOG(DLOG_CRITICAL,__FILE__,__LINE__,args)
#define ALERTF(args...)  DLOG(DLOG_ALERT,__FILE__,__LINE__,args)
#define EMERGENCYF(args...)  DLOG(DLOG_EMERGENCY,__FILE__,__LINE__,args)

static int debug_level = DLOG_DEFAULT;

static void emit_log(int level, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    if ((level == DLOG_EMERGENCY) ||
	((debug_level >= 0) && (level <= debug_level))) {
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
// find gpio pin in list
//--------------------------------------------------------------------
static gpio_pin_t** find_pin(gpio_ctx_t* ctx, 
			     uint8_t pin_register, 
			     uint8_t pin) 
{
    gpio_pin_t** gpp = &ctx->first;

    while(*gpp) {
	gpio_pin_t* gp = *gpp;
	if ((gp->pin_register == pin_register) && (gp->pin == pin))
	    return gpp;
	gpp = &gp->next;
    }
    return NULL;
}

//--------------------------------------------------------------------
// create new gpio pin first in list 
//--------------------------------------------------------------------
static int create_pin(gpio_ctx_t* ctx, 
		      uint8_t pin_register, 
		      uint8_t pin,
		      int fd)
{
    gpio_pin_t* gp;

    if ((gp = driver_alloc(sizeof(gpio_pin_t))) == NULL) {
	errno = ENOMEM;
	return GPIO_NOK;
    }

    gp->next = ctx->first;
    gp->pin_register = pin_register;
    gp->pin = pin;
    ctx->first = gp;
    gp->value_fd = fd;
    gp->state = gpio_state_undef;
    gp->direction = gpio_direction_undef;
    return GPIO_OK;
}

//--------------------------------------------------------------------
// general control reply function 
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
// Open the export file that we can use to ask the kernel
// to export control to us. See kernel/Documentation/gpio.txt
//--------------------------------------------------------------------
static int export(int pin)
{
    int fd = -1;
    char buf[128];
    char *path = "/sys/class/gpio/export";
    int result = GPIO_OK;

    if ((fd = open(path, O_WRONLY)) < 0) {
        DEBUGF("Failed to open %s: reason, %s", path, strerror(errno));
        return GPIO_NOK;
    }
    // Write  the pin number we want to use and close the export file
    sprintf(buf, "%d", pin);
    if (write(fd, buf, strlen(buf)) < 0)
	result = GPIO_NOK;

    close(fd);
    return result; 
}

//--------------------------------------------------------------------
// Open the value file, used for input/ouptput. 
// See kernel/Documentation/gpio.txt
//--------------------------------------------------------------------
static int open_value_file(int pin)
{
    int fd = -1;
    mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
    char *fname = "/sys/class/gpio/gpio%d/value";
    char path[128];
    int n;

    // Generate a correct path to the file
    if ((n = snprintf(path, sizeof(path), fname, pin)) >= sizeof(path))
	return -1;

    if ((fd = open(path, O_RDWR, mode)) < 0)
        DEBUGF("Failed to open %s: %s", path, strerror(errno));
    
    DEBUGF("Value file %s has fd %d", path, fd);
    return fd; 
}

static int init_pin(int pin_register, int pin, gpio_ctx_t* ctx) 
{
    int fd = -1;
    
    // Tell linux we will take over pin
    if (export(pin) != GPIO_OK) 
	return GPIO_NOK;
    
    // Prepare value file
    if((fd = open_value_file(pin)) < 0)
	return GPIO_NOK;
    
    if (create_pin(ctx, pin_register, pin, fd) != GPIO_OK)
    {
	close(fd);
	return GPIO_NOK;
    }

    return GPIO_OK;
}

static int gpio_set_state(gpio_pin_t* gp, gpio_state_t state)
{
    DEBUGF("Changing state from %d to %d on pin %d:%d", 
	   gp->state, state, gp->pin_register, gp->pin);

    // Do we already have the correct state?
    if (gp->state == state)
        return GPIO_OK;
    gp->state = state;

    switch(state) {
    case gpio_state_low:
	DEBUGF("Writing low to value file fd %d", gp->value_fd);
        if (write(gp->value_fd, "0\n", 2) < 0)
	    return GPIO_NOK;
        return GPIO_OK;

    case gpio_state_high:
	DEBUGF("Writing high to value file fd %d", gp->value_fd);
        if (write(gp->value_fd, "1\n", 2) < 0)
	    return GPIO_NOK;
        return GPIO_OK;

    default:
	errno = EINVAL;
        break;
    }
    return GPIO_NOK;
}

static int gpio_set_direction(gpio_pin_t* gp, gpio_direction_t direction)
{
    int dir_fd = -1;
    mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
    char *fname = "/sys/class/gpio/gpio%d/direction";
    char path[128];
    int n;
    int result = GPIO_OK;

    DEBUGF("Changing direction from %d to %d on pin %d:%d", 
	   gp->direction, direction, gp->pin_register, gp->pin);

    // Do we already have the correct direction?
    if (gp->direction == direction)
        return GPIO_OK;

    gp->direction = direction;

    // Generate a correct path to the direction file
    if ((n = snprintf(path, sizeof(path), fname, gp->pin)) >= sizeof(path))
	return GPIO_NOK;

    // Open the direction file.
    if ((dir_fd = open(path, O_WRONLY, mode)) < 0)
	return GPIO_NOK;

    if (direction == gpio_direction_in)
	if (write(dir_fd, "in", 1) < 0)
	    return GPIO_NOK;
    else if (direction == gpio_direction_out)
    {
	if (gp->state == gpio_state_low)
	    if (write(dir_fd, "low", 1) < 0)
		return GPIO_NOK;
	else if (gp->state == gpio_state_high)
	    if (write(dir_fd, "high", 1) < 0)
		return GPIO_NOK;
	// What about state undef ??
    }
    else if (direction == gpio_direction_bi)
    {
	// What ??
    }
    else
    {
	errno = EINVAL;
	result = GPIO_NOK;
    }
    
    return result;
}
//--------------------------------------------------------------------
// ErlDriver functions
//--------------------------------------------------------------------
//--------------------------------------------------------------------
// setup global object area
// load atoms etc.
//--------------------------------------------------------------------
static int gpio_drv_init(void)
{
    debug_level = DLOG_DEFAULT;
    DEBUGF("gpio_driver_init");
    INIT_ATOM(ok);
    INIT_ATOM(error);
    INIT_ATOM(undefined);
    return 0;
}

//--------------------------------------------------------------------
// clean up global stuff
//--------------------------------------------------------------------
static void gpio_drv_finish(void)
{
}

//--------------------------------------------------------------------
//--------------------------------------------------------------------
static ErlDrvData gpio_drv_start(ErlDrvPort port, char* command)
{
    (void) command;
    gpio_ctx_t* ctx;

    if ((ctx = (gpio_ctx_t*) 
	 driver_alloc(sizeof(gpio_ctx_t))) == NULL) {
	errno = ENOMEM;
	return ERL_DRV_ERROR_ERRNO;
    }

    ctx->port = port;
    ctx->first = NULL;

    DEBUGF("gpio_drv: start (%s)", command);
#ifdef PORT_CONTROL_BINARY
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
#endif
    return (ErlDrvData) ctx;
}

static void gpio_drv_stop(ErlDrvData d)
{
    gpio_ctx_t* ctx = (gpio_ctx_t*) d;
    gpio_pin_t* gp = ctx->first;
    while(gp) {
	gpio_pin_t* gpn = gp->next;
	driver_select(ctx->port, (ErlDrvEvent) gp->value_fd, ERL_DRV_USE, 0);
	driver_free(gp);
	gp = gpn;
    }    // add structure cleanup here
    driver_free(ctx);
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
    DEBUGF("gpio_drv: ctl: cmd=%u, len=%d", cmd, len);

    uint8_t pin_register;
    uint8_t pin;
    // Get arguments
    if (len != 2) goto badarg;
    pin_register = get_uint8(buf);
    pin = get_uint8(buf+1);

    switch(cmd) {
    case CMD_INIT: {

	// Pin already initialized
	if (find_pin(ctx, pin_register, pin) != NULL)
	    goto ok; // already open
	
	if (init_pin(pin_register, pin, ctx) != GPIO_OK)
	    goto error;
 
	goto ok;
    }

    case CMD_RELEASE: {
	gpio_pin_t** gpp;
	gpio_pin_t* gp;

	if ((gpp = find_pin(ctx, pin_register, pin)) == NULL)
	    goto badarg; // or ok ???
	
	gp = *gpp;
	driver_select(ctx->port, (ErlDrvEvent) gp->value_fd, ERL_DRV_USE, 0);
	*gpp = gp->next; // unlink
	driver_free(gp);

	goto ok;
    }

    case CMD_SET: {
	gpio_pin_t** gpp;
	gpio_pin_t* gp;

	// Localise pin or init it
	if ((gpp = find_pin(ctx, pin_register, pin)) == NULL) {
	    if (init_pin(pin_register, pin, ctx) != GPIO_OK)
		goto error;
	}
	gp = *gpp;
	if (gpio_set_state(gp, gpio_state_high) != GPIO_OK)
	    goto error;

	goto ok;
    }

     case CMD_CLR: {
	gpio_pin_t** gpp;
	gpio_pin_t* gp;

	// Localise pin or init it
	if ((gpp = find_pin(ctx, pin_register, pin)) == NULL) {
	    if (init_pin(pin_register, pin, ctx) != GPIO_OK)
		goto error;
	}
	gp = *gpp;
	if (gpio_set_state(gp, gpio_state_low) != GPIO_OK)
	    goto error;

	goto ok;
    }

    case CMD_GET:
    {
	gpio_pin_t** gpp;
	gpio_pin_t* gp;
	uint8_t state;

	// Localise pin or init it
	if ((gpp = find_pin(ctx, pin_register, pin)) == NULL) {
	    if (init_pin(pin_register, pin, ctx) != GPIO_OK)
		goto error;
	}
	gp = *gpp;
	state = (uint8_t) gp->state;
	DEBUGF("Read state %d for pin %d:%d", state, pin_register, pin);
	return ctl_reply(1, &state, sizeof(state), rbuf, rsize);
    }

    case CMD_INPUT:
    {
	gpio_pin_t** gpp;
	gpio_pin_t* gp;

	// Localise pin or init it
	if ((gpp = find_pin(ctx, pin_register, pin)) == NULL) {
	    if (init_pin(pin_register, pin, ctx) != GPIO_OK)
		goto error;
	}
	gp = *gpp;
	if (gpio_set_direction(gp, gpio_direction_in) != GPIO_OK)
	    goto error;

	goto ok;
    }

    case CMD_OUTPUT:
    {
	gpio_pin_t** gpp;
	gpio_pin_t* gp;

	// Localise pin or init it
	if ((gpp = find_pin(ctx, pin_register, pin)) == NULL) {
	    if (init_pin(pin_register, pin, ctx) != GPIO_OK)
		goto error;
	}
	gp = *gpp;
	if (gpio_set_direction(gp, gpio_direction_out) != GPIO_OK)
	    goto error;

	goto ok;
    }

    case CMD_DIRECTION:
    {
	gpio_pin_t** gpp;
	gpio_pin_t* gp;
	uint8_t direction;

	// Localise pin or init it
	if ((gpp = find_pin(ctx, pin_register, pin)) == NULL) {
	    if (init_pin(pin_register, pin, ctx) != GPIO_OK)
		goto error;
	}
	gp = *gpp;
	direction = (uint8_t) gp->direction;
	DEBUGF("Read direction %d for pin %d:%d", 
	       direction, pin_register, pin);
	return ctl_reply(1, &direction, sizeof(direction), rbuf, rsize);
    }

  default:
	goto badarg;
    }

ok:
    DEBUGF("Successfully executed %d on pin %d:%d", cmd, pin_register, pin);
    return ctl_reply(0, NULL, 0, rbuf, rsize);
badarg:
    errno = EINVAL;
error:
    {
        char* err_str = erl_errno_id(errno);
	DEBUGF("Failed executing %d on pin %d:%d, reason %s.", 
	       cmd, pin_register, pin, err_str);
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
    (void) d;
    (void) e;
    (void) ed;
//  gpio_ctx_t* ctx = (gpio_ctx_t*) d;
    DEBUGF("gpio_drv: event called");
}

//--------------------------------------------------------------------
//--------------------------------------------------------------------
static void gpio_drv_ready_input(ErlDrvData d, ErlDrvEvent e)
{
    (void) d;
    (void) e;
//  gpio_ctx_t* ctx = (gpio_ctx_t*) d;
    DEBUGF("gpio_drv: ready_input called");
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
