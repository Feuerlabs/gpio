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
#include <poll.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>

#include "erl_driver.h"
// #include "dthread.h"

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

#define GPIO_NOK -1
#define GPIO_OK 0

typedef enum {
    gpio_direction_in = 1,
    gpio_direction_out = 2,
    gpio_direction_low  = 3,  // out but start low
    gpio_direction_high = 4  // out but start high
} gpio_direction_t;


typedef enum  {
    gpio_state_undef = -1,
    gpio_state_low = 0,
    gpio_state_high = 1,
} gpio_state_t;

typedef enum {
    gpio_interrupt_none    = 0,
    gpio_interrupt_rising  = 1,
    gpio_interrupt_falling = 2,
    gpio_interrupt_both    = 3
} gpio_interrupt_t;

typedef struct gpio_pin_t
{
    struct gpio_pin_t* next;   // when linked    
    uint8_t  pin_register;
    uint8_t  pin;
    ErlDrvEvent fd;   // To /sys/class/gpio/gpioX/value
    gpio_state_t state;
    gpio_direction_t direction;
    gpio_interrupt_t interrupt;
    ErlDrvTermData target;     // interrupt target
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

static ErlDrvEntry gpio_drv_entry;

// keep the last errno here, errno will be destroyed in debug logging
// and other system calls otherwise, this errno should reflect the actual
// error.
static int gpio_errno = 0;

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
static gpio_pin_t* find_pin(gpio_ctx_t* ctx, 
			    uint8_t pin_register, 
			    uint8_t pin,
			    gpio_pin_t*** gppp)
{
    gpio_pin_t** gpp = &ctx->first;

    while(*gpp) {
	gpio_pin_t* gp = *gpp;
	if ((gp->pin_register == pin_register) && (gp->pin == pin)) {
	    if (gppp) *gppp = gpp;
	    return gp;
	}
	gpp = &gp->next;
    }
    return NULL;
}

//--------------------------------------------------------------------
// create new gpio pin first in list 
//--------------------------------------------------------------------
static gpio_pin_t* create_pin(gpio_ctx_t* ctx, 
			      uint8_t pin_register, 
			      uint8_t pin,
			      int fd)
{
    gpio_pin_t* gp;

    if ((gp = driver_alloc(sizeof(gpio_pin_t))) == NULL) {
	gpio_errno = ENOMEM;
	return NULL;
    }
    gp->next = ctx->first;
    ctx->first = gp;

    gp->pin_register = pin_register;
    gp->pin = pin;
    gp->interrupt = 0;
    gp->fd = (ErlDrvEvent) fd;
    gp->state = gpio_state_undef;
    gp->direction = gpio_direction_in; // assume in for now

    return gp;
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
// Utility to write a string to a file and then close it
// 
//--------------------------------------------------------------------
static int write_value(char* fpath, int pin, char* value)
{
    char path[128];
    int n;
    int fd;

    if (snprintf(path, sizeof(path), fpath, pin) >= sizeof(path)) {
        DEBUGF("Failed to format %s: reason , too long", fpath);
	gpio_errno = EINVAL;
	return GPIO_NOK; // format to long
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
// Check if pin is already exported
// 
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
	DEBUGF("Value file %s has fd %d", path, fd);
    }
    return fd; 
}

static gpio_pin_t* init_pin(gpio_ctx_t* ctx, int pin_register, int pin) 
{
    gpio_pin_t* gp;
    int fd = -1;

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
    if ((gp=create_pin(ctx, pin_register, pin, fd)) == NULL)
	close(fd);
    // set default interrupt target to process that created the pin struct
    gp->target = driver_caller(ctx->port);
    return gp;
}

static int gpio_set_state(gpio_pin_t* gp, gpio_state_t state)
{
    int fd;
    DEBUGF("Changing state from %d to %d on pin %d:%d", 
	   gp->state, state, gp->pin_register, gp->pin);
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

static int gpio_set_direction(gpio_pin_t* gp, gpio_direction_t direction)
{
    char* value = "";

    DEBUGF("set direction to %d on pin %d:%d", 
	   direction, gp->pin_register, gp->pin);

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
    if ((direction == gpio_direction_low) ||
	(direction == gpio_direction_high))
	gp->direction = gpio_direction_out;
    else
	gp->direction = direction;
    return GPIO_OK;
}

static int gpio_set_interrupt(gpio_pin_t* gp, gpio_interrupt_t interrupt)
{
    char* value = "";

    DEBUGF("set interrupt to %d on pin %d:%d", 
	   interrupt, gp->pin_register, gp->pin);

    switch(interrupt) {
    case gpio_interrupt_none:   value = "none"; break;
    case gpio_interrupt_rising: value = "rising"; break;
    case gpio_interrupt_falling: value = "falling"; break;
    case gpio_interrupt_both:    value = "both"; break;
    default:
	gpio_errno = EINVAL;
	return GPIO_NOK;
    }
    if (write_value("/sys/class/gpio/gpio%d/edge", gp->pin, value) == GPIO_NOK)
	return GPIO_NOK;
    gp->interrupt = interrupt;
    return GPIO_OK;
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
    INIT_ATOM(gpio_interrupt);
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
	if (gp->interrupt) {
	    struct erl_drv_event_data evd;
	    evd.events  = 0;
	    evd.revents = 0;
	    driver_event(ctx->port, gp->fd, &evd);
	    gp->interrupt = 0;
	}
	driver_select(ctx->port, gp->fd, ERL_DRV_USE, 0);
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
    gpio_pin_t** gpp;
    gpio_pin_t* gp;
    uint8_t pin_register;
    uint8_t pin;

    DEBUGF("gpio_drv: ctl: cmd=%u, len=%d", cmd, len);

    switch(cmd) {
    case CMD_INIT: {
	if (len != 2) goto badarg;
	pin_register = get_uint8(buf);
	pin = get_uint8(buf+1);

	// Pin already initialized
	if (find_pin(ctx, pin_register, pin, NULL) != NULL)
	    goto ok; // already open
	if (init_pin(ctx, pin_register, pin) == NULL)
	    goto error;
	goto ok;
    }

    case CMD_RELEASE: {
	if (len != 2) goto badarg;
	pin_register = get_uint8(buf);
	pin = get_uint8(buf+1);

	if (unexport(pin) != GPIO_OK)
	    return GPIO_NOK;

	if ((gp=find_pin(ctx, pin_register, pin, &gpp)) == NULL)
	    goto ok; // or badarg ???
	if (gp->interrupt) {
	    struct erl_drv_event_data evd;
	    evd.events  = 0;
	    evd.revents = 0;
	    driver_event(ctx->port, gp->fd, &evd);
	    gp->interrupt = 0;
	}
	// async close the file
	driver_select(ctx->port, gp->fd, ERL_DRV_USE, 0);
	*gpp = gp->next; // unlink
	driver_free(gp);
	goto ok;
    }

    case CMD_SET: {
	if (len != 2) goto badarg;
	pin_register = get_uint8(buf);
	pin = get_uint8(buf+1);

	// Localise pin or init it
	if ((gp = find_pin(ctx, pin_register, pin, NULL)) == NULL) {
	    if ((gp=init_pin(ctx, pin_register, pin)) == NULL)
		goto error;
	    if (gpio_set_direction(gp, gpio_direction_out) != GPIO_OK)
		goto error;
	}
	if (gpio_set_state(gp, gpio_state_high) != GPIO_OK)
	    goto error;
	goto ok;
    }

     case CMD_CLR: {
	if (len != 2) goto badarg;
	pin_register = get_uint8(buf);
	pin = get_uint8(buf+1);

	// Localise pin or init it
	if ((gp = find_pin(ctx, pin_register, pin, NULL)) == NULL) {
	    if ((gp = init_pin(ctx, pin_register, pin)) == NULL)
		goto error;
	    if (gpio_set_direction(gp, gpio_direction_out) != GPIO_OK)
		goto error;
	}
	if (gpio_set_state(gp, gpio_state_low) != GPIO_OK)
	    goto error;
	goto ok;
    }

    case CMD_GET: {
	uint8_t state;

	if (len != 2) goto badarg;
	pin_register = get_uint8(buf);
	pin = get_uint8(buf+1);

	// Localise pin or init it
	if ((gp = find_pin(ctx, pin_register, pin, NULL)) == NULL) {
	    if ((gp=init_pin(ctx, pin_register, pin)) == NULL)
		goto error;
	    if (gpio_set_direction(gp, gpio_direction_in) != GPIO_OK)
		goto error;
	}
	lseek(INT_EVENT(gp->fd), 0, SEEK_SET);
	if (read(INT_EVENT(gp->fd), &state, 1) != 1) {
	    gpio_errno = errno;
	    goto error;
	}
	DEBUGF("Read state %c for pin %d:%d", (char)state, pin_register, pin);
	state -= '0';
	if (state > 1) {
	    gpio_errno = EINVAL;
	    goto error;
	}
	return ctl_reply(1, &state, 1, rbuf, rsize);
    }

    case CMD_SET_DIRECTION: {
	uint8_t dir;
	if (len != 3) goto badarg;
	pin_register = get_uint8(buf);
	pin = get_uint8(buf+1);
	dir = get_uint8(buf+2);

	if ((gp = find_pin(ctx, pin_register, pin, NULL)) == NULL) {
	    if ((gp=init_pin(ctx, pin_register, pin)) == NULL)
		goto error;
	}
	if (gpio_set_direction(gp, (gpio_direction_t) dir) != GPIO_OK)
	    goto error;
	goto ok;
    }

    case CMD_GET_DIRECTION: {
	uint8_t dir;
	if (len != 2) goto badarg;
	pin_register = get_uint8(buf);
	pin = get_uint8(buf+1);

	if ((gp = find_pin(ctx, pin_register, pin, NULL)) == NULL) {
	    gpio_errno = ENOENT;
	    goto error;
	}
	dir = (uint8_t) gp->direction;
	DEBUGF("Read direction %d for pin %d:%d", 
	       dir, pin_register, pin);
	return ctl_reply(1, &dir, sizeof(dir), rbuf, rsize);
    }

    case CMD_SET_INTERRUPT: {
	struct erl_drv_event_data evd;
	gpio_interrupt_t intval;

	if (len != 3) goto badarg;
	pin_register = get_uint8(buf);
	pin = get_uint8(buf+1);
	intval = (gpio_interrupt_t) get_uint8(buf+2);

	// Localise pin or init it
	if ((gp = find_pin(ctx, pin_register, pin, NULL)) == NULL) {
	    if ((gp=init_pin(ctx, pin_register, pin)) == NULL)
		goto error;
	    if (gpio_set_direction(gp, gpio_direction_in) != GPIO_OK)
		goto error;
	}
	if (gpio_set_interrupt(gp, intval) == GPIO_NOK)
	    goto error;
	if (intval == gpio_interrupt_none) {
	    if (gp->interrupt) {
		evd.events = 0;
		evd.revents = 0;
		driver_event(ctx->port,gp->fd,&evd);
		gp->interrupt = 0;
	    }
	}
	else {
	    if (gp->direction != gpio_direction_in)
		goto badarg;
	    evd.events = POLLPRI | POLLERR;
	    evd.revents = 0;
	    if (driver_event(ctx->port, gp->fd, &evd) < 0)
		gpio_errno = errno;
	    gp->interrupt = intval;
	    gp->target = driver_caller(ctx->port);
	    goto ok;
	}
    }

    case CMD_GET_INTERRUPT: {
	uint8_t intval;
	if (len != 2) goto badarg;
	pin_register = get_uint8(buf);
	pin = get_uint8(buf+1);

	if ((gp = find_pin(ctx, pin_register, pin, NULL)) == NULL) {
	    gpio_errno = ENOENT;
	    goto error;
	}
	intval = (uint8_t) gp->interrupt;
	DEBUGF("Read interrupt %d for pin %d:%d", 
	       intval, pin_register, pin);
	return ctl_reply(1, &intval, sizeof(intval), rbuf, rsize);
    }

    default:
	goto badarg;
    }

ok:
    DEBUGF("Successfully executed %d on pin %d:%d", cmd, pin_register, pin);
    return ctl_reply(0, NULL, 0, rbuf, rsize);
badarg:
    gpio_errno = EINVAL;
error:
    {
        char* err_str = erl_errno_id(gpio_errno);
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
    gpio_ctx_t* ctx = (gpio_ctx_t*) d;
    gpio_pin_t* gp = ctx->first;
    uint8_t state;

    DEBUGF("gpio_drv: event called");
    while(gp && (gp->fd != e))
	gp = gp->next;
    if (!gp)
	return;
    // does this reset the interrupt?
    lseek(INT_EVENT(gp->fd), 0, SEEK_SET);
    if (read(INT_EVENT(gp->fd), &state, 1) != 1) {
	state = '?';
	goto error;
    }
    state -= '0';
    if (state > 1)
	goto error;
    if (ed->revents & POLLERR)
	goto error;
    if (ed->revents & POLLPRI) {
	ErlDrvTermData message[16];
	int i = 0;
	// {gpio_interrupt, <reg>, <pin>, <value>}
	push_atom(ATOM(gpio_interrupt));
	push_int(gp->pin_register);
	push_int(gp->pin);
	push_int(state);
	push_tuple(4);
	driver_send_term(ctx->port, gp->target, message, i); 
    }
    return;
error:
    DEBUGF("interrupt read error %c (revents=%x) for pin %d:%d", 
	   (char)state, ed->revents, gp->pin_register, gp->pin);
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
