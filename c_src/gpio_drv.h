/*
 *  Driect access methods
 */
#ifndef __GPIO_DRV_H__
#define __GPIO_DRV_H__

#include <stdint.h>
#include <stdarg.h>
#include <unistd.h>

#define GPIO_NOK -1
#define GPIO_OK 0

typedef enum  {
    gpio_chipset_none = 0,
    bcm2835 = 1,
} gpio_chipset_t;

typedef enum  {
    direct_access_off = 0,
    direct_access_on = 1,
} gpio_direct_access_t;

typedef enum {
    gpio_direction_undef = 0,
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


typedef struct {
    char*   name;   // name of chipset
    off_t   base;   // register base address
    size_t  len;    // register area length in bytes
    
    int (*set_direction)(volatile uint32_t* gpio_reg, int reg, int pin,
			 gpio_direction_t direction);
    int (*get_direction)(volatile uint32_t* gpio_reg, int reg, int pin,
			 gpio_direction_t* direction);
    int (*set_mask)(volatile uint32_t* gpio_reg, int reg, uint32_t mask);
    int (*clr_mask)(volatile uint32_t* gpio_reg, int reg, uint32_t mask);
    uint32_t (*get_mask)(volatile uint32_t* gpio_reg, int reg);
} gpio_methods_t;

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
#define DLOG_DEFAULT DLOG_NONE
#endif

#define DLOG(level,file,line,args...) do { \
	if (((level) == DLOG_EMERGENCY) ||				\
	    ((gpio_debug_level >= 0) && ((level) <= gpio_debug_level))) { \
	    gpio_emit_log((level),(file),(line),args);			\
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

extern void gpio_emit_log(int level, char* file, int line, ...);
extern int gpio_debug_level;

#endif
