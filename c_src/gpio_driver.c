/*! @file gpio_driver.c

    Copyright (C) 2011, Feuerlabs, Inc. All rights reserved.
    Redistribution and use in any form, with or without modification, is strictly prohibited.
*/

#include "erl_driver.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>

#define DEBUG 1

static ErlDrvData gpio_start (ErlDrvPort port, char *command);

static void gpio_stop (ErlDrvData drv_data);

static ErlDrvSSizeT gpio_control(ErlDrvData drv_data,
                                 unsigned int command,
                                 char *buf,
                                 ErlDrvSizeT len,
                                 char **rbuf,
                                 ErlDrvSizeT rlen);

static void gpio_ready_input(ErlDrvData drv_data, ErlDrvEvent event);

#define GPIO_DRV_MAJOR_VER 1
#define GPIO_DRV_MINOR_VER 0

static ErlDrvEntry gpio_driver_entry = {
    NULL,                        // init
    gpio_start,
    gpio_stop,
    NULL,                        // output
    gpio_ready_input,            // ready_input
    NULL,                        // ready_output
    "gpio_driver",               // the name of the driver
    NULL,                        // finish
    NULL,                        // handle
    gpio_control,
    NULL,                        // timeout
    NULL,                        // outputv
    NULL,                        // ready_async
    NULL,                        // flush
    NULL,                        // call
    NULL,                        // event
    ERL_DRV_EXTENDED_MARKER,     // Extended
    ERL_DRV_EXTENDED_MAJOR_VERSION, // Driver major version
    ERL_DRV_EXTENDED_MINOR_VERSION, // Driver minor version
    0,                           // Driver flags
    NULL,                        // handle2
    NULL,                        // process exit
    NULL                         // stop_select
};

#define GPIO_EXPORT_PATH = "/sys/class/gpio/export";
#define GPIO_DIRECTION_PATH = "/sys/class/gpio/gpio%d/direction";
#define GPIO_VALUE_PATH = "/sys/class/gpio/gpio%d/direction";

#define GPIODRV_CMD_MASK  0x0000000F

#define GPIODRV_CMD_OPEN_FOR_INPUT  0x00000001
#define GPIODRV_CMD_OPEN_FOR_OUTPUT 0x00000002
#define GPIODRV_CMD_OPEN_FOR_BIDIRECTIONAL 0x00000003
#define GPIODRV_CMD_SET_STATE 0x00000004
#define GPIODRV_CMD_GET_STATE 0x00000005
#define GPIODRV_CMD_CLOSE  0x00000006
#define GPIODRV_CMD_GET_DEFAULT_STATE 0x00000007

#define GPIODRV_CMD_ARG_MASK 0x000000F0
#define GPIODRV_CMD_ARG_LOW 0x00000010
#define GPIODRV_CMD_ARG_HIGH 0x00000020

#define GPIODRV_RES_OK 0
#define GPIODRV_RES_LOW 1
#define GPIODRV_RES_HIGH 2
#define GPIODRV_RES_ILLEGAL_ARG 3
#define GPIODRV_RES_IO_ERROR 4
#define GPIODRV_RES_INCORRECT_STATE 5


typedef enum {
    GPIOUndefinedDirection = 0,
    GPIOIn = GPIODRV_CMD_OPEN_FOR_INPUT,
    GPIOOut = GPIODRV_CMD_OPEN_FOR_OUTPUT,
    GPIOInOut = GPIODRV_CMD_OPEN_FOR_BIDIRECTIONAL,
}  GPIODirection;


typedef enum  {
    GPIOUndefinedState = 0,
    GPIOLow = GPIODRV_CMD_ARG_LOW,
    GPIOHigh = GPIODRV_CMD_ARG_HIGH,
} GPIOState;


typedef struct {
    ErlDrvPort mPort;
    GPIODirection mDirection;
    GPIOState mDefaultState;
    GPIOState mCurrentState;
    int mPin;
    int mDescriptor; // To /sys/class/gpio/gpioX/vale
} GPIOContext;

DRIVER_INIT(gpio_driver)
{
    return &gpio_driver_entry;
}

static char state_to_return_value(GPIOState state)
{
    switch(state) {
    case GPIOLow:
        return GPIODRV_RES_LOW;

    case GPIOHigh:
        return GPIODRV_RES_HIGH;

    default:
        break;
    }

    return GPIODRV_RES_INCORRECT_STATE;
}

static ErlDrvData gpio_start(ErlDrvPort port, char *command)
{
    GPIOContext *ctx = 0;

    ctx = (GPIOContext*) driver_alloc(sizeof(GPIOContext));
    ctx->mDirection = GPIOUndefinedDirection;
    ctx->mCurrentState = GPIOUndefinedState;
    ctx->mDefaultState = GPIOUndefinedState;
    ctx->mPin = -1;
    ctx->mDescriptor = -1; // Not open
    ctx->mPort = port;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    return (ErlDrvData) ctx;
}

static void gpio_stop (ErlDrvData drv_data)
{
    driver_free(drv_data);
}

static unsigned char gpio_get_state(GPIOContext* context)
{
    return context->mCurrentState;
}

static unsigned char gpio_get_default_state(GPIOContext* context)
{
    return context->mDefaultState;
}


static unsigned char gpio_set_state(GPIOContext* context, GPIOState state)
{

#ifdef DEBUG
    write(context->mDescriptor, (state == GPIOHigh)?"1\n":"0\n", 2);
    context->mCurrentState = state;

    return GPIODRV_RES_OK;
#endif

    // Do we already have the correct state?
    if (context->mCurrentState == state)
        return GPIODRV_RES_OK;

    // Do we have the pin open?
    if (context->mDescriptor == -1) {
        return GPIODRV_RES_INCORRECT_STATE;
    }

    context->mCurrentState = state;
    switch(state) {
    case GPIOLow:
        write(context->mDescriptor, "0\n", 2);
        return GPIODRV_RES_OK;


    case GPIOHigh:
        write(context->mDescriptor, "1\n", 2);
        return GPIODRV_RES_OK;

    default:
        break;
    }
    return GPIODRV_RES_ILLEGAL_ARG;
}


static ErlDrvSSizeT gpio_open_port(GPIOContext* ctx)
{
    int desc = -1;
    char pin_buf[128];

    if (ctx->mDescriptor != -1) {
        close(ctx->mDescriptor);
        ctx->mDescriptor = -1;
    }

#ifdef DEBUG
    ctx->mCurrentState = ctx->mDefaultState;
    if (ctx->mDirection == GPIOIn ||
        ctx->mDirection == GPIOInOut) {

        sprintf(pin_buf, "/tmp/gpio%d.in", ctx->mPin);
        mkfifo(pin_buf, 0666);
        ctx->mDescriptor = open(pin_buf, O_RDONLY | O_NONBLOCK);
        if (ctx->mDescriptor == -1) {
            printf("Failed to open %s: %s\n\r", pin_buf, strerror(errno));
            return GPIODRV_RES_OK;
        }

        driver_select(ctx->mPort, (ErlDrvEvent) ctx->mDescriptor, DO_READ, 1);
        return GPIODRV_RES_OK;
    }

    // This is an output pin
    sprintf(pin_buf, "/tmp/gpio%d.out", ctx->mPin);
    mkfifo(pin_buf, 0666);
    ctx->mDescriptor = open(pin_buf, O_RDWR);
    if (ctx->mDescriptor == -1) {
        printf("Failed to open %s: %s\n\r", pin_buf, strerror(errno));
        return GPIODRV_RES_OK;
    }

    printf("gpio_open_port(DEBUG): current_state[%d]\r\n", ctx->mCurrentState);
    return GPIODRV_RES_OK;
#endif

    // Open the export file that we can use to ask the kernel
    // to export control to us. See kernel/Documentation/gpio.txt
    desc = open("/sys/class/gpio/export", O_WRONLY);

    // Did we fail to open the export file?
    if (desc == -1) {
        printf("Failed to open /sys/class/gpio/export: %s\r\n", strerror(errno));
        return GPIODRV_RES_IO_ERROR;
    }
    // Write  the pin number we want to use and close the export file
    sprintf(pin_buf, "%d", ctx->mPin);
    write(desc, pin_buf, strlen(pin_buf));
    close(desc);

    // We should now have a /sys/class/gpio/gpio<pin>/direction
    // file that we can open and write to in order to specify
    // the direction of the gpio pin.

    // Generate a correct path to the file
    sprintf(pin_buf, "/sys/class/gpio/gpio%d/direction", ctx->mPin);

    // Open the direciton file.
    desc = open(pin_buf, O_WRONLY);

    // Did we fail to open the direction file?
    if (desc == -1) {
        printf("Failed to open [%s]: %s\r\n", pin_buf, strerror(errno));
        return GPIODRV_RES_IO_ERROR;
    }

    // If this is an output pin, setup the initial state.
    if (ctx->mDirection == GPIOOut || ctx->mDirection == GPIOInOut) {
        if (ctx->mDefaultState == GPIOLow)
            write(desc, "low", 1);
        else
            write(desc, "high", 1);

        ctx->mCurrentState = ctx->mDefaultState;
    }
    else
        write(desc, "in", 1);

    close(desc);

    //
    // Open the 'value' file for either reading or writing, depending
    // on the direction of the pin.
    //
    sprintf(pin_buf, "/sys/class/gpio/gpio%d/value", ctx->mPin);

    // Open the value file in either read or write mode.
    switch (ctx->mDirection) {
    case GPIOIn:
        ctx->mDescriptor = open(pin_buf, O_RDONLY);
        break;

    case GPIOOut:
        ctx->mDescriptor = open(pin_buf, O_WRONLY);
        break;

    case GPIOInOut:
        ctx->mDescriptor = open(pin_buf, O_RDWR);
        break;

    default:
        close(ctx->mDescriptor);
        return GPIODRV_RES_ILLEGAL_ARG;
    }

    // Did we fail to open the direction file?
    if (ctx->mDescriptor == -1) {
        printf("Failed to open [%s]: %s\r\n", pin_buf, strerror(errno));
        return GPIODRV_RES_IO_ERROR;
    }

    if (ctx->mDirection == GPIOIn ||
        ctx->mDirection == GPIOInOut)
        driver_select(ctx->mPort, (ErlDrvEvent) ctx->mDescriptor, DO_READ, 1);

    return GPIODRV_RES_OK;
}


/*
 * buf contains port number to open.
 */
static ErlDrvSSizeT gpio_control (ErlDrvData drv_data,
                                  unsigned int command,
                                  char *buf,
                                  ErlDrvSizeT len,
                                  char **rbuf,
                                  ErlDrvSizeT rlen)
{
    GPIOContext* ctx = 0;

    ctx = (GPIOContext*) drv_data;

    switch(command & GPIODRV_CMD_MASK) {
    case GPIODRV_CMD_OPEN_FOR_INPUT:
    case GPIODRV_CMD_OPEN_FOR_OUTPUT:
    case GPIODRV_CMD_OPEN_FOR_BIDIRECTIONAL:
    {
        char* endptr = 0;
        char pin_str[16];
        // Make a stack copy of buf so that we can add a null.
        if (len > sizeof(pin_str) - 1) {
            // Avoid overflow.
            memcpy(pin_str, buf, sizeof(pin_str) - 1);
            pin_str[sizeof(pin_str)-1] = 0;
        } else {
            memcpy(pin_str, buf, len);
            pin_str[len] = 0;
        }

        ctx->mPin = strtoul(pin_str, &endptr, 10);

        // Did we have illegal characters in the pin number?
        if (*endptr != 0) {
            **rbuf = GPIODRV_RES_ILLEGAL_ARG;
            return 1;
        }

        printf("Will open pin [%d] Direction[%d] State[%d] command[%X]\r\n",
               ctx->mPin,
               (GPIODirection) (command & GPIODRV_CMD_MASK),
               (GPIOState) (command & GPIODRV_CMD_ARG_MASK),
               command);

        // Find out direction and default state.

        ctx->mDirection = (GPIODirection) (command & GPIODRV_CMD_MASK);
        ctx->mDefaultState = (GPIOState) (command & GPIODRV_CMD_ARG_MASK);

        **rbuf =  gpio_open_port(ctx);

        return 1;
    }


    // Are we polling?
    case GPIODRV_CMD_GET_STATE:
        **rbuf = state_to_return_value(gpio_get_state(ctx));
        return 1;

    case GPIODRV_CMD_GET_DEFAULT_STATE:
        **rbuf = state_to_return_value(gpio_get_default_state(ctx));
        return 1;

    // Are we setting state
    case GPIODRV_CMD_SET_STATE:
        **rbuf = gpio_set_state(ctx, command & GPIODRV_CMD_ARG_MASK);
        return 1;

    // Are we closing?
    case GPIODRV_CMD_CLOSE:
        if (ctx->mDescriptor != -1)
            driver_select(ctx->mPort, (ErlDrvEvent) ctx->mDescriptor, DO_READ, 0);

        close(ctx->mDescriptor);
        // FIXME: UNEXPORT
        ctx->mDescriptor = -1;
        **rbuf = GPIODRV_RES_OK;
        return 1;

    default:
        break;
    }

    puts("Illegal command\r");

    **rbuf = GPIODRV_RES_ILLEGAL_ARG;
    return 1;
}

static void gpio_ready_input(ErlDrvData drv_data, ErlDrvEvent event)
{
    GPIOContext* ctx = 0;
    char buf[2];
    ctx = (GPIOContext*) drv_data;
    ssize_t rd_res = 0;

    if (ctx->mDescriptor == -1) {
        printf("gpio_ready_input(): File not open\n");
        return;
    }

    rd_res = read(ctx->mDescriptor, buf, 2);
    if (rd_res != 2) {
        printf("gpio_ready_input(): Wanted 2 bytes, got %zd\n", rd_res);
        return;
    }
    driver_output(ctx->mPort, buf, 1);
}


