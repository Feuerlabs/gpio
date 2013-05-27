
#include "gpio_drv.h"

#define GPIO_BASE		0x20200000
#define GPIO_LEN		0x100
#define GPIO_FSEL0		(0x00/4)
#define GPIO_FSEL1		(0x04/4)
#define GPIO_SET0		(0x1c/4)
#define GPIO_SET1		(0x20/4)
#define GPIO_CLR0		(0x28/4)
#define GPIO_CLR1		(0x2c/4)
#define GPIO_LEV0		(0x34/4)
#define GPIO_LEV1		(0x38/4)
#define GPIO_PULLEN		(0x94/4)
#define GPIO_PULLCLK		(0x98/4)

// GPIO pin Function selection FSEL values
#define GPIO_MODE_IN		0
#define GPIO_MODE_OUT		1

static int set_direction(volatile uint32_t* gpio_reg, 
			 int reg, int pin, 
			 gpio_direction_t direction);
static int get_direction(volatile uint32_t* gpio_reg, 
			 int reg, int pin, 
			 gpio_direction_t* direction);
static int set_mask(volatile uint32_t* gpio_reg, 
		    int reg, uint32_t mask);
static int clr_mask(volatile uint32_t* gpio_reg, 
		    int reg, uint32_t mask);
static uint32_t get_mask(volatile uint32_t* gpio_reg, 
			 int reg);

gpio_methods_t gpio_bcm2835_meth = {
    .name = "bcm2835",
    .base = GPIO_BASE,     // base address in kernel space (not physical space)
    .len =  GPIO_LEN,      // length of register area
    .set_direction = set_direction,
    .get_direction = get_direction,
    .set_mask = set_mask,
    .clr_mask = clr_mask,
    .get_mask = get_mask
};


static int set_direction(volatile uint32_t* gpio_reg, 
			 int reg, int pin, 
			 gpio_direction_t direction)
{
    int index;
    int freg;
    int fbit;
    uint32_t mode;
    uint32_t fsel;

    DEBUGF("set_direction: pin %d:%d, direction %d", 
	   reg, pin, direction);
    
    if (direction == gpio_direction_in) 
	mode = GPIO_MODE_IN;
    else 
	mode = GPIO_MODE_OUT;

    freg = pin / 10;
    fbit = (pin % 10)*3;

    if (reg == 0)
	index = GPIO_FSEL0 + freg;
    else 
	index = GPIO_FSEL1 + freg;

    fsel = gpio_reg[index];
    fsel = (fsel & ~(7 << fbit)) | (mode << fbit);
    gpio_reg[index] = fsel;
    return 0;
}

static int get_direction(volatile uint32_t* gpio_reg, 
			 int reg, int pin, 
			 gpio_direction_t* direction)
{
    int index;
    int freg;
    int fbit;
    uint32_t mode;
    uint32_t fsel;

    DEBUGF("get_direction: pin %d:%d", reg, pin);
    
    freg = pin / 10;
    fbit = (pin % 10)*3;

    if (reg == 0)
	index = GPIO_FSEL0 + freg;
    else 
	index = GPIO_FSEL1 + freg;

    fsel = gpio_reg[index];
    mode = (fsel >> fbit) & 7;

    if (mode == GPIO_MODE_IN)
	*direction = gpio_direction_in;
    else if (mode == GPIO_MODE_OUT)
	*direction = gpio_direction_out; 
    else
	*direction = gpio_direction_undef;

    return 0;
}

static int set_mask(volatile uint32_t* gpio_reg, 
		    int reg, uint32_t mask)
{
    DEBUGF("set_mask: reg %d, mask %08x", reg, mask);
    if (reg == 0)
	gpio_reg[GPIO_SET0] = mask;
    else
	gpio_reg[GPIO_SET1] = mask;
    return 0;
}


static int clr_mask(volatile uint32_t* gpio_reg, 
		    int reg, uint32_t mask)
{
    DEBUGF("clr_mask: reg %d, mask %08x", reg, mask);
    if (reg == 0)
	gpio_reg[GPIO_CLR0] = mask;
    else
	gpio_reg[GPIO_CLR1] = mask;
    return 0;
}


static uint32_t get_mask(volatile uint32_t* gpio_reg, int reg)
{
    DEBUGF("get_mask: reg %d", reg);
    if (reg == 0)
	return gpio_reg[GPIO_LEV0];
    else
	return gpio_reg[GPIO_LEV1];
}
