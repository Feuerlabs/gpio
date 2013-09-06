gpio
===========

gpio is a device driver application for GPIO (General Purpose IO) written in erlang and C.

IO-pins are accessed either in a generic, Linux-based, way using control files or by direct memory acces. <br>
Direct memory acces is currently implemented for the following processors:
<ul>
<li>bcm 2835 (Raspberry Pi)</li>
</ul>

### Dependencies

To build gpio you will need a working installation of Erlang R15B (or
later).<br/>
Information on building and installing [Erlang/OTP](http://www.erlang.org)
can be found [here](https://github.com/erlang/otp/wiki/Installation)
([more info](https://github.com/erlang/otp/blob/master/INSTALL.md)).

gpio is built using rebar that can be found [here](https://github.com/basho/rebar), with building instructions [here](https://github.com/basho/rebar/wiki/Building-rebar).

### Download

Clone the repository in a suitable location:

```
$ git clone git://github.com/Feuerlabs/gpio.git
```
### Build

Rebar will compile all needed dependencies.<br/>
Compile:

```sh
$ cd gpio
$ rebar compile
...
==> gpio (compile)
```

### Run

gpio is started in a standard erlang fashion:

```
$ erl
(node@host) 1> application:start(gpio).
```

### API

The interface has the following functions for accessing a single pin:
<ul>
<li>init</li>
<li>init_direct</li>
<li>release</li>
<li>set</li>
<li>get</li>
<li>clr</li>
<li>input</li>
<li>output</li>
<li>set_direction</li>
<li>get_direction</li>
<li>set_interrupt</li>
<li>get_interrupt</li>
</ul>

The interface has the following functions for accessing several pins using a mask:
<ul>
<li>set_mask</li>
<li>get_mask</li>
<li>clr_mask</li>
</ul>

### Example

An example on how to setup a gpio interrupt.

    gpio:set_interrupt(25, falling),
    receive 
        {gpio_interrupt, 0, 25, Value} ->
          io:format("pin 25, high to low\n")
    end.

Given that the gpio application is started with the following config
in environment (raspberry pi mode):

    {gpio, [{options, [{chipset,bcm2835}]}]}

Then we can do some fast gpio access

    gpio:init_direct(24),
    gpio:set_direction(24, low),
    gpio:set(24),
    timer:sleep(1010),
    gpio:clr(24),  
    timer:sleep(2210),
    ok.

This code will init pin 24 in direct access mode, using io registers
instead of the normal linux /sys/class/gpio file interface. Then
set pin 24 in output mode that default to a low output value. After that 
the pin is set to high for 1010 ms and then set to low again.
(This is the power-on sequence for a SIM900 GPRS module).


For details see the generated documentation.

### Documentation

gpio is documented using edoc. 
To generate the documentation do:

```
$ cd gpio
$ rebar doc
```

The result is a collection of html-documents under ```gpio/doc```.
