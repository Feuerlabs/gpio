%% (C) 2012 Feuerlabs, Inc
%%
-module(gpio).

-export([open_pin/3, sequence/2]).

open_pin(Pin, Direction, DefaultState) ->
    gen_server:call(gpio_server, {open_pin, Pin, Direction, DefaultState }).

sequence(Port, T) ->
    gen_server:call(gpio_server, {sequence, Port, T}).
