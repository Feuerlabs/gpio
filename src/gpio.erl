%% (C) 2012 Feuerlabs, Inc
%%
-module(gpio).

-export([open_pin/3, sequence/2, get_pin_state/1, i/0]).

open_pin(Pin, Direction, DefaultState) ->
    gen_server:call(gpio_server, {open_pin, Pin, Direction, DefaultState }).

sequence(Pin, T) ->
    gen_server:call(gpio_server, {sequence, Pin, T}).

get_pin_state(Pin) ->
    gen_server:call(gpio_server, {get_pin_state, Pin}).

i() ->
    gen_server:call(gpio_server, {i}).
