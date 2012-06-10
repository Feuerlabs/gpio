%% (C) 2012 Feuerlabs, Inc
%%
-module(gpio).

-export([open_pin/4, sequence/2]).

open_pin(Pin, Direction, DefaultState, SharedLib) ->
    gen_server:call(gpio_server, {open_pin, Pin, Direction, DefaultState, SharedLib }).

sequence(Port, T) ->
    gen_server:call(gpio_server, {sequence, Port, T}).


