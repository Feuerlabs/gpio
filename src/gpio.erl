%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------

-module(gpio).

-export([open_pin/3, sequence/3, get_pin_value/1, subscribe/2,i/0]).

open_pin(Pin, Direction, DefaultState) ->
    gen_server:call(gpio_server, {open_pin, Pin, Direction, DefaultState }).

sequence(Pin, T, Replace) ->
    gen_server:call(gpio_server, {sequence, Pin, T, Replace}).

get_pin_value(Pin) ->
    gen_server:call(gpio_server, {get_pin_state, Pin}).


subscribe(Pin, SubsPort) ->
    gen_server:call(gpio_server, {subscribe, Pin, SubsPort}).

i() ->
    gen_server:call(gpio_server, {i}).
