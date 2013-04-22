%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2013 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Magnus Feuer <magnus@feuerlabs.com>
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2013, Feuerlabs, Inc.
%%% @doc
%%%  GPIO interface
%%%
%%% Created: 2012 by Magnus Feuer 
%%% @end

-module(gpio).
-include("gpio.hrl").

%% Basic api
-export([init/1,
	 release/1,
	 set/1, 
	 clr/1, 
	 get/1,
	 input/1,
	 output/1,
	 set_direction/2,
	 get_direction/1,
	 set_interrupt/2,
	 get_interrupt/1]).

%% Extended api
%%-export([set_pin/2]).
-export([set_mask/1,
	 clr_mask/1,
	 set_mask/2,
	 clr_mask/2]).

%% Turn on/off debug
-export([debug/1]).

%% MUST BE EQUAL TO DEFINES IN gpio_drv.c !!!!
%% Port commands
-define (CMD_INIT,1).
-define (CMD_SET, 2).
-define (CMD_CLR, 3).
-define (CMD_GET, 4).
-define (CMD_SET_DIRECTION, 5).
-define (CMD_GET_DIRECTION, 6).
-define (CMD_SET_MASK, 7).
-define (CMD_CLR_MASK, 8).
-define (CMD_RELEASE, 9).
-define (CMD_SET_INTERRUPT, 10).
-define (CMD_GET_INTERRUPT, 11).
-define (CMD_DEBUG_LEVEL, 12).

%% Directions
-define(DIR_IN,      1).
-define(DIR_OUT,     2).
-define(DIR_LOW,     3).
-define(DIR_HIGH,    4).

%% Interrupt types
-define(INT_NONE,    0).
-define(INT_RISING,  1).
-define(INT_FALLING, 2).
-define(INT_BOTH,    3).

%% Debug level
-define(DLOG_DEBUG,     7).
-define(DLOG_INFO,      6).
-define(DLOG_NOTICE,    5).
-define(DLOG_WARNING,   4).
-define(DLOG_ERROR,     3).
-define(DLOG_CRITICAL,  2).
-define(DLOG_ALERT,     1).
-define(DLOG_EMERGENCY, 0).
-define(DLOG_NONE,     -1).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Inits pin in pin register 0, i.e. prepares it for use.
%% @end
%%--------------------------------------------------------------------
-spec gpio:init(Pin::unsigned()) -> ok | {error,Reason::posix()}.
init(Pin) 
  when is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_INIT, <<0:8, Pin:8>>).


%%--------------------------------------------------------------------
%%@doc Releases pin in pin register 0.  @end
%%--------------------------------------------------------------------
-spec gpio:release(Pin::unsigned()) -> ok | {error,Reason::posix()}.
release(Pin) 
  when is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_RELEASE, <<0:8, Pin:8>>).


%%--------------------------------------------------------------------
%% @doc
%% Sets pin in pin register 0, i.e. sets it to 1.
%% @end
%%--------------------------------------------------------------------
-spec gpio:set(Pin::unsigned())  -> ok | {error,Reason::posix()}.
set(Pin) 
  when is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET, <<0:8, Pin:8>>).

%%--------------------------------------------------------------------
%% @doc
%% Clears pinin pin register 0, i.e. sets it to 0.
%% @end
%%--------------------------------------------------------------------
-spec gpio:clr(Pin::unsigned())  -> ok | {error,Reason::posix()}.
clr(Pin) 
  when is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_CLR, <<0:8, Pin:8>>).

%%--------------------------------------------------------------------
%% @doc
%% Gets value for pin in pin register 0.
%% @end
%%--------------------------------------------------------------------
-spec gpio:get(Pin::unsigned()) -> boolean().
get(Pin) 
  when is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_GET, <<0:8, Pin:8>>).

%%--------------------------------------------------------------------
%% @doc
%% Sets direction in for pin in pin register 0.
%% @end
%%--------------------------------------------------------------------
-spec gpio:input(Pin::unsigned()) -> ok | {error,Reason::posix()}.

input(Pin) ->
    set_direction(Pin,in).

%%--------------------------------------------------------------------
%% @doc
%% Sets direction out for pin in pin register 0.
%% @end
%%--------------------------------------------------------------------
-spec gpio:output(Pin::unsigned()) -> ok | {error,Reason::posix()}.

output(Pin) ->
    set_direction(Pin,out).

%%--------------------------------------------------------------------
%% @doc
%% Sets direction for pin in pin register 0.
%% @end
%%--------------------------------------------------------------------
-spec set_direction(Pin::unsigned(),
		    Dir::in | out | high | low) ->
			   ok | {error,Reason::posix()}.

set_direction(Pin,in) when is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_DIRECTION, <<0:8,Pin:8,?DIR_IN>>);
set_direction(Pin,out) when is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_DIRECTION, <<0:8,Pin:8,?DIR_OUT>>);
set_direction(Pin,high) when is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_DIRECTION, <<0:8,Pin:8,?DIR_HIGH>>);
set_direction(Pin,low) when is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_DIRECTION, <<0:8,Pin:8,?DIR_LOW>>).

%%--------------------------------------------------------------------
%% @doc
%% Gets direction for pin in pin register 0.
%% @end
%%--------------------------------------------------------------------
-spec get_direction(Pin::unsigned()) -> 
			   {ok,in|out} | {error,Reason::posix()}.

get_direction(Pin) 
  when is_integer(Pin), Pin >= 0 ->
    case call(?GPIO_PORT, ?CMD_GET_DIRECTION, <<0:8, Pin:8>>) of
	{ok, ?DIR_IN}  -> {ok,in};
	{ok, ?DIR_OUT} -> {ok,out};
	Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Sets interrupt style for pin in pin register 0.
%% @end
%%--------------------------------------------------------------------
-spec set_interrupt(Pin::unsigned(),
		    Dir::none | rising | falling | both) ->
			   ok | {error,Reason::posix()}.

set_interrupt(Pin,none) when is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_INTERRUPT, <<0:8,Pin:8,?INT_NONE>>);
set_interrupt(Pin,rising) when is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_INTERRUPT, <<0:8,Pin:8,?INT_RISING>>);
set_interrupt(Pin,falling) when is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_INTERRUPT, <<0:8,Pin:8,?INT_FALLING>>);
set_interrupt(Pin,both) when is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_INTERRUPT, <<0:8,Pin:8,?INT_BOTH>>).

%%--------------------------------------------------------------------
%% @doc
%% Gets interrupt style for pin in pin register 0.
%% @end
%%--------------------------------------------------------------------
-spec get_interrupt(Pin::unsigned()) -> 
			   {ok,none|rising|falling|both} |
			   {error,Reason::posix()}.

get_interrupt(Pin) 
  when is_integer(Pin), Pin >= 0 ->
    case call(?GPIO_PORT, ?CMD_GET_INTERRUPT, <<0:8, Pin:8>>) of
	{ok, ?INT_NONE}    -> {ok,none};
	{ok, ?INT_RISING}  -> {ok,rising};
	{ok, ?INT_FALLING} -> {ok,falling};
	{ok, ?INT_BOTH}    -> {ok,both};
	Error -> Error
    end.

%% extended api

%%--------------------------------------------------------------------
-spec gpio:set_mask(Mask::unsigned()) -> ok | {error,Reason::posix()}.
set_mask(Mask) 
  when is_integer(Mask), Mask >= 0 ->
    set_mask(0, Mask).

%%--------------------------------------------------------------------
-spec gpio:clr_mask(Mask::unsigned()) -> ok | {error,Reason::posix()}.
clr_mask(Mask) 
  when is_integer(Mask), Mask >= 0 ->
    clr_mask(0, Mask).

%%--------------------------------------------------------------------
-spec gpio:set_mask(PinReg::unsigned(), Mask::unsigned()) ->
			        ok | {error,Reason::posix()}.
set_mask(PinReg, Mask) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Mask), Mask >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_MASK, <<PinReg:8, Mask:32>>).

%%--------------------------------------------------------------------
-spec gpio:clr_mask(PinReg::unsigned(), Mask::unsigned()) ->
			       ok | {error,Reason::posix()}.
clr_mask(PinReg, Mask) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Mask), Mask >= 0 ->
    call(?GPIO_PORT, ?CMD_CLR_MASK, <<PinReg:8, Mask:32>>).

%%--------------------------------------------------------------------
%% @doc
%% Controls debug.
%% @end
%%--------------------------------------------------------------------
-type level():: 
	none |
	info |
	notice |
	warning |
	error |
	critical |
	alert |
	emergency.

-spec gpio:debug(Level::level()) -> ok | {error,Reason::posix()}.

debug(none) ->
    call(?GPIO_PORT, ?CMD_DEBUG_LEVEL, <<?DLOG_NONE:8>>);
debug(debug) ->
    call(?GPIO_PORT, ?CMD_DEBUG_LEVEL, <<?DLOG_DEBUG:8>>);
debug(info) ->
    call(?GPIO_PORT, ?CMD_DEBUG_LEVEL, <<?DLOG_INFO:8>>);
debug(notice) ->
    call(?GPIO_PORT, ?CMD_DEBUG_LEVEL, <<?DLOG_NOTICE:8>>);
debug(warning) ->
    call(?GPIO_PORT, ?CMD_DEBUG_LEVEL, <<?DLOG_WARNING:8>>);
debug(error) ->
    call(?GPIO_PORT, ?CMD_DEBUG_LEVEL, <<?DLOG_ERROR:8>>);
debug(critical) ->
    call(?GPIO_PORT, ?CMD_DEBUG_LEVEL, <<?DLOG_CRITICAL:8>>);
debug(alert) ->
    call(?GPIO_PORT, ?CMD_DEBUG_LEVEL, <<?DLOG_ALERT:8>>);
debug(emergency) ->
    call(?GPIO_PORT, ?CMD_DEBUG_LEVEL, <<?DLOG_EMERGENCY:8>>).



%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
call(Port, Cmd, Data) ->
    case erlang:port_control(Port, Cmd, Data) of
	<<0>> ->
	    ok;
	<<255,E/binary>> -> 
	    {error, erlang:binary_to_atom(E, latin1)};
	<<1,Y>> -> {ok,Y};
	<<2,Y:16/native-unsigned>> -> {ok, Y};
	<<4,Y:32/native-unsigned>> -> {ok, Y};
	<<3,Return/binary>> -> {ok,Return}
    end.
