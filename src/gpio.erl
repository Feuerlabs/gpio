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
	 init/2,
	 init_direct/1,
	 init_direct/2,
	 release/1,
	 release/2,
	 set/1, 
	 set/2, 
	 clr/1, 
	 clr/2, 
	 get/1,
	 get/2,
	 input/1,
	 input/2,
	 output/1,
	 output/2,
	 set_direction/2,
	 set_direction/3,
	 get_direction/1,
	 get_direction/2,
	 set_interrupt/2,
	 set_interrupt/3,
	 get_interrupt/1,
	 get_interrupt/2]).

%% Mask api, addressing several pins at once
-export([set_mask/1,
	 clr_mask/1,
	 set_mask/2,
	 clr_mask/2,
	 get_mask/1,
	 get_mask/2]).


%% Testing
-export([debug/1,
	 dump/0]).

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
-define (CMD_DUMP, 13).
-define (CMD_GET_MASK, 14).

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

%% Direct access
-define(DIRECT_ACCESS_OFF, 0).
-define(DIRECT_ACCESS_ON,  1).

%% Debug level
-define(DLOG_DEBUG,     7).
-define(DLOG_INFO,      6).
-define(DLOG_NOTICE,    5).
-define(DLOG_WARNING,   4).
-define(DLOG_ERROR,     3).
-define(DLOG_CRITICAL,  2).
-define(DLOG_ALERT,     1).
-define(DLOG_EMERGENCY, 0).
-define(DLOG_NONE,      -1).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Inits pin in pin register 0, i.e. prepares it for use.
%% @end
%%--------------------------------------------------------------------
-spec gpio:init(Pin::unsigned()) -> ok | {error,Reason::posix()}.
init(Pin) ->
    init(0, Pin).


%%--------------------------------------------------------------------
%% @doc
%% Inits pin in pin register, i.e. prepares it for use.
%% @end
%%--------------------------------------------------------------------
-spec gpio:init(PinReg::unsigned(), Pin::unsigned()) -> 
		       ok | {error,Reason::posix()}.
init(PinReg, Pin) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_INIT, <<PinReg:8, Pin:8, ?DIRECT_ACCESS_OFF:8>>).


%%--------------------------------------------------------------------
%% @doc
%% Inits pin in pin register 0, i.e. prepares it for use.
%% Actions on this pin will access physical address directly.
%% @end
%%--------------------------------------------------------------------
-spec gpio:init_direct(Pin::unsigned()) -> ok | {error,Reason::posix()}.
init_direct(Pin) ->
    init_direct(0, Pin).


%%--------------------------------------------------------------------
%% @doc
%% Inits pin in pin register, i.e. prepares it for use.
%% Actions on this pin will access physical address directly.
%% @end
%%--------------------------------------------------------------------
-spec gpio:init_direct(PinReg::unsigned(), Pin::unsigned()) -> 
			      ok | {error,Reason::posix()}.
init_direct(PinReg, Pin) 
  when is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_INIT, <<PinReg:8, Pin:8, ?DIRECT_ACCESS_ON:8>>).


%%--------------------------------------------------------------------
%% @doc Releases pin in pin register 0.  
%% @end
%%--------------------------------------------------------------------
-spec gpio:release(Pin::unsigned()) -> ok | {error,Reason::posix()}.
release(Pin) ->
    release(0, Pin).


%%--------------------------------------------------------------------
%% @doc Releases pin in pin register.  
%% @end
%%--------------------------------------------------------------------
-spec gpio:release(PinReg::unsigned(), Pin::unsigned()) -> 
			  ok | {error,Reason::posix()}.
release(PinReg, Pin) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_RELEASE, <<PinReg:8, Pin:8>>).


%%--------------------------------------------------------------------
%% @doc
%% Sets pin in pin register 0, i.e. sets it to 1.
%% @end
%%--------------------------------------------------------------------
-spec gpio:set(Pin::unsigned())  -> ok | {error,Reason::posix()}.
set(Pin) ->
    set(0, Pin).

%%--------------------------------------------------------------------
%% @doc
%% Sets pin in pin register, i.e. sets it to 1.
%% @end
%%--------------------------------------------------------------------
-spec gpio:set(PinReg::unsigned(), Pin::unsigned())  -> 
		      ok | {error,Reason::posix()}.
set(PinReg, Pin) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET, <<PinReg:8, Pin:8>>).

%%--------------------------------------------------------------------
%% @doc
%% Clears pin in pin register 0, i.e. sets it to 0.
%% @end
%%--------------------------------------------------------------------
-spec gpio:clr(Pin::unsigned())  -> ok | {error,Reason::posix()}.
clr(Pin) ->
    clr(0, Pin).

%%--------------------------------------------------------------------
%% @doc
%% Clears pin in pin register, i.e. sets it to 0.
%% @end
%%--------------------------------------------------------------------
-spec gpio:clr(PinReg::unsigned(), Pin::unsigned())  -> 
		      ok | {error,Reason::posix()}.
clr(PinReg, Pin) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_CLR, <<PinReg:8, Pin:8>>).

%%--------------------------------------------------------------------
%% @doc
%% Gets value for pin in pin register 0.
%% @end
%%--------------------------------------------------------------------
-spec gpio:get(Pin::unsigned()) -> 
		      {ok,uint1()} | {error,Reason::posix()}.
get(Pin) ->
    get(0, Pin).

%%--------------------------------------------------------------------
%% @doc
%% Gets value for pin in pin register.
%% @end
%%--------------------------------------------------------------------
-spec gpio:get(PinReg::unsigned(), Pin::unsigned()) -> 
		      {ok,uint1()} | {error,Reason::posix()}.
get(PinReg, Pin) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_GET, <<PinReg:8, Pin:8>>).

%%--------------------------------------------------------------------
%% @doc
%% Sets direction in for pin in pin register 0.
%% @end
%%--------------------------------------------------------------------
-spec gpio:input(Pin::unsigned()) -> ok | {error,Reason::posix()}.

input(Pin) ->
    set_direction(0, Pin, in).

%%--------------------------------------------------------------------
%% @doc
%% Sets direction in for pin in pin register.
%% @end
%%--------------------------------------------------------------------
-spec gpio:input(PinReg::unsigned(), Pin::unsigned()) -> 
			ok | {error,Reason::posix()}.

input(PinReg, Pin) ->
    set_direction(PinReg, Pin, in).

%%--------------------------------------------------------------------
%% @doc
%% Sets direction out for pin in pin register 0.
%% @end
%%--------------------------------------------------------------------
-spec gpio:output(Pin::unsigned()) -> ok | {error,Reason::posix()}.

output(Pin) ->
    set_direction(0, Pin, out).

%%--------------------------------------------------------------------
%% @doc
%% Sets direction out for pin in pin register.
%% @end
%%--------------------------------------------------------------------
-spec gpio:output(PinReg::unsigned(), Pin::unsigned()) -> 
			 ok | {error,Reason::posix()}.

output(PinReg, Pin) ->
    set_direction(PinReg, Pin, out).

%%--------------------------------------------------------------------
%% @doc
%% Sets direction for pin in pin register 0.
%% @end
%%--------------------------------------------------------------------
-type direction()::in | out | high | low.
	
-spec set_direction(Pin::unsigned(),
		    Dir::direction()) ->
			   ok | {error,Reason::posix()}.

set_direction(Pin, Dir) ->
    set_direction(0, Pin, Dir).

%%--------------------------------------------------------------------
%% @doc
%% Sets direction for pin in pin register.
%% @end
%%--------------------------------------------------------------------
-spec set_direction(PinReg::unsigned(), 
		    Pin::unsigned(),
		    Dir::direction()) ->
			   ok | {error,Reason::posix()}.

set_direction(PinReg, Pin, in) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_DIRECTION, <<PinReg:8,Pin:8,?DIR_IN>>);
set_direction(PinReg, Pin, out) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_DIRECTION, <<PinReg:8,Pin:8,?DIR_OUT>>);
set_direction(PinReg, Pin, high) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_DIRECTION, <<PinReg:8,Pin:8,?DIR_HIGH>>);
set_direction(PinReg, Pin, low) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_DIRECTION, <<PinReg:8,Pin:8,?DIR_LOW>>).

%%--------------------------------------------------------------------
%% @doc
%% Gets direction for pin in pin register 0.
%% @end
%%--------------------------------------------------------------------
-spec get_direction(Pin::unsigned()) -> 
			   {ok,in|out} | {error,Reason::posix()}.

get_direction(Pin) ->
    get_direction(0, Pin).

%%--------------------------------------------------------------------
%% @doc
%% Gets direction for pin in pin register.
%% @end
%%--------------------------------------------------------------------
-spec get_direction(PinReg::unsigned(), 
		    Pin::unsigned()) -> 
			   {ok,in|out} | {error,Reason::posix()}.

get_direction(PinReg, Pin) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    case call(?GPIO_PORT, ?CMD_GET_DIRECTION, <<PinReg:8, Pin:8>>) of
	{ok, ?DIR_IN}  -> {ok,in};
	{ok, ?DIR_OUT} -> {ok,out};
	Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Sets interrupt style for pin in pin register 0.
%% @end
%%--------------------------------------------------------------------
-type style()::none | rising | falling | both.

-spec set_interrupt(Pin::unsigned(),
		    Style::style()) ->
			   ok | {error,Reason::posix()}.

set_interrupt(Pin, Style) ->
    set_interrupt(0, Pin, Style).

%%--------------------------------------------------------------------
%% @doc
%% Sets interrupt style for pin in pin register.
%% @end
%%--------------------------------------------------------------------
-spec set_interrupt(PinReg::unsigned(), 
		    Pin::unsigned(),
		    Style::style()) ->
			   ok | {error,Reason::posix()}.

set_interrupt(PinReg, Pin, none) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_INTERRUPT, <<PinReg:8,Pin:8,?INT_NONE>>);
set_interrupt(PinReg, Pin, rising) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_INTERRUPT, <<PinReg:8,Pin:8,?INT_RISING>>);
set_interrupt(PinReg, Pin, falling) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_INTERRUPT, <<PinReg:8,Pin:8,?INT_FALLING>>);
set_interrupt(PinReg, Pin, both) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_INTERRUPT, <<PinReg:8,Pin:8,?INT_BOTH>>).

%%--------------------------------------------------------------------
%% @doc
%% Gets interrupt style for pin in pin register 0.
%% @end
%%--------------------------------------------------------------------
-spec get_interrupt(Pin::unsigned()) -> 
			   {ok,Style::style()} |
			   {error,Reason::posix()}.

get_interrupt(Pin) ->
    get_interrupt(0, Pin).

%%--------------------------------------------------------------------
%% @doc
%% Gets interrupt style for pin in pin register.
%% @end
%%--------------------------------------------------------------------
-spec get_interrupt(PinReg::unsigned(), 
		    Pin::unsigned()) -> 
			   {ok,Style::style()} |
			   {error,Reason::posix()}.

get_interrupt(PinReg, Pin) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Pin), Pin >= 0 ->
    case call(?GPIO_PORT, ?CMD_GET_INTERRUPT, <<PinReg:8, Pin:8>>) of
	{ok, ?INT_NONE}    -> {ok,none};
	{ok, ?INT_RISING}  -> {ok,rising};
	{ok, ?INT_FALLING} -> {ok,falling};
	{ok, ?INT_BOTH}    -> {ok,both};
	Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Sets pins in mask in pin register 0, i.e. sets them to 1.
%% @end
%%--------------------------------------------------------------------
-spec gpio:set_mask(Mask::unsigned()) -> ok | {error,Reason::posix()}.
set_mask(Mask) ->
    set_mask(0, Mask).

%%--------------------------------------------------------------------
%% @doc
%% Clears pins in mask in pin register 0, i.e. sets them to 0.
%% @end
%%--------------------------------------------------------------------
-spec gpio:clr_mask(Mask::unsigned()) -> ok | {error,Reason::posix()}.
clr_mask(Mask) ->
    clr_mask(0, Mask).

%%--------------------------------------------------------------------
%% @doc
%% Sets pins in mask in pin register, i.e. sets them to 1.
%% @end
%%--------------------------------------------------------------------
-spec gpio:set_mask(PinReg::unsigned(), Mask::unsigned()) ->
			        ok | {error,Reason::posix()}.
set_mask(PinReg, Mask) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Mask), Mask >= 0 ->
    call(?GPIO_PORT, ?CMD_SET_MASK, <<PinReg:8, Mask:32>>).

%%--------------------------------------------------------------------
%% @doc
%% Clears pins in mask in pin register, i.e. sets them to 0.
%% @end
%%--------------------------------------------------------------------
-spec gpio:clr_mask(PinReg::unsigned(), Mask::unsigned()) ->
			       ok | {error,Reason::posix()}.
clr_mask(PinReg, Mask) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Mask), Mask >= 0 ->
    call(?GPIO_PORT, ?CMD_CLR_MASK, <<PinReg:8, Mask:32>>).


%%--------------------------------------------------------------------
%% @doc
%% Read pins in mask in pin register 0
%% @end
%%--------------------------------------------------------------------
-spec get_mask(PinReg::unsigned()) -> 
		      {ok,Value::unsigned()} | {error,Reason::posix()}.

get_mask(Mask) when 
      is_integer(Mask), Mask >= 0 ->
    get_mask(0, Mask).

%%--------------------------------------------------------------------
%% @doc
%% Read pins in mask in pin register.
%% @end
%%--------------------------------------------------------------------
-spec get_mask(PinReg::unsigned(), Mask::unsigned()) -> 
		      {ok,Value::unsigned()} | {error,Reason::posix()}.

get_mask(PinReg, Mask) 
  when is_integer(PinReg), PinReg >= 0, is_integer(Mask), Mask >= 0 ->
    call(?GPIO_PORT, ?CMD_GET_MASK, <<PinReg:8, Mask:32>>).

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
%% @doc
%% Dumps driver data.
%% Debug level must be set to debug.
%% @end
%%--------------------------------------------------------------------
-spec gpio:dump() -> ok | {error,Reason::posix()}.

dump() ->
    call(?GPIO_PORT, ?CMD_DUMP, <<>>).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%% @private
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
