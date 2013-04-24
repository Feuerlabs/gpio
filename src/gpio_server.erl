%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2013 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Magnus Feuer <magnus@feuerlabs.com>
%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2013, Feuerlabs, Inc.
%%% @doc
%%%  GPIO interface
%%%
%%% Created: 11 Jun 2012 by Magnus Feuer 
%%% @end
%%%-------------------------------------------------------------------

-module(gpio_server).
-behavior(gen_server).

-include("gpio.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
         terminate/2, 
	 code_change/3]).

%% Loop data record.
-record(pin, {
	  pin_register::unsigned(),
	  pin::unsigned(), 
	  subs = []::list(), 
	  active = false::boolean()
	 }).
-record(loop_data, {
	  port, 
	  auto_export = true::boolean(),
	  pin_list = []::list(#pin{})
	 }).

%%
%% Bitmasks used when interface port driver.
%%
-define (GPIODRV_CMD_MASK, 16#0000000F).
-define (GPIODRV_CMD_OPEN_FOR_INPUT,16#00000001).
-define (GPIODRV_CMD_OPEN_FOR_OUTPUT, 16#00000002).
-define (GPIODRV_CMD_OPEN_FOR_BIDIRECTIONAL, 16#00000003).
-define (GPIODRV_CMD_SET_STATE, 16#00000004).
-define (GPIODRV_CMD_GET_STATE, 16#00000005).
-define (GPIODRV_CMD_CLOSE, 16#00000006).
-define (GPIODRV_CMD_GET_DEFAULT_STATE, 16#00000007).

-define (GPIODRV_CMD_ARG_MASK, 16#000000F0).
-define (GPIODRV_CMD_ARG_LOW, 16#00000010).
-define (GPIODRV_CMD_ARG_HIGH, 16#00000020).

-define (GPIODRV_RES_OK, <<0:8>>).
-define (GPIODRV_RES_LOW, <<1:8>>).
-define (GPIODRV_RES_HIGH, <<2:8>>).
-define (GPIODRV_RES_ILLEGAL_ARG, <<3:8>>).
-define (GPIODRV_RES_IO_ERROR, <<4:8>>).
-define (GPIODRV_RES_INCORRECT_STATE, <<5:8>>).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args::list()) ->  
			{ok, Pid::pid()} | 
			{error, Reason::atom()}.
start_link(Args) ->
    F =	case proplists:get_value(linked,Args,true) of
	    true -> start_link;
	    false -> start
	end,

    gen_server:F({local, ?GPIO_SRV}, ?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Args::list()) -> 
		  {ok, LD::#loop_data{}} |
		  {stop, Reason::atom()}.
init(Options) ->
    ?dbg("init: options ~p", [Options]),
    process_flag(trap_exit, true),
    case erl_ddll:load(code:priv_dir(gpio), ?GPIO_DRV) of
	LoadRes when LoadRes =:= ok; 
		     LoadRes =:= { error, already_loaded } ->
	    Dbg = case proplists:get_value(debug, Options, false) of
		     true -> " d";
		     false -> ""
		  end,
	    AC = case proplists:get_value(no_auto_create, Options, false) of
		     true -> " n";
		     false -> ""
		 end,

	    %% Add more chipsets when available
	    CS = case proplists:get_value(chip_set, Options, false) of
		     false -> "";
		     bcm2835 -> " b";
		     Other ->
			 ?ei("Unknown chip set %p, ignored",[Other]),
			 ""
		 end,
	    Cmd = atom_to_list(?GPIO_DRV) ++ Dbg ++ AC ++ CS,
	    Port = erlang:open_port({spawn_driver, Cmd},[binary]),
	    true = erlang:register(?GPIO_PORT, Port),
	    {ok, #loop_data{ port=Port }};
	{error, Reason} ->
	    ?ee("gpio: Failed loading driver, reason ~p", [Reason]),
	    {stop, no_driver}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-type call_request()::
	stop.

-spec handle_call(Request::call_request(),
		  From::{pid(), term()}, LD::#loop_data{}) ->
			 {reply, Reply::term(), LD::#loop_data{}} |
			 {noreply, LD::#loop_data{}} |
			 {stop, Reason::term(), Reply::term(), LD::#loop_data{}}.



handle_call(stop, _From, LD) ->
    {stop, normal, ok, LD};

handle_call(_Request, _From, LD) ->
    ?dbg("handle_call: unknown request ~p", [ _Request]),
    {reply, {error, bad_call}, LD}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), LD::#loop_data{}) -> 
			 {noreply, LD::#loop_data{}} |
			 {noreply, LD::#loop_data{}, Timeout::timeout()} |
			 {stop, Reason::term(), LD::#loop_data{}}.

handle_cast(_Msg, LD) ->
    ?dbg("handle_cast: unknown msg ~p", [_Msg]),
    {noreply, LD}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
%%--------------------------------------------------------------------
-type info()::
	{Port::unsigned(), {data, [BinPinValue::unsigned()]}} |
	{'EXIT', Pid::pid(), Reason::term()}. 

-spec handle_info(Info::info(), LD::#loop_data{}) -> 
			 {noreply, LD::#loop_data{}}.

handle_info(_Info, LD) ->
    ?dbg("handle_info: unknown info ~p", [_Info]),
    {noreply, LD}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process loop data when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn::term(), LD::#loop_data{}, Extra::term()) -> 
			 {ok, NewLD::#loop_data{}}.

code_change(_OldVsn, LD, _Extra) ->
    ?dbg("code_change: Old version ~p", [_OldVsn]),
    {ok, LD}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, LD) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), LD::#loop_data{}) -> 
		       no_return().

terminate(_Reason, _LD) ->
    ?dbg("terminate: reason ~p", [_Reason]),
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
convert_to_bits(output) -> ?GPIODRV_CMD_OPEN_FOR_OUTPUT;
convert_to_bits(input) -> ?GPIODRV_CMD_OPEN_FOR_INPUT;
convert_to_bits(bidirectional) -> ?GPIODRV_CMD_OPEN_FOR_BIDIRECTIONAL;
convert_to_bits(close) -> ?GPIODRV_CMD_CLOSE;
convert_to_bits(set_state) -> ?GPIODRV_CMD_SET_STATE;
convert_to_bits(get_state) -> ?GPIODRV_CMD_GET_STATE;
convert_to_bits(high) -> ?GPIODRV_CMD_ARG_HIGH;
convert_to_bits(low) -> ?GPIODRV_CMD_ARG_LOW.

convert_return_value(Bits) ->
    if Bits =:= ?GPIODRV_RES_OK -> ok;
       Bits =:= ?GPIODRV_RES_HIGH -> high;
       Bits =:= ?GPIODRV_RES_LOW -> low;
       Bits =:= ?GPIODRV_RES_ILLEGAL_ARG -> illegal_arg;
       Bits =:= ?GPIODRV_RES_IO_ERROR -> io_error;
       Bits =:= ?GPIODRV_RES_INCORRECT_STATE -> incorrect_state;
       true -> unknown_error
    end.

get_inverse_state(low) -> high;
get_inverse_state(high) -> low.




