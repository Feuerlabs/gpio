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

-record(state,
	{
	  port
	}).

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
		  {ok, State::#state{}} |
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
	    CS = case proplists:get_value(chipset, Options, false) of
		     false -> "";
		     bcm2835 -> " b";
		     Other ->
			 ?ei("Unknown chip set %p, ignored",[Other]),
			 ""
		 end,
	    Cmd = atom_to_list(?GPIO_DRV) ++ Dbg ++ AC ++ CS,
	    Port = erlang:open_port({spawn_driver, Cmd},[binary]),
	    true = erlang:register(?GPIO_PORT, Port),
	    {ok, #state{ port=Port }};
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
		  From::{pid(), term()}, State::#state{}) ->
			 {reply, Reply::term(), State::#state{}} |
			 {noreply, State::#state{}} |
			 {stop, Reason::term(), Reply::term(), State::#state{}}.



handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    ?dbg("handle_call: unknown request ~p", [ _Request]),
    {reply, {error, bad_call}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), State::#state{}) -> 
			 {noreply, State::#state{}} |
			 {noreply, State::#state{}, Timeout::timeout()} |
			 {stop, Reason::term(), State::#state{}}.

handle_cast(_Msg, State) ->
    ?dbg("handle_cast: unknown msg ~p", [_Msg]),
    {noreply, State}.

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

-spec handle_info(Info::info(), State::#state{}) -> 
			 {noreply, State::#state{}}.

handle_info(_Info, State) ->
    ?dbg("handle_info: unknown info ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process loop data when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn::term(), State::#state{}, Extra::term()) -> 
			 {ok, NewState::#state{}}.

code_change(_OldVsn, State, _Extra) ->
    ?dbg("code_change: Old version ~p", [_OldVsn]),
    {ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), State::#state{}) -> 
		       no_return().

terminate(_Reason, _State) ->
    ?dbg("terminate: reason ~p", [_Reason]),
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
