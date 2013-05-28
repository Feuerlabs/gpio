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
%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2013, Feuerlabs, Inc.
%%% @doc
%%%  GPIO application supervisor
%%%
%%% Created: 2012 by Magnus Feuer 
%%% @end

-module(gpio_sup).
-behaviour(supervisor).

-include("gpio.hrl").

%% API
-export([start_link/1,
	 stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor. <br/>
%% Arguments are sent on to the supervisor.
%%
%% @end
%%--------------------------------------------------------------------
-type option()::
	no_auto_create |
	chipset |
	debug |
	linked.

-spec start_link(Args::list({option(), Value::term()})) -> 
			{ok, Pid::pid()} | 
			ignore | 
			{error, Error::term()}.

start_link(Args) ->
    ?ei("~p: start_link: args = ~p", [?MODULE, Args]),

    F =	case proplists:get_value(linked,Args,true) of
	    true -> start_link;
	    false -> start
	end,

    try supervisor:F({local, ?MODULE}, ?MODULE, Args) of
	{ok, Pid} ->
	    {ok, Pid, {normal, Args}};
	Error -> 
	    ?ee("~p: start_link: Failed to start process, reason ~p",  
		[?MODULE, Error]),
	    Error
    catch 
	error:Reason ->
	    ?ee("~p: start_link: Try failed, reason ~p", 
		[?MODULE,Reason]),
	    Reason
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stops the supervisor.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(StartArgs::list(term())) -> ok | {error, Error::term()}.

stop(_StartArgs) ->
    ?ei("~p: stop.", [?MODULE]),
    exit(normal).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
%% @private
init(Args) ->
    ?ei("~p: init: args = ~p,\n pid = ~p", [?MODULE, Args, self()]),
    GpioServer = {?GPIO_SRV, 
		  {?GPIO_SRV, start_link, [Args]}, 
		  permanent, 5000, worker, [?GPIO_SRV]},
    Processes = [GpioServer],
    ?dbg("~p: About to start ~p\n", [?MODULE, Processes]),
    {ok, { {one_for_one, 5, 10}, Processes} }.



