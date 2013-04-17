%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------

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
-spec start_link(Args::list(term())) -> 
			{ok, Pid::pid()} | 
			ignore | 
			{error, Error::term()}.

start_link(Args) ->
    ?ei("~p: start_link: args = ~p\n", [?MODULE, Args]),
    try supervisor:start_link({local, ?MODULE}, ?MODULE, Args) of
	{ok, Pid} ->
	    {ok, Pid, {normal, Args}};
	Error -> 
	    ?ee("~p: start_link: Failed to start process, reason ~p\n",  
		[?MODULE, Error]),
	    Error
    catch 
	error:Reason ->
	    ?ee("~p: start_link: Try failed, reason ~p\n", 
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
    exit(stopped).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
%% @private
init(Args) ->
    ?ei("~p: init: args = ~p,\n pid = ~p\n", [?MODULE, Args, self()]),
    GpioServer = {?GPIO_SRV, 
		  {?GPIO_SRV, start_link, [Args]}, 
		  permanent, 5000, worker, [?GPIO_SRV]},
    Processes = [GpioServer],
    ?ei("~p: About to start ~p\n", [?MODULE, Processes]),
    {ok, { {one_for_one, 5, 10}, Processes} }.



