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
%%%  GPIO application
%%%
%%% Created: 2012 by Magnus Feuer 
%%% @end
-module(gpio_app).

-behaviour(application).

-include("gpio.hrl").

%% Application callbacks
-export([start/2,
	 stop/1]).

%% Shortcut API
-export([start/0,
	 start/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the application.<br/>
%% Arguments are ignored, instead the options for the application server are 
%% retreived from the application environment (sys.config).
%%
%% @end
%%--------------------------------------------------------------------
-spec start(StartType:: normal | 
			{takeover, Node::atom()} | 
			{failover, Node::atom()}, 
	    StartArgs::term()) -> 
		   {ok, Pid::pid()} |
		   {ok, Pid::pid(), State::term()} |
		   {error, Reason::term()}.

start(_StartType, _StartArgs) ->
    ?ei("~p: start: arguments ignored.\n", [?MODULE]),
    Opts = case application:get_env(gpio, options) of
	       undefined -> [];
	       {ok, O1} -> O1
	   end,
    gpio_sup:start_link(Opts).


%%--------------------------------------------------------------------
%% @doc
%% Stops the application.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(State::term()) -> ok | {error, Error::term()}.

stop(_State) ->
    ok.

%% @private
start() ->
    start(normal, []).

start(Opts) ->
    gpio_sup:start_link(Opts).
