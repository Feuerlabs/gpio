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
%%% Created: 2013 by Malotte W Lönne
%%% @end
-ifndef(GPIO_HRL).
-define(GPIO_HRL, true).

-type unsigned() :: non_neg_integer().
-type posix() :: atom().

-define(GPIO_SRV, gpio_server).
-define(GPIO_DRV, gpio_drv).
-define(GPIO_PORT, gpio_port).

-define(dbg(String, List), 
	io:format("~p: " ++ String, [?MODULE | List])).

%% Convenience defines 
-ifndef(ee).
-define(ee(String, List), error_logger:error_msg(String, List)).
-endif.
-ifndef(ei).
-define(ei(String, List),  error_logger:info_msg(String, List)).
-endif.

-endif.
