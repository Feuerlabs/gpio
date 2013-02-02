%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author magnus <magnus@t520.local>
%%% @doc
%%%
%%% @end
%%% Created : 11 Jun 2012 by magnus <magnus@t520.local>
%%%-------------------------------------------------------------------

-module(gpio_server).
-behavior(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Timer invoked function
-export([next_step/2]).

-define(SERVER, ?MODULE).

%% State record.
-record(pin_list_elem, { pin, subs = [], port, active = false, seq = []}).
-record(state, { pin_list = [] }).

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
-define (GPIO_DRIVER, "gpio_driver").


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({ sequence, Pin, NewSeq, Replace}, _From, State) ->
    PinList = State#state.pin_list,

    case lists:keyfind(Pin, #pin_list_elem.pin, PinList) of
        #pin_list_elem { subs = Subs, port = Port, active = Active, seq = Seq } ->
            io:format("seq(): PinList= ~w Seq= ~w NewSeq= ~w\n",
                      [ State#state.pin_list, Seq, NewSeq ] ),

            %% Is NewSeq the first elements on the queue?
	    %% Are we to replace the existing sequence?
            case { Replace, Active } of
                { false, false } ->
                    NewPinList =
                        lists:keyreplace(Pin,
                                         #pin_list_elem.pin,
                                         PinList,
                                         #pin_list_elem {
                                           pin = Pin,
                                           subs = Subs,
                                           active = true,
                                           port = Port,
                                           seq = Seq ++ NewSeq
                                          }),


                    { reply, ok, #state{ pin_list = NewPinList }};

                { _, _ } ->
                    [ Duration | T ] = NewSeq,
                    NewPinList = lists:keyreplace(Pin,
                                                  #pin_list_elem.pin,
                                                  PinList,
                                                  #pin_list_elem {
                                                    pin = Pin,
                                                    active = true,
                                                    subs = Subs,
                                                    port = Port,
                                                    seq = T
                                                  }),
                    io:format("seq(): NewPinList= ~w\n", [ NewPinList ] ),
                    set_pin_value(Port, get_inverse_state(get_default_pin_value(Port))),

                    timer:apply_after(Duration,
                                      gpio_server,
                                      next_step,
                                      [Pin, get_default_pin_value(Port)]),
                    { reply, ok, #state { pin_list = NewPinList }}
            end;

        false -> { reply, not_found, State }
    end;


handle_call({ get_pin_value, Pin }, _From, State) ->

    case lists:keyfind(Pin, #pin_list_elem.pin, State#state.pin_list) of
        false ->
            { reply, not_found, State };

        #pin_list_elem { port = Port }  ->
            { reply, get_pin_value(Port), State }
    end;


handle_call({ i }, _From, State) ->
    { reply, State, State };


handle_call({ open_pin, Pin, Direction, DefaultState}, _From, State) ->
    process_flag(trap_exit, true),
    LoadRes = erl_ddll:load(code:priv_dir(gpio), ?GPIO_DRIVER),
    if LoadRes =:= ok; LoadRes =:= { error, already_loaded } ->
            open_gpio_pin(Pin, Direction, DefaultState, State);

       true -> { reply, LoadRes, State }
    end;

handle_call({ subscribe, Pin, SubsPort }, _From, State) ->
    PinList = State#state.pin_list,


    case lists:keytake(Pin, #pin_list_elem.pin, PinList) of
        {
          value,
          #pin_list_elem {
            subs = Subs,
            active = Active,
            port = Port,
            seq = Seq },
          TempState
        } -> {
          reply,
          ok,
          #state {
            pin_list = [ #pin_list_elem {
                            pin = Pin,
                            active = Active,
                            subs = Subs ++ [ SubsPort ],
                            port = Port,
                            seq = Seq
                           }
                       ] ++ TempState
           }
         };

        _ ->
            { reply, not_found, State }
    end;

handle_call({ pop_next_duration, Pin}, _From, State) ->
    PinList = State#state.pin_list,

    io:format("pop_next_duration(): PinList ~w\n", [ PinList ]),

    case lists:keytake(Pin, #pin_list_elem.pin, PinList) of
        { value, #pin_list_elem { subs = Subs, port = Port, seq = Seq }, TempState } ->
            case Seq of
                [ Duration | T ] -> {
                  reply,
                  {
                    ok,
                    Port,
                    Duration
                  },
                  #state {
                    pin_list = [ #pin_list_elem {
                                    pin = Pin,
                                    active = true,
                                    subs = Subs,
                                    port = Port,
                                    seq = T
                                   }
                               ] ++ TempState
                   }
                 };

                [] ->  {
                  reply,
                  {
                    empty,
                    Port
                  },
                  #state {
                    pin_list = [ #pin_list_elem {
                                    pin = Pin,
                                    active = false,
                                    subs = Subs,
                                    port = Port,
                                    seq = []
                                   }
                               ] ++ TempState
                   }
                 }
            end;

        false ->
            { reply, not_found, State }

    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    { Port, { data, [ BinPinValue ] } } = Info,

    case BinPinValue of
        1 -> PinValue = high;
        0 -> PinValue = low;
        _ -> PinValue = unknown
    end,

    case lists:keyfind(Port, #pin_list_elem.port, State#state.pin_list) of
        false ->
            {noreply, State};

        #pin_list_elem { pin = Pin, subs = Subs }  ->
            lists:map(fun(TargetSubs) ->
                              TargetSubs ! { gpio_pin_input, Pin, PinValue },
                              false
                      end,
                      Subs),
            { noreply, State }
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
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
terminate(_Reason, _State) ->
    ok.

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


set_pin_value(Port, PinValue) ->
    Res = port_control(Port,
                       ?GPIODRV_CMD_SET_STATE bor convert_to_bits(PinValue),
                       [0]),
    io:format("set_pin_value Port[~w] [~w]~n", [ Port, PinValue ]),
    convert_return_value(Res).


get_pin_value(Port) ->
    Res = port_control(Port,
                         ?GPIODRV_CMD_GET_STATE,
                         [0]),
    io:format("~nget_pin_value State[~w]~n", [ convert_return_value(Res) ]),
    convert_return_value(Res).


get_default_pin_value(Port) ->
    Res = port_control(Port,
                       ?GPIODRV_CMD_GET_DEFAULT_STATE,
                       [0]),
    io:format("~nget_default_pin_value State[~w]~n", [ Res ]),
    convert_return_value(Res).



next_step(Pin, PinValue ) ->
    io:format("next_step(): Pin:~w Value:~w\n", [ Pin, PinValue ]),

    %% next_step is invoked by the timer:apply_after function, which
    %% runs in its own process. This means that we need to pop off
    %% the next duration from the gen_serer's Seq list for the
    %% given pin.

    PopRes = gen_server:call(gpio_server, { pop_next_duration, Pin }),

    io:format("next_step(): PopRes: ~w \n", [ PopRes ]),
    case PopRes of
        { ok, Port, Duration } ->
            io:format("next_step(): Duration: ~w\n", [ Duration ]),
            set_pin_value(Port, PinValue),
            timer:apply_after(Duration, gpio_server, next_step,
                              [Pin, get_inverse_state(PinValue) ]),
            ok;

        { empty, Port } ->
            io:format("next_step(): Empty [~w]\n", [ { ok } ]),
            set_pin_value(Port, get_default_pin_value(Port)),
            empty;

        not_found ->
            not_found;

        _X ->
            error
    end.




%%-----------
open_gpio_pin(Pin, Direction, DefaultPinValue, State) ->
    io:format("DefaultPinValue[~w] State[~w]\n", [ DefaultPinValue, State ] ),

    PinList = State#state.pin_list,

    %%
    %% Check if we've already opened the pin.
    %%
    case lists:keyfind(Pin, #pin_list_elem.pin, PinList) of
        %% No exist?
        false ->
            Port = open_port({spawn, ?GPIO_DRIVER}, []),
            Res = convert_return_value(
                    port_control(Port,
                                 convert_to_bits(Direction) bor
                                     convert_to_bits(DefaultPinValue),
                                 integer_to_list(Pin))),

            if Res =:= ok ->
               NewState = #state {
                 pin_list = lists:append(PinList, [ #pin_list_elem { pin = Pin, port = Port } ])
                },
               { reply, ok, NewState };
               true -> { reply, Res, State }
            end;

        %% Exists?
        { _, { _, _ } } -> { reply, ok, State}
    end.


