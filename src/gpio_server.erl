%%%-------------------------------------------------------------------
%%% @author magnus <magnus@t520.local>
%%% @copyright (C) 2012, Feuerlabs, Inc. All Rights Reserved
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

%% timer exposed function 
-export([next_step/3]).

-define(SERVER, ?MODULE). 

%% State record.
-record(state, {
          pins = [] 
         }).

%%
%% Bitmasks used when interface port driver.
%%
-define (GPIODRV_CMD_MASK, 32#0000000F).
-define (GPIODRV_CMD_OPEN_FOR_INPUT,32#00000001).
-define (GPIODRV_CMD_OPEN_FOR_OUTPUT, 32#00000002).
-define (GPIODRV_CMD_OPEN_FOR_BIDIRECTIONAL, 32#00000003).
-define (GPIODRV_CMD_SET_STATE, 32#00000004).
-define (GPIODRV_CMD_GET_STATE, 32#00000005).
-define (GPIODRV_CMD_CLOSE, 32#00000006).

-define (GPIODRV_CMD_ARG_MASK, 32#000000F0).
-define (GPIODRV_CMD_ARG_LOW, 32#00000010).
-define (GPIODRV_CMD_ARG_HIGH, 32#00000020).

-define (GPIODRV_RES_OK, <<0:8>>).
-define (GPIODRV_RES_HIGH, <<1:8>>).
-define (GPIODRV_RES_LOW, <<2:8>>).
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
handle_call({sequence, Port, T}, _From, State) ->
    io:format("handle_call{ sequence, Port[~w] T[~w]}~n", [ Port, T ]),
    case get_pin_state(Port) of
        low -> { reply, next_step(Port, high, T), State };
        high -> { reply, next_step(Port, low, T), State };
        _Res -> { error, _Res, State }
    end;



handle_call({ open_pin, Pin, Direction, DefaultState}, _From, State) ->
    io:format("handle_call{ open_pin, Pin[~w] Direction[~w] DefaultState[~w] Lib[~w]}~n",
              [Pin, Direction, DefaultState, ?GPIO_DRIVER]),

    process_flag(trap_exit, true),

    LoadRes = erl_ddll:load(code:priv_dir(gpio), ?GPIO_DRIVER),

    if LoadRes =:= ok; LoadRes =:= { error, already_loaded } -> 
            open_gpio_pin(Pin, Direction, DefaultState, State);

       true -> { reply, LoadRes, State }
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
handle_info(_Info, State) ->
    {noreply, State}.

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


set_pin_state(Port, State) ->
    io:format("set_pin_state Port[~w] [~w]~n", [ Port, State ]),
    Res = port_control(Port,
                       ?GPIODRV_CMD_SET_STATE bor convert_to_bits(State),
                       [0]),
    io:format("~nset_pin_state State[~w,~w]~n", [ Res, convert_return_value(Res) ]),
    convert_return_value(Res).


get_pin_state(Port) ->
    io:format("get_pin_state Port[~w]~n", [ Port ]),
    Res = port_control(Port,
                         ?GPIODRV_CMD_GET_STATE,
                         [0]),
    io:format("~nget_pin_state State[~w,~w]~n", [ Res, convert_return_value(Res) ]),
    convert_return_value(Res).


next_step(Port, low, [Duration | T]) ->
    Res = set_pin_state(Port, low),
    if Res == ok ->
       timer:apply_after(Duration, gpio_server, next_step, [Port, high, T])
    end,
    Res;

next_step(Port, high, [Duration | T]) ->
    Res = set_pin_state(Port, high),
    if Res == ok ->
       timer:apply_after(Duration, gpio_server, next_step, [Port, low, T])
    end,
    Res;

next_step(Port, State, []) ->
    set_pin_state(Port, State).

open_gpio_pin(Pin, Direction, DefaultState, State) ->
    %%
    %% Check if we've already opened the pin.
    %%
    Existing = proplists:get_value(Pin, State),

    case Existing of 
        undefined ->
            Port = open_port({spawn, ?GPIO_DRIVER}, []),
            port_control(Port,
                         convert_to_bits(Direction) bor convert_to_bits(DefaultState),
                         integer_to_list(Pin)),

            NewState = lists:append(State, [ { Pin, Port } ]),
            { reply, Port, NewState };
        Port -> { reply, Port, State}
    end.
