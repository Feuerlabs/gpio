%% (C) 2012 Feuerlabs, Inc
%%


-module(gpio_server).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([next_step/3]).

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
-define (DEFAULT_GPIO_DRIVER, "gpio_driver").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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



handle_call({sequence, Port, T}, _From, State) ->
    io:format("handle_call{ sequence, Port[~w] T[~w]}~n", [ Port, T ]),
    case get_pin_state(Port) of
        low -> { reply, next_step(Port, high, T), State };
        high -> { reply, next_step(Port, low, T), State };
        _Res -> { error, _Res, State }
    end;


handle_call({ open_pin, Pin, Direction, DefaultState, SharedLib}, _From, State) ->
    io:format("handle_call{ open_pin, Pin[~w] Direction[~w] DefaultState[~w] Lib[~w]}~n",
              [Pin, Direction, DefaultState, SharedLib]),

    process_flag(trap_exit, true),

    Res = case erl_ddll:load("priv", SharedLib) of
        ok -> ok;
        { error, already_loaded } -> ok;
        _X -> _X
    end,

    case Res of
        ok ->
            Port = open_port({spawn, SharedLib}, []),
            port_control(Port,
                         convert_to_bits(Direction) bor convert_to_bits(DefaultState),
                         integer_to_list(Pin)),
            { reply, {ok, Port}, State };

        _ -> { reply, {Res, nil}, State}
    end.


init(_Arg) ->
    { ok, nil }.

terminate(_Reason, Port) ->
    convert_return_value(port_control(Port, ?GPIODRV_CMD_CLOSE, "")).

handle_info({_Msg, _Pid, _Reason}, Port) ->
    { noreply, Port }.

code_change(_OldVsn, Port, _Extra) ->
    {ok, Port}.

handle_cast(_Msg, State) ->
    {noreply, State}.


