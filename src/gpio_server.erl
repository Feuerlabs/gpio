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

%% Timer invoked function 
-export([next_step/2]).

-define(SERVER, ?MODULE).


%% State record.
-record(state, { pinlist = [] }).

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
handle_call({ sequence, Pin, NewSequence}, _From, State) ->
    {_, PinList } = State,

    case lists:keyfind(Pin, 1, PinList) of 
        false -> { reply, not_found, State };

        { _Pin, { Port, Sequence } } ->
            io:format("seq(): PinList= ~w Sequence= ~w NewSequence= ~w\n",
                      [ PinList, Sequence, NewSequence ] ),

            %% is NewSequence the first elements on the queue?
            case Sequence of 
                [_ | _] ->  
                    NewPinList = lists:keyreplace(Pin, 1, PinList, {Pin, { Port, Sequence ++ Ne
wSequence } }),
                    { reply, ok, { state, NewPinList }};

                [] -> 
                    [ Duration | T ] = Sequence ++ NewSequence,
                    NewPinList = lists:keyreplace(Pin, 1, PinList, {Pin, { Port, T } }),
                    io:format("seq(): NewPinList= ~w\n", [ NewPinList ] ),
                    set_pin_state(Port, get_inverse_state(get_default_pin_state(Port))),

                    timer:apply_after(Duration, 
                                      gpio_server, 
                                      next_step, 
                                      [Pin, get_default_pin_state(Port)]),
                    { reply, ok, { state, NewPinList }}
                end
    end;

handle_call({ get_pin_state, Pin }, _From, State) ->

    {_, PinList } = State,

    case proplists:get_value(Pin, PinList, not_found) of
        { Port, _Sequence } ->
            { reply, get_pin_state(Port), State };        
            
        not_found ->
            { reply, not_found, State }
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

handle_call({ pop_next_duration, Pin}, _From, State) ->
    {_, PinList } = State,

    case lists:keytake(Pin, 1, PinList) of 
        { value, { _, {Port, Sequence} }, TempState } ->
            case Sequence of 
                [ Duration | T ] -> 
                    { reply,  {ok, Port, Duration}, { state, [ { Pin, { Port, T } } ] ++ TempState }
                    };
 
                [] ->  { reply, { empty, Port }, State }
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

get_inverse_state(low) -> high;
get_inverse_state(high) -> low.


set_pin_state(Port, State) ->
    Res = port_control(Port,
                       ?GPIODRV_CMD_SET_STATE bor convert_to_bits(State),
                       [0]),
    io:format("set_pin_state Port[~w] [~w]~n", [ Port, State ]),
    convert_return_value(Res).


get_pin_state(Port) ->
    Res = port_control(Port,
                         ?GPIODRV_CMD_GET_STATE,
                         [0]),
    io:format("~nget_pin_state State[~w]~n", [ convert_return_value(Res) ]),
    convert_return_value(Res).


get_default_pin_state(Port) ->
    Res = port_control(Port,
                       ?GPIODRV_CMD_GET_DEFAULT_STATE,
                       [0]),
    io:format("~nget_default_pin_state State[~w]~n", [ Res ]),
    convert_return_value(Res).



next_step(Pin, PinValue ) ->
    io:format("next_step(): Pin:~w Value:~w\n", [ Pin, PinValue ]),

    %% next_step is invoked by the timer:apply_after function, which
    %% runs in its own process. This means that we need to pop off
    %% the next duration from the gen_serer's Sequence list for the
    %% given pin.
    
    PopRes = gen_server:call(gpio_server, { pop_next_duration, Pin }),

    io:format("next_step(): PopRes: ~w \n", [ PopRes ]),
    case PopRes of 
        { ok, Port, Duration } ->
            io:format("next_step(): Duration: ~w\n", [ Duration ]),
            set_pin_state(Port, PinValue),
            timer:apply_after(Duration, gpio_server, next_step, 
                              [Pin, get_inverse_state(PinValue) ]),
            ok;

        { empty, Port } -> 
            io:format("next_step(): Empty [~w]\n", [ { ok } ]),
            set_pin_state(Port, get_default_pin_state(Port)),
            empty;

        not_found ->
            not_found;

        _X -> 
            error
    end.




%%-----------
open_gpio_pin(Pin, Direction, DefaultPinValue, State) ->
    io:format("DefaultPinValue[~w] State[~w]\n", [ DefaultPinValue, State ] ),
    {_, PinList } = State,


    %%
    %% Check if we've already opened the pin.
    %%
    case lists:keyfind(Pin, 1, PinList) of
        %% No exist?
        false ->
            Port = open_port({spawn, ?GPIO_DRIVER}, []),
            Res = convert_return_value(
                    port_control(Port,
                                 convert_to_bits(Direction) bor 
                                     convert_to_bits(DefaultPinValue),
                                 integer_to_list(Pin))),

            if Res =:= ok ->
               NewState = { state, lists:append(PinList, [ { Pin, { Port, [] } } ] ) },
               { reply, ok, NewState };
               true -> { reply, Res, State }
            end;

        %% Exists?
        { _, { _, _ } } -> { reply, ok, State}
    end.
