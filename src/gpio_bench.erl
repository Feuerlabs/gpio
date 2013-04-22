%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Toggle a pin as fast as possible
%%% @end
%%% Created : 22 Apr 2013 by Tony Rogvall <tony@rogvall.se>

-module(gpio_bench).

-export([run/2, run/3]).

run(N, Pin) ->
    run(N, Pin, 0).

run(N, Pin, Delay) when is_integer(N), N >= 0,
			is_integer(Pin), Pin >= 0,
			is_integer(Delay), Delay >= 0 ->
    T0 = os:timestamp(),
    loop(N, Pin, Delay),
    T1 = os:timestamp(),
    timer:now_diff(T1, T0).


loop(0, _Pin, _Delay) ->
    ok;
loop(I, Pin, 0) ->
    gpio:set(Pin),
    gpio:clr(Pin),
    loop(I-1,Pin,0);
loop(I, Pin, Delay) ->
    gpio:set(Pin),
    receive
    after Delay -> ok
    end,
    gpio:clr(Pin),
    receive
    after Delay -> ok
    end,
    loop(I-1, Pin, Delay).

    

    
