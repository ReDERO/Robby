-module(pingpong).

-export([start/0, ping/2, pong/0]).

ping(0, Pong_PID) ->
    Pong_PID ! finished, 
    io:format("Пинг завершил работу~n", []);

ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()}, 
    receive
        pong ->
            io:format("Пинг получил понг~n", [])
    end, 
    ping(N - 1, Pong_PID).

pong() ->
    receive
        finished ->
            io:format("Понг завершил работу~n", []);
        {ping, Ping_PID} ->
            io:format("Понг получил пинг~n", []), 
            Ping_PID ! pong, 
            pong()
    end.

start() ->
    Pong_PID = spawn(pingpong, pong, []), %register(pong, spawn(tut16, pong, [])), 
    spawn(pingpong, ping, [3, Pong_PID]).