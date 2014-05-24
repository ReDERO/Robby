-module(brain).
-on_load(switch_on/0).

-export([proceed/1, switch_on/0, switch_off/0]).

-compile("D:Robby/output").
-import(output,[print/1]).

-define(me, tol).

%%% Запускается при подключении модуля.
%%% Присваивает имя ноде. (Позволяет подключиться к сети)
switch_on()->
	if
		node() =:= nonode@nohost ->
			net_kernel:start([?me, shortnames]),
			io:format("Brain is switched on.~n");
		true ->
			io:format("Brain have already been switched on.~n")
	end.

%%% Убирает имя ноды. (Прекращает доступ к сети)
switch_off()->
	net_kernel:stop(),
	io:format("Brain is switched off.~n").

%%% Отправляет входную строку на оброботку
% Input string
proceed(Input)->
	proceed(Input,[]).

%%% Обрабатывает входную строку, разделяя его на предложения
% Input string, Sentence
proceed([End|[]], Sentence) when End=:=$.; End=:=$?; End=:=$!->
	some_function(End,string:tokens(Sentence," "));

proceed([End,$ ,Char|Rest], Sentence) when  ((Char>=$A) and (Char=<$Z) orelse (Char>=$А) and (Char=<$Я)) andalso ((End=:=$.) or (End=:=$?) or (End=:=$!)) ->
	some_function(End,string:tokens(Sentence," ")),
	proceed([Char|Rest],[]);

proceed([Char|Rest], Sentence)->
	proceed(Rest, Sentence ++ [Char]).

% Function type, List of words
some_function(Type, Words)->
	print(Type),
	print(Words).