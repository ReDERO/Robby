%%% brain - отвечает за обработку входящего сообщения.

-module(brain).
-on_load(switch_on/0).

-export([proceed/1, switch_on/0, switch_off/0, restart/0, select/1]).

-define(path, "D:Robby/").

-import(output,[print/1]).

-define(me, tol).

compile()->
	compile:file("sources/output.erl"),
	compile:file("sources/members.erl").

%%% Запускается при подключении модуля.
%%% Присваивает имя ноде. (Позволяет подключиться к сети)
switch_on()->
	compile(),
	if
		node() =:= nonode@nohost ->
			net_kernel:start([?me, shortnames]),
			members:init(),
			io:format("Мозг функционирует.~n");
			%io:format("Brain is enabled.~n");
		true ->
			io:format("Невозможно включить Мозг, т.к. он уже функционирует.~n")
			%io:format("Brain is already enabled.~n")
	end.

%%% Убирает имя ноды. (Прекращает доступ к сети)
switch_off()->
	members:hide(),
	net_kernel:stop(),
	io:format("Мозг отключен.~n").
	%io:format("Brain is disabled.~n").

%%% Производит перезапуск мозга
restart()->
	members:hide(),
	net_kernel:stop(),
	net_kernel:start([?me, shortnames]),
	members:init(),
	io:format("Мозг перезапущен.~n").

%%% Отправляет входную строку на оброботку
% Input string
proceed(Input)->
	proceed(Input,[]).

%%% Обрабатывает входную строку, разделяя его на предложения
% Input string, Sentence
proceed([End|[]], Sentence) when End=:=$.; End=:=$?; End=:=$!->
	Words = string:tokens(Sentence," "),
	members:add_words(Words),
	some_function(End,Words);

proceed([End,$ ,Char|Rest], Sentence) when  ((Char>=$A) and (Char=<$Z) orelse (Char>=$А) and (Char=<$Я)) andalso ((End=:=$.) or (End=:=$?) or (End=:=$!)) ->
	Words = string:tokens(Sentence," "),
	members:add_words(Words),
	some_function(End,Words),
	proceed([Char|Rest],[]);

proceed([Char|Rest], Sentence)->
	proceed(Rest, Sentence ++ [Char]).

% Function type, List of words
some_function(Type, Words)->
	print(Type),
	print(Words).


%-------Wrappers----------
select(Table)->
	members:select(Table).
