%%% members - отвечает за хранение данных.

-module(members).

-export([init/0, hide/0, select/1, add_word/1, add_words/1]).

-include_lib("stdlib/include/qlc.hrl").

-record(words, {word, time}).

init()->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(words, [{attributes, record_info(fields, words)}]).

hide()->
	mnesia:stop().

%%% Добавляет слово в словарь
% Word
add_word(Word) ->
    Row = #words{word=Word, time=now()},
    F = fun() ->
		mnesia:write(Row)
	end,
    mnesia:transaction(F).

%%% Добавляет список слов в словарь
% Word List
add_words([]) -> ok;
add_words([Word|Words]) ->
	add_word(Word),
	add_words(Words).


%% SQL equivalent
%%  SELECT * FROM words;
select(words) ->
    do(qlc:q([X || X <- mnesia:table(words)])).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.