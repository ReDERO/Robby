%%% output - отвечает за вывод информации на экран.

-module(output).
-export([print/1]).

% List of words
print([])-> io:format("~n");
print([Word|Words]) when is_list(Word)->
	print(Word),
	print(Words);

% Word
print(Word)->
	io:format("~ts ",[unicode:characters_to_binary([Word])]).
