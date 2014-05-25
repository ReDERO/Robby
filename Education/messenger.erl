%%% Пользовательский интерфейс программы отправки сообщений

%%% login(Name)
%%%   Одновременно войти в систему на одном Erlang-узле может только один 
%%%   пользователь, указав соответствующее имя (Name). Если имя уже 
%%%   используется на другом узле, или если кто-то уже вошел в систему на 
%%%   этом узле, во входе будет отказано с соответствующим 
%%%   сообщением об ошибке.
%%% logoff()
%%%     Отключает пользователя от системы

%%% message(ToName, Message)
%%%   Отправляет Message ToName. Сообщения об ошибках выдаются, если
%%%   пользователь или получатель (ToName) не вошел в систему.
%%%
%%% Один узел в сети Erlang-узлов исполняет сервер, который поддерживает 
%%% данные о пользователях, вошедших в систему. Сервер зарегистрирован как 
%%% "messenger".
%%% Каждый узел, где имеется вошедший в систему пользователь, исполняет 
%%% клиентский процесс, зарегистрированный как "mess_client".
%%%
%%% Протокол между клиентскими процессами и сервером
%%% ----------------------------------------------------
%%% 
%%% На сервер: {ClientPid, logon, UserName}
%%% Ответ: {messenger, stop, user_exists_at_other_node} останавливает работу 
%%% клиента
%%% Ответ: {messenger, logged_on} подключение прошло успешно
%%%
%%% Когда клиент завершает работу по какой-либо причине
%%% На сервер: {'EXIT', ClientPid, Reason}
%%%
%%% На сервер: {ClientPid, message_to, ToName, Message} посылает сообщение
%%% Ответ: {messenger, stop, you_are_not_logged_on} останавливает работу 
%%% клиента
%%% Ответ: {messenger, receiver_not_found} пользователь с таким именем не 
%%% подключен
%%% Ответ: {messenger, sent} Сообщение отослано(без гарантии доставки)
%%%
%%% Клиенту: {message_from, Name, Message}, 
%%%
%%% Протокол между «командами» и клиентом
%%% ---------------------------------------------- 
%%%
%%% При запуске: messenger:client(Server_Node, Name)
%%% Клиенту: logoff
%%% Клиенту: {message_to, ToName, Message}
%%%
%%% Настройка: изменить функцию server_node() таким образом, чтобы она
%%% возвращала имя узла, на котором запущен сервер отправки сообщений

-module(messenger).
-export([start_server/0, server/0, 
         logon/1, logoff/0, message/2, client/2]).

%%% Измените функцию server_node() таким образом, чтобы она возвращала
%%% имя узла, на котором запущен сервер отправки сообщений

server_node() ->
    messenger@super.

%%% Это процесс сервера - "messenger"
%%% Список пользователей имеет формат 
%%% [{ClientPid1, Name1}, {ClientPid22, Name2}, ...]

server() ->
    process_flag(trap_exit, true), 
    server([]).

server(User_List) ->
    receive
        {From, logon, Name} ->
            New_User_List = server_logon(From, Name, User_List), 
            server(New_User_List);
        {'EXIT', From, _} ->
            New_User_List = server_logoff(From, User_List), 
            server(New_User_List);
        {From, message_to, To, Message} ->
            server_transfer(From, To, Message, User_List), 
            io:format("Текущее состояние списка: ~p~n", [User_List]), 
            server(User_List)
    end.

%%% Запуск сервера

start_server() ->
    register(messenger, spawn(messenger, server, [])).

%%% Сервер добавляет нового пользователя в список пользователей

server_logon(From, Name, User_List) ->
    %% проверить, не вошел ли пользователь в систему на другом узле 
    case lists:keymember(Name, 2, User_List) of
        true ->
            From ! #abort_client{message=user_exists_at_other_node}, 
            User_List;
        false ->
            From ! #server_reply{message=logged_on}, 
            link(From), 
            [{From, Name} | User_List]       %добавить в список пользователей
    end.

%%% Сервер удаляет пользователя из списка пользователей.

server_logoff(From, User_List) ->
    lists:keydelete(From, 1, User_List).

%%% Сервер пересылает сообщение от одного пользователя другому

server_transfer(From, To, Message, User_List) ->
    %% проверить, зарегистрирован ли пользователь, и кто он
    case lists:keysearch(From, 1, User_List) of
        false ->
            From ! #abort_client{message=you_are_not_logged_on};
        {value, {_, Name}} ->
            server_transfer(From, Name, To, Message, User_List)
    end.

%%% Если пользователь существует, послать сообщение

server_transfer(From, Name, To, Message, User_List) ->
    %% Найти получателя и послать сообщение
    case lists:keysearch(To, 2, User_List) of
        false ->
            From ! #server_reply{message=receiver_not_found};
        {value, {ToPid, To}} ->
            ToPid ! #message_from{from_name=Name, message=Message}, 
            From !  #server_reply{message=sent} 
    end.

%%% Пользовательские команды

logon(Name) ->
    case whereis(mess_client) of 
        undefined ->
            register(mess_client, 
                     spawn(messenger, client, [server_node(), Name]));
        _ -> already_logged_on
    end.

logoff() ->
    mess_client ! logoff.

message(ToName, Message) ->
    case whereis(mess_client) of % Проверка, запущен ли клиент
        undefined ->
            not_logged_on;
        _ -> mess_client ! {message_to, ToName, Message}, 
             ok
end.

%%% Клиентский процесс, который запускается на каждом пользовательском узле

client(Server_Node, Name) ->
    {messenger, Server_Node} ! #logon{client_pid=self(), username=Name}, 
    await_result(), 
    client(Server_Node).

client(Server_Node) ->
    receive
        logoff ->
            exit(normal);
        #message_to{to_name=ToName, message=Message} ->
            {messenger, Server_Node} ! 
                #message{client_pid=self(), to_name=ToName, message=Message}, 
            await_result();
        {message_from, FromName, Message} ->
            io:format("Сообщение от ~p: ~p~n", [FromName, Message])
    end, 
    client(Server_Node).
%%% Ждет ответа от сервера
await_result() ->
    receive
        #abort_client{message=Why} ->
            io:format("~p~n", [Why]), 
            exit(normal);
        #server_reply{message=What} ->
            io:format("~p~n", [What])
    after 5000 ->
            io:format("Сервер не отвечает~n", []), 
            exit(timeout)
    end.