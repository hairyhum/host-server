-module(todo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("Starting todo server~n", []),

    {ok, IP}         = application:get_env(todo, ip),
    {ok, Port}       = application:get_env(todo, port),
    {ok, ServerName} = application:get_env(todo, server_name),
    {ok, DocRoot}    = application:get_env(todo, doc_root), 

    yaws:start_embedded(DocRoot, [
            {servername, ServerName},
            {listen, IP},
            {port, Port},
            {appmods, [{"/api", web_front}]} 
        ]),
    io:format("After db&yaws~n"),

    todo_sup:start_link().

stop(_State) ->
    ok.
