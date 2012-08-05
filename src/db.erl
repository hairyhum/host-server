-module(db).

-behaviour(gen_server).

-export([start_link/0, handle_call/3, handle_cast/2, handle_info/2, 
  code_change/3, init/1, terminate/2]).

-export([squery/1, equery/2]).


start_link() ->
  io:format("Starting db... "),
  gen_server:start_link({local, db}, db, [], []).

squery(Query) ->
  gen_server:call(db, {squery, Query}).

equery(Query, Params) ->
  gen_server:call(db, {equery, Query, Params}).

%%
init(_) -> 
  {ok, DbHost}     = application:get_env(todo, dbhost),
  {ok, DbUser}     = application:get_env(todo, dbuser),
  {ok, DbPass}     = application:get_env(todo, dbpass),
  {ok, DbName}     = application:get_env(todo, dbname),

  {ok, C} = pgsql:connect(DbHost, DbUser, DbPass, [{database, DbName}]),
  io:format("ok.~n"),
  {ok, C}.

handle_call({squery, Query}, _Form, C) ->
  {reply, pgsql:squery(C, Query), C};

handle_call({equery, Query, Params}, _Form, C) ->
  {reply, pgsql:equery(C, Query, Params), C}.

handle_cast(_, C) ->
  {noreply, C}.

handle_info(_, C) ->
  {noreply, C}.

code_change(_, C, _) ->
  {ok, C}.

terminate(_, C) ->
  pgsql:close(C),
  ok.

