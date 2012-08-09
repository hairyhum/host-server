-module(schema).
-include("tables.hrl").

-export([init/0]).

init() ->
    install([node()]),
    application:get_env(mnesia).

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    application:start(mnesia),
    mnesia:create_table(todo_user,
        [{attributes, record_info(fields, todo_user)},
            {disc_copies, Nodes}]),
    mnesia:create_table(todo_device,
        [{attributes, record_info(fields, todo_device)},
            {index, [#todo_device.login]},
            {disc_copies, Nodes},
            {type, bag}]),
    mnesia:create_table(todo_entity,
        [{attributes, record_info(fields, todo_entity)},
            {index, [#todo_entity.login]},
            {disc_copies, Nodes}]),
    mnesia:create_table(todo_field,
        [{attributes, record_info(fields, todo_field)},
            {disc_copies, Nodes},
            {type, bag}]),
    mnesia:create_table(todo_clock,
        [{attributes, record_info(fields, todo_clock)},
            {disc_copies, Nodes},
            {type, bag}]),
    application:stop(mnesia).

