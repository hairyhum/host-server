-module(entiry).

-include("tables.hrl").

-export([fields/1]).

fields(#todo_entity{id = Id}) ->
    Tr = fun() ->
        mnesia:read(todo_field, Id)
    end,
    Fields = mnesia:transaction(Tr),
    read_clocks(Fields).

read_clocks(Fields) ->
    GetClocks = fun(Field) ->
        EntityId = Field#todo_field.entity_id,
        Name = Field#todo_field.name,
        Clocks = data:read(#todo_clock{field_spec = {EntityId, Name}}),
        Field#todo_field{clocks = Clocks}
    end,
    lists:map(GetClocks, Fields).








