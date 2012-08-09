-module(entity).

-include("tables.hrl").

-export([fields/1, consistent/2]).

fields(Entity) ->
    Tr = fun() ->
        mnesia:read(todo_field, Entity)
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


consistent(Entity, MergeFields) ->
    case lists:filter(fun(Field) -> Field /= ok end, MergeFields) of
        [] ->
           ;
        MergeErrors ->
            {conflict, ok, MergeErrors}
    end.







