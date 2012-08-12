-module(entity).

-include("tables.hrl").

-export([fields/1, valid/2]).

-define(ITEM_FIELDS, [a, b, c]).

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



valid(Entity, Fields = [#todo_field{name = "listName"}]) ->
    valid_fields(Fields, list);
valid(Entity, Fields) when length(Fields) == length(?ITEM_FIELDS) ->
    if lists:map(
        fun(Item) ->
            Item#todo_field.name
        end,
        Fields) /= ?ITEM_FIELDS ->
        {error, {fields, "invalid fields names"}};
        true ->
            valid_fields(Fields, item)
    end;
valid(_, _) ->
    {error, {fields, "invalid fields count"}}.



valid_fields([#todo_field{value = Value}], list) ->
    if string:len(string:strip(Value)) == 0 ->
        [{error, {value, "should not be empty"}}];
        true -> ok
    end;
valid_fields(Fields, item) ->
    ok.










