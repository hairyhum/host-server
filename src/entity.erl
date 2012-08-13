-module(entity).

-include("tables.hrl").

-export([fields/1, valid/1]).

-define(ITEM_FIELDS, [title, position, listId]).
-defin(LIST_FIELDS, [listName, position]).

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



valid(Fields) when field_names(Fields) /= ?ITEM_FIELDS, field_names(Fields) /= ?LIST_FIELDS ->
    {errors, [{fields, "invalid fields"}]};
valid(Fields) ->
    fields_errors(Fields).


fields_errors(Fields) ->
    FieldsErrors = lists:map(
        fun(#todo_field{name = Name, value = Value}) ->
            field_error(Name, Value)
        end,
        Fields),
    case lists:filter(
        fun(ok) -> false;
        (_) -> true
        end,
        FieldsErrors) of
        [] -> ok;
        Arr -> {errors, Arr}
    end.


field_error(Name, Value) when Name == listName; Name == title  ->
    if
        length(string:strip(Value)) /= 0 ->
            ok;
        true ->
            {Name, "should not be empty"}
    end;
field_error(position, Value) ->
    if
        is_float(Value) ->
            ok;
        true ->
            {position, "should be float"}
    end.









