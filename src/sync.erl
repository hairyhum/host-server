-module(sync).

-include("tables.hrl").

-export([]).

sync(User, Token, Data) ->
    Lock = {sync, self()},
    global:set_lock(Lock),
    Device = case data:read() of
        [] -> create_device(User, Token);
        [Device] -> Device
    end,
    Result = sync_data(Data, Device),
    global:del_lock(Lock),
    Result.

sync_data(Items, Device) ->
    lists:map(
        fun(Item) ->
            sync_item(Item, Device)
        end,
        Items).

sync_item({Id, []}, _) ->
    {Id, ok, []};
sync_item({Id, Fields}, Device) ->
    MergedFields = lists:map(
        fun(Field) ->
            sync_field(Field, Device)
        end,
        Fields),
    MergeErrors = lists:filter(
        fun
        ({error, _}) -> true;
        ({ok, _}) -> false
        end,
        MergedFields
    ),
    Valid = if
        MergeErrors == [] ->
            Login = Device#todo_device.login,
            entity:consistent(#todo_entity{login = Login, id = Id}, MergedFields);
        true -> ok
    end,
    {Id, Valid, MergeErrors}.













