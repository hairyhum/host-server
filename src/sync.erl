-module(sync).

-include("tables.hrl").

-export([sync/3]).

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
        fun(Field#todo_field{entity = Entity}) ->
            OldState = data:read(#todo_field{entity = Entity}),
            sync_field(Field, OldState, Device)
        end,
        Fields),
    Login = Device#todo_device.login,
    Consistent = case lists:filter(fun(Field) -> Field /= ok end, MergedFields) of
        [] ->
            entity:valid(Entity, MergedFields);
        MergeErrors ->
            {conflict, ok, MergeErrors}
    end,
    case Consistent of
        ok ->
           {Id, field:batch_save(Fields), []};
        {conflict, Valid, MergeErrors} ->
           {Id, Valid, MergeErrors}
    end.

sync_field(Field, [], _) ->
    field:pretend(Field, insert);
sync_field(Field#todo_field{clocks = Clocks}, [OldField#todo_field{clocks = OldClocks}], Device) ->
    case updateble(OldClocks, Clocks, Device) of
        ok ->
            {field:pretend(Field, update), Field};
        conflict ->
            {conflict, OldField}
    end.












