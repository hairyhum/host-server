-module(data).

-import("tables.hrl").

-export([save/2,read/1]).


read(Record) ->
    Id = element(2, Record),
    Table = element(1, Record),
    F = fun() ->
        mnesia:read(Table, Id)
    end,
    mnesia:transaction(F).

save(Record, Action) when (Action == update) or (Action == insert)  ->
    RecordName = element(1, Record),
    F = fun() ->
        mnesia:write_table_lock(RecordName),
        validations:validate(Record, Action),
        if Action == insert ->
            validate_insert(Record)
        end,
        mnesia:write(Record)
    end,
    mnesia:transaction(F).

validate_insert(Record) ->
    Id = element(2, Record),
    Table = element(1, Record),
    if
        {atomic, []} = mnesia:read(Table, Id) ->
            ok;
        true ->
            mnesia:abort({id_exist, Id, Table})
    end.