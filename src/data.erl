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

save(Rec, Action) when (Action == update) or (Action == insert)  ->
    Record = transformations:transform(Rec),
    RecordName = element(1, Record),
    F = fun() ->
        mnesia:write_table_lock(RecordName),
        validations:validate(Record, Action),
        mnesia:write(Record)
    end,
    mnesia:transaction(F).

