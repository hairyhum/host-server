-module(transformations).

-include("tables.hrl").

-export([transform/2]).


transform(Item = #todo_entity{clocks = [_]}, _) ->
    Item#todo_entity{clocks = []};
transform(_,_) ->
    ok.

