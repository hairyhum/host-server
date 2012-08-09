-module(validations).

-include("tables.hrl").

-export([validate/2]).


validate(Item = #todo_user{}, insert) ->
    ok;
validate(Item = #todo_user{}, update) ->
    ok;
validate(_,_) ->
    ok.

