%% vim: ts=2 sw=2 et

-module(web_front).
-export([out/1]).

-include_lib("yaws/include/yaws_api.hrl").

out(Args) ->
  QList = yaws_api:parse_query(Args),
  QDist = dict:from_list(QList),

  {Check, Error, Vars} = parse_keys([
      {"user", string},
      {"pass", string},
      {"device", string}
    ], QDist),

  {Check2, JsonQuery} = case yaws_api:postvar(Args, "json") of
    undefined -> {false, undefined};
    {ok, Val} -> {Check, Val}
  end,

  if 
    Check2 -> 
      [User, Pass, Device] = Vars,
      {ok, _, Rows} = db:equery("SELECT * FROM \"Users\" WHERE login='$1'", [User]),
      if 
        length(Rows) == 1 ->
          UserPass = hexstring(crypto:sha1(Pass)),
          [Id, BLogin, BEmail, BPass] = Rows,
          DbPass = binary_to_list(BPass),
          if 
            DbPass == UserPass ->           
              proccessJson(Id, Device, JsonQuery);
            true ->
              jerror("Bad auth")
          end            
      end;
    true ->
      jerror("Bad request")
  end.


%
proccessJson(UserId, Device, JsonQuery) ->
  case catch jiffy:decode(JsonQuery) of
    {error, _} -> jerror("JSON parsing error");
    {Json} -> 
      Dict   = dict:from_list(Json),
      Type   = dict:fetch(<<"type">>, Dict),
      Params = dict:fetch(<<"params">>, Dict),
      proccessType(UserId, Device, Type, Params)
  end.

proccessType(UserId, Device, <<"sync">>, {Params}) ->
  locker:lock(UserId),
  O = lists:foldl(fun ({ItemName, {ItemBody}}) -> 
        Q = lists:foldl(fun ({EntityName, {EntityBody}}) ->
              Value  = lists:keyfind(<<"value">>, 2, EntityBody),
              JClocks = lists:keyfind(<<"clocks">>, 2, EntityBody),
              Clocks = jsonToClock(JClocks),
              {Id, DbValue, DbClocks, DbState} = loadFromDb(UserId, ItemName, EntityBody),
              if
                checkVectorClockDescendant(DbClocks, Clocks) ->
                  saveToDb(Id, Value, Clocks, DbState),
                  {[{"ok", "yes"}]};
                true ->
                  {[{"fail", "yes"}, {"value", Value}, {"clocks"}]}
            end, [], ItemBody),
      end, [], Params),

checkCreate(UserId, Name, EntityId, Clocks, Value) ->
  {ok, _, Rows} = db:equery("SELECT * FROM \"Entity\" WHERE (userId = '$1') AND (entityId = '$2') AND (name = '$3'", [UserId, EntityId, Name]),
  if
    length(Rows) > 0 -> {};
    true -> db:qquery("INSERT INTO \"Entity\" (\"entityId\", \"name\", \"userId\") VALUES ('$1', '$2', '$3')", [EntityId, Name, UserId])
  end.


loadFromDb(UserId, Name, EntityId) -> 
  Q = db:equery("SELECT id, value, state FROM \"Entity\" WHERE (userId = '$1') AND (entityId = '$2') AND (name = '$3')",
              [UserId, EntityId, Name]),
  {ok, _, [Row]} = Q,
  {Id, Value, State} = Row,
  C = db:equery("SELECT deviceId, seq FROM \"Clock\" WHERE entityId = '$1'", Id),
  [ok, _, [CC]] = C,
  Clocks = [{<<Name>>, Seq} || {Name, Seq} <- CC],
  {Id, Value, Clocks, State}.

saveToDb(Id, Value, Clocks, State) ->
  Q = db:equery("DELETE FROM \"Clock\" WHERE entityId = $1", [Id]),
  [ db:equery("INSERT INTO \"Clock\" VALUES ('$1', '$2', '$3', timestamp())", [Id, Device, Seq]) || {Device, Seq} <- Clocks],
  db:query("UPDATE \"Entity\" SET value = '$1', state = '$2' WHERE id='$3'", [Value, State, Id]).
  
jsonToClock({Json}) -> jsonToClock(Json).
jsonToClock(Json) ->
  [{lists:keyfind(<<"id">>,2,JC), lists:keyfind(<<"seq">>, 2, JC)} || JC <- Json].

clockToJson(Clock) ->
  [{[{"id", Id}, {"seq", Seq}]} || {Id, Seq} <-Clock].


checkVectorClockDescendant(Parent, Child) ->
  [R || {Pid, Pts} <- Parent, 
        R <- [case lists:keyfind(Pid, 1, Child) of 
                false -> fail;
                Cts when Cts < Pts -> fail;
                true -> ok
              end],
        R == fail
  ] == [].


%%%%
      
jok(Msg)    -> jjson("{\"ok\":\""    ++ Msg ++ "\"}").
jerror(Msg) -> jjson("{\"error\":\"" ++ Msg ++ "\"}").
jjson(Data) -> {content, "application/json", Data}.

parse_keys(KeyList, Dict) ->
  lists:foldr(
    fun ({Key, Type}, {Acc, Txt, Vals}) -> 
      InDict = dict:is_key(Key, Dict),
      if
        InDict ->
          Value = get_val(Key, Type, Dict),
          ValNotErr = Value /= error,
          if
            ValNotErr -> { % ok
                          Acc,
                          Txt,
                          [Value|Vals]
                         };
            true      -> { % Couldn't parse
                          false,
                          "Key '" ++ Key ++ "' is unparseble. " ++ Txt,
                          [error|Vals]
                         }
          end; % /if ValNotErr
        true          -> { % Not in dict
                          false,
                          "Key '" ++ Key ++ "' is missing. " ++ Txt,
                          [error|Vals]
                         }
      end % /if InDict
    end, {true, "", []}, KeyList).	

get_val(Name, integer,  Dict) ->
  {Value, _} = string:to_integer(dict:fetch(Name, Dict)),
  Value;

get_val(Name, float, Dict) ->
  {Value, _} = string:to_float(dict:fetch(Name, Dict)),
  if
    Value == error -> 0.0 +  get_val(Name, integer, Dict);
    true           -> Value
  end;

get_val(Name, string, Dict) ->
  dict:fetch(Name, Dict).

% http://www.enchantedage.com/hex-format-hash-for-md5-sha1-sha256-and-sha512
hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~128.16.0b", [X])).

