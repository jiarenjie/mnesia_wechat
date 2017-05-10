%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jan 2017 10:21 AM
%%%-------------------------------------------------------------------
-module(recop_util).
-author("simon").

%% API
-export([
  table_name/1
  , fields/1
  , new/2
  , new_empty/1
  , get/3
  , get/4
  , set/3
  , set/4
  , inc/3
  , inc/4
  , from_map/2
  , from_model/2
  , to_model/2
  , to_map/2
  , to_proplists/2
  , pr/2
  , lager/3
]).

-compile(export_all).
%% exprecs interface
table_name(M) when is_atom(M) ->
  [TableName] = apply(M, '#exported_records-', []),
  TableName.

fields(M) when is_atom(M) ->
  TableName = table_name(M),
  Fields = apply(M, '#info-', [TableName, fields]),
  Fields.

new_empty(M) when is_atom(M) ->
  TableName = table_name(M),
  apply(M, '#new-', [TableName]).

new(M, List) when is_atom(M), is_list(List) ->
  EmptyRec = new_empty(M),
  NewRec = apply(M, '#set-', [List, EmptyRec]),
  NewRec;

new(M, Map) when is_atom(M), is_map(Map) ->
  List = maps:to_list(Map),
  new(M, List).
%%-------------------------------------------------------------------
%% getter/setter
get(M, Repo, Key, Default) when is_atom(M), is_tuple(Repo), is_atom(Key) ->
  case get(M, Repo, Key) of
    undefined -> Default;
    Value -> Value
  end.

get(M, Model, up_index_key)
  when (M =:= protocol_up_req_pay)
  or (M =:= protocol_up_resp_pay)
  or (M =:= protocol_up_resp_query)
  ->
  VL = get(M, Model, [merId, txnTime, orderId]),
  list_to_tuple(VL);
get(M, Model, mcht_index_key)
  when (M =:= protocol_mcht_req_pay)
  or (M =:= protocol_mcht_req_query)
  or (M =:= protocol_mcht_req_refund)
  or (M =:= protocol_mcht_resp_refund)
  or (M =:= protocol_mcht_notify_refund)
  ->
  VL = get(M, Model, [mcht_id, mcht_txn_date, mcht_txn_seq]),
  list_to_tuple(VL);
get(M, Model, orig_mcht_index_key)
  when (M =:= protocol_mcht_req_refund)
  or (M =:= protocol_mcht_req_query)
  or (M =:= repo_mcht_txn_log_pt)
  ->
  VL = get(M, Model, [mcht_id, orig_mcht_txn_date, orig_mcht_txn_seq]),
  list_to_tuple(VL);
get(M, Repo, Key) when is_atom(M), is_tuple(Repo), is_atom(Key) ->
  Value = apply(M, '#get-', [Key, Repo]),
  Value;
get(M, Repo, Keys) when is_atom(M), is_tuple(Repo), is_list(Keys) ->
  Values = apply(M, '#get-', [Keys, Repo]),
  Values.

set(_M, _Repo, id, _Value) ->
  {error, pk_could_not_be_changed};
set(M, Repo, Key, Value) when is_atom(M), is_tuple(Repo), is_atom(Key) ->
  ValueList = [{Key, Value}],
  set(M, Repo, ValueList).

set(M, Repo, ValueLists) when is_atom(M), is_tuple(Repo), is_list(ValueLists) ->
  %%TableName = table_name(M),
  RepoNew = apply(M, '#set-', [ValueLists, Repo]),
  RepoNew.
%%-------------------------------------------------------------------
inc(_M, _Repo, id, _Value) ->
  {error, pk_could_not_be_changed};
inc(M, Repo, Key, IncValue) when is_atom(M), is_tuple(Repo), is_atom(Key), is_integer(IncValue) ->
  OldValue = get(M, Repo, Key),
  ValueList = [{Key, OldValue + IncValue}],
  RepoNew = apply(M, '#set-', [ValueList, Repo]),
  RepoNew.

inc(M, Repo, {Key, IncValue}) when is_atom(M), is_tuple(Repo), is_integer(IncValue), is_atom(Key) ->
  inc(M, Repo, Key, IncValue).

%%-------------------------------------------------------------------
%% model (map) <==> repo(record)
to_proplists(M, Repo) when is_atom(M), is_tuple(Repo) ->
  Fields = fields(M),
  ValueList = tl(tuple_to_list(Repo)),
  lists:zip(Fields, ValueList).

to_map(M, Repo) when is_atom(M), is_tuple(Repo) ->
  List = to_proplists(M, Repo),
  maps:from_list(List).

to_model(M, List) when is_atom(M), is_list(List) ->
  [to_model(M, Repo) || Repo <- List];
to_model(M, Repo) when is_atom(M), is_tuple(Repo) ->
  to_map(M, Repo).

from_model(M, Model) when is_atom(M), is_map(Model) ->
  from_map(M, Model).

from_map(M, Model) when is_atom(M), is_map(Model) ->
  List = maps:to_list(Model),
  %%TableName = table_name(M),
  EmptyR = new_empty(M),
  apply(M, '#fromlist-', [List, EmptyR]).
%%-------------------------------------------------------------------
pr(M, Repo) when is_atom(M), is_tuple(Repo) ->
  VL = to_proplists(M, Repo),
  L = [pr_field(Field, Value) || {Field, Value} <- VL],
  lists:flatten(L);
pr(M, Model) when is_atom(M), is_map(Model) ->
  L = [pr_field(Field, maps:get(Field, Model)) || Field <- maps:keys(Model)],
  lists:flatten(L).

pr_field(Field, Value) when
  ((Field =:= mcht_order_desc)
    or (Field =:= mcht_resp_msg)
    or (Field =:= resp_msg)
    or (Field =:= reqReserved)
    or (Field =:= signature)
    or (Field =:= reserved)
    or (Field =:= up_respMsg)
    or (Field =:= up_orderDesc)
    or (Field =:= up_reqReserved)
    or (Field =:= prod_bank_acct_corp_name)
    or (Field =:= prod_bank_name)
  ) ->
  io_lib:format("~p=~ts,", [Field, Value]);
pr_field(Field, Value) ->
  io_lib:format("~p=~p,", [Field, Value]).



trim_pretty(L) ->
  F = fun(Char, AccIn) when (Char =:= 32) or (Char =:= 10) ->
    AccIn;
    (Char, AccIn) ->
      [Char | AccIn]
      end,

  RL = lists:foldl(F, [], L),
  lists:reverse(RL).

%%-------------------------------------------------------------------
lager(Level, M, Model) ->
  String = pr(M, Model),
  lager_out(Level, M, String).

lager_out(debug, M, String) ->
  lager:debug("~p = ~ts", [M, String]);
lager_out(info, M, String) ->
  lager:info("~p = ~ts", [M, String]);
lager_out(error, M, String) ->
  lager:error("~p = ~ts", [M, String]).

