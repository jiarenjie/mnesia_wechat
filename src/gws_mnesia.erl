%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 五月 2017 13:40
%%%-------------------------------------------------------------------
-module(gws_mnesia).
-author("jiarj").
-behavior(gen_server).
-record(state, {}).
-define(SERVER, ?MODULE).

-include_lib("stdlib/include/qlc.hrl").

-export([start_link/0, read_all/1, read_by_role/2, save/2, out_2_model/2, read_by_pk/2, save_update/2]).
%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([save_test/0, save_test2/0, save_test3/0, save_update_test/0]).


read_all(M)->
  gen_server:call(?SERVER, {read_all, M}).
read_by_pk(M,PK)->
  gen_server:call(?SERVER, {read_by_pk, M,PK}).
read_by_role(M,Role)->
  gen_server:call(?SERVER, {read_by_role, M,Role}).
save(M,Model) ->
  gen_server:call(?SERVER, {save_one, M,Model}).
save_update(M,Model) ->
  gen_server:call(?SERVER, {save_update, M,Model}).



start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

handle_call({read_all,M}, _From, State) ->
  TableName = recop_util:table_name(M),
  Repo=do(qlc:q([ X || X <- mnesia:table(TableName)])),
  {reply, Repo, State};
handle_call({read_by_pk,M,Pk}, _From, State) ->
  Repo=read_one(M,Pk),
  {reply, Repo, State};
handle_call({read_by_role,M,Role}, _From, State) ->
  TableName = recop_util:table_name(M),
  User = recop_util:new(M,[{role,Role}]),
  Repo = do(qlc:q([User|| User <- mnesia:table(TableName)])),
  {reply, Repo, State};
handle_call({ save_one , M , Model}, _From, State) ->
  ok=save_one(M,Model),
  {reply, ok, State};
handle_call({save_update,M,Model}, _From, State) ->
  IndexName=apply(M,get_pk_name,[]),
  IndexValue=proplists:get_value(atom_to_binary(IndexName,utf8), Model),
  Ropo = read_one(M,IndexValue),
  case Ropo of
    []->
      save_one(M,Model);
    [Rec]->
      PL= out_2_model(M,Model),
      NewRec=recop_util:set(M,Rec,PL),
      ok = mnesia:dirty_write(NewRec)
  end,
  {reply, ok, State};
handle_call(_, _From, State)->
{reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("gws_user-admin terminated.~n", []),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

read_one(M,Pk)->
  TableName = recop_util:table_name(M),
  mnesia:dirty_read(TableName, Pk).
save_one(M,Model)->
  Pl=out_2_model(M,Model),
  Model2 = recop_util:new(M, Pl),
  ok=mnesia:dirty_write(Model2),
  ok.


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.


out_2_model(M,List)->
  Fields = get_out_fields(M),
  Model = apply(M,get_model,[]),
  F = fun out_2_model_one_field/2,
  {VL, _, _} = lists:foldl(F, {[], Model, List}, Fields),
  VL.
out_2_model_one_field(Field, {Acc, Model2OutMap, PL})  ->
  Config = maps:get(Field, Model2OutMap),
  Value = do_out_2_model_one_field(Config, PL),
  %% omit undefined key/value , which means not appear in PL
  case Value of
    undefined ->
      {Acc, Model2OutMap, PL};
    Value ->
      AccNew = [{Field, Value} | Acc],
      {AccNew, Model2OutMap, PL}
  end.

do_out_2_model_one_field({KeyInPL, integer}, PL) when is_binary(KeyInPL), is_list(PL) ->
  Value = proplists:get_value(KeyInPL, PL),
  case Value of
    undefined->undefined;
    _ -> binary_to_integer(Value)
end;

do_out_2_model_one_field(KeyInPL, PL) when is_binary(KeyInPL), is_list(PL) ->
  proplists:get_value(KeyInPL, PL).

get_out_fields(M) ->
  apply(M, fields, []).





save_test2()->
  User = [{<<"openid">> , <<"2">>}
    ,{<<"access_token">> , <<"2">>}
    ,{<<"refresh_token">> , <<"2">>}
    ,{<<"timestamp">> , <<"2">>}
    ,{<<"nickname">> , <<"def">>}
    ,{<<"headimgurl">> , <<"2">>}
    ,{<<"role">> , <<"empty">>}
    ,{<<"info">> , []}
    ,{<<"sex">> , <<"2">>}],
  ok = save(repo_user,User),
  ok.


save_test3()->
  User = [{<<"openid">> , <<"3">>}
    ,{<<"access_token">> , <<"3">>}
    ,{<<"refresh_token">> , <<"3">>}
    ,{<<"timestamp">> , <<"3">>}
    ,{<<"headimgurl">> , <<"3">>}
    ,{<<"sex">> , <<"2">>}],
  ok = save(repo_user,User),
  ok.

save_test()->
  User = #{openid => <<"1">>
      ,access_token => <<"1">>
      ,refresh_token => <<"1">>
      ,timestamp => <<"1">>
      ,nickname => <<"abc">>
      ,headimgurl => <<"1">>
      ,sex => 1},

  io:format("User~p",[User]),
  ok = save(repo_user,User),
  ok.
save_update_test()->
  Model=[{<<"openid">> , <<"4">>}
    ,{<<"role">> , <<"role">>}],
  save_update(repo_user,Model),
  ok.

