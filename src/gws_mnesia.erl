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

-export([start_link/0, read_all/1, read_by_openid/2, read_by_role/2, save/2, out_2_model/2]).
%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([save_test/0, save_test2/0, save_test3/0]).


read_all(M)->
  gen_server:call(?SERVER, {read_all, M}).
read_by_openid(M,Openid)->
  gen_server:call(?SERVER, {read_by_openid, M,Openid}).
read_by_role(M,Role)->
  gen_server:call(?SERVER, {read_by_role, M,Role}).
save(M,Model) ->
  gen_server:call(?SERVER, {save, M,Model}).


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

%%查询全部
handle_call({read_all,M}, _From, State) ->
  TableName = recop_util:table_name(M),
  Repo=do(qlc:q([ X || X <- mnesia:table(TableName)])),
  {reply, Repo, State};
%%通过openid查询
handle_call({read_by_openid,M,Openid}, _From, State) ->
  TableName = recop_util:table_name(M),
  Repo = mnesia:dirty_read(TableName, Openid),
  {reply, Repo, State};
%%通过角色查询
handle_call({read_by_role,M,Role}, _From, State) ->
  TableName = recop_util:table_name(M),
  User = recop_util:new(M,[{role,Role}]),
  Repo = do(qlc:q([User|| User <- mnesia:table(TableName)])),
  {reply, Repo, State};
%%保存用户
handle_call({save,M,Model}, _From, State) when is_map(Model) ->
  List = maps:to_list(Model),
  EmptyR = recop_util:new_empty(M),
  Repo = apply(M, '#fromlist-', [List, EmptyR]),
  ok = mnesia:dirty_write(Repo),
  {reply, ok, State};
handle_call({save,M,Model}, _From, State) when is_tuple(Model) ->
  ok = mnesia:dirty_write(Model),
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


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.


out_2_model(M,List)->
  Fields = get_out_fields(M),
  Model = apply(M,get_model,[]),
  F = fun out_2_model_one_field/2,
  {VL, _, _} = lists:foldl(F, {[], Model, List}, Fields),
  recop_util:new(M, VL).
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
  binary_to_integer(Value);
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
  Pl=out_2_model(repo_user,User),
  io:format("Pl~p",[Pl]),
  ok = save(repo_user,Pl),
  ok.


save_test3()->
  User = [{<<"openid">> , <<"3">>}
    ,{<<"access_token">> , <<"3">>}
    ,{<<"refresh_token">> , <<"3">>}
    ,{<<"timestamp">> , <<"3">>}
    ,{<<"headimgurl">> , <<"3">>}
    ,{<<"sex">> , <<"2">>}],
  Pl=out_2_model(repo_user,User),
  io:format("Pl~p",[Pl]),
  ok = save(repo_user,Pl),
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

