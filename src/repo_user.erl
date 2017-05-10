%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 五月 2017 12:01
%%%-------------------------------------------------------------------
-module(repo_user).
-author("jiarj").
-compile({parse_trans, exprecs}).
-record(user_info,{openid,access_token,refresh_token,timestamp,role= <<"empty">>,nickname,headimgurl,sex,info=[]}).
-export_records([user_info]).
%% API
-export([init/1, init/0, get_model/0, fields/0]).

init()->
  {ok, Dir} = application:get_env(mnesia, dir),
  lager:debug("Mnesia dir = ~p", [Dir]),
  application:set_env(mnesia, dir, Dir),
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  mnesia:create_schema([node()]),
  ok = mnesia:start().

init(M) ->
  %% table init
  TableName = recop_util:table_name(M),
  Fields = recop_util:fields(M),

  {atomic, ok} = mnesia:create_table(
    TableName,
    [
      {attributes, Fields},
      {disc_copies, [node()]}
    ]
  ),
  ok.

get_model()->
  #{openid => <<"openid">>
    ,access_token => <<"access_token">>
    ,refresh_token => <<"refresh_token">>
    ,timestamp => <<"timestamp">>
    ,role => <<"role">>
    ,nickname => <<"nickname">>
    ,headimgurl => <<"headimgurl">>
    ,sex => {<<"sex">>,integer}
    ,info => <<"info">>}.

fields()->
  [openid,access_token,refresh_token,timestamp,role,nickname,headimgurl,sex,info].