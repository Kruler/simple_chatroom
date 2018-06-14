%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-06-14 09:24:57
%%%-------------------------------------------------------------------
-module(simple_chatroom).

%% API
-export([start/0]).

start() ->
	application:ensure_all_started(simple_chatroom).
