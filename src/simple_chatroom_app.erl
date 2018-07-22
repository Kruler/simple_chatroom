%%%-------------------------------------------------------------------
%% @doc simple_chatroom public API
%% @end
%%%-------------------------------------------------------------------

-module(simple_chatroom_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include("simple_chatroom.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	mnesia:start(),
	ets:new(?MESSAGE_TAB, [set, named_table, public]),
	ets:new(?NOTIFY_TAB, [set, named_table, public]),
	ets:new(?SOCKET_TAB, [set, named_table, public]),
	ets:new(?LOGIN_USERS, [set, named_table, public]),
	ets:new(?LOGIN_USERS, [set, named_table, public, {keypos, #group.id}]),
    simple_chatroom_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
