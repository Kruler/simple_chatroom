%%%-------------------------------------------------------------------
%% @doc simple_chatroom top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(simple_chatroom_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(APP, simple_chatroom).
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	Port = application:get_env(?APP, port),
	MWTime = application:get_env(?APP, max_wait_time),
	ChatSessionSup = chatroom_util:child_supervisor_spec(?MODULE, chat_session_sup, [MWTime]),
	ConnectListener = chatroom_util:child_worker_spec(connect_listener, [Port]),
	chatroom_util:supervisor_spec(one_for_one, [ConnectListener,
												ChatSessionSup])

init([chat_session_sup, MWTime]) ->
	Child = chatroom_util:child_worker_spec(chat_session, [MWTime]),
	chatroom_util:supervisor_spec(simple_one_for_one, [Child]).

%%====================================================================
%% Internal functions
%%====================================================================
