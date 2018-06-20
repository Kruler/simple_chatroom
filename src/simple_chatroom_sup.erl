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
-include("simple_chatroom.hrl").

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
	Port = application:get_env(?APP, port, 7000),
	ChatSessionSup = child_supervisor_spec(?MODULE, chat_session_sup, []),
	ConnectListener = child_worker_spec(connect_listener, [Port]),
	
	chatroom_util:supervisor_spec(one_for_one, [ChatSessionSup,
												ConnectListener
												]);

init([chat_session_sup]) ->
	Child = child_worker_spec(chat_session, []),
	supervisor_spec(simple_one_for_one, [Child]).

%%====================================================================
%% Internal functions
%%====================================================================
supervisor_spec(Type, Workers) when is_list(Workers) 
							   andalso (Type =:= simple_one_for_one
							   orelse Type =:= one_for_one
							   orelse Type =:= one_for_all)->
	{ok, {{Type, ?MRESTART, ?MTIME}, Workers}}.

child_worker_spec(Mod, Args) when is_list(Args) ->
	{Mod, {Mod, start_link, Args}, transient, ?SHUTDOWN, worker, [Mod]}.

child_supervisor_spec(Mod, SupName, Args) when is_list(Args) ->
	{Mod, {?MODULE, start_supervisor, [Mod, SupName, Args]}, 
		  transient, ?SHUTDOWN, supervisor, [Mod]}.

start_supervisor(Mod, SupName, Args) ->
	supervisor:start_link({local, SupName}, Mod, [SupName|Args]).