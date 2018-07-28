%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-07-16 16:51:34
%%%-------------------------------------------------------------------
-module(group_syncer).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("simple_chatroom.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec sync_group(GroupId :: integer()) -> ok | {error, Reason :: term()}.
sync_group(GroupId) ->
	case ets:lookup(?GROUP_TAB, GroupId) of
		[] ->
			{error, no_such_group};
		[Group] ->
			{ok, Group};
		{error, Reason} ->
			{error, Reason}
	end.

-spec sync_from_db() -> ok.
sync_from_db() ->
	?SERVER ! sync_from_db.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the SERVER
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	self() ! sync_from_db,
	case mnesia:subscribe({table, group, simple}) of
	    {ok, _} -> 
	    	group_handler:start(),
	    	{ok, #state{}};
	    {error, Reason1} ->
	        lager:warning("subscribe exchange failed: ~p", [Reason1]),
	        ignore
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    lager:warning("Can't handle request: ~p", [_Request]),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    lager:warning("Can't handle msg: ~p", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(sync_from_db, State) ->
	case ets:delete_all_objects(?GROUP_TAB) of
		true ->
			case chatroom_util:mnesia_query(group, []) of
				{ok, Groups} ->
					lists:foreach(
						fun(Group) ->
							ets:insert(?GROUP_TAB, Group)
					end, Groups);
				{error, Reason} ->
					lager:error("query groups failed:~p", [Reason])
			end;
		{error, Reason} ->
			lager:error("clear ~p failed: ~p", [?GROUP_TAB, Reason])
	end,
	{noreply, State};

handle_info({mnesia_table_event, {write, #group{id = GroupId} = Group, _}}, 
                                            #state{} = State)->
    ets:insert(?GROUP_TAB, Group),
    PName = chatroom_util:generate_name(group_handler, GroupId),
    case whereis(PName) of
    	undefined ->
    		group_handler:start(Group);
    	_ ->
    		PName ! {sync, Group}
    end,
    {noreply, State};

handle_info({mnesia_table_event, {delete, {group, GroupField}, _}}, 
                                            #state{} = State)->
    GroupId = erlang:element(1, GroupField),
    ets:delete(?GROUP_TAB, GroupId),
    group_handler:stop(GroupId),
    {noreply, State};


handle_info(_Info, State) ->
    lager:warning("Can't handle info: ~p", [_Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
