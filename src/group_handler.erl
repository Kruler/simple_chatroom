%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-07-16 16:51:34
%%%-------------------------------------------------------------------
-module(group_handler).

-behaviour(gen_server).

%% API
-export([group_chat/3,
		 req_add_group/2,
		 rep_add_group/5,
		 start/0,
		 start/1,
		 stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("simple_chatroom.hrl").

-record(state, {id,
				owner,
				members,
				managers,
				forbided,
				reqs = dict:new(),
				req_count = 0
				}).

%%%===================================================================
%%% API
%%%===================================================================
group_chat(GroupId, UId, Context) ->
	PName = chatroom_util:generate_pname(?MODULE, GroupId),
	gen_server:cast(PName, {group_chat, UId, Context}).

req_add_group(GroupId, UId) ->
	PName = chatroom_util:generate_pname(?MODULE, GroupId),
	gen_server:cast(PName, {req_add_group, UId}).

rep_add_group(GroupId, UId, SetedUId, PushId, Rep) ->
	PName = chatroom_util:generate_pname(?MODULE, GroupId),
	gen_server:cast(PName, {rep_add_group, UId, SetedUId, PushId, Rep}).		

start() ->
	case chatroom_util:mnesia_query(group, []) of
		{ok, Groups} ->
			lists:foreach(fun(#group{id = GroupId}) -> group_handler:start(GroupId) end, Groups);
		{error, Reason} ->
			lager:error("start group failed:~p", [Reason])
	end.

start(GroupId) ->
	PName = chatroom_util:generate_pname(?MODULE, GroupId),
    supervisor:start_child(group_handler_sup, [GroupId]).

stop(GroupId) ->
	PName = chatroom_util:generate_pname(?MODULE, GroupId),
	gen_server:cast(PName, stop).


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
init([GroupId]) ->
	self() ! sync,
    {ok, #state{id = GroupId}}.

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
handle_cast({group_chat, UId, Context}, State) ->
	#state{members = Members, 
		   id = GroupId,
		   forbided = Forbided} = State,
	case sets:is_element(UId, Forbided) of
		false ->
			sets:foldl(
				fun(Member, _) ->
					message_router:send_message([GroupId, UId], Member, Context)
			end, ok, Members);
		true ->
			lager:warning("user ~p is forbided", [UId])
	end,
	{noreply, State};

handle_cast({req_add_group, UId}, State) ->
	#state{managers = Managers,
		   reqs = Reqs,
		   req_count = ReqC,
		   id = GroupId} = State,
	sets:foldl(
		fun(Manager, _) ->
			message_router:send_notify(Manager, ?REQ_ADD_GROUP, [UId, GroupId, ReqC])
	end, ok, Managers),
	{noreply, State#state{req_count = ReqC + 1,
						  reqs = dict:store(ReqC, UId, Reqs)}};

handle_cast({rep_add_group, UId, SetedUId, PushId, Rep}, State) ->
	#state{managers = Managers,
		   reqs = Reqs,
		   id = GroupId} = State,
	case dict:find(PushId, Reqs) of
		UId ->
			case Rep of
				?RESP_OK ->
					add_group(GroupId, SetedUId);
				_ ->
					ignore
			end,
			sets:foldl(
				fun(Manager, _) ->
					message_router:send_notify(Manager, ?REP_ADD_GROUP, [UId, SetedUId, GroupId, PushId, Rep])
			end, ok, Managers);
		_ ->
			ignore
	end,
	{noreply, State#state{reqs = dict:erase(PushId, Reqs)}};

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
handle_info(sync, #state{id = GroupId} = State) ->
	case group_sync:sync_group(GroupId) of
		{ok, Group} ->
			#group{id = GroupId,
				   members = Members,
				   managers = Managers,
				   owner = Owner,
				   forbided = Forbided} = Group,
			NMembers = sets:from_list(Members),
			NManagers = sets:from_list(Managers),
			NForbided = sets:from_list(Forbided),
			{noreply, State#state{id = GroupId,
								  members = NMembers,
								  managers = NManagers,
								  owner = Owner,
								  forbided = NForbided}};
		{error, Reason} ->
			lager:error("sync group ~p failed: ~p", [GroupId, Reason]),
			{stop, normal, State}
	end;

handle_info({sync, Group}, State) ->
	#group{id = GroupId,
		   members = Members,
		   managers = Managers,
		   owner = Owner,
		   forbided = Forbided} = Group,
	NMembers = sets:from_list(Members),
	NManagers = sets:from_list(Managers),
	NForbided = sets:from_list(Forbided),
	{noreply, State#state{id = GroupId,
						  members = NMembers,
						  managers = NManagers,
						  owner = Owner,
						  forbided = NForbided}};

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
add_group(GroupId, UId) ->
	F = fun() ->
			case {chatroom_util:mnesia_query(group, [{id, GroupId}]),
				  chatroom_util:mnesia_query(user, [{uid, UId}])} of
			  {{ok, [#group{members = Members} = Group]},
			   {ok, [#user{joined_group = Joined} = User]}} ->
			  		mensia:write(Group#group{members = [UId|Members]}),
			  		mensia:write(User#user{joined_group = [GroupId|Joined]});
			  {{ok, []}, _} ->
			  		lager:error("no group with id ~p", [GroupId]);
			  {_, {ok, []}} ->
			  		lager:error("no user with id ~p", [UId]);
			  {{error, Reason}, _} ->
			  		lager:error("find group ~p error: ~p", [GroupId, Reason]);
			  {_, {error, Reason}} ->
			  		lager:error("fing user ~p error: ~p", [UId, Reason])
			end
		end,
	mensia:transaction(F).
