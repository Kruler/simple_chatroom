%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-07-16 13:26:28
%%%-------------------------------------------------------------------
-module(group_manager).

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

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


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
    {ok, #state{}}.

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
handle_cast({?CREATE_GROUP, ReqId, UId, Socket}, State) ->
    GroupId = mnesia:dirty_update_counter(id_count, user, 1),
    Group = #group{id = GroupId,
                   members = [UId],
                   owner = UId,
                   managers = [UId],
                   created_at = os:timestamp()},
    F = fun() -> mnesia:write(Group) end,
    Reply = chatroom_util:mnesia_return(F),
    chatroom_util:encode_and_reply(?CREATE_GROUP, ReqId, Reply, Socket), 
    {noreply, State};

handle_cast({?DISSOLVE_GROUP, ReqId, UId, GroupId, Socket}, State) ->
    F = fun() ->
            case chatroom_util:mnesia_query(group, [{id, GroupId}]) of
                {ok, [#group{owner = UId}]} ->
                    mnesia:delete(group, GroupId, write);
                {ok, [_]} ->
                    {error, no_permission};
                {ok, []} ->
                    {error, no_such_group};
                {error, Reason} ->
                    {error, Reason}
            end
        end,
    Reply = chatroom_util:mnesia_return(F),
    chatroom_util:encode_and_reply(?SET_MANAGER, ReqId, Reply, Socket),    
    {noreply, State};

handle_cast({?SET_MANAGER, ReqId, UId, SetedUId, GroupId, Socket}, State) ->
    F = fun() ->
        case chatroom_util:mnesia_query(group, [{id, GroupId}]) of
            {ok, [#group{owner = UId,
                         members = Members,
                         managers = Managers} = Group]} ->
                case lists:member(SetedUId, Managers) of
                    true ->
                        {error, already_manager};
                    false ->
                        case lists:memeber(SetedUId, Members) of
                            false ->
                                {error, not_memebers};
                            true ->
                                mnesia:write(Group#group{managers = [SetedUId|Managers]})
                        end
                end;
            {ok, [_]} ->
                {error, no_permission};
            {ok, []} ->
                {error, no_such_group};
            {error, Reason} ->
                {error, Reason}
        end
    end,
    Reply = chatroom_util:mnesia_return(F),
    chatroom_util:encode_and_reply(?SET_MANAGER, ReqId, Reply, Socket), 
    {noreply, State};

handle_cast({?DELETE_MANAGER, ReqId, UId, SetedUId, GroupId, Socket}, State) ->
    F = fun() ->
        case chatroom_util:mnesia_query(group, [{id, GroupId}]) of
            {ok, [#group{owner = UId,
                         members = Members,
                         managers = Managers} = Group]} ->
                case lists:member(SetedUId, Managers) of
                    false ->
                        {error, already_not_manager};
                    true ->
                        case lists:memeber(SetedUId, Members) of
                            false ->
                                {error, not_memebers};
                            true ->
                                NManagers = lists:delete(SetedUId, Managers),
                                mnesia:write(Group#group{managers = NManagers})
                        end
                end;
            {ok, [_]} ->
                {error, no_permission};
            {ok, []} ->
                {error, no_such_group};
            {error, Reason} ->
                {error, Reason}
        end
    end,
    Reply = chatroom_util:mnesia_return(F),
    chatroom_util:encode_and_reply(?SET_MANAGER, ReqId, Reply, Socket),  
    {noreply, State};

handle_cast({?FORBID_CHAT, ReqId, UId, SetedUId, GroupId, Socket}, State) ->
    F = fun() ->
            case chatroom_util:mnesia_query(group, [{id, GroupId}]) of
                {ok, [Group]} ->
                    #group{owner = Owner,
                           managers = Managers,
                           members = Members,
                           forbided = Forbid} = Group,
                    case {lists:member(SetedUId, Managers), lists:member(UId, Managers)} of
                        {true, _} when UId =:= Owner ->
                            mnesia:write(Group#group{forbided = [SetedUId|Forbid]});
                        {false, true} ->
                            mnesia:write(Group#group{forbided = [SetedUId|Forbid]});
                        _ ->
                            {error, no_permission}
                    end;
                {ok, []} ->
                    {error, no_such_group};
                {error, Reason} ->
                    {error, Reason}
            end
        end,
    Reply = chatroom_util:mnesia_return(F),
    chatroom_util:encode_and_reply(?FORBID_CHAT, ReqId, Reply, Socket),  
    {noreply, State};

handle_cast({?FORBID_ALL, ReqId, UId, GroupId, Socket}, State) ->
    F = fun() ->
            case chatroom_util:mnesia_query(group, [{id, GroupId}]) of
                {ok, [Group]} ->
                    #group{owner = UId,
                           members = Members} = Group,
                    mnesia:write(Group#group{forbided = Members});
                {ok, [_]} ->
                    {error, no_permission};
                {ok, []} ->
                    {error, no_such_group};
                {error, Reason} ->
                    {error, Reason}
            end
        end,
    Reply = chatroom_util:mnesia_return(F),
    chatroom_util:encode_and_reply(?FORBID_ALL, ReqId, Reply, Socket),  
    {noreply, State};

handle_cast({?CANCEL_FORBID_ALL, ReqId, UId, GroupId, Socket}, State) ->
    F = fun() ->
            case chatroom_util:mnesia_query(group, [{id, GroupId}]) of
                {ok, [Group]} ->
                    #group{owner = UId} = Group,
                    mnesia:write(Group#group{forbided = []});
                {ok, [_]} ->
                    {error, no_permission};
                {ok, []} ->
                    {error, no_such_group};
                {error, Reason} ->
                    {error, Reason}
            end
        end,
    Reply = chatroom_util:mnesia_return(F),
    chatroom_util:encode_and_reply(?CANCEL_FORBID_ALL, ReqId, Reply, Socket),  
    {noreply, State};

handle_cast({?CANCEL_FORBID, ReqId, UId, SetedUId, GroupId, Socket}, State) ->
    F = fun() ->
            case chatroom_util:mnesia_query(group, [{id, GroupId}]) of
                {ok, [Group]} ->
                    #group{owner = Owner,
                           managers = Managers,
                           members = Members,
                           forbided = Forbid} = Group,
                    case {lists:member(SetedUId, Managers), lists:member(UId, Managers)} of
                        {true, _} when UId =:= Owner ->
                            NForbid = lists:delete(SetedUId, Forbid),
                            mnesia:write(Group#group{forbided = NForbid});
                        {false, true} ->
                            NForbid = lists:delete(SetedUId, Forbid),
                            mnesia:write(Group#group{forbided = NForbid});
                        _ ->
                            {error, no_permission}
                    end;
                {ok, []} ->
                    {error, no_such_group};
                {error, Reason} ->
                    {error, Reason}
            end
        end,
    Reply = chatroom_util:mnesia_return(F),
    chatroom_util:encode_and_reply(?CANCEL_FORBID, ReqId, Reply, Socket),  
    {noreply, State};

handle_cast({?KICK, ReqId, UId, SetedUId, GroupId, Socket}, State) ->
    F = fun() ->
            case chatroom_util:mnesia_query(group, [{id, GroupId}]) of
                {ok, [Group]} ->
                    #group{owner = Owner,
                           managers = Managers,
                           members = Members} = Group,
                    case {lists:member(SetedUId, Managers), lists:member(UId, Managers)} of
                        {true, _} when UId =:= Owner ->
                            NManagers = lists:delete(SetedUId, Managers),
                            NMembers = lists:delete(SetedUId, Members),
                            mnesia:write(Group#group{managers = NManagers,
                                                     members = NMembers});
                        {false, true} ->
                            NMembers = lists:delete(SetedUId, Members),
                            mnesia:write(Group#group{members = NMembers});
                        _ ->
                            {error, no_permission}
                    end;
                {ok, []} ->
                    {error, no_such_group};
                {error, Reason} ->
                    {error, Reason}
            end
        end,
    Reply = chatroom_util:mnesia_return(F),
    chatroom_util:encode_and_reply(?CHANGE_OWNER, ReqId, Reply, Socket),  
    {noreply, State};

handle_cast({?INVITE, ReqId, UId, InvitedUId, GroupId, Socket}, State) ->
    Reply = 
        case chatroom_util:mnesia_query(group, [{id, GroupId}]) of
            {ok, [#group{members = Members}]} ->
                case {lists:member(UId, Members), lists:member(InvitedUId, Members)} of
                    {true, false} ->
                        message_router:send_notify(InvitedUId, ?INVITE, [UId, GroupId]);
                    _ ->
                        {error, bad_request}
                end;
            {ok, []} ->
                {error, no_such_group};
            {error, Reason} ->
                {error, Reason}
        end,
    chatroom_util:encode_and_reply(?SET_MANAGER, ReqId, Reply, Socket),    
    {noreply, State};

handle_cast({?CHANGE_OWNER, ReqId, UId, SetedUId, GroupId, Socket}, State) ->
    F = fun() ->
        case chatroom_util:mnesia_query(group, [{id, GroupId}]) of
            {ok, [#group{owner = UId} = Group]} ->
                mnesia:write(Group#group{owner = SetedUId});
            {ok, [_]} ->
                {error, no_permission};
            {ok, []} ->
                {error, no_such_group};
            {error, Reason} ->
                {error, Reason}
        end
    end,
    Reply = chatroom_util:mnesia_return(F),
    chatroom_util:encode_and_reply(?CHANGE_OWNER, ReqId, Reply, Socket),    
    {noreply, State};

handle_cast({?GROUP_INFO, ReqId, GroupId, Socket}, State) ->
    Reply = 
        case mnesia:dirty_read(group, GroupId) of
            [Group] ->
                #group{id = GroupId,
                       members = Members,
                       managers = Managers,
                       owner = Owner,
                       name = Name,
                       created_at = CreatedAt} = Group,
                [GroupId, Name, Members, Managers, Owner, CreatedAt];
            [] ->
                {error, no_such_group}
        end,
    chatroom_util:encode_and_reply(?GROUP_INFO, ReqId, Reply, Socket),    
	{noreply, State};


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
handle_info({mnesia_table_event, {write, #group{} = _, _}}, 
                                            #state{} = State)->
    lager:info("start sync trading sessions"),
    gen_server:cast(self(), load_init_data),
    {noreply, State};

handle_info({mnesia_table_event, {delete, {group, {_, _, _, _}}, _}}, 
                                            #state{} = State)->
    lager:info("start sync trdading sessions"),
    gen_server:cast(self(), load_init_data),
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
set_owner(GroupId, UId, SetedUId) ->
    F = fun() ->
        case chatroom_util:mnesia_query() of
            {ok, [#group{owner = UId} = Group]} ->
                mnesia:write(Group#group{owner = SetedUId});
            {ok, [_]} ->
                {error, no_permission};
            {ok, []} ->
                {error, no_such_group};
            {error, Reason} ->
                {error, Reason}
        end
    end,
    chatroom_util:mnesia_return(F).