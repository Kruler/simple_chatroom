%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-06-15 09:27:39
%%%-------------------------------------------------------------------
-module(user_manager).

-behaviour(gen_server).

%% API
-export([get_online_users/0,
         force_logout/1]).

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
-spec get_online_users() -> list().
get_online_users() ->
    gen_server:call(?SERVER, get_online_users).

-spec force_logout(integer()) -> ok | {error, term()}.
force_logout(UId) ->
    logout(UId).

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
    ets:new(?LOGIN_USERS, [set, named_table, public]),
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
handle_call(get_online_users, _From, State) ->
    Reply = lists:map(fun({UId, _}) -> UId end, ets:tab2list(?LOGIN_USERS)),
    {reply, Reply, State};

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
handle_cast({?LOGIN = Code, ReqId, UserName, PassWord, Socket}, State) ->
    F = fun() ->
            case chatroom_util:mnesia_query(user, [{username, UserName}, 
                                                   {password, PassWord}]) of
                {ok, [#user{uid = UId, 
                            friends = Friends} = User]} ->
                    mnesia:write(User#user{status = ?ONLINE}),
                    [{Socket, Pid}] = ets:lookup(?SOCKET_TAB, Socket),
                    case ets:lookup(?LOGIN_USERS, UId) of
                        [{UId, PrePId}] ->
                            chatroom_util:encode_and_reply(?LOGOUT, 0, {error, <<"somone force login your account"/utf8>>, Socket}),
                            gen_server:cast(PrePId, logout);
                        [] ->
                            ok;                          
                        {error, Reason} ->
                            mnesia:abort(Reason)
                    end,
                    ets:insert(?LOGIN_USERS, {UId, Pid}),
                    gen_server:cast(Pid, {login, UId}),
                    gen_server:cast(message_router, {login, UId, Pid}),
                    [UId, Friends];
                {ok, []} ->
                    {error, no_such_user};
                {error, Reason} ->
                    {error, Reason}
            end
        end,
    Reply = chatroom_util:mnesia_return(mnesia:transaction(F)),
    chatroom_util:encode_and_reply(Code, ReqId, Reply, Socket),
    {noreply, State};

handle_cast({?LOGOUT = Code, ReqId, UId, Socket}, State) ->
    Reply = logout(UId),
    chatroom_util:encode_and_reply(Code, ReqId, Reply, Socket),
    {noreply, State};

handle_cast({logout, UId}, State) ->
    logout(UId),
    {noreply, State};

handle_cast({?REGISTER = Code, ReqId, UserName, PassWord, Socket}, State) ->
    F = fun() -> 
            case chatroom_util:mnesia_query(user, [{username, UserName}]) of
                {ok, [User]} ->
                    {error, already_exists};
                {ok, []} ->
                    UId = mnesia:dirty_update_counter(id_count, user, 1),
                    mnesia:write(#user{uid = UId, 
                                       username = UserName,
                                       password = PassWord}),
                    [UId];
                {error, Reason} ->
                    {error, Reason}
            end
        end,
    Reply = chatroom_util:mnesia_return(mnesia:transaction(F)),
    chatroom_util:encode_and_reply(Code, ReqId, Reply, Socket),
    {noreply, State};

handle_cast({?USERINFO = Code, ReqId, UId, Socket}, State) ->
    Reply = 
        case mnesia:dirty_read(user, UId) of
            [] ->
                {error, no_such_user};
            [#user{username = UserName}] ->
                {ok, [UserName]};
            {aborted, Reason} ->
                {error, Reason}
        end,
    chatroom_util:encode_and_reply(Code, ReqId, Reply, Socket),
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
logout(UId) ->
    F = fun() -> 
            case chatroom_util:mnesia_query(user, [{uid, UId}]) of
                {ok, [User]} ->
                    mnesia:write(User#user{status = ?OFFLINE}),
                    case ets:lookup(?LOGIN_USERS, UId) of
                        [{UId, Pid}] ->
                            gen_server:cast(Pid, logout),
                            ets:delete(?LOGIN_USERS, UId);
                        [] ->
                            ok;
                        {error, Reason} ->
                            mnesia:abort(Reason)
                    end;
                {ok, []} ->
                    {error, no_such_user};
                {error, Reason} ->
                    {error, Reason}
            end
        end,
    chatroom_util:mnesia_return(mnesia:transaction(F)).