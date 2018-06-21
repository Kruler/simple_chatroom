%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-06-15 09:27:39
%%%-------------------------------------------------------------------
-module(chatroom_manager).

-behaviour(gen_server).

%% API
-export([user_login/3,
         user_logout/1,
         register/2,
         get_users/0,
         get_online_user/0]).

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
user_login(UserName, PassWord, PId) ->
    case check_user(UserName) of
        {ok, #user{password = PassWord,
                   status = ?OFFLINE,
                   uid = UId} = User} ->
            gen_server:call(?SERVER, {link, PId}),
            ets:insert(?USER_TAB, User#user{status = ?ONLINE,
                                            link_pid = PId}),
            gen_server:cast(message_passageway, {login, UId, PId}),
            {ok, UId};
        {ok, #user{password = PassWord,
                   status = ?ONLINE,
                   link_pid = OldPId,
                   uid = UId} = User} ->
            gen_server:call(?SERVER, {unlink, OldPId}),
            chat_session:stop(OldPId),
            gen_server:call(?SERVER, {link, PId}),
            ets:insert(?USER_TAB, User#user{status = ?ONLINE,
                                            link_pid = PId}),
            {ok, UId};           
        {ok, #user{}} ->
            {error, password_err};
        {error, Reason} ->
            {error, Reason}
    end.

user_logout(UId) ->
    case ets:lookup(?USER_TAB, UId) of
        [#user{status = ?ONLINE, 
               link_pid = Pid} = User] ->
            gen_server:call(?SERVER, {unlink, Pid}),
            ets:insert(?USER_TAB, User#user{status = ?OFFLINE, 
                                            link_pid = undefined}),
            chat_session:stop(Pid);
        [#user{status = ?OFFLINE}] ->
            {error, already_logout};
        [] ->
            {error, no_such_user};
        {error, Reason} ->
            {error, Reason}
    end.

register(UserName, PassWord) ->
    case check_user(UserName) of
        {ok, _} ->
            {error, already_existed};
        {error, no_such_user} ->
            UId =
                case ets:last(?USER_TAB) of
                    '$end_of_table' ->
                        1;
                    LastUId ->
                        LastUId + 1
                end,
            ets:insert(?USER_TAB, #user{uid = UId,
                                        username = UserName,
                                        password = PassWord}),
            ok
    end.

get_users() ->
    lists:map(fun(#user{uid = UId, status = Status, username = UserName}) -> {UId, UserName, Status} end, 
              ets:tab2list(?USER_TAB)).

get_online_user() ->
    lists:filter(fun({_, _, Status}) -> Status =:= ?ONLINE end, get_users()).
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
    erlang:process_flag(trap_exit, true),
    Pos = #user.uid,
    ets:new(?USER_TAB, [set, named_table, public, {keypos, Pos}]),
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
handle_call({link, Pid}, _From, State) ->
    link(Pid),
    {reply, ok, State};

handle_call({unlink, Pid}, _From, State) ->
    unlink(Pid),
    {reply, ok, State};

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
handle_info({'EXIT', Pid, Why}, State) ->
    case ets:select(?USER_TAB, [match_user_spec([{link_pid, Pid}])]) of
        [] ->
            ok;
        [#user{uid = UId} = User] ->
            lager:info("~p exit with reason: ~p", [UId, Why]),
            ets:insert(?USER_TAB, User#user{status = ?OFFLINE,
                                            link_pid = undefined})
    end,
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
check_user(UserName) ->
    case ets:select(?USER_TAB, [match_user_spec([{username, UserName}])]) of
        [] ->
            {error, no_such_user};
        [User] ->
            {ok, User}
    end.

match_user_spec(Query) ->
    Fields = record_info(fields, user),
    MatchHead = list_to_tuple([user|lists:map(
                                        fun(Field) -> proplists:get_value(Field, Query, '_') 
                                    end, Fields)]),
    {MatchHead, [], ['$_']}.