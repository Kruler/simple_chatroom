%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-06-13 23:59:54
%%%-------------------------------------------------------------------
-module(chat_session).

-behaviour(gen_server).

%% API
-export([start/1]).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("simple_chatroom.hrl").

-record(state, {socket,
                owner,
                username,
                friends}).

%%%===================================================================
%%% API
%%%===================================================================
start(Socket) ->
    case supervisor:start_child(chat_session_sup, [Socket]) of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, _Info} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).


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
init([Socket]) ->
    {ok, #state{socket = Socket}}.

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
handle_info({tcp, Socket, Bin}, #state{socket = Socket} = State) ->
    Req = chatroom_util:decode_packet(Bin),
    {Reply, NState} = reply_action(Req, State),
    Packet = chatroom_util:encode_packet(Reply),
    gen_tcp:send(Packet),
    {noreply, NState};

handle_info(Message, #state{socket = Socket} = State) ->
    Packet = chatroom_util:encode_packet(Message),
    gen_tcp:send(Socket, Packet),
    {noreply, State};

handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    {stop, normal, State};

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
reply_action({login, UserName, PassWord}, State) ->
    {Reply, NState} = 
        case mysql_connection:is_valid_user(UserName, PassWord) of
            ok ->
            	case mysql_connection:is_valid_password(UserName, PassWord) of
            		{ok, UId} ->
            			{{ok, UId}, State};
            		{error, password_err} ->
            			{{error, password_err}, State};
            		{error, Reason} ->
            			lager:error("check password with username ~p password ~p", [UserName, PassWord]),
            			{{error, unexpected_error}, State}
            	end;
            {error, no_such_user} ->
                {{error, no_such_user}, State};
            {error, too_many_users} ->
                lager:error("too many users with username ~p password ~p", [UserName, PassWord]),
                {{error, unexpected_error}, State};
            {error, Reason} ->
                lager:error("login failed with username ~p password ~p by unexpected reason: ~p", 
                            [UserName, PassWord, Reason]),
                {{error, unexpected_error}, State}
        end,
    {Reply, State};

reply_action(logout, #state{owner = UId} = State) ->
    lager:info("user ~p logout", [UId]),
    Reply = {ok, ok},
    {Reply, State};

reply_action({register, UserName, PassWord}, #state{owner = undefined} = State) ->
    {NUId, NUserName, NFriends, Reply} = 
        case mysql_connection:add_user(UserName, PassWord) of
            {ok, UId, Friends} ->
                {UId, Friends, UserName, {ok, ok}};
            {error, Reason} ->
                {undefined, undefined, undefined, {error, register_failed, Reason}}
        end,
    {Reply, State#state{owner = NUId,
                          username = NUserName,
                          friends = NFriends}};

reply_action({message, ToUId, Context}, State) ->
    #state{friends = Friends} = State,
    Reply = 
        case sets:is_element(ToUId, Friends) of
            true ->
                Message = #message{to_uid = ToUId,
                                   from_uid = self(),
                                   context = Context},
                message_passageway:send_message(Message),
                {ok, ok};
            false ->
                {send_failed, not_friend}
        end,
    {Reply, State};

reply_action(_, State) ->
    {{error, invalid_packet}, State}.