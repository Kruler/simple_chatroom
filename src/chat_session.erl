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
-export([start/1,
         stop/1]).

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
                username}).

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

stop(Pid) ->
    gen_server:cast(Pid, stop).
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
handle_cast(stop, State) ->
    {stop, normql, State};

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
    Req = decode_packet(Bin),
    {Reply, NState} = reply_action(Req, State),
    Packet = encode_packet(Reply),
    gen_tcp:send(Socket, Packet),
    {noreply, NState};

handle_info(Message, #state{socket = Socket} = State) ->
    Packet = encode_packet(Message),
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
    case chatroom_manager:user_login(UserName, PassWord, self()) of
        {ok, UId} ->
            {ok, State#state{owner = UId,
                             username = UserName}};
        Err ->
            {Err, State}
    end;

reply_action(logout, #state{owner = Owner} = State) when Owner =/= undefined->
    Reply = chatroom_manager:user_logout(Owner),
    {Reply, State#state{username = undefined, owner = undefined}};

reply_action({register, UserName, PassWord}, #state{owner = undefined} = State) ->
    Reply = chatroom_manager:register(UserName, PassWord),
    {Reply, State};

reply_action(get_users, #state{owner = Owner} = State) when Owner =/= undefined ->
    Reply = chatroom_manager:get_users(),
    {Reply, State};

reply_action(get_online_users, #state{owner = Owner} = State) when Owner =/= undefined ->
    Reply = chatroom_manager:get_online_users(),
    {Reply, State};

reply_action({message, ToUId, Context}, #state{owner = Owner} = State) when Owner =/= undefined->
    Message = #message{to_uid = ToUId, from_uid = Owner, context = Context},
    message_passageway:send_message(Message),
    {ok, State};

reply_action(_, State) ->
    {{error, invalid_packet}, State}.




decode_packet(Bin) ->
    erlang:binary_to_term(Bin).

encode_packet(Term) ->
    erlang:term_to_binary(Term).