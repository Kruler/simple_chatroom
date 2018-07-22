%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-06-15 09:16:13
%%%-------------------------------------------------------------------
-module(message_router).

-behaviour(gen_server).

%% API
-export([send_message/3,
		 send_notify/3,
		 keep_message_len/1,
		 clear_cache/0,
		 clear_cache/1]).


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

-record(state, {max_queue_len}).

%%%===================================================================
%%% API
%%%===================================================================
-spec send_message(integer(), integer(), binary()) -> ok.
send_message(From, To, Context) ->
	Message = #message{from_uid = From,
					   to_uid = To,
					   context = Context},
	gen_server:cast(?MODULE, Message).

-spec send_notify(integer(), integer(), list()) -> ok.
send_notify(To, Code, Payload) ->
	Notify = #notify{to_uid = To,
					 type = Code,
					 payload = Payload},
	gen_server:cast(?MODULE, Notify).

-spec broadcast(integer(), binary()) -> ok.
broadcast(UId, Context) ->
	gen_server:cast({broadcast, UId, Context}).

-spec keep_message_len(integer()) -> integer() | {error, term()}.
keep_message_len(UId) ->
	case ets:lookup(?MESSAGE_TAB, UId) of
		[{UId, Messages}] ->
			length(Messages);
		[] ->
			0;
		{error, Reason} ->
			{error, Reason}
	end.

-spec clear_cache(integer()) -> ok.
clear_cache(UId) ->
	ets:delete(?MESSAGE_TAB, UId).

-spec clear_cache() -> ok.
clear_cache() ->
	ets:delete(?MESSAGE_TAB).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(MaxMessageLen) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MaxMessageLen], []).


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
init([MaxMessageLen]) ->
	self() ! load_message,
    {ok, #state{max_queue_len = MaxMessageLen}}.

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
handle_cast({set_maxqlen, Len}, State) ->
	{noreply, State#state{max_queue_len = Len}};

handle_cast(#message{to_uid = ToUId} = Message, State) ->
	#state{max_queue_len = MaxMessageLen} = State,
	case ets:lookup(?LOGIN_USERS, ToUId) of
		[{ToUId, Pid}] ->
			#message{from_uid = FromUId, 
					 to_uid = ToUId,
					 context = Context} = Message,
			Packet = chatroom_util:encode_packet(?PUSH_MESSAGE, 0, [FromUId, ToUId, Context]),
			gen_server:cast(Pid, {reply, Packet});
		[] ->
			case ets:lookup(?MESSAGE_TAB, ToUId) of
				[{ToUId, MessageQ}] when length(MessageQ) >= MaxMessageLen->
					{_, NMessageQ0} = queue:out(MessageQ),
					NMessageQ1 = queue:in(Message, NMessageQ0),
					ets:insert(?MESSAGE_TAB, {ToUId, NMessageQ1});
				[{ToUId, MessageQ}] ->
					NMessageQ = queue:in(Message, MessageQ),
					ets:insert(?MESSAGE_TAB, {ToUId, NMessageQ});					
				[] ->
					MessageQ = queue:new(),
					NMessageQ = queue:in(Message, MessageQ),
					ets:insert(?MESSAGE_TAB, {ToUId, NMessageQ});
				{error, Reason} ->
					lager:error("lookup uid ~p in ~p failed: ~p", [ToUId, ?MESSAGE_TAB, Reason])
			end;
		{error, Reason} ->
			lager:error("lookup uid ~p in ~p failed: ~p", [ToUId, ?LOGIN_USERS, Reason])
	end,
	{noreply, State};

handle_cast(#notify{to_uid = UId} = Notify, State) ->
	case ets:lookup(?LOGIN_USERS, UId) of
		[{UId, Pid}] ->
			#notify{type = Type,
					payload = Payload} = Notify,
			Packet = chatroom_util:encode_packet(Type, 0, Payload),
			gen_server:cast(Pid, {reply, Packet});
		[] ->
			case ets:lookup(?NOTIFY_TAB, UId) of
				[{UId, NotifyQ}] ->
					NNotifyQ = queue:in(Notify, NotifyQ),
					ets:insert(?NOTIFY_TAB, {UId, NNotifyQ});					
				[] ->
					NotifyQ = queue:new(),
					NNotifyQ = queue:in(Notify, NotifyQ),
					ets:insert(?NOTIFY_TAB, {UId, NNotifyQ});
				{error, Reason} ->
					lager:error("lookup uid ~p in ~p failed: ~p", [UId, ?NOTIFY_TAB, Reason])
			end;
		{error, Reason} ->
			lager:error("lookup uid ~p in ~p failed: ~p", [UId, ?LOGIN_USERS, Reason])
	end,
	{noreply, State};

handle_cast({broadcast, FromUId, Context}, State) ->
	ets:foldl(
		fun({ToUId, Pid}, _) ->
		    Packet = chatroom_util:encode_packet(?BROADCAST, 0, [FromUId, ToUId, Context]),
			gen_server:cast(Pid, {reply, Packet})
	end, ok, ?LOGIN_USERS),
	{noreply, State};

handle_cast({login, UId, Pid}, State) ->
	case ets:lookup(?MESSAGE_TAB, UId) of
		[{UId, MessageQ}] ->
			lists:foreach(
				fun(#message{from_uid = FromUId, 
					 		 to_uid = ToUId,
					 		 context = Context}) -> 
					Packet = chatroom_util:encode_packet(?PUSH_MESSAGE, 0, [FromUId, ToUId, Context]),
					gen_server:cast(Pid, {reply, Packet})
			end, queue:to_list(MessageQ));
		[] ->
			ignore;
		{error, Reason0} ->
			lager:error("find ~p's message in ~p failed :~p", [UId, ?MESSAGE_TAB, Reason0])
	end,
	case ets:lookup(?NOTIFY_TAB, UId) of
		[{UId, NotifyQ}] ->
			lists:foreach(
				fun(#notify{to_uid = UId,
							type = Type,
							payload = Payload}) ->
					Packet = chatroom_util:encode_packet(Type, 0, Payload),
					gen_server:cast(Pid, {reply, Packet})
			end, queue:to_list(NotifyQ));
		[] ->
			ignore;
		{error, Reason1} ->
			lager:error("find ~p's notify in ~p failed :~p", [UId, ?NOTIFY_TAB, Reason1])
	end,
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
handle_info(load_message, #state{max_queue_len = MaxQLen} = State) ->
	case chatroom_util:mnesia_query(messages, []) of
		{ok, Messages} ->
			lists:foreach(
				fun(#messages{uid = UId, messages = UMessages}) ->
					UMessagesFilter = filter_message(UMessages, MaxQLen),
					ets:insert({UId, queue:from_list(UMessagesFilter)})
			end, Messages),
			mnesia:clear_table(messages);
		{error, Reason} ->
			lager:error("load messages failed: ~p", [Reason])
	end,
	{noreply, State};

handle_info(load_notify, State) ->
	case chatroom_util:mnesia_query(notifys, []) of
		{ok, Notifys} ->
			lists:foreach(
				fun(#notifys{uid = UId, notifys = UNotifys}) ->
					ets:insert({UId, queue:from_list(UNotifys)})
			end, Notifys),
			mnesia:clear_table(notifys);
		{error, Reason} ->
			lager:error("load notifys failed: ~p", [Reason])
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
	F = fun() ->
			ets:foldl(
				fun({UId, MessageQ}, _) ->
				Messages = queue:to_list(MessageQ),
				mnesia:write(#messages{uid = UId, messages = Messages}),
				ok
			end, ok, ?MESSAGE_TAB),
			ets:foldl(
				fun({UId, NotifyQ}, _) ->
				Notifys = queue:to_list(NotifyQ),
				mnesia:write(#notifys{uid = UId, notifys = Notifys}),
				ok
			end, ok, ?NOTIFY_TAB)
		end,
	case chatroom_util:mnesia_return(F) of
		{ok, ok} ->
			ok;
		{error, Reason} ->
			lager:error("write messages into mnesia failed: ~p", [Reason])
	end,
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
filter_message(UMessages, MaxQLen) ->
	filter_message(lists:reverse(UMessages), [], MaxQLen).

filter_message([], NMessages, _MaxQLen) ->
	NMessages;
filter_message(Messages, NMessages, MaxQLen) when length(NMessages) =< MaxQLen->
	NMessages;
filter_message([Message|T], NMessages, MaxQLen) ->
	filter_message(T, [Message|NMessages], MaxQLen).
