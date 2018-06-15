%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-06-15 09:16:13
%%%-------------------------------------------------------------------
-module(message_passageway).

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

-record(state, {max_queue_len}).

%%%===================================================================
%%% API
%%%===================================================================
send_message(#message{} = Message) ->
	gen_server:cast(?MODULE, Message).

keep_message_len(UId) ->
	case ets:lookup(?MESSAGE_TAB, UId) of
		[{UId, Messages}] ->
			len(Messages);
		[] ->
			0;
		{error, Reason} ->
			{error, Reason}
	end.

clear_cache(UId) ->
	ets:delete(?MESSAGE_TAB, UId).

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
	ets:new(?MESSAGE_TAB, [set, named_table, public]),
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
	#state{max_queue_len = MaxMessageLen} = State;
	case ets:lookup(?USER_TAB, ToUId) of
		[{ToUId, Pid}] ->
			Pid ! Message;
		[] ->
			case ets:lookup(?MESSAGE_TAB, ToUId) of
				[{ToUId, MessageQ}] when len(MessageQ) >= MaxMessageLen->
					{_, NMessageQ0} = queue:out(MessageQss),
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
			lager:error("lookup uid ~p in ~p failed: ~p", [ToUId, ?USER_TAB, Reason])
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
