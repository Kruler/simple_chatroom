%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-06-20 13:32:25
%%%-------------------------------------------------------------------
-module(chat_client).

-behaviour(gen_server).

%% API
-export([start_link/0,
         login/2,
         logout/1,
         register/2,
         message/3,
         stop/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {owner, username, socket}).

%%%===================================================================
%%% API
%%%===================================================================
login(Username, Password) ->
    gen_server:cast(?SERVER, {login, Username, Password}).

logout(UId) ->
    gen_server:cast(?SERVER, {logout, UId}).

register(Username, Password) ->
    gen_server:cast(?SERVER, {register, Username, Password}).

message(From, To, Context) ->
    gen_server:cast(?SERVER, {message, From, To, Context}).  

stop() ->
    gen_server:cast(?SERVER, stop).
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
	{ok, S} = gen_tcp:connect("localhost", 7000, [binary, {active, true}]),
    {ok, #state{socket = S}}.

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
    error_logger:warning_msg("Can't handle request: ~p", [_Request]),
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
handle_cast({login, Username, Password}, #state{socket = S} = State) ->
	Bin = jsx:encode([{code, 10001}, {req_id, 1}, {payload, [Username, Password]}]),
    gen_tcp:send(S, Bin),
	{noreply, State};

handle_cast({logout, UId}, #state{socket = S, owner = Owner} = State) ->
	Bin = jsx:encode([{code, 10002}, {req_id, 2}, {payload, [UId]}]),
	gen_tcp:send(S, Bin),
	{noreply, State};

handle_cast({register, Username, Password}, #state{socket = S} = State) ->
	Bin = jsx:encode([{code, 10003}, {req_id, 1}, {payload, [Username, Password]}]),
	gen_tcp:send(S, Bin),
	{noreply, State};

handle_cast({message, From, To, Context}, #state{socket = S} = State) ->
    Bin = jsx:encode([{code, 999}, {req_id, 1}, {payload, [From, To, Context]}]),
    gen_tcp:send(S, Bin),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    error_logger:warning_msg("Can't handle msg: ~p", [_Msg]),
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
    error_logger:warning_msg("receive socket packet: ~p", [Bin]),
    {noreply, State};

handle_info(_Info, State) ->
    error_logger:warning_msg("Can't handle info: ~p", [_Info]),
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
