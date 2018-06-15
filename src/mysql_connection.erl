%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-06-15 10:14:31
%%%-------------------------------------------------------------------
-module(mysql_connection).

-behaviour(gen_server).

%% API
-export([is_valid_user/2,
		 add_user/2]).

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

-record(state, {connection,
	 			opt,
	 			add_user_stmt,
	 			check_user_stmt}).

%%%===================================================================
%%% API
%%%===================================================================
is_valid_user(UserName, PassWord) ->
	try gen_server:call(?MODULE, {check_user, UserName, PassWord}) of
		Rep -> Rep
	catch _:Exception ->
			{error, Exception}
	end.

add_user(UserName, PassWord) ->
	try gen_server:call(?MODULE, {add_user, UserName, PassWord}) of
		Rep -> Rep
	catch _:Exception ->
			{error, Exception}
	end.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(MysqlOpt) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MysqlOpt], []).


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
init([MysqlOpt]) ->
	case mysql:start_link(MysqlOpt) of
		{ok, Pid} ->
			self() ! prepare_init,
			{ok, #state{opt = MysqlOpt, 
						connection = Pid}};
		{error, Reason} ->
			lager:error("connect to mysql by option ~p failed: ~p", [MysqlOpt, Reason]),
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
handle_call({check_user, UserName, PassWord}, _From, State) ->
	#state{check_user_stmt = CUStmt} = State,
	Reply = 
		case mysql:execute(CUStmt, [UserName, PassWord]) of
			{ok, _, [UId]} ->
				{ok, UId};
			{ok, _, []} ->
				{error, no_such_user};
			{ok, _, _Users} ->
				{error, too_many_users};
			{error ,Reason} ->
				{error, Reason}
		end,
	{reply, Reply, State};

handle_call({add_user, UserName, PassWord}, _From, State) ->
	#state{add_user_stmt = AUStmt} = State,
	Reply = 
		case mysql:execute(AUStmt, [UserName, PassWord]) of
			{ok, _, [UId]} ->
				{ok, UId};
			{ok, _, []} ->
				{error, no_such_user};
			{ok, _, _Users} ->
				lager:error("too many users with username ~p password ~p"),
				{error, too_many_users};
			{error ,Reason} ->
				{error, Reason}
		end,
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
handle_info(prepare_init,  State) ->
	#state{connection = Conn,
		   check_user_stmt = CheckUserS,
		   add_user_stmt = AddUserS} = State,
	NCheckUserS = 
		case mysql:prepare(Conn, <<"select id from users where username = ? and password = ?">>) of
			{ok, CUStmt} ->
				CUStmt;
			{error, Reason0} ->
				CheckUserS
		end,
	NAddUserS = 
		case mysql:prepare(Conn, <<"insert into users(id, username, password) values(?, ?)">>) of
			{ok, AUStmt} ->
				AUStmt;
			{error, Reason1} ->
				AddUserS
		end,
	{noreply, State#state{check_user_stmt = NCheckUserS,
						  add_user_stmt = NAddUserS}};

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
