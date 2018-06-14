%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-06-13 23:56:42
%%%-------------------------------------------------------------------
-module(connect_listener).

%% API
-export([start_link/1,
	     loop/1]).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Port) ->
    case gen_tcp:listen(Port, [binary, {packet, raw}, {active, once}]) of
    	{ok, LSocket} ->
    		Pid = spawn_link(fun() -> loop(LSocket) end),
    		{ok, Pid};
    	{error, Reason} ->
    		{error, Reason}
    end.


loop(LSocket) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			case chat_session:start(Socket) of
				{ok, Pid} ->
					case gen_tcp:controlling_process(Socket, Pid) of
						ok ->
							loop(LSocket);
						{error, Reason} ->
							lager:error("assign socket to ~p failed: ~p", [Pid, Reason])
					end;
				{error, Reason} ->
					lager:error("start chat_session failed: ~p", [Reason])
			end;
		{error, Reason} ->
			lager:error("accept from listen socket failed: ~p", [Reason])
	end.
