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
    		spawn((LSocket)_;
    	{error, Reason} ->
    		{error, Reason}
    end.


loop(LSocket) ->
