%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-06-13 22:38:40
%%%-------------------------------------------------------------------
-module(chatroom_util).

-include("simple_chatroom.hrl").

-compile(export_all).

supervisor_spec(Type, Workers) when is_list(Workers) 
							   andalso (Type =:= simple_one_for_one
							   orelse Type =:= one_for_one
							   orelse Type =:= one_for_all)->
	{ok, {{Type, ?MRESTART, ?MTIME}, Workers}}.

child_worker_spec(Mod, Args) when is_list(Args) ->
	{Mod, {Mod, start_link, Args}, transient, ?SHUTDOWN, worker, [Mod]}.

child_supervisor_spec(Mod, SupName, Args) when is_list(Args) ->
	{Mod, {?MODULE, start_supervisor, [Mod, SupName, Args]}, 
		  transient, ?SHUTDOWN, supervisor, [Mod]}.

start_supervisor(Mod, SupName, Args) ->
	supervisor:start_link({local, SupName}, Mod, [SupName|Args]).


decode_packet(Bin) ->
	erlang:binary_to_term(Bin).