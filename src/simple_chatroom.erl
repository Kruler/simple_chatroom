%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-06-14 09:24:57
%%%-------------------------------------------------------------------
-module(simple_chatroom).

%% API
-export([start/0,
		 init/0]).

-include("simple_chatroom.hrl").

start() ->
	application:ensure_all_started(simple_chatroom).

init() ->
	stopped = mnesia:stop(),
	case mnesia:delete_schema([node()]) of
		ok ->
			ok = mnesia:create_schema([node()]),
			case mnesia:start() of
				ok ->
					mnesia:create_table(user, [{disc_copies, [node()]},
											   {attributes, record_info(fields, user)}]),
					mnesia:create_table(messages, [{disc_copies, [node()]},
											       {attributes, record_info(fields, messages)}]),
					mnesia:create_table(notifys, [{disc_copies, [node()]},
											      {attributes, record_info(fields, notifys)}]),
					mnesia:create_table(id_count, [{disc_copies, [node()]},
											       {attributes, record_info(fields, id_count)}]),
					chatroom_util:mnesia_insert(#id_count{table = user, count = 10000});
				{error, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

