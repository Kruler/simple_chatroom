%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-07-03 18:07:01
%%%-------------------------------------------------------------------
-module(chatroom_util).

-include("simple_chatroom.hrl").

-compile(export_all).

decode_packet(Bin, Socket) ->
    try jsx:decode(Bin) of
        Req ->
            handle_req(Req, Socket)
    catch _:_Exception ->
            reply_invalid_packet(Socket)
    end.

encode_packet(Code, ReqId, Payload) when is_list(Payload)->
    try jsx:encode([{<<"code">>, Code}, {<<"req_id">>, ReqId}, {<<"payload">>, Payload}]) of
        Encoded ->
            Encoded
    catch _:Exception ->
        lager:error("encode packet with code ~p req_id ~p payload ~p failed: ~p",
            [Code, ReqId, Payload, Exception]),
        {error, encoded_failed}
    end;
encode_packet(Code, ReqId, {ok, ok}) ->
    encode_packet(Code, ReqId, ok);
encode_packet(Code, ReqId, {ok, Ret}) ->
    encode_packet(Code, ReqId, [{?RESP_OK, Ret}]);
encode_packet(Code, ReqId, ok) ->
    encode_packet(Code, ReqId, [?RESP_OK]);
encode_packet(Code, ReqId, {error, Reason}) ->
    encode_packet(Code, ReqId, [{?RESP_ERR, Reason}]).

handle_req(Req, Socket) ->
    Code = proplists:get_value(<<"code">>, Req),
    ReqId = proplists:get_value(<<"req_id">>, Req),
    Payload = proplists:get_value(<<"payload">>, Req),
    case lists:member(undefined, [Code, ReqId, Payload]) of
        true ->
            reply_invalid_packet(Socket);
        false ->
            handle_req(Code, ReqId, Payload, Socket)
    end.

handle_req(?LOGIN = Code, ReqId, [UserName, PassWord], Socket) ->
    gen_server:cast(user_manager, {?LOGIN, ReqId, UserName, PassWord, Socket});
handle_req(?LOGOUT, ReqId, [UId], Socket) ->
    gen_server:cast(user_manager, {?LOGOUT, ReqId, UId, Socket});
handle_req(?REGISTER, ReqId, [UserName, PassWord], Socket) ->
    gen_server:cast(user_manager, {?REGISTER, ReqId, UserName, PassWord, Socket});
handle_req(?MESSAGE, _ReqId, [FromUId, ToUId, Context], _Socket) ->
    message_router:send_message(FromUId, ToUId, Context);
handle_req(_, _, _, Socket) ->
    reply(invalid_packet(), Socket).

reply_invalid_packet(Socket) ->
    reply(invalid_packet(), Socket).

encode_and_reply(Code, ReqId, Payload, Socket) ->
    reply(encode_packet(Code, ReqId, Payload), Socket).

reply(Packet, Socket) ->
    case ets:lookup(?SOCKET_TAB, Socket) of
        [] ->
            ignore;
        [{Socket, Pid}] ->
            gen_server:cast(Pid, {reply, Packet});
        {error, Reason} ->
            lager:error("find socket ~p in ~p failed: ~p", [Socket, ?SOCKET_TAB, Reason])
    end.

invalid_packet() ->
    jsx:decode([{<<"code">>, ?RESP_ERR}, {message, <<"非法请求"/utf8>>}]).


mnesia_query(Tab, Limit) ->
    RecName = mnesia:table_info(Tab, record_name),
    TabInfo = mnesia:table_info(Tab, attributes),
    FieldQ = 
        lists:foldl(
            fun(Key, Acc) ->
                V = proplists:get_value(Key, Limit, '_'),
                Acc ++ [V]
        end, [], TabInfo),
    MatchHead = list_to_tuple([RecName|FieldQ]),
    Guard = [],
    Result = ['$_'],
    F = fun() -> mnesia:select(Tab, [{MatchHead, Guard, Result}]) end,
    mnesia_return(mnesia:transaction(F)).

mnesia_insert(Rec) ->
    F = fun() -> mnesia:write(Rec) end,
    mnesia_return(mnesia:transaction(F)).

mnesia_return(Return) ->
    case Return of
        {ok, V} -> {ok, V};
        {error, Reason} -> {error, Reason};
        {atomic, {error, Reason}} -> {error, Reason};
        {atomic, V} -> {ok, V};
        {aborted, Reason} -> {error, Reason}
    end.
    