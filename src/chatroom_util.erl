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
-include("error_ret.hrl").

-compile(export_all).

handle_packet(Bin, Socket) ->
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
    NReason = 
        case proplists:get_value(Reason, ?ERROR_RET) of
            undefined ->
                lager:warning("can't handle error return ~p", [Reason]), 
                <<"未知错误"/utf8>>;
            R ->
                R
        end,
    encode_packet(Code, ReqId, [{?RESP_ERR, NReason}]).

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
handle_req(?BROADCAST, _ReqId, [UId, Context], _Socket) ->
    message_router:broadcast(UId, Context);
handle_req(?USERINFO, ReqId, [UId], Socket) ->
    gen_server:cast(user_manager, {?USERINFO, ReqId, UId, Socket});
handle_req(?ADD_FRIEND, ReqId, [FromUId, ToUId], Socket) ->
    message_router:send_notify(ToUId, ?FRIEND_REQ, [FromUId]),
    encode_and_reply(?ADD_FRIEND, ReqId, [?RESP_OK, <<"send request success">>], Socket);
handle_req(?FRIEND_RESP, ReqId, [Resp, UId, ReqUId] = Payload, Socket) ->
    gen_server:cast(user_manager, {?FRIEND_RESP, ReqId, Payload, Socket});
handle_req(?SEARCH_USER, ReqId, [UserName], Socket) ->
    gen_server:cast(user_manager, {?SEARCH_USER, ReqId, UserName, Socket});



handle_req(?CREATE_GROUP, ReqId, [UId], Socket) ->
    gen_server:cast(group_manager, {?CREATE_GROUP, ReqId, UId, Socket});
handle_req(?DISSOLVE_GROUP, ReqId, [UId, GroupId], Socket) ->
    gen_server:cast(group_manager, {?DISSOLVE_GROUP, ReqId, UId, GroupId, Socket});
handle_req(?SET_MANAGER, ReqId, [UId, SetedUId, GroupId], Socket) ->
    gen_server:cast(group_manager, {?SET_MANAGER, ReqId, UId, SetedUId, GroupId, Socket});
handle_req(?DELETE_MANAGER, ReqId, [UId, SetedUId, GroupId], Socket) ->
    gen_server:cast(group_manager, {?DELETE_MANAGER, ReqId, UId, SetedUId, GroupId, Socket});
handle_req(?GROUP_CHAT, ReqId, [UId, GroupId, Context], Socket) ->
    group_handler:group_chat(UId, GroupId, Context);
handle_req(?FORBID_CHAT, ReqId, [UId, SetedUId, GroupId], Socket) ->
    gen_server:cast(group_manager, {?FORBID_CHAT, ReqId, UId, SetedUId, GroupId, Socket});
handle_req(?FORBID_ALL, ReqId, [UId, GroupId], Socket) ->
    gen_server:cast(group_manager, {?FORBID_ALL, ReqId, UId, GroupId, Socket});
handle_req(?CANCEL_FORBID, ReqId, [UId, SetedUId, GroupId], Socket) ->
    gen_server:cast(group_manager, {?CANCEL_FORBID, ReqId, UId, SetedUId, GroupId, Socket});
handle_req(?CANCEL_FORBID_ALL, ReqId, [UId, GroupId], Socket) ->
    gen_server:cast(group_manager, {?CANCEL_FORBID_ALL, ReqId, UId, GroupId, Socket});
handle_req(?KICK, ReqId, [UId, SetedUId, GroupId], Socket) ->
    gen_server:cast(group_manager, {?KICK, ReqId, UId, SetedUId, GroupId, Socket});
handle_req(?INVITE, ReqId, [UId, InvitedUId, GroupId], Socket) ->
    gen_server:cast(group_manager, {?INVITE, ReqId, UId, InvitedUId, GroupId, Socket});
handle_req(?CHANGE_OWNER, ReqId, [UId, SetedUId, GroupId], Socket) ->
    gen_server:cast(group_manager, {?CHANGE_OWNER, ReqId, UId, SetedUId, GroupId, Socket});
handle_req(?REQ_ADD_GROUP, ReqId, [UId, GroupId], Socket) ->
    gen_server:cast(group_manager, {?REQ_ADD_GROUP, ReqId, UId, GroupId, Socket});
handle_req(?REP_ADD_GROUP, ReqId, [GroupId, UId, SetedUId, PushId, Rep], Socket) ->
    gen_server:cast(group_manager, {?REP_ADD_GROUP, ReqId, UId, GroupId, SetedUId, PushId, Rep, Socket});
handle_req(?REP_INVITE, ReqId, [UId, InviteUId, GroupId, Rep], Socket) ->
    gen_server:cast(group_manager, {?REP_INVITE, ReqId, UId, GroupId, InviteUId, Rep, Socket});
handle_req(?GROUP_INFO, ReqId, [GroupId], Socket) ->
    gen_server:cast(grouop_manager, {?GROUP_INFO, ReqId, GroupId, Socket});
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
    jsx:encode([{<<"code">>, ?RESP_ERR}, {message, <<"非法请求"/utf8>>}]).


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

mnesia_id_query(Tab, Id) ->
    F = fun() -> mnesia:read(Tab, Id) end,
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
    

generate_name(NameSpace, Id) when is_atom(NameSpace)->
    generate_name(atom_to_list(NameSpace), Id);
generate_name(NameSpace, Id) when is_binary(NameSpace) ->
    generate_name(binary_to_list(NameSpace), Id);
generate_name(NameSpace, Id) when is_integer(NameSpace) ->
    generate_name(integer_to_list(NameSpace), Id);
generate_name(NameSpace, Id) when is_list(NameSpace)->
    list_to_atom(NameSpace ++ "_" ++ integer_to_list(erlang:phash2(Id)));
generate_name(_, _) ->
    throw(badarg).
