-define(MRESTART, 10000).
-define(MTIME, 3000).
-define(SHUTDOWN, 2000).

-define(MESSAGE_TAB, message_tab).
-define(SOCKET_TAB, socket_tab).
-define(LOGIN_USERS, login_users).
-define(NOTIFY_TAB, notify_tab).
-define(GROUP_TAB, group_tab).

-define(LOGIN, 10001).
-define(LOGOUT, 10002).
-define(REGISTER, 10003).
-define(USERINFO, 10004).
-define(FRIEND, 10005).
-define(ADD_FRIEND, 10006).
-define(FRIEND_RESP, 10007).
-define(SEARCH_USER, 10008).


-define(CREATE_GROUP, 11001).
-define(DISSOLVE_GROUP, 11002).
-define(SET_MANAGER, 11003).
-define(DELETE_MANAGER, 11004).
-define(GROUP_CHAT, 11005).
-define(FORBID_CHAT, 11006).
-define(FORBID_ALL, 11007).
-define(CANCEL_FORBID, 11008).
-define(CANCEL_FORBID_ALL, 11009).
-define(KICK, 11010).
-define(INVITE, 11011).
-define(CHANGE_OWNER, 11012).
-define(REQ_ADD_GROUP, 11013).
-define(GROUP_INFO, 11014).

-define(RESP_OK, 1000).
-define(RESP_ERR, 1001).

-define(MESSAGE, 999).
-define(BROADCAST, 998).
-define(PUSH_MESSAGE, 997).
-define(FRIEND_REQ, 996).
-define(FRIEND_REP, 995).
-define(REP_ADD_GROUP, 994).

-define(ONLINE, 0).
-define(OFFLINE, 1).


-record(user, {uid,
			   username,
			   password,
			   status = ?OFFLINE,
			   friends = [],
			   joined_group = []}).

-record(group, {id,
				owner,
				managers,
				members,
				name,
				forbided,
				created_at}).

-record(messages, {uid,
				   messages = []}).


-record(message, {to_uid,
				  from_uid,
				  context}).

-record(notify, {to_uid,
				 type,
				 payload = []}).

-record(notifys, {uid,
				  notifys = []}).

-record(id_count, {table,
				   count = 0}).


