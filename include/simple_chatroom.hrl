-define(MRESTART, 10000).
-define(MTIME, 3000).
-define(SHUTDOWN, 2000).

-define(MESSAGE_TAB, message_tab).
-define(SOCKET_TAB, socket_tab).
-define(LOGIN_USERS, login_users).
-define(NOTIFY_TAB, notify_tab).

-define(LOGIN, 10001).
-define(LOGOUT, 10002).
-define(REGISTER, 10003).
-define(USERINFO, 10004).
-define(FRIEND, 10005).
-define(ADD_FRIEND, 10006).
-define(FRIEND_RESP, 10007).
-define(SEARCH_USER, 10008).

-define(RESP_OK, 1000).
-define(RESP_ERR, 1001).

-define(MESSAGE, 999).
-define(PUSH_MESSAGE, 998).
-define(FRIEND_REQ, 997).
-define(FRIEND_REP, 996).

-define(ONLINE, 0).
-define(OFFLINE, 1).


-record(user, {uid,
			   username,
			   password,
			   status = ?OFFLINE,
			   friends = []}).

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


