-define(MRESTART, 10000).
-define(MTIME, 3000).
-define(SHUTDOWN, 2000).

-define(MESSAGE_TAB, message_tab).
-define(SOCKET_TAB, socket_tab).
-define(LOGIN_USERS, login_users).

-define(LOGIN, 10001).
-define(LOGOUT, 10002).
-define(REGISTER, 10003).

-define(RESP_OK, 1000).
-define(RESP_ERR, 1001).

-define(MESSAGE, 999).
-define(PUSH_MESSAGE, 998).

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

-record(id_count, {table,
				   count = 0}).