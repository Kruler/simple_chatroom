-define(MRESTART, 10000).
-define(MTIME, 3000).
-define(SHUTDOWN, 2000).

-define(MESSAGE_TAB, message_tab).
-define(USER_TAB, user_tab).

-define(ONLINE, 0).
-define(OFFLINE, 1).


-record(user, {uid,
			   username,
			   password,
			   status = ?OFFLINE,
			   link_pid}).


-record(message, {to_uid,
				  from_uid,
				  context}).