-define(MRESTART, 10000).
-define(MTIME, 3000).
-define(SHUTDOWN, 2000).

-define(MESSAGE_TAB, message_tab).
-define(USER_TAB, user_tab)


-record(user, {uid,
			   username,
			   password,
			   status}).


-record(message, {to_user,
				  from_user,
				  message_context}).