-define(ERROR_RET, 
	[{no_such_user,  <<"没有该用户"/utf8>>},
	 {force_logined,  <<"该用户在其他设备上登录"/utf8>>},
	 {user_already_exists, <<"该用户已存在"/utf8>>}]).