-include_lib("eunit/include/eunit.hrl").

-define(EUNIT_TEST(Test),
	(fun() ->
		__Fun = Test,
		[_, FunName1 | _] = string:tokens(??Test, " "),
		[FirstChar | TailFunName] =
			re:replace(FunName1, "_", " ", [{return, list}, global]),
		FunName = [string:to_upper(FirstChar) | TailFunName],
		case erlang:fun_info(Test, arity)
			of {_, 0} ->
				{FunName, ?_test(__Fun())}
			; {_, 1} ->
				fun(__Arg) -> {FunName, ?_test(__Fun(__Arg))} end
		end
	end)()).

-define(EUNIT_WAIT_FOR_EXIT(__Pid, __StopFun, __Reason),
	(fun() ->
		Monitor = monitor(process, __Pid),
		unlink(__Pid),
		__StopFun(__Pid),
		receive {'DOWN', Monitor, _, _, __Reason} ->
			ok
		after 1000 ->
			?assert(timeout)
		end
	end)()).
-define(EUNIT_WAIT_FOR_EXIT(Pid, StopFun),
	?EUNIT_WAIT_FOR_EXIT(Pid, StopFun, _)).

-define(EUNIT_WAIT_FOR_EXIT_NORMAL(Pid),
	?EUNIT_WAIT_FOR_EXIT(Pid, fun (__Pid) -> exit(__Pid, normal) end)).
-define(EUNIT_WAIT_FOR_EXIT_KILL(Pid),
	?EUNIT_WAIT_FOR_EXIT(Pid, fun (__Pid) -> exit(__Pid, kill) end)).

-define(MECK_CHECK(Mod), ?assert(meck:validate(Mod))).
