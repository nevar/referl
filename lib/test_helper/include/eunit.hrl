-include_lib("eunit/include/eunit.hrl").

-define(EUNIT_TEST_NAME(Name),
    fun(FunName) ->
        [_, FunName1 | _] = string:tokens(FunName, " "),
        [FirstChar | TailFunName] =
			re:replace(FunName1, "_", " ", [{return, list}, global]),
        [string:to_upper(FirstChar) | TailFunName]
    end(Name)).

-define(EUNIT_ARGS_TEST(Test),
    fun(__Arg) -> {?EUNIT_TEST_NAME(??Test), ?_test(Test(__Arg))} end).
-define(EUNIT_TEST(Test), {?EUNIT_TEST_NAME(??Test), ?_test(Test())}).

-define(EUNIT_WAIT_FOR_EXIT(Pid, StopFun, Reason),
	fun(__Pid, __StopFun) ->
        Monitor = monitor(process, __Pid),
        unlink(__Pid),
        __StopFun(__Pid),
        receive {'DOWN', Monitor, _, _, Reason} -> ok end
    end(Pid, StopFun)).
-define(EUNIT_WAIT_FOR_EXIT(Pid, StopFun),
    ?EUNIT_WAIT_FOR_EXIT(Pid, StopFun, _)).

-define(EUNIT_WAIT_FOR_EXIT_NORMAL(Pid),
    ?EUNIT_WAIT_FOR_EXIT(Pid, fun (__Pid) -> exit(__Pid, normal) end)).
-define(EUNIT_WAIT_FOR_EXIT_KILL(Pid),
    ?EUNIT_WAIT_FOR_EXIT(Pid, fun (__Pid) -> exit(__Pid, kill) end)).

-define(EM_RET(Expr), {return, Expr}).
-define(EM_RET_FUN(Fun), {function, Fun}).

-define(EM_RET_OK, ?EM_RET(ok)).
-define(EM_DONE(Pid), ?EM_RET_FUN(fun (_) -> Pid ! done, ok end)).
-define(EM_GET_DONE, receive done -> ok end).

-define(EM_CHECK(M), ?assertEqual(ok, em:verify(M))).
