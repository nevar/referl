-module(formalize_tests).

-include_lib("test_helper/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Help-full function %%%%%%%%%%%%%%%%%%%%%%%%%%%
check_tokens(FormalizePid, "", _) ->
	FormalizePid ! eof,
	receive eof ->
		?assert(true)
	after 1000 ->
		?assert(timeout)
	end;
check_tokens(FormalizePid, String, Line) ->
	{done, Result, StringTail} =
		erl_scan:tokens([], String, Line, [return_comments]),
	{Expect, NextLine} = case Result
		of {ok, TokensWithComment, L} ->
			{done, {ok, Tokens, _}, _} =
				erl_scan:tokens([], String, Line, []),
			FormalizePid ! {tokens_form, TokensWithComment},
			{ok, ExpectForm} = extend_parser:parse_form(Tokens),
			{ExpectForm, L}
		; {eof, _} ->
			FormalizePid ! eof,
			{eof, none}
		; Error ->
			?assert(Error)
	end,
	receive eof ->
		?assertEqual(Expect, eof)
	; {form, FormScaned, []} ->
		?assertEqual(Expect, FormScaned),
		check_tokens(FormalizePid, StringTail, NextLine)
	; {form, FormScaned, Comments} ->
		?assertEqual(Expect, FormScaned),
		?assert(lists:all(fun({comment, _, _}) -> true end, Comments)),
		check_tokens(FormalizePid, StringTail, NextLine)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Moc      function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Setup/cleanup function %%%%%%%%%%%%%%%%%%%%%%%
setup() ->
	WorkPid = spawn_link(fun() -> ok = formalize:tokens2syntax([]) end),
	M = em:new(),
	{WorkPid, M}.

cleanup({WorkPid, M}) ->
	?assertNot(is_process_alive(WorkPid)),
	?EM_CHECK(M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test suite    function %%%%%%%%%%%%%%%%%%%%%%%
main_test_() ->
	{foreach, fun setup/0, fun cleanup/1, [
		?EUNIT_TEST(fun test_formalize/1)
		]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test     function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_formalize({WorkPid, M}) ->
	TestProcess = self(),
	em:stub(M, chain, send, [em:any(), next, em:any()],
		?EM_RET_FUN(fun([Message, next, _]) ->
			TestProcess ! Message
		end)),
	em:replay(M),
	FileName = "../test/data/erl_test_file",
	{ok, Binary} = file:read_file(FileName),
	check_tokens(WorkPid, binary_to_list(Binary), 1).
