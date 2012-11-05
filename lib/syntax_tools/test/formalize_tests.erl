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
	; {form, FormScaned} ->
		?assertEqual(Expect, FormScaned),
		check_tokens(FormalizePid, StringTail, NextLine)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Moc      function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Setup/cleanup function %%%%%%%%%%%%%%%%%%%%%%%
setup() ->
	meck:new(chainer),
	meck:expect(chainer, get, fun() -> receive M -> M end end),
	spawn(fun() -> ok = formalize:tokens2syntax([]) end).

cleanup(WorkPid) ->
	exit(WorkPid, kill),
	meck:unload(chainer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test suite    function %%%%%%%%%%%%%%%%%%%%%%%
main_test_() ->
	{foreach, fun setup/0, fun cleanup/1, [
		?EUNIT_TEST(fun test_formalize/1)
		]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test     function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_formalize(WorkPid) ->
	TestProcess = self(),
	meck:expect(chainer, send, fun(Message, next, _) ->
			TestProcess ! Message
		end),
	FileName = "../test/data/erl_test_file",
	{ok, Binary} = file:read_file(FileName),
	check_tokens(WorkPid, binary_to_list(Binary), 1),
	?MECK_CHECK(chainer).
