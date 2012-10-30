-module(tokenizer_tests).

-include_lib("test_helper/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Help-full function %%%%%%%%%%%%%%%%%%%%%%%%%%%
check_tokens("", Line) ->
	receive eof ->
		?assert(true)
	after 1000 ->
		?assert(timeout)
	end;
check_tokens(String, Line) ->
	{done, Result, StringTail} =
		erl_scan:tokens([], String, Line, [return_comments]),
	{Expect, NextLine} = case Result
		of {ok, TokensExpext, L} ->
			{TokensExpext, L}
		; {eof, _} ->
			{eof, none}
		; Error ->
			{Error, none}
	end,
	receive eof ->
		?assertEqual(Expect, eof)
	; {tokens_form, Tokens} ->
		?assertEqual(Expect, Tokens),
		check_tokens(StringTail, NextLine)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Moc      function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Setup/cleanup function %%%%%%%%%%%%%%%%%%%%%%%
setup() ->
	WorkPid = spawn_link(fun() -> ok = tokenizer:source2token([]) end),
	M = em:new(),
	{WorkPid, M}.

cleanup({WorkPid, M}) ->
	?assertNot(is_process_alive(WorkPid)),
	?EM_CHECK(M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test suite    function %%%%%%%%%%%%%%%%%%%%%%%
main_test_() ->
	{foreach, fun setup/0, fun cleanup/1, [
		?EUNIT_TEST(fun test_file_tokenizer/1)
		, ?EUNIT_TEST(fun test_string_tokenizer/1)
		]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test     function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_file_tokenizer({WorkPid, M}) ->
	TestProcess = self(),
	em:stub(M, chain, send, [em:any(), next, em:any()],
		?EM_RET_FUN(fun([Message, next, _]) ->
			TestProcess ! Message
		end)),
	em:replay(M),
	FileName = "../test/data/erl_test_file",
	WorkPid ! {file, FileName},
	{ok, Binary} = file:read_file(FileName),
	check_tokens(binary_to_list(Binary), 1).

test_string_tokenizer({WorkPid, M}) ->
	TestProcess = self(),
	em:stub(M, chain, send, [em:any(), next, em:any()],
		?EM_RET_FUN(fun([Message, next, _]) ->
			TestProcess ! Message
		end)),
	em:replay(M),
	FileName = "../test/data/erl_test_file",
	{ok, Binary} = file:read_file(FileName),
	String = binary_to_list(Binary),
	WorkPid ! {string, String},
	{ok, Binary} = file:read_file(FileName),
	check_tokens(String, 1).
