-module(tokenizer_tests).

-include_lib("test_helper/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Help-full function %%%%%%%%%%%%%%%%%%%%%%%%%%%
check_tokens("", _) ->
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
	after 1000 ->
		?assert(timeout)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Moc      function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Setup/cleanup function %%%%%%%%%%%%%%%%%%%%%%%
setup() ->
	meck:new(chainer),
	meck:expect(chainer, get, fun() -> receive M -> M end end),
	spawn(fun() -> ok = tokenizer:source2token([]) end).

cleanup(WorkPid) ->
	meck:unload(chainer),
	exit(WorkPid, kill).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test suite    function %%%%%%%%%%%%%%%%%%%%%%%
main_test_() ->
	{foreach, fun setup/0, fun cleanup/1, [
		?EUNIT_TEST(fun test_file_tokenizer/1)
		, ?EUNIT_TEST(fun test_string_tokenizer/1)
		]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test     function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_file_tokenizer(WorkPid) ->
	TestProcess = self(),
	meck:expect(chainer, send, fun(Message, next, _) ->
			TestProcess ! Message
		end),
	FileName = "../test/data/erl_test_file",
	WorkPid ! {file, FileName},
	{ok, Binary} = file:read_file(FileName),
	check_tokens(binary_to_list(Binary), 1),
	?MECK_CHECK(chainer).

test_string_tokenizer(WorkPid) ->
	TestProcess = self(),
	meck:expect(chainer, send, fun(Message, next, _) ->
			TestProcess ! Message
		end),
	FileName = "../test/data/erl_test_file",
	{ok, Binary} = file:read_file(FileName),
	String = binary_to_list(Binary),
	WorkPid ! {string, String},
	{ok, Binary} = file:read_file(FileName),
	check_tokens(String, 1),
	?MECK_CHECK(chainer).
