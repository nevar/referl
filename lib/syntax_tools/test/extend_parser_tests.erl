-module(extend_parser_tests).

-include_lib("test_helper/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Help-full function %%%%%%%%%%%%%%%%%%%%%%%%%%%
scan_parse_test(String, Define) ->
	{ok, Tokens, _} = erl_scan:string(String),
	Form = extend_parser:parse_form(Tokens),
	?assertEqual(Define, Form).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Moc      function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Setup/cleanup function %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test suite    function %%%%%%%%%%%%%%%%%%%%%%%
main_test_() ->
	[
		?EUNIT_TEST(fun test_macro_define/0)
		, ?EUNIT_TEST(fun test_macro_arg/0)
		, ?EUNIT_TEST(fun test_macro_args/0)
		, ?EUNIT_TEST(fun test_macro_call/0)
		, ?EUNIT_TEST(fun test_macro_in_try/0)
		, ?EUNIT_TEST(fun test_call_and_macro/0)
		, ?EUNIT_TEST(fun test_string_and_macro/0)
		].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test     function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_macro_define() ->
	scan_parse_test("-define(A, 1).",
		{ok, {attribute, 1, define, [{var, 1, 'A'}, {integer, 1, 1}]}}).

test_macro_arg() ->
	scan_parse_test("-define(A(), 1).",
		{ok, {attribute, 1, define, [
			{call, 1, {var, 1, 'A'}, []}
			, {integer, 1, 1}
			]}}).

test_macro_args() ->
	scan_parse_test("-define(A(X, Y), 1).",
		{ok, {attribute, 1, define, [
			{call, 1, {var, 1, 'A'}, [{var, 1, 'X'}, {var, 1, 'Y'}]}
			, {integer, 1, 1}
			]}}).

test_macro_call() ->
	scan_parse_test("name() -> ?A, ?B(), ?C(1, 2, 3).",
		{ok, {function, 1, name, 0, [
			{clause, 1, [], [], [
				{macro, 1, {var,1,'A'}}
				, {call, 1, {macro, 1, {var, 1, 'B'}}, []}
				, {call, 1, {macro, 1, {var, 1, 'C'}}, [
					{integer,1,1}
					, {integer,1,2}
					, {integer,1,3}
					]}
				]}
			]}}).

test_macro_in_try() ->
	scan_parse_test("name() -> try ok of ok -> ok catch m:?A() -> ok end.",
		{ok, {function, 1, name, 0, [
			{clause, 1, [], [], [
				{'try', 1, [{atom,1,ok}], [
					{clause, 1, [{atom, 1, ok}], [], [{atom, 1, ok}]}], [
						{clause, 1, [
							{tuple, 1, [
								{atom, 1, m}, {call, 1, {macro, 1, {var, 1, 'A'}}, []}, {var,1,'_'}
								]}
							],
						[], [{atom, 1, ok}]}
					], []}
				]}
			]}}).

test_call_and_macro() ->
	scan_parse_test("name() -> m:?A(1, 2, 3).",
		{ok, {function, 1, name, 0, [
			{clause, 1, [], [], [
				{call, 1,
					{remote, 1, {atom, 1, m}, {macro, 1, {var, 1, 'A'}}}, [
						{integer, 1, 1}
						, {integer, 1, 2}
						, {integer, 1, 3}
						]}
					]}
			]}}).

%% TODO: may be some time add to parser
test_string_and_macro() ->
	scan_parse_test(
		"name() -> \"abcd\"?A?B()\"abcd\"\"abcd\"?A?A()?A(1, 2, 3).",
		{error,{1,extend_parser,["syntax error before: ","'?'"]}}).
