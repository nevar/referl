-module(documenter_tests).

-include_lib("test_helper/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Help-full function %%%%%%%%%%%%%%%%%%%%%%%%%%%
make_form(String) ->
	{done, {ok, Tokens, _}, _} = erl_scan:tokens([], String, 1, []),
	{ok, Form} = extend_parser:parse_form(Tokens),
	Form.

test(WorkPid, Form, ExpectString) ->
	WorkPid ! {form, Form},
	receive {document, Doc} ->
		ResultString = prettypr:format(Doc),
		?assertEqual(ExpectString, ResultString)
	after 1000 ->
		?assert(timeout)
	end.

test_expr(WorkPid, String) ->
	{function, _, _, _, [{clause, _, _, _, [Form]}]} =
		make_form("test() -> " ++ String ++ ".\n"),
	test(WorkPid, Form, String ++ "\n").

test_exprs(WorkPid, StringList) ->
	TestProcess = self(),
	meck:expect(chainer, send, fun(Message, next, _) ->
			TestProcess ! Message,
			ok
		end),
	lists:foreach(fun(String) -> test_expr(WorkPid, String) end, StringList).

test_fun(WorkPid, StringList) ->
	TestProcess = self(),
	meck:expect(chainer, send, fun(Message, next, _) ->
			TestProcess ! Message,
			ok
		end),
	lists:foreach(fun(String) ->
			test(WorkPid, make_form(String), String)
		end, StringList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Moc      function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Setup/cleanup function %%%%%%%%%%%%%%%%%%%%%%%
meck_up() ->
	meck:new([chainer]),
	meck:expect(chainer, get, fun() -> receive M -> M end end).

meck_down(_) ->
	meck:unload([chainer]).

setup() ->
	spawn(fun() -> ok = documenter:syntax2document([{indent_size, 1}]) end).

cleanup(WorkPid) ->
	exit(WorkPid, kill).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test suite    function %%%%%%%%%%%%%%%%%%%%%%%
main_test_() ->
	{setup, fun meck_up/0, fun meck_down/1,
		{foreach, fun setup/0, fun cleanup/1, [
			?EUNIT_TEST(fun test_macro/1)
			, ?EUNIT_TEST(fun test_macro_call/1)
			, ?EUNIT_TEST(fun test_atomic/1)
			, ?EUNIT_TEST(fun test_list/1)
			, ?EUNIT_TEST(fun test_binary/1)
			, ?EUNIT_TEST(fun test_comprehension/1)
			, ?EUNIT_TEST(fun test_tuple/1)
			, ?EUNIT_TEST(fun test_expr/1)
			, ?EUNIT_TEST(fun test_block/1)
			, ?EUNIT_TEST(fun test_if/1)
			, ?EUNIT_TEST(fun test_case/1)
			, ?EUNIT_TEST(fun test_receive/1)
			, ?EUNIT_TEST(fun test_fun/1)
			, ?EUNIT_TEST(fun test_try/1)
			, ?EUNIT_TEST(fun test_query/1)
			, ?EUNIT_TEST(fun test_match/1)
			, ?EUNIT_TEST(fun test_call/1)
			, ?EUNIT_TEST(fun test_pp/1)
			, ?EUNIT_TEST(fun test_attribute/1)
			, ?EUNIT_TEST(fun test_type_define/1)
			, ?EUNIT_TEST(fun test_spec/1)
			, ?EUNIT_TEST(fun test_record_define/1)
			, ?EUNIT_TEST(fun test_record/1)
			, ?EUNIT_TEST(fun test_function/1)
			, ?EUNIT_TEST(fun test_some_expr/1)
			]}
		}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test     function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_macro(WorkPid) ->
	test_exprs(WorkPid, ["?TEST", "?test", "??TEST", "??test", "?'Test'"]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_macro_call(WorkPid) ->
	test_exprs(WorkPid, ["?test()", "?test(A)", "?test(A, 1, C)"]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_atomic(WorkPid) ->
	test_exprs(WorkPid, [
		"123", % integer
		"LALA", % variable
		"$a", "$\\t", "$\\000", % char
		"1.333e+5", "1.3e-5", "1.5", "1.444", % float
		"ok", "'Test'", % atom
		"\"abcd\"" % string
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_list(WorkPid) ->
	test_exprs(WorkPid, ["[1, 2, 3]", "[A, B, C | _]", "[]"]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_binary(WorkPid) ->
	test_exprs(WorkPid, ["<<\"abcd\">>", "<<A:32, B:Size>>",
		"<<A:32/binary, B:Size/bitstring>>",
		"<<A:32/binary:8, B:Size/little-signed-integer-unit:8>>",
		"<<(A + 1):(35 + 53)/binary:8>>",
		"<<>>"]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_comprehension(WorkPid) ->
	test_exprs(WorkPid, ["[X || X <- List]", "[X || X <- List, X > 10]",
		"<< <<X>> || <<X>> <= Binary >>",
		"<< <<X>> || <<X>> <= Binary, X > 10 >>", "[X || <<X>> <= Binary]"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_tuple(WorkPid) ->
	test_exprs(WorkPid, [
		"{1, 2, 3}",
		"{VeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeryLongName,\n"
			" 2, 3}",
		"{}"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_expr(WorkPid) ->
	test_exprs(WorkPid, ["-A", "-1", "1 + 2", "(1 + 2) * 3", "A and B",
			"not true", "1 band 1", "1 div 1", "catch ok"]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_block(WorkPid) ->
	test_exprs(WorkPid, [
		"begin X, Y, Z end",
		"begin\n"
			" X,\n"
			" Y,\n"
			" LongLongLongLongVeryLongLongLongLongLongLongVeryLongLongName\n"
		"end"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_if(WorkPid) ->
	test_exprs(WorkPid, [
		"if X == atom ->\n"
			" ok\n"
		"; is_integer(X), X > 10 ->\n"
			" not_ok\n"
		"end",
		"if X == VeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeryLongName;\n"
		"   X == none; X == atom\n"
		"->\n"
			" ok\n"
		"end"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_case(WorkPid) ->
	test_exprs(WorkPid, [
		"case Test\n"
			" of true ->\n"
				"  ok\n"
			" ; false ->\n"
				"  not_ok\n"
		"end",
		"case Test\n"
			" of true when A == ok ->\n"
				"  ok\n"
		" ; false when B == ok; C == ok ->\n"
			"  not_ok\n"
		"end",
		"case Test\n"
			" of true when\n"
			"  X == VeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeryLongName,\n"
			"  X == VeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeryLongName\n"
			" ->\n"
				"  ok\n"
		"end"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_receive(WorkPid) ->
	test_exprs(WorkPid, [
		"receive\n"
			" true ->\n"
				"  ok\n"
			" ; false ->\n"
				"  not_ok\n"
		"end",
		"receive\n"
			" true when A == ok ->\n"
				"  ok\n"
			" ; false when B == ok; C == ok ->\n"
				"  not_ok\n"
		"after 1000 ->\n"
			" ok\n"
		"end",
		"receive\n"
			" true when\n"
			"  X == VeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeryLongName,\n"
			"  X == VeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeryLongName\n"
			" ->\n"
				"  ok\n"
		"end",
		"receive\n"
		"after 1000 ->\n"
			" ok\n"
		"end"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_fun(WorkPid) ->
	test_exprs(WorkPid, ["fun name/1", "fun module:name/1",
		"fun (one) ->\n"
			" ok\n"
		"; (two) ->\n"
			" ok2\n"
		"end",
		"fun (A, B) ->\n"
			" A + B\n"
		"end"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_try(WorkPid) ->
	test_exprs(WorkPid, [
		% Try of + catch
		"try ok, ok, A\n"
			" of test ->\n"
				"  ok\n"
			" ; test2 ->\n"
				"  ok2\n"
		"catch\n"
			" exit:normal ->\n"
				"  ok\n"
		"end",
		% Try of + after
		"try ok, ok, A\n"
			" of test ->\n"
				"  ok\n"
			" ; test2 ->\n"
				"  ok2\n"
		"after\n"
			" ok\n"
		"end",
		% Try of + catch + after
		"try ok, ok, A\n"
			" of test ->\n"
				"  ok\n"
			" ; test2 ->\n"
				"  ok2\n"
		"catch\n"
			" exit:normal ->\n"
				"  ok\n"
		"after\n"
			" after_ok\n"
		"end",
		% Try + catch
		"try ok, ok, A\n"
		"catch\n"
			" exit:normal ->\n"
				"  ok\n"
		"end",
		% Try + after
		"try ok, ok, A\n"
		"after\n"
			" ok\n"
		"end",
		% Try + catch + after
		"try ok, ok, A\n"
		"catch\n"
			" exit:normal ->\n"
				"  ok\n"
		"after\n"
			" after_ok\n"
		"end",
		% Try catch with guard
		"try ok, ok, A\n"
		"catch\n"
			" exit:M when M == normal ->\n"
				"  ok\n"
		"after\n"
			" after_ok\n"
		"end",
		% Try call in catch
		"try ok, ok, A\n"
		"catch\n"
			" exit:(m:element(2, {1, 2, 3})) ->\n"
				"  ok\n"
			" ; exit:(?M()) ->\n"
				"  ok\n"
		"after\n"
			" after_ok\n"
		"end"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_query(WorkPid) ->
	test_exprs(WorkPid, [
		"query\n"
			" [X || X <- [1, 2, 3]]\n"
		"end"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_match(WorkPid) ->
	test_exprs(WorkPid, [
		"A = B",
		"LoooooooooooooooooooooooooooooooooooooooooooooooooooooooongName =\n"
			" Value == 1",
		"Test ! A = B",
		"(Test ! A) = B"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_call(WorkPid) ->
	test_exprs(WorkPid, [
		"test()",
		"test(1, 2, 3)",
		"module:name()",
		"module:name(1)",
		"Name()",
		"Name(1, 2)",
		"Module:Name()",
		"Module:Name(1, 2)",
		"module:Name()",
		"Module:name()"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_pp(WorkPid) ->
	test_fun(WorkPid, [
		"-define(A, 1).\n",
		"-define(A(), 1 + 3).\n",
		"-define(A(X, Y), X + Y).\n",
		"-define(A(X, Y), ??X ++ ??Y).\n",
		"-undef(A).\n",
		"-undef(a).\n",
		"-ifdef(a).\n",
		"-ifdef(A).\n",
		"-ifndef(a).\n",
		"-ifndef(A).\n",
		"-else.\n",
		"-endif.\n",
		"-include(\"test.hrl\").\n",
		"-include_lib(\"test.hrl\").\n"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_attribute(WorkPid) ->
	test_fun(WorkPid, [
		"-export([test/1, test/2]).\n",
		"-import(io, [format/1]).\n",
		"-file(\"test.erl\", 1).\n",
		"-vsn(1).\n",
		"-custom(1).\n",
		"-compile({inline, [pi/0, pi_2/0]}).\n"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_type_define(WorkPid) ->
	test_fun(WorkPid, [
		"-type test() :: integer().\n",
		"-type test(A, B) :: integer().\n",
		"-type test(A, B) :: integer(atom()).\n",
		"-type test(A, B) :: test:type1(atom()).\n",
		"-type test(A, B) :: {A, B}.\n",
		"-type test(A, B) :: {A, B} | term().\n",
		"-type test(A, B) :: 'fun' | 'case'.\n",
		"-type test() :: [].\n",
		"-type test() :: 0..12.\n",
		"-type test() :: 10.\n",
		"-type test() :: tuple().\n",
		"-type test() :: {}.\n",
		"-type test() :: fun().\n",
		"-type test() :: fun((...) -> term()).\n",
		"-type test() :: fun(() -> term()).\n",
		"-type test() :: fun((term(), term()) -> term() | term()).\n",
		"-type test() :: binary().\n",
		"-type test() :: <<>>.\n",
		"-type test() :: <<_: 5>>.\n",
		"-type test() :: <<_:_ * 7>>.\n",
		"-type test() :: <<_: 5, _:_ * 8>>.\n",
		"-type test() :: atom.\n",
		"-type test() :: atom | atom1 | atom2.\n",
		"-opaque test() :: atom.\n",
		"-opaque test() :: atom | atom1 | atom2.\n"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_spec(WorkPid) ->
	test_fun(WorkPid, [
		"-spec test() -> no_return().\n",
		"-spec test() -> no_return()\n"
		"      ; () -> ok.\n",
		"-spec test() ->\n"
		"       loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong_type()\n"
		"      ; () -> ok.\n",
		"-spec test(X :: atom(), Y :: integer()) -> no_return().\n",
		"-spec test(X) -> X when X :: integer()\n"
		"      ; (Y) -> Y when Y :: atom().\n"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_record_define(WorkPid) ->
	test_fun(WorkPid, [
		"-record(test, {field1, field2}).\n",
		"-record(test, {loooooooooooooooooooooooooooooooooooooooooooooooooongName,\n"
		"\t       field1, field2}).\n",
		"-record(test, {field1 = 1, field2 = atom}).\n",
		"-record(test, {field1 :: undefined | integer(),\n"
		"\t       field2 :: undefined | atom()}).\n",
		"-record(test, {field1 = 1 :: integer(),\n"
		"\t       field2 = atom :: atom()}).\n"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_record(WorkPid) ->
	test_exprs(WorkPid, [
		"#test.field",
		"#test.field#test2.field2",
		"A#test.field#test2.field2",
		"(element(2, Tuple))#test.field#test2.field2",
		"#test{a = 1}",
		"A#test{a = 1}",
		"(element(2, Tuple))#test2{field2 = 0}",
		"(element(2, Tuple))#test.field#test2{field2 = 0}"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_function(WorkPid) ->
	test_fun(WorkPid, [
		% simple
		"name() ->\n"
			" ok.\n",
		% two clauses
		"name1() ->\n"
			" ok;\n"
		"name1() ->\n"
			" ok.\n",
		% clauses with guard
		"name1(A) when A > 10 ->\n"
			" ok;\n"
		"name1(B) when B > 10 ->\n"
			" ok.\n",
		% clauses with guard
		"name1(A) when\n"
		" X == VeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeryLongName\n"
		"->\n"
			" ok;\n"
		"name1(B) when\n"
		" X == VeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeryLongName\n"
		"->\n"
			" ok.\n"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).

test_some_expr(WorkPid) ->
	test_exprs(WorkPid, [
		"GuardsDoc = list2doc_(Guard,\n"
		"\t\t      fun (Element, LocOpt) ->\n"
		"\t\t       prettypr:par(list2doc_(Element, ?COMMA, LocOpt))\n"
		"\t\t      end,\n"
		"\t\t      floating_text(\";\"), NewOpt)"
		]),
	?EUNIT_WAIT_FOR_EXIT(WorkPid, fun(Pid) -> Pid ! eof end, normal).
