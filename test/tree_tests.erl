-module(tree_tests).

-include_lib("test_helper/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Help-full function %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Moc      function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Setup/cleanup function %%%%%%%%%%%%%%%%%%%%%%%
setup() ->
	{ok, Form} = epp_dodger:parse_file("../test/data/rename_file", [nofail]),
	Comment = erl_comment_scan:file("../test/data/rename_file"),
	TopTree = erl_recomment:recomment_forms(Form, Comment),
	erl_syntax:form_list_elements(TopTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test suite    function %%%%%%%%%%%%%%%%%%%%%%%
main_test_() ->
	{foreach, fun setup/0, [
		?EUNIT_ARGS_TEST(fun test_spec_find/1)
		, ?EUNIT_ARGS_TEST(fun test_fun_clause_find/1)
		, ?EUNIT_ARGS_TEST(fun test_fun_in_fun_find/1)
		, ?EUNIT_ARGS_TEST(fun test_fun_in_shadow_fun_find/1)
		, ?EUNIT_ARGS_TEST(fun test_fun_deep_shadow_fun_find/1)
		]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test     function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_spec_find(TreeList) ->
	NeedForm = lists:nth(2, TreeList),
	Found = tree:find_var(TreeList, 5, "FileName"),
	?assertEqual(Found, NeedForm).

test_fun_clause_find(TreeList) ->
	NeedFun = lists:nth(3, TreeList),
	[_, NeedForm | _] = erl_syntax:function_clauses(NeedFun),
	Found = tree:find_var(TreeList, 14, "X"),
	?assertEqual(NeedForm, Found).

test_fun_in_fun_find(TreeList) ->
	NeedFun = lists:nth(4, TreeList),
	NeedForm = lists:nth(1, erl_syntax:function_clauses(NeedFun)),
	Found = tree:find_var(TreeList, 19, "A"),
	?assertEqual(NeedForm, Found).

test_fun_in_shadow_fun_find(TreeList) ->
	NeedFun = lists:nth(5, TreeList),
	FunClauses = lists:nth(1, erl_syntax:function_clauses(NeedFun)),
	NeedClause = lists:nth(3, erl_syntax:clause_body(FunClauses)),
	[NeedForm | _] = erl_syntax:fun_expr_clauses(NeedClause),
	Found = tree:find_var(TreeList, 26, "A"),
	?assertEqual(NeedForm, Found).

test_fun_deep_shadow_fun_find(TreeList) ->
	NeedFun = lists:nth(7, TreeList),
	FunClauses = lists:nth(1, erl_syntax:function_clauses(NeedFun)),
	NeedExpr = lists:nth(3, erl_syntax:clause_body(FunClauses)),
	[NeedForm | _] = erl_syntax:fun_expr_clauses(NeedExpr),

	FunClauses1 = lists:nth(3, erl_syntax:clause_body(NeedForm)),
	[NeedForm1 | _] = erl_syntax:fun_expr_clauses(FunClauses1),

	Found = tree:find_var(TreeList, 42, "A"),
	?assertEqual(NeedForm1, Found).
