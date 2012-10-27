%% @author Slava Yurin <YurinVV@ya.ru>
%% @doc Make document from syntax tree
-module(document).

-export([make_document/2]).

%% @doc Make abstract documention
%%
%% Make document layout for use in `prettypr'.
-spec make_document(erl_syntax:syntaxTree(), term()) -> iolist().
make_document(SyntaxTree, Opt) ->
	Type = erl_syntax:type(SyntaxTree),
	Document = make_document_element(Type, SyntaxTree, Opt),
	case erl_syntax:has_comments(SyntaxTree)
		of true ->
			Document1 = add_pre_comments(Document, SyntaxTree, Opt),
			add_post_comments(Document1, SyntaxTree, Opt)
		; false ->
			Document
	end.

add_pre_comments(Document, SyntaxTree, Opt) ->
	PreComments = list2doc(erl_syntax:get_precomments(SyntaxTree),
		fun make_document/2, none, Opt),
	Comment = join_line(PreComments),
	prettypr:above(Comment, Document).

add_post_comments(Document, SyntaxTree, Opt) ->
	PostComments = list2doc(erl_syntax:get_postcomments(SyntaxTree),
		fun make_document/2, none, [{padding, 2} | Opt]),
	Comment = prettypr:floating(prettypr:break(join_line(PostComments)), 1, 0),
	prettypr:beside(Document, Comment).

%% @doc Make abstract document for one element
% Top of tree
make_document_element(form_list, TopTree, Opt) ->
	TopTreeDoc = list2doc(erl_syntax:form_list_elements(TopTree),
		fun make_document/2, none, Opt),
	prettypr:break(join_line(TopTreeDoc, text("")));
% Module attribute
make_document_element(attribute, Attribute, Opt) ->
	AttributeName = erl_syntax:attribute_name(Attribute),
	IsSpec = erl_syntax:is_atom(AttributeName, spec),
	IsType = erl_syntax:is_atom(AttributeName, type),
	AttributeBody = if IsSpec ->
		[MainSpec] = erl_syntax:attribute_arguments(Attribute),
		[SpecNameTuple, SpecsList] = erl_syntax:tuple_elements(MainSpec),
		[SpecName, _SpecArity] = erl_syntax:tuple_elements(SpecNameTuple),
		Name = make_document(SpecName, Opt),
		SpecBody = join_line(spec_types:make_document(SpecsList, Opt)),
		join([text(" "), Name, SpecBody])
	; IsType ->
		[Type] = erl_syntax:attribute_arguments(Attribute),
		[Name, Define, Param] = erl_syntax:tuple_elements(Type),
		NameDoc = make_document(Name, Opt),
		DefineDoc = make_spec(Define, Opt),
		ParamDoc = case erl_syntax:list_length(Param)
			of 0 ->
				[prettypr:empty()]
			; _ ->
				list2doc(erl_syntax:list_elements(Param), fun make_spec/2,
					floating_text(","), Opt)
		end,
		join([text(" "), NameDoc, text("("), prettypr:par(ParamDoc),
			floating_text(") "), prettypr:par([text("::"), DefineDoc])])
	; true ->
		% Other attributes
		Args = list2doc(erl_syntax:attribute_arguments(Attribute),
			fun make_document/2, floating_text(","), Opt),
		join([text("("), prettypr:par(Args), floating_text(")")])
	end,
	join([text("-"), make_document(AttributeName, Opt), AttributeBody,
		floating_text(".")]);
% Erlang comment
make_document_element(comment, Comment, Opt) ->
	SetPadding = proplists:get_value(padding, Opt, 0),
	Padding = erl_syntax:comment_padding(Comment),
	Indent = if SetPadding > 0, Padding > 0 ->
		% Indent exists and we want set it
		Padding
	; true ->
		% Indent not exists but we may set it
		SetPadding
	end,
	join_line([text(lists:duplicate(Indent, 32) ++ [$% | Line]) ||
		Line <- erl_syntax:comment_text(Comment)]);
% function/2
make_document_element(arity_qualifier, ArityQualifier, Opt) ->
	Name = erl_syntax:arity_qualifier_body(ArityQualifier),
	Arity = erl_syntax:arity_qualifier_argument(ArityQualifier),
	join([make_document(Name, Opt), prettypr:text("/"),
		make_document(Arity, Opt)]);
% Erlang variable
make_document_element(variable, Variable, _) ->
	text(erl_syntax:variable_literal(Variable));
% Erlang atom
make_document_element(atom, Atom, _) ->
	text(erl_syntax:atom_literal(Atom));
% Erlang integer
make_document_element(integer, Integer, _) ->
	text(erl_syntax:integer_literal(Integer));
% Erlang float
make_document_element(float, Float, _) ->
	text(erl_syntax:float_literal(Float));
% Erlang char
make_document_element(char, Char, _) ->
	text(erl_syntax:char_literal(Char));
% Erlang string
make_document_element(string, String, _) ->
	%% TODO: split string by word
	text(erl_syntax:string_literal(String));
% Erlang []
make_document_element(nil, _, _) ->
	text("[]");
% { .. }
make_document_element(tuple, Tuple, Opt) ->
	TupleElements = list2doc(erl_syntax:tuple_elements(Tuple),
		fun make_document/2, floating_text(","), clear_p(Opt)),
	join([floating_text("{"), prettypr:par(TupleElements), floating_text("}")]);
% [ .. | .. ]
make_document_element(list, List, Opt) ->
	NewOpt = clear_p(Opt),
	CompactList = erl_syntax:compact_list(List),
	ListElements = list2doc(erl_syntax:list_prefix(CompactList),
		fun make_document/2, floating_text(","), NewOpt),
	BeginList = join([prettypr:text("["), prettypr:par(ListElements)]),
	case erl_syntax:list_suffix(CompactList)
		of none ->
			join([BeginList, prettypr:text("]")])
		; ListTail ->
			EndList = join([prettypr:text("| "),
				make_document(ListTail, NewOpt), prettypr:text("]")]),
			prettypr:follow(BeginList, EndList)
	end;
% Erlang underscore
make_document_element(underscore, _, _) ->
	text("_");
% Erlang operator
make_document_element(operator, Operator, _) ->
	floating_text(erl_syntax:operator_literal(Operator));
% Erlang expr with priority
make_document_element(infix_expr, Expr, Opt) ->
	Operator = erl_syntax:infix_expr_operator(Expr),
	{PriorityL, Priority, PriorityR} = case erl_syntax:type(Operator)
		of operator ->
			erl_parse:inop_prec(erl_syntax:operator_name(Operator))
		; _ ->
			{0, 0, 0}
	end,
	LeftDoc = make_document(erl_syntax:infix_expr_left(Expr),
		set_priority(PriorityL, Opt)),
	RightDoc = make_document(erl_syntax:infix_expr_right(Expr),
		set_priority(PriorityR, Opt)),
	OperatorDoc = make_document(Operator, clear_p(Opt)),
	ExprIndent = proplists:get_value(expr_indent, Opt, 8),
	ExprDoc = prettypr:par([LeftDoc, OperatorDoc, RightDoc], ExprIndent),
	add_parentheses(ExprDoc, Priority, Opt);
% Erlang expr with priority
make_document_element(prefix_expr, Expr, Opt) ->
	Operator = erl_syntax:prefix_expr_operator(Expr),
	{{Priority, PriorityR}, Name} = case erl_syntax:type(Operator)
		of operator ->
			N = erl_syntax:operator_name(Operator),
			{erl_parse:preop_prec(N), N}
		; _ ->
			{{0, 0}, any}
	end,
	OperatorDoc = make_document(Operator, clear_p(Opt)),
	RightDoc = make_document(erl_syntax:prefix_expr_argument(Expr),
		set_priority(PriorityR, Opt)),
	ExprDoc = if Name == '+'; Name == '-' ->
		join([OperatorDoc, RightDoc])
	; true ->
		%% TODO: indent
		ExprIndent = proplists:get_value(expr_indent, Opt, 8),
		prettypr:par([OperatorDoc, RightDoc], ExprIndent)
	end,
	add_parentheses(ExprDoc, Priority, Opt);
% .. ( ... ) // Erlang function call
make_document_element(application, Application, Opt) ->
	{PriorityL, Priority} = erl_parse:func_prec(),
	FunctionCall = make_document(erl_syntax:application_operator(Application),
		set_priority(PriorityL, Opt)),
	Arguments = list2doc(erl_syntax:application_arguments(Application),
		fun make_document/2, floating_text(","), clear_p(Opt)),
	ApplicationDoc = join([FunctionCall, text("("), prettypr:par(Arguments),
		floating_text(")")]),
	add_parentheses(ApplicationDoc, Priority, Opt);
% .. = .. // Erlang match
make_document_element(match_expr, Match, Opt) ->
	{PriorityL, Priority, PriorityR} = erl_parse:inop_prec('='),
	LeftExpr = make_document(erl_syntax:match_expr_pattern(Match),
		set_priority(PriorityL, Opt)),
	LeftDoc = join([LeftExpr, floating_text(" =")]),
	RightDoc = make_document(erl_syntax:match_expr_body(Match),
		set_priority(PriorityR, Opt)),
	MatchIdent = proplists:get_value(match_indent, Opt, 8),
	MatchDoc = prettypr:follow(LeftDoc, RightDoc, MatchIdent),
	add_parentheses(MatchDoc, Priority, Opt);
% Erlang clause
make_document_element(clause, Clause, Opt) ->
	NewOpt = new(Opt),
	Patterns = prettypr:par(list2doc(erl_syntax:clause_patterns(Clause),
		fun make_document/2, floating_text(","), NewOpt)),
	Guard = case erl_syntax:clause_guard(Clause)
		of none ->
			none
		; G ->
			make_document(G, NewOpt)
	end,
	Body = join_line(list2doc(erl_syntax:clause_body(Clause),
		fun make_document/2, floating_text(","), NewOpt)),
	InClause = proplists:get_value(clause, Opt),
	if InClause == fun_expr ->
		function_clause(none, Patterns, Guard, Body, Opt)
	; is_tuple(InClause), element(1, InClause) == function ->
		{function, Function} = InClause,
		function_clause(Function, Patterns, Guard, Body, Opt)
	; InClause == if_expr; InClause == cond_expr ->
		if_clause(Patterns, Guard, Body, Opt)
	; InClause == case_expr; InClause == receive_expr; InClause == try_expr ->
		case_clause(Patterns, Guard, Body, Opt)
	; is_tuple(InClause), element(1, InClause) == rule ->
		{rule, Name} = InClause,
		Head = join([Name, add_parentheses(Patterns, -1, [])]),
		HeadGuard = append_guard(Head, Guard, Opt),
		clause_body(HeadGuard, Body, floating_text(" :-"), Opt)
	; true ->
		function_clause(none, Patterns, Guard, Body, Opt)
	end;
% name .. ; name .. ; name .. . // Erlang function clauses
make_document_element(function, Function, Opt) ->
	NewOpt = clear_p(Opt),
	FunctionName = make_document(erl_syntax:function_name(Function), NewOpt),
	Clauses = clauses(erl_syntax:function_clauses(Function),
		fun make_document/2, floating_text(";"), {function, FunctionName},
		NewOpt),
	join([Clauses, floating_text(".")]);
% case .. of .. end
make_document_element(case_expr, Case, Opt) ->
	NewOpt = clear_p(Opt),
	CaseExpr = make_document(erl_syntax:case_expr_argument(Case), NewOpt),
	CaseClauses = clauses(erl_syntax:case_expr_clauses(Case),
		fun make_document/2, floating_text(";"), case_expr, NewOpt),
	CaseIdent = proplists:get_value(case_indent, Opt, 8),
	prettypr:sep([prettypr:par([text("case"), CaseExpr, text("of")]),
		prettypr:nest(CaseIdent, CaseClauses), text("end")]);
% if .. end
make_document_element(if_expr, If, Opt) ->
	IfClauses = clauses(erl_syntax:if_expr_clauses(If), fun make_document/2,
		floating_text(";"), if_expr, clear_p(Opt)),
	IfIdent = proplists:get_value(if_indent, Opt, 8),
	prettypr:sep([prettypr:follow(text("if"), IfClauses, IfIdent), text("end")]);
% cond .. end
make_document_element(cond_expr, Cond, Opt) ->
	Body = clauses(erl_syntax:cond_expr_clauses(Cond), fun make_document/2,
		floating_text(";"), cond_expr, clear_p(Opt)),
	CondIdent = proplists:get_value(cond_indent, Opt, 8),
	prettypr:sep([text("cond"), prettypr:nest(CondIdent, Body), text("end")]);
% fun ... end
make_document_element(fun_expr, Fun, Opt) ->
	FunClauses = clauses(erl_syntax:fun_expr_clauses(Fun), fun make_document/2,
		floating_text(";"), fun_expr, clear_p(Opt)),
	FunIdent = proplists:get_value(fun_indent, Opt, 8),
	prettypr:sep([prettypr:follow(text("fun"), FunClauses, FunIdent),
		text("end")]);
% Erlang M:F
make_document_element(module_qualifier, MF, Opt) ->
	{PriorityL, _Priority, PriorityR} = erl_parse:inop_prec(':'),
	LeftDoc = make_document(erl_syntax:module_qualifier_argument(MF),
		set_priority(PriorityL, Opt)),
	RightDoc = make_document(erl_syntax:module_qualifier_body(MF),
		set_priority(PriorityR, Opt)),
	join([LeftDoc, text(":"), RightDoc]);
% __ . __ . __ //Doted module name
make_document_element(qualified_name, Qualifier, Opt) ->
	Name = erl_syntax:qualified_name_segments(Qualifier),
	case erl_syntax:is_atom(hd(Name), '')
		of true ->
			Doc = list2doc(tl(Name), fun make_document/2, text("."), Opt),
			join([text("."), join(Doc)])
		; _ ->
			join(list2doc(Name, fun make_document/2, text("."), Opt))
    end;
% << .. >>
make_document_element(binary, Binary, Opt) ->
	Body = list2doc(erl_syntax:binary_fields(Binary), fun make_document/2,
		floating_text(","), clear_p(Opt)),
	join([floating_text("<<"), prettypr:par(Body), floating_text(">>")]);
% ../.. in bit syntax
make_document_element(binary_field, Field, Opt) ->
	MaxP = set_max_p(Opt),
	Body = make_document(erl_syntax:binary_field_body(Field), MaxP),
	case erl_syntax:binary_field_types(Field)
		of [] ->
			Body
		; TypeInfo ->
			Type = list2doc(TypeInfo, fun make_document/2, floating_text("-"),
				MaxP),
			join([Body, floating_text("/"), join(Type, text("-"))])
	end;
% begin .. end
make_document_element(block_expr, Block, Opt) ->
	Body = list2doc(erl_syntax:block_expr_body(Block), fun make_document/2,
		floating_text(","), clear_p(Opt)),
	Indent = proplists:get_value(clause_indent, Opt, 8),
	prettypr:sep([text("begin"), prettypr:nest(Indent, prettypr:sep(Body)),
		text("end")]);
% catch ..
make_document_element(catch_expr, Catch, Opt) ->
	{Priority, PriorityR} = erl_parse:preop_prec('catch'),
	CatchBody = make_document(erl_syntax:catch_expr_body(Catch),
		set_priority(PriorityR, Opt)),
	ClauseIndent = proplists:get_value(clause_indent, Opt, 8),
	CatchDoc = prettypr:follow(floating_text("catch"), CatchBody, ClauseIndent),
	add_parentheses(CatchDoc, Priority, Opt);
% exit:Reason for catch in try
make_document_element(class_qualifier, Qualifier, Opt) ->
	ElementList = [erl_syntax:class_qualifier_argument(Qualifier),
		erl_syntax:class_qualifier_body(Qualifier)],
	join(list2doc(ElementList, fun make_document/2, text(":"), set_max_p(Opt)));
% .., .., ..
make_document_element(conjunction, Conjunction, Opt) ->
	prettypr:par(list2doc(erl_syntax:conjunction_body(Conjunction),
			fun make_document/2, floating_text(","), clear_p(Opt)));
% ..; ..; ..
make_document_element(disjunction, Disjuction, Opt) ->
	%% For clarity, we don't paragraph-format
	%% disjunctions; only conjunctions (see above).
	prettypr:sep(list2doc(erl_syntax:disjunction_body(Disjuction),
		fun make_document/2, floating_text(";"), clear_p(Opt)));
% Error message
make_document_element(error_marker, Error, Opt) ->
	Doc = error_message(erl_syntax:error_marker_info(Error), clear_p(Opt)),
	join([text("** "), Doc, text(" **")]);
% Noop
make_document_element(eof_marker, _, _) ->
	prettypr:empty();
% .. <- ..
make_document_element(generator, Gen, Opt) ->
	NewOpt = clear_p(Opt),
	Pattern = make_document(erl_syntax:generator_pattern(Gen), NewOpt),
	Body = make_document(erl_syntax:generator_body(Gen), NewOpt),
	join([Pattern, text("<-"), Body], text(" "));
% .. <= ..
make_document_element(binary_generator, Gen, Opt) ->
	NewOpt = clear_p(Opt),
	Pattern = make_document(erl_syntax:generator_pattern(Gen), NewOpt),
	Body = make_document(erl_syntax:generator_body(Gen), NewOpt),
	join([Pattern, text("<="), Body], text(" "));
% Function refer
make_document_element(implicit_fun, Fun, Opt) ->
	FunName = make_document(erl_syntax:implicit_fun_name(Fun), clear_p(Opt)),
	join([floating_text("fun "), FunName]);
% List comprehension
make_document_element(list_comp, ListComp, Opt) ->
	NewOpt = clear_p(Opt),
	CompExpr = erl_syntax:list_comp_template(ListComp),
	CompExprDoc = make_document(CompExpr, NewOpt),
	CompIf = list2doc(erl_syntax:list_comp_body(ListComp),
		fun make_document/2, floating_text(","), NewOpt),
	CompDoc = prettypr:par([CompExprDoc, floating_text("||") | CompIf]),
	join([floating_text("["), CompDoc, floating_text("]")]);
% Binary comprehension
make_document_element(binary_comp, BinaryComp, Opt) ->
	NewOpt = clear_p(Opt),
	CompExpr = erl_syntax:binary_comp_template(BinaryComp),
	CompExprDoc = make_document(CompExpr, NewOpt),
	CompIf = list2doc(erl_syntax:binary_comp_body(BinaryComp),
		fun make_document/2, floating_text(","), NewOpt),
	CompDoc = prettypr:par([CompExprDoc, floating_text("||") | CompIf]),
	join([floating_text("<<"), CompDoc, floating_text(">>")]);
% Macro call
make_document_element(macro, Macro, Opt) ->
	%% This is formatted similar to a normal function call, but
	%% prefixed with a "?".
	NewOpt = clear_p(Opt),
	MacroName = make_document(erl_syntax:macro_name(Macro), NewOpt),
	MacroDoc = case erl_syntax:macro_arguments(Macro)
		of none ->
			MacroName
		; MacroArgs ->
			MacroArgsDoc = list2doc(MacroArgs, fun make_document/2,
				floating_text(","), set_max_p(NewOpt)),
			join([MacroName, text("("), prettypr:par(MacroArgsDoc),
				floating_text(")")])
	end,
	add_parentheses(join([floating_text("?"), MacroDoc]), 0, Opt);
% ( .. )
make_document_element(parentheses, P, Opt) ->
	Body = make_document(erl_syntax:parentheses_body(P), clear_p(Opt)),
	add_parentheses(Body, -1, Opt);
% query .. end
make_document_element(query_expr, Query, Opt) ->
	Body = make_document(erl_syntax:query_expr_body(Query), clear_p(Opt)),
	ClauseIndent = proplists:get_value(clause_indent, Opt, 8),
	prettypr:sep([text("query"), prettypr:nest(ClauseIndent, Body),
		text("end")]);
% receive ... after ... end
make_document_element(receive_expr, Receive, Opt) ->
	NewOpt = clear_p(Opt),
	ReceiveClauses = clauses(erl_syntax:receive_expr_clauses(Receive),
		fun make_document/2, floating_text(";"), receive_expr, NewOpt),
	After = case erl_syntax:receive_expr_timeout(Receive)
		of none ->
			prettypr:empty()
		; Time ->
			Timeout = make_document(Time, NewOpt),
			AfterList = list2doc(erl_syntax:receive_expr_action(Receive),
				fun make_document/2, floating_text(","), NewOpt),
			AfterBody = clause_body(Timeout, prettypr:sep(AfterList),
				[{clause_indent, 2} | Opt]),
			prettypr:follow(text("after"), AfterBody)
	end,
	ReceiveIdent = proplists:get_value(receive_indent, Opt, 8),
	prettypr:sep([text("receive"), prettypr:nest(ReceiveIdent, ReceiveClauses),
		After, text("end")]);
% #record.field
make_document_element(record_access, Record, Opt) ->
	{PriorityL, Priority, PriorityR} = erl_parse:inop_prec('#'),
	RecordVar = erl_syntax:record_access_argument(Record),
	RecordVarType = erl_syntax:type(RecordVar),
	P = if RecordVarType == record_access ->
		Priority
	; true ->
		PriorityL
	end,
	RecordVariable = make_document(RecordVar, set_priority(P, Opt)),
	RecordField = make_document(erl_syntax:record_access_field(Record),
		set_priority(PriorityR, Opt)),
	RecordAccess = case erl_syntax:record_access_type(Record)
		of none ->
			join([RecordVariable, floating_text("."), RecordField])
		; RecordName ->
			RecordNameDoc = make_document(RecordName, clear_p(Opt)),
			join([RecordVariable, floating_text("#"), RecordNameDoc,
					floating_text("."), RecordField])
	end,
	add_parentheses(RecordAccess, Priority, Opt);
% A#test{field1 = 1}
make_document_element(record_expr, Record, Opt) ->
	NewOpt = clear_p(Opt),
	{PriorityL, Priority, _} = erl_parse:inop_prec('#'),
	RecordName = make_document(erl_syntax:record_expr_type(Record), NewOpt),
	RecordFields = list2doc(erl_syntax:record_expr_fields(Record),
		fun make_document/2, floating_text(","), NewOpt),
	RecordDefine = join([floating_text("#"), RecordName, floating_text("{"),
		prettypr:par(RecordFields), floating_text("}")]),
	RecordConstruct = case erl_syntax:record_expr_argument(Record)
		of none ->
			RecordDefine
		; FromRecord ->
			FromRecordDoc = make_document(FromRecord,
				set_priority(PriorityL, Opt)),
			join([FromRecordDoc, RecordDefine])
	end,
	add_parentheses(RecordConstruct, Priority, Opt);
% field = 2 :: integer()
make_document_element(record_field, RecordField, Opt) ->
	NewOpt = clear_p(Opt),
	RecordFieldName = erl_syntax:record_field_name(RecordField),
	NameDoc = make_document(RecordFieldName, NewOpt),
	Value = erl_syntax:record_field_value(RecordField),
	Type = erl_syntax:record_field_type(RecordField),
	RecordIndent = proplists:get_value(record_indent, Opt, 8),
	if Value == none, Type == none ->
		NameDoc
	; Type == none ->
		ValueDoc = make_document(Value, NewOpt),
		prettypr:par([NameDoc, floating_text("="), ValueDoc], RecordIndent)
	; Value == none ->
		TypeDoc = make_spec(Type, Opt),
		prettypr:par([NameDoc, floating_text("::"), TypeDoc], RecordIndent)
	; true ->
		ValueDoc = make_document(Value, NewOpt),
		TypeDoc = make_spec(Type, Opt),
		prettypr:par([NameDoc, floating_text("="), ValueDoc,
			floating_text("::"), TypeDoc], RecordIndent)
	end;
% #test.field
make_document_element(record_index_expr, Record, Opt) ->
	{Priority, PriorityR} = erl_parse:preop_prec('#'),
	RecordName = make_document(erl_syntax:record_index_expr_type(Record),
		clear_p(Opt)),
	RecordField = make_document(erl_syntax:record_index_expr_field(Record),
		set_priority(PriorityR, Opt)),
	RecordIndex = join([floating_text("#"), RecordName, floating_text("."),
		RecordField]),
	add_parentheses(RecordIndex, Priority, Opt);
% Rule defenition
make_document_element(rule, Rule, Opt) ->
	NewOpt = clear_p(Opt),
	Name = make_document(erl_syntax:rule_name(Rule), NewOpt),
	RuleClauses = clauses(erl_syntax:rule_clauses(Rule), fun make_document/2,
		floating_text(","), {rule, Name}, NewOpt),
	join([RuleClauses, floating_text(".")]);
% Name:Size
make_document_element(size_qualifier, Qualifier, Opt) ->
	ElementList = [erl_syntax:size_qualifier_body(Qualifier),
		erl_syntax:size_qualifier_argument(Qualifier)],
	join(list2doc(ElementList, fun make_document/2, text(":"), set_max_p(Opt)));
% Simple text
make_document_element(text, Text, _) ->
	text(erl_syntax:text_string(Text));
% try ..of .. catch .. after .. end
make_document_element(try_expr, Try, Opt) ->
	NewOpt = clear_p(Opt),
	CatchIndent = proplists:get_value(catch_indent, Opt, 8),
	TryBody = list2doc(erl_syntax:try_expr_body(Try), fun make_document/2,
		floating_text(","), NewOpt),
	TryEnd = case erl_syntax:try_expr_after(Try)
		of [] ->
			[text("end")]
		; AfterList ->
			After = clauses(AfterList, fun make_document/2, floating_text(","),
				try_expr, NewOpt),
			[text("after"), prettypr:nest(CatchIndent, After), text("end")]
	end,
	TryCatch = case erl_syntax:try_expr_handlers(Try)
		of [] ->
			TryEnd
		; CatchList ->
			Catch = clauses(CatchList, fun make_document/2, floating_text(";"),
				try_expr, NewOpt),
			[text("catch"), prettypr:nest(CatchIndent, Catch) | TryEnd]
	end,
	TryExpr = case erl_syntax:try_expr_clauses(Try)
		of [] ->
			TryCatch
		; ExprList ->
			Expr = clauses(ExprList, fun make_document/2, floating_text(";"),
				try_expr, NewOpt),
			[text("of"), prettypr:nest(CatchIndent, Expr) | TryCatch]
	end,
	TryDoc = [prettypr:follow(text("try"), prettypr:sep(TryBody), CatchIndent),
		hd(TryExpr)],
	prettypr:sep([prettypr:par(TryDoc) | tl(TryExpr)]);
% Warning message
make_document_element(warning_marker, Warning, Opt) ->
	Doc = error_message(erl_syntax:warning_marker_info(Warning), clear_p(Opt)),
	join([text("%% WARNING: "), Doc]).

function_clause(none, Patterns, Guard, Body, Opt) ->
	case_clause(add_parentheses(Patterns, -1, []), Guard, Body, Opt);
function_clause(Function, Patterns, Guard, Body, Opt) ->
	Header = join([Function, add_parentheses(Patterns, -1, [])]),
	case_clause(Header, Guard, Body, Opt).

if_clause(_P, Guard, Body, Opt) ->
    %% We ignore the patterns; they should be empty anyway.
	if Guard =:= none ->
		clause_body(text("true"), Body, Opt)
	; true ->
		clause_body(Guard, Body, Opt)
	end.

case_clause(Header, Guard, Body, Opt) ->
	Begin = append_guard(Header, Guard, Opt),
	clause_body(Begin, Body, Opt).

append_guard(Head, none, _Options) ->
	Head;
append_guard(Header, Guard, Opt) ->
	GuardIndent = proplists:get_value(guard_indent, Opt, 8),
	InnerGuardIndent = proplists:get_value(inner_guard_indent, Opt, 8),
	GuardDoc = prettypr:follow(text("when"), Guard, InnerGuardIndent),
    prettypr:par([Header, GuardDoc], GuardIndent).

clause_body(Begin, Body, Opt) ->
	clause_body(Begin, Body, floating_text(" ->"), Opt).

clause_body(Begin, Body, Separator, Opt) ->
	ClauseIndent = proplists:get_value(clause_indent, Opt, 8),
	ClauseBegin = join([Begin, Separator]),
	ClauseBody = prettypr:nest(ClauseIndent, Body),
	join_line([ClauseBegin, ClauseBody]).

clauses(Clauses, ToDoc, Separator, InClause, Opt) ->
	join_line(list2doc(Clauses, ToDoc, Separator, [{clause, InClause} | Opt])).

error_message({L, M, T}, Opt) when is_integer(L), is_atom(M) ->
	case catch M:format_error()
		of S when is_list(S), L > 0 ->
			text(integer_to_list(L) ++ ":" ++ S)
		; S when is_list(S) ->
			text(S)
		; _ ->
			make_document(erl_syntax:abstract(T), Opt)
	end;
error_message(Error, Opt) ->
	make_document(erl_syntax:abstract(Error), Opt).

list2doc(List, ToDoc, Separator, Opt) ->
	list2doc(List, ToDoc, Separator, Opt, []).

list2doc([], _, _, _, []) ->
	[prettypr:empty()];
list2doc([Last], ToDoc, _, Opt, Acc) ->
	lists:reverse([ToDoc(Last, Opt) | Acc]);
list2doc([Element | Tail], ToDoc, none, Opt, Acc) ->
	Add = ToDoc(Element, Opt),
	list2doc(Tail, ToDoc, none, Opt, [Add | Acc]);
list2doc([Element | Tail], ToDoc, Separator, Opt, Acc) ->
	Add = prettypr:beside(ToDoc(Element, Opt), Separator),
	list2doc(Tail, ToDoc, Separator, Opt, [Add | Acc]).

%% @doc Concatenate list of documents withot separator
join_line(Document) ->
	join_line(Document, none).

%% @doc Concatenate list of documents with line break
join_line([], _) ->
	prettypr:empty();
join_line(Lines, none) ->
	foldl(Lines, fun(NextLine, Acc) -> prettypr:above(Acc, NextLine) end);
join_line(Lines, Separator) ->
	foldl(Lines, fun(NextLine, Acc) ->
			prettypr:above(prettypr:above(Acc, Separator), NextLine) end).

%% @doc Concatenate list of documents withot separator
join(Document) ->
	join(Document, none).

%% @doc Concatenate list of documents with separator
join([], _) ->
	prettypr:empty();
join(Documents, none) ->
	foldl(Documents, fun(NextDoc, Acc) -> prettypr:beside(Acc, NextDoc) end);
join(Documents, Separator) ->
	foldl(Documents, fun(NextDoc, Acc) ->
			prettypr:beside(prettypr:beside(Acc, Separator), NextDoc) end).

foldl(Documents, Fun) ->
	lists:foldl(Fun, hd(Documents), tl(Documents)).

%% @doc Make document for floating text
floating_text(String) ->
	prettypr:floating(prettypr:text(String)).

%% @doc Make document for string
text(String) ->
	prettypr:text(String).

%% @doc Add parentheses
add_parentheses(Expr, Priority, Opt) ->
	CurrentPriority = proplists:get_value(priority, Opt, 0),
	if CurrentPriority > Priority ->
		join([floating_text("("), Expr, floating_text(")")])
	; true ->
		Expr
	end.

make_spec(Spec, Opt) ->
	SpecTerm = erl_syntax:concrete(Spec),
	spec_types:make_spec_document(SpecTerm, Opt).

set_priority(P, Opt) ->
	[{priority, P} | Opt].

set_max_p(Opt) ->
	[{priority, erl_parse:max_prec()} | Opt].

clear_p(Opt) ->
	proplists:delete(priority, Opt).

new(Opt) ->
	Opt1 = clear_p(Opt),
	Opt2 = proplists:delete(clause, Opt1),
	[{clause, undefined} | Opt2].
