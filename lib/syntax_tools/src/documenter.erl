%% @author Slava Yurin <YurinVV@ya.ru>
%% @doc Make document from syntax tree
-module(documenter).

-define(COMMA, prettypr:floating(prettypr:text(","))).

-export([syntax2document/1]).

%% @doc Formal syntax -> document
-spec syntax2document([term()]) -> no_return().
syntax2document(Opt) ->
	receive {form, Form} ->
		Doc = prettypr:break(document(Form, Opt)),
		ok = chain:send({document, Doc}, next, Opt),
		syntax2document(Opt)
	; eof ->
		ok = chain:send(eof, next, Opt),
		ok
	; _Unknown ->
		syntax2document(Opt)
	end.

%% @doc Make document for abstract syntax
document({attribute, _, define, [Name, Body]}, Opt) ->
	NameDoc = prettypr:beside(document(Name, Opt), floating_text(",")),
	BodyDoc = document(Body, Opt),
	join([text("-define("),
		prettypr:follow(NameDoc, BodyDoc, 1), floating_text(").")]);
document({attribute, _, undef, Name}, Opt) ->
	NameDoc = document(Name, Opt),
	join([text("-undef"), add_parentheses(NameDoc), text(".")]);
document({attribute, _, ifdef, Name}, Opt) ->
	NameDoc = document(Name, Opt),
	join([text("-ifdef"), add_parentheses(NameDoc), text(".")]);
document({attribute, _, ifndef, Name}, Opt) ->
	NameDoc = document(Name, Opt),
	join([text("-ifndef"), add_parentheses(NameDoc), text(".")]);
document({attribute, _, else, _}, _) ->
	text("-else.");
document({attribute, _, endif, _}, _) ->
	text("-endif.");
document({attribute, _, include, {string, _, String}}, _) ->
	text("-include(" ++ io_lib:write_string(String) ++ ").");
document({attribute, _, include_lib, {string, _, String}}, _) ->
	text("-include_lib(" ++ io_lib:write_string(String) ++ ").");
document({attribute, _, type, {TypeName, Type, Args}}, Opt) ->
	make_type_def(type, TypeName, Type, Args, Opt);
document({attribute, _, opaque, {TypeName, Type, Args}}, Opt) ->
	make_type_def(opaque, TypeName, Type, Args, Opt);
document({attribute, _, spec, {{Name, _}, Types}}, Opt) ->
	TypesDoc = list2doc(Types, [spec | Opt]),
	SpecDoc = clauses(text(atom_to_list(Name)), text("; "), TypesDoc),
	join([text("-spec "), SpecDoc, text(".")]);
document({attribute, _, record, {Name, Fields}}, Opt) ->
	NameDoc = text(io_lib:write_atom(Name)),
	FieldsDoc = prettypr:par(list2doc(Fields, ?COMMA, Opt)),
	join([text("-record("), NameDoc, text(", {"), FieldsDoc, text("}).")]);
document({attribute, _, export, FunList}, _) ->
	join([text("-export("), fun_list(FunList), text(").")]);
document({attribute, _, import, Import}, _) ->
	ImportBody = case Import
		of {Name, FunList} ->
			join([module_name(Name), text(", "), fun_list(FunList)])
		; Name ->
			module_name(Name)
	end,
	join([text("-import("), ImportBody, text(").")]);
document({attribute, _, file, {Name, Line}}, _) ->
	join([text("-file("), text(io_lib:write_string(Name)), text(", "),
		text(integer_to_list(Line)), text(").")]);
document({attribute, _, Name, Term}, Opt) ->
	Doc = document(extend_parser:abstract(Term), Opt),
	join([text("-" ++ io_lib:write_atom(Name) ++ "("), Doc, text(").")]);
document({record_field, _, Name}, Opt) ->
	document(Name, Opt);
document({record_field, _, Name, Value}, Opt) ->
	NameDoc = document(Name, Opt),
	ValueDoc = document(Value, Opt),
	prettypr:follow(prettypr:beside(NameDoc, text(" =")), ValueDoc, 1);
document({record_field, _, Expr, Name, Field}, Opt) ->
	NameDoc = text(io_lib:write_atom(Name)),
	FieldDoc = document(Field, Opt),
	ExprDoc = document(Expr, set_priority(750, Opt)),
	Doc = join([ExprDoc, text("#"), NameDoc, text("."), FieldDoc]),
	add_parentheses(Doc, 750, Opt);
document({typed_record_field, Field, Type}, Opt) ->
	FieldDoc = document(Field, Opt),
	TypeDoc = document(Type, Opt),
	join([FieldDoc, text(" :: "), TypeDoc]);
document({record_index, _, Name, Field}, Opt) ->
	prettypr:beside(text("#" ++ io_lib:write_atom(Name) ++ "."),
		document(Field, Opt));
document({record, _, Name, Fields}, Opt) ->
	NameDoc = text("#" ++ io_lib:write_atom(Name)),
	ArgsDoc = prettypr:par(list2doc(Fields, ?COMMA, Opt)),
	join([NameDoc, text("{"), ArgsDoc, text("}")]);
document({record, _, Expr, Name, Fields}, Opt) ->
	ExprDoc = document(Expr, set_priority(750, Opt)),
	NameDoc = text("#" ++ io_lib:write_atom(Name)),
	ArgsDoc = prettypr:par(list2doc(Fields, ?COMMA, clear_p(Opt))),
	join([ExprDoc, NameDoc, text("{"), ArgsDoc, text("}")]);
document({type, _, nil, []}, _) ->
	text("[]");
document({type, _, range, [{integer, _, R}, {integer, _, L}]}, _) ->
	text(integer_to_list(R) ++ ".." ++ integer_to_list(L));
document({type, _, union, Args}, Opt) ->
	prettypr:par(list2doc(Args, text(" |"), Opt));
document({type, _, constraint, [{atom, _, is_subtype}, [Name, Type]]}, Opt) ->
	join([document(Name, Opt), text(" :: "), document(Type, Opt)]);
document({type, _, tuple, Args}, Opt) ->
	if Args == any ->
		text("tuple()")
	; true ->
		ArgsDoc = prettypr:par(list2doc(Args, ?COMMA, Opt)),
		join([text("{"), ArgsDoc, text("}")])
	end;
document({type, _, 'fun', [InSpec, OutSpec]}, Opt) ->
	ArgsDoc = case InSpec
		of {type, _, any} ->
			text("...")
		; {type, _, product, Args} ->
			prettypr:par(list2doc(Args, ?COMMA, Opt))
	end,
	InDoc = prettypr:beside(add_parentheses(ArgsDoc), text(" ->")), 
	Doc = prettypr:par([InDoc, document(OutSpec, Opt)], 1),
	IsSpec = proplists:get_bool(spec, Opt),
	if IsSpec ->
		Doc
	; true ->
		prettypr:beside(text("fun"), add_parentheses(Doc))
	end;
document({type, _, bounded_fun, [Fun, When]}, Opt) ->
	WhenDoc = prettypr:par(list2doc(When, ?COMMA, Opt)),
	prettypr:par([document(Fun, Opt), prettypr:follow(text("when"), WhenDoc, 1)]);
document({type, _, binary, [{integer, _, BaseSize}, {integer, _, UnitSize}]}, _) ->
	Size = case {BaseSize, UnitSize}
		of {0, 0} ->
			prettypr:empty()
		; {_, 0} ->
			text("_: " ++ integer_to_list(BaseSize))
		; {0, _} ->
			text("_:_ * " ++ integer_to_list(UnitSize))
		; _ ->
			join([text("_: " ++ integer_to_list(BaseSize)), text(", "),
				text("_:_ * " ++ integer_to_list(UnitSize))])
	end,
	join([text("<<"), Size, text(">>")]);
document({type, _, TypeName, Args}, Opt) ->
	TypeNameDoc = text(atom_to_list(TypeName)),
	ArgsDoc = add_parentheses(prettypr:par(list2doc(Args, ?COMMA, Opt))),
	prettypr:beside(TypeNameDoc, ArgsDoc);
document({remote_type, Line, [Module, Name, Args]}, Opt) ->
	document({call, Line, {remote, Line, Module, Name}, Args}, Opt);
document({atom, _, module, Module}, _) ->
	ModuleBody = case Module
		of {Name, _VarList} ->
			module_name(Name),
			exit(todo_param_moduel)
		; Name ->
			module_name(Name)
	end,
	join([text("-module("), ModuleBody, text(").")]);
document({op, _, Name, Left, Right}, Opt) ->
	{PriorityL, Priority, PriorityR} = extend_parser:inop_prec(Name),
	LeftDoc = document(Left, set_priority(PriorityL, Opt)),
	RightDoc = document(Right, set_priority(PriorityR, Opt)),
	Doc = prettypr:par([LeftDoc, text(atom_to_list(Name)), RightDoc], 1),
	add_parentheses(Doc, Priority, Opt);
document({op, _, Name, Right}, Opt) ->
	{Priority, PriorityR} = extend_parser:preop_prec(Name),
	RightDoc = document(Right, set_priority(PriorityR, Opt)),
	NameDoc = text(atom_to_list(Name)),
	Doc = if Name == '+'; Name == '-' ->
		prettypr:beside(NameDoc, RightDoc)
	; true ->
		prettypr:par([NameDoc, RightDoc], 1)
	end,
	add_parentheses(Doc, Priority, Opt);
document({'catch', _, Right}, Opt) ->
	{Priority, PriorityR} = extend_parser:preop_prec('catch'),
	RightDoc = document(Right, set_priority(PriorityR, Opt)),
	Doc = prettypr:par([text("catch"), RightDoc], 1),
	add_parentheses(Doc, Priority, Opt);
document({function, _, Name, _Arity, Clauses}, Opt) ->
	NewOpt = [{clause, function} | clear_p(Opt)],
	NameDoc = text(io_lib:write_atom(Name)),
	ClausesDoc = function_clauses(NameDoc, list2doc(Clauses, NewOpt)),
	prettypr:beside(ClausesDoc, text("."));
document({'if', _, Clauses}, Opt) ->
	NewOpt = [{clause, 'if'} | clear_p(Opt)],
	ClausesDoc = list2doc(Clauses, NewOpt),
	prettypr:above(clauses(text("if "), text("; "), ClausesDoc), text("end"));
document({'case', _, Expr, Clauses}, Opt) ->
	NewOpt = [{clause, 'case'} | clear_p(Opt)],
	ExprDoc = prettypr:beside(text("case "), document(Expr, NewOpt)),
	Body = clauses(text("of "), text("; "), list2doc(Clauses, NewOpt)),
	join_line([ExprDoc, prettypr:nest(1, Body), text("end")]);
document({'receive', _, Clauses}, Opt) ->
	NewOpt = [{clause, 'receive'} | clear_p(Opt)],
	Body = clauses(prettypr:empty(), text("; "), list2doc(Clauses, NewOpt)),
	join_line([text("receive"), prettypr:nest(1, Body), text("end")]);
document({'receive', _, Clauses, Expr, Body}, Opt) ->
	NewOpt = [{clause, 'receive'} | clear_p(Opt)],
	ClausesDoc = list2doc(Clauses, NewOpt),
	ReceiveBody = clauses(prettypr:empty(), text("; "), ClausesDoc),
	AfterBody = join_line(list2doc(Body, ?COMMA, NewOpt)),
	AfterHead = join([text("after "), document(Expr, NewOpt),
		floating_text(" ->")]),
	After = prettypr:above(AfterHead, prettypr:nest(1, AfterBody)),
	join_line([text("receive"), prettypr:nest(1, ReceiveBody), After,
		text("end")]);
document({'fun', _, {function, Name, Arity}}, _) ->
	text("fun " ++ io_lib:write_atom(Name) ++ "/" ++ integer_to_list(Arity));
document({'fun', _, {function, Module, Name, Arity}}, _) ->
	text("fun " ++ io_lib:write_atom(Module) ++ ":" ++
		io_lib:write_atom(Name) ++ "/" ++ integer_to_list(Arity));
document({'fun', _, {clauses, Clauses}}, Opt) ->
	ClausesDoc = list2doc(Clauses, [{clause, 'fun'} | clear_p(Opt)]),
	join_line([clauses(text("fun "), text("; "), ClausesDoc), text("end")]);
document({'try',_, Expr, Match, Catch, After}, Opt) ->
	NewOpt = [{clause, 'try'} | clear_p(Opt)],
	ExprDoc = prettypr:beside(text("try "),
		prettypr:sep(list2doc(Expr, ?COMMA, NewOpt))),
	MatchDoc = if Match == [] ->
		prettypr:empty()
	; true ->
		MatchClauses = list2doc(Match, NewOpt),
		prettypr:nest(1, clauses(text("of "), text("; "), MatchClauses))
	end,
	CatchDoc = if Catch == [] ->
		prettypr:empty()
	; true ->
		CatchOpt = [{clause, 'catch'} | clear_p(Opt)],
		CatchClauses = list2doc(Catch, CatchOpt),
		CatchBody = clauses(prettypr:empty(), text("; "), CatchClauses),
		prettypr:above(text("catch"), prettypr:nest(1, CatchBody))
	end,
	AfterDoc = if After == [] ->
		prettypr:empty()
	; true ->
		AfterBody = join_line(list2doc(After, ?COMMA, NewOpt)),
		prettypr:above(text("after"), prettypr:nest(1, AfterBody))
	end,
	prettypr:sep([ExprDoc, MatchDoc, CatchDoc, AfterDoc, text("end")]);
document({'query', _, Query}, Opt) ->
	join_line([text("query"), prettypr:nest(1, document(Query, Opt)),
		floating_text("end")]);
document({clause, _, Args, Guard, Body}, Opt) ->
	NewOpt = new(Opt),
	Type = proplists:get_value(clause, Opt),
	ArgsDoc = list2doc(Args, ?COMMA, NewOpt),
	% Make document on list of list
	MakeGuardAnd = fun(Element, LocOpt) ->
			prettypr:par(list2doc(Element, ?COMMA, LocOpt))
		end,
	GuardsDoc = list2doc(Guard, MakeGuardAnd, floating_text(";"), NewOpt),
	GuardDoc = prettypr:par(GuardsDoc),
	BodyDoc = join_line(list2doc(Body, ?COMMA, NewOpt)),
	ClauseHead = if Type == 'if' ->
		prettypr:beside(prettypr:empty(), GuardDoc)
	; Type == 'case'; Type == 'receive'; Type == 'try' ->
		if Guard == [] ->
			hd(ArgsDoc)
		; true ->
			Match = prettypr:beside(hd(ArgsDoc), floating_text(" when")),
			prettypr:par([Match, GuardDoc], 1)
		end
	; Type == 'catch' ->
		[{tuple, _, [CatchClass, CatchName, {var, _, '_'}]}] = Args,
		CatchClassDoc = document(CatchClass, NewOpt),
		CatchNameDoc = document(CatchName, set_max_p(NewOpt)),
		CatchArg = join([CatchClassDoc, text(":"), CatchNameDoc]),
		if Guard == [] ->
			CatchArg
		; true ->
			Match = prettypr:beside(CatchArg, floating_text(" when")),
			prettypr:par([Match, GuardDoc], 1)
		end
	; Type == rule ->
		layout:rule_clause(ArgsDoc, GuardDoc, BodyDoc)
	; true ->
		ParentheseArgs = add_parentheses(prettypr:par(ArgsDoc)),
		if Guard == [] ->
			ParentheseArgs
		; true ->
			ArgList = prettypr:beside(ParentheseArgs, floating_text(" when")),
			prettypr:par([ArgList, GuardDoc], 1)
		end
	end,
	prettypr:above(prettypr:sep([ClauseHead, floating_text("->")]),
		prettypr:nest(1, BodyDoc));
document({match, _, Left, Right}, Opt) ->
	{PriorityL, Priority, PriorityR} = extend_parser:inop_prec('='),
	LeftDoc = document(Left, set_priority(PriorityL, Opt)),
	RightDoc = document(Right, set_priority(PriorityR, Opt)),
	Doc = prettypr:follow(prettypr:beside(LeftDoc, text(" =")), RightDoc, 1),
	add_parentheses(Doc, Priority, Opt);
document({call, _, Name, Args}, Opt) ->
	{PriorityL, Priority} = erl_parse:func_prec(),
	NameDoc = document(Name, set_priority(PriorityL, Opt)),
	ArgsDoc = prettypr:par(list2doc(Args, ?COMMA, clear_p(Opt))),
	Doc = prettypr:beside(NameDoc, add_parentheses(ArgsDoc)),
	add_parentheses(Doc, Priority, Opt);
document({remote, _, Module, Name}, Opt) ->
	ModuleDoc = document(Module, Opt),
	NameDoc = document(Name, Opt),
	join([ModuleDoc, text(":"), NameDoc]);
document({macro, _, Name}, _) ->
	prettypr:beside(text("?"), document(Name, []));
document({integer, _, Integer}, _) ->
	text(integer_to_list(Integer));
document({atom, _, Atom}, _) ->
	text(io_lib:write_atom(Atom));
document({float, _, Float}, _) ->
	IsExp = (Float < -10) or (Float > 10) or ((Float < 1) and (Float > -1)),
	if IsExp ->
		S = lists:flatten(io_lib:format("~.15e", [Float])),
		[V, E] = string:tokens(S, "e"),
		CleanValue = remove_zero(lists:reverse(V)),
		CleanExp = remove_zero(lists:reverse(E)),
		text(CleanValue ++ "e" ++ CleanExp)
	; true ->
		V = lists:flatten(io_lib:format("~.15f", [Float])),
		text(remove_zero(lists:reverse(V)))
	end;
document({macro_string, _, Name}, _) ->
	prettypr:beside(text("??"), document(Name, []));
document({char, _, Char}, _) ->
	text(io_lib:write_char(Char));
document({cons, _, _, _} = ListChain, Opt) ->
	Doc = case collect_list(ListChain, [])
		of {Elements, []} ->
			prettypr:par(list2doc(Elements, ?COMMA, Opt))
		; {Elements, Tail} ->
			ElementsDoc = prettypr:par(list2doc(Elements, ?COMMA, Opt)),
			ElementDoc = prettypr:beside(ElementsDoc, floating_text(" |")),
			prettypr:par([ElementDoc, document(Tail, Opt)])
	end,
	join([text("["), Doc, floating_text("]")]);
document({bin, _,  Binary}, Opt) ->
	Doc = prettypr:par(list2doc(Binary, ?COMMA, Opt)),
	join([text("<<"), Doc, floating_text(">>")]);
document({bin_element, _, Name, Size, Type}, Opt) ->
	NewOpt = set_max_p(Opt),
	SizeDoc = if Size == default ->
		prettypr:empty()
	; true ->
		prettypr:beside(text(":"), document(Size, NewOpt))
	end,
	TypeDoc = if Type == default ->
		prettypr:empty()
	; true ->
		prettypr:beside(text("/"), make_bin_type(Type, []))
	end,
	join([document(Name, NewOpt), SizeDoc, TypeDoc]);
document({nil, _}, _) ->
	text("[]");
document({tuple, _, Elements}, Opt) ->
	Doc = prettypr:par(list2doc(Elements, ?COMMA, Opt)),
	join([text("{"), Doc, floating_text("}")]);
document({block, _, Body}, Opt) ->
	Doc = prettypr:sep(list2doc(Body, ?COMMA, clear_p(Opt))),
	prettypr:sep([text("begin"), prettypr:nest(1, Doc), text("end")]);
document({string, _, String}, _) ->
	%% TODO: split string by word
	text(io_lib:write_string(String));
document({var, _, Var}, _) ->
	text(atom_to_list(Var));

document({lc, _, Expr, Generator}, Opt) ->
	comprehension(lc, Expr, Generator, clear_p(Opt));
document({bc, _, Expr, Generator}, Opt) ->
	comprehension(bc, Expr, Generator, clear_p(Opt));
document({generate, _, Value, List}, Opt) ->
	generate(generate, Value, List, clear_p(Opt));
document({b_generate, _, Value, List}, Opt) ->
	generate(b_generate, Value, List, clear_p(Opt)).

module_name(Name) when is_atom(Name) ->
	text(io_lib:write_atom(Name));
module_name(Name) ->
	list2doc(Name, fun(Atom, _) -> text(io_lib:write_atom(Atom)) end,
		text("."), []).

make_type_def(Name, TypeName, Type, Args, Opt) ->
	NameDoc = text("-" ++ atom_to_list(Name) ++ " "),
	TypeNameDoc = text(atom_to_list(TypeName)),
	TypeDoc = document(Type, Opt),
	ArgsDoc = add_parentheses(prettypr:par(list2doc(Args, ?COMMA, Opt))),
	TypeBegin = join([NameDoc, TypeNameDoc, ArgsDoc, text(" ::")]),
	TypeDef = prettypr:follow(TypeBegin, TypeDoc, 1),
	prettypr:beside(TypeDef, text(".")).

fun_list(FunList) ->
	Doc = list2doc(FunList, fun({Fun, Arity}, _) ->
			text(io_lib:write_atom(Fun) ++ "/" ++ integer_to_list(Arity))
		end, ?COMMA, []),
	join([text("["), prettypr:par(Doc), text("]")]).

remove_zero([$0 | Tail]) ->
	remove_zero(Tail);
remove_zero(Value) ->
	lists:reverse(Value).

collect_list({cons, _, E, {cons, _, _, _} = SubList}, List) ->
	collect_list(SubList, [E | List]);
collect_list({cons, _, E, {nil, _}}, List) ->
	{lists:reverse([E | List]), []};
collect_list({cons, _, E, Last}, List) ->
	{lists:reverse([E | List]), Last}.

make_bin_type([], TypeList) ->
	join(lists:reverse(TypeList), text("-"));
make_bin_type([{Type, Size} | _], Tail) ->
	NameDoc = text(atom_to_list(Type)),
	SizeDoc = text(integer_to_list(Size)),
	Last = join([NameDoc, text(":"), SizeDoc]),
	make_bin_type([], [Last | Tail]);
make_bin_type([Type | Tail], TypeList) ->
	TypeDoc = text(atom_to_list(Type)),
	make_bin_type(Tail, [TypeDoc | TypeList]).

comprehension(Type, Expr, Generator, Opt) ->
	Begin = prettypr:beside(document(Expr, Opt), text(" ||")),
	GeneratorDoc = list2doc(Generator, ?COMMA, Opt),
	Comprehension = prettypr:par([Begin | GeneratorDoc]),
	case Type
		of lc ->
			join([text("["), Comprehension, floating_text("]")])
		; bc ->
			join([text("<< "), Comprehension, floating_text(" >>")])
	end.

generate(Type, Value, List, Opt) ->
	ValueDoc = case Type
		of generate ->
			prettypr:beside(document(Value, Opt), text(" <-"))
		; b_generate ->
			prettypr:beside(document(Value, Opt), text(" <="))
	end,
	prettypr:par([ValueDoc, document(List, Opt)]).

function_clauses(NameDoc, Clauses) ->
	Clauses1 = [prettypr:add(NameDoc, Clause) || Clause <- Clauses],
	Separator = floating_text(";"),
	foldl(Clauses1, fun(NextLine, Acc) ->
		prettypr:above(prettypr:beside(Acc, Separator), NextLine) end).

clauses(Header, Separator, Clauses) ->
	[ClauseBegin | ClausesTail] = Clauses,
	Begin = prettypr:add(Header, ClauseBegin),
	Body = [prettypr:add(Separator, Clause) || Clause <- ClausesTail],
	join_line([Begin | Body]).

list2doc(List, Opt) ->
	list2doc(List, fun document/2, Opt, none, []).

list2doc(List, Separator, Opt) ->
	list2doc(List, fun document/2, Opt, Separator, []).

list2doc(List, ToDoc, Separator, Opt) ->
	list2doc(List, ToDoc, Opt, Separator, []).

list2doc([], _, _, _, []) ->
	[prettypr:empty()];
list2doc([Last], ToDoc, Opt, _, Acc) ->
	lists:reverse([ToDoc(Last, Opt) | Acc]);
list2doc([Element | Tail], ToDoc, Opt, none, Acc) ->
	Doc = ToDoc(Element, Opt),
	list2doc(Tail, ToDoc, Opt, none, [Doc | Acc]);
list2doc([Element | Tail], ToDoc, Opt, Separator, Acc) ->
	Doc = ToDoc(Element, Opt),
	DocSep = prettypr:beside(Doc, Separator),
	list2doc(Tail, ToDoc, Opt, Separator, [DocSep | Acc]).

add_parentheses(Expr) ->
	add_parentheses(Expr, -1, []).

%% @doc Add parentheses
add_parentheses(Expr, Priority, Opt) ->
	CurrentPriority = proplists:get_value(priority, Opt, 0),
	if CurrentPriority > Priority ->
		join([floating_text("("), Expr, floating_text(")")])
	; true ->
		Expr
	end.

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

%%%%%% @doc Make document for abstract syntax
%%%%% Erlang comment
%%%%make_document_element(comment, Comment, Opt) ->
%%%%	SetPadding = proplists:get_value(padding, Opt, 0),
%%%%	Padding = erl_syntax:comment_padding(Comment),
%%%%	Indent = if SetPadding > 0, Padding > 0 ->
%%%%		% Indent exists and we want set it
%%%%		Padding
%%%%	; true ->
%%%%		% Indent not exists but we may set it
%%%%		SetPadding
%%%%	end,
%%%%	join_line([text(lists:duplicate(Indent, 32) ++ [$% | Line]) ||
%%%%		Line <- erl_syntax:comment_text(Comment)]);
%%%%% Rule defenition
%%%%make_document_element(rule, Rule, Opt) ->
%%%%	NewOpt = clear_p(Opt),
%%%%	Name = make_document(erl_syntax:rule_name(Rule), NewOpt),
%%%%	RuleClauses = clauses(erl_syntax:rule_clauses(Rule), fun make_document/2,
%%%%		floating_text(","), {rule, Name}, NewOpt),
%%%%	join([RuleClauses, floating_text(".")]);
