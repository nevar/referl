%% @author Slava Yurin <YurinVV@ya.ru>
%% @doc Make document for spec types from syntax tree
-module(spec_types).

-export([make_document/2, make_spec_document/2]).

%% @doc Make document for spec
-spec make_document(erl_syntax:syntaxTree(), term()) -> prettypr:document().
make_document(FunSpecs, Options) ->
	list2doc(erl_syntax:list_elements(FunSpecs), fun make_document_element/2,
		floating_text(";"), Options).

make_document_element(SpecSyntax, Options) ->
	Spec = erl_syntax:concrete(SpecSyntax),
	SpecType = element(3, Spec),
	if SpecType == bounded_fun ->
		[InOutSpecTuple, ConstrainTuple] = element(4, Spec),
		[InSpecTuple, OutSpecTuple] = element(4, InOutSpecTuple)
	; true ->
		[InSpecTuple, OutSpecTuple] = element(4, Spec),
		ConstrainTuple = none
	end,
	InSpecDoc = list2doc(element(4, InSpecTuple), fun make_spec_document/2,
		floating_text(","), Options),
	SpecHeader = join([text("("), prettypr:par(InSpecDoc), text(") ->")]),
	SpecReturn = make_spec_document(OutSpecTuple, Options),
	SpecDoc = prettypr:follow(SpecHeader, SpecReturn, 8),
	if SpecType == bounded_fun ->
		ConstrainDoc = list2doc(ConstrainTuple,
			fun make_constrain_document/2, floating_text(","), Options),
		prettypr:follow(SpecDoc,
			prettypr:follow(text("when"), prettypr:par(ConstrainDoc)))
	; true ->
		SpecDoc
	end.

make_spec_document(Spec, Options) ->
	Type = element(1, Spec),
	SubType = element(3, Spec),
	case Type
		of type when SubType == range ->
			join(list2doc(element(4, Spec),
				fun({_, _, Int}, _) -> text(integer_to_list(Int)) end,
				floating_text(".."), Options))
		; type when SubType == union ->
			prettypr:par(list2doc(element(4, Spec),
				fun make_spec_document/2, floating_text(" |"), Options))
		; type ->
			TypeParam = element(4, Spec),
			TypeNameDoc = text(io_lib:write_atom(SubType)),
			ParamDoc = if TypeParam == any; length(TypeParam) == 0 ->
				[prettypr:empty()]
			; true ->
				list2doc(TypeParam, fun make_spec_document/2,
					floating_text(","), Options)
			end,
			join([TypeNameDoc, text("("), prettypr:par(ParamDoc),
				floating_text(")")])
		; var ->
			text(atom_to_list(SubType))
		; remote_type ->
			[TupleModule, TupleFunction, ParamList] = SubType,
			{_, _Line, ModuleName} = TupleModule,
			ModuleNameDoc = text(io_lib:write_atom(ModuleName)),
			{_, _Line, FunctionName} = TupleFunction,
			FunctionNameDoc = text(io_lib:write_atom(FunctionName)),
			ParamDoc = if length(ParamList) == 0 ->
				[prettypr:empty()]
			; true ->
				list2doc(ParamList, fun make_spec_document/2,
					floating_text(","), Options)
			end,
			join([ModuleNameDoc, text(":"), FunctionNameDoc, text("("),
				prettypr:par(ParamDoc), floating_text(")")])
		; ann_type ->
			[Ann, AnnType] = SubType,
			{_, _Line, AnnName} = Ann,
			AnnNameDoc = text(atom_to_list(AnnName)),
			TypeDoc = make_spec_document(AnnType, Options),
			join([AnnNameDoc, text("::"), TypeDoc])
		; atom ->
			text(io_lib:write_atom(SubType))
		; integer ->
			text(integer_to_list(SubType))
		; _ ->
			exit({unknown_spec, Spec})
	end.

make_constrain_document({_, _, _, Constrain}, Options) ->
	[_ConstrainType, ConstrainIf] = Constrain,
	[ConstrainVar, ConstrainType] = ConstrainIf,
	ConstrainVarDoc = make_spec_document(ConstrainVar, Options),
	ConstrainTypeDoc = make_spec_document(ConstrainType, Options),
	join([ConstrainVarDoc, text(" :: "), ConstrainTypeDoc]).

list2doc(List, ToDoc, Separator, Options) ->
	add_separator([ToDoc(Element, Options) || Element <- List], Separator, []).

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

%% @doc Insert Seporator to list
%%
%% For each element in list of documents add seporator, except last.
add_separator([], _, _) ->
	[prettypr:empty()];
add_separator([LastDocument], _, NewDocuments) ->
	lists:reverse([LastDocument | NewDocuments]);
add_separator([Document | DocumentsTail], Separator, NewDocuments) ->
	NewElement = prettypr:beside(Document, Separator),
	add_separator(DocumentsTail, Separator, [NewElement | NewDocuments]).

%% @doc Make document for floating text
floating_text(String) ->
	prettypr:floating(prettypr:text(String)).

%% @doc Make document for string
text(String) ->
	prettypr:text(String).
