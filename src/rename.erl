%% @author Slava Yurin <YurinVV@ya.ru>
%% @doc Module for renames
-module(rename).

-export([variable/5]).

%% @doc Rename variable Name in one form at line
-spec variable(file:filename(), pos_integer(), string(), string(), list(term())) ->
	iolist().
variable(FileName, AtLine, VarName, NewVarName, Opt) ->
	{ok, Form} = epp_dodger:parse_file(FileName, [nofail]),
	Comment = erl_comment_scan:file(FileName),
	TopTree = erl_recomment:recomment_forms(Form, Comment),
	TreeList = erl_syntax:form_list_elements(TopTree),
	FoundForm = tree:find_var(TreeList, AtLine, VarName),

	NewVar = erl_syntax:variable(NewVarName),
	NewForm = tree:update(FoundForm, variable,
		fun(Var) ->
			case erl_syntax:variable_literal(Var)
				of VarName ->
					NewVar
				; _ ->
					Var
			end
		end),

	NewTreeList = tree:replace(TopTree, FoundForm, NewForm),
	kawaiiprint:syntax_tree(NewTreeList, Opt).
