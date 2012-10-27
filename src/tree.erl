%% @author Slava Yurin <YurinVV@ya.ru>
%% @doc Syntax tree travel
-module(tree).

-export([find/2, find_var/3, update/3, replace/3]).

-type syntax() :: erl_syntax:syntaxTree().

%% @doc Find form for specified line
%%
%% For function return only clause that has line. If in function exists other
%% fun declaration, that return clause of nested function.
-spec find([syntax()], pos_integer()) -> syntax().
find(ElementList, AtLine) ->
	find_nearest(ElementList, AtLine, none).

find_nearest([], _, Last) ->
	Last;
find_nearest([Element | Tail], AtLine, Last) ->
	case erl_syntax:get_pos(Element)
		of Line when is_integer(Line), Line =< AtLine ->
			find_nearest(Tail, AtLine, Element)
		; Line when is_integer(Line) ->
			Last
		; {Line, _} when is_integer(Line), Line =< AtLine ->
			find_nearest(Tail, AtLine, Element)
		; {Line, _} when is_integer(Line) ->
			Last
		; Pos ->
			throw({unknown_position, Pos})
	end.

%% @doc Find form in wich we must rename variable
-spec find_var([syntax()], pos_integer(), atom()) ->
	syntax().
find_var(ElementList, AtLine, VarName) ->
	FoundForm = find(ElementList, AtLine),
	case erl_syntax:type(FoundForm)
		of function ->
			% may be we want rename in defined function that has shadow variable
			Clauses = erl_syntax:function_clauses(FoundForm),
			DefaultClause = find_nearest(Clauses, AtLine, none),
			ClauseList = erl_syntax:clause_body(DefaultClause),
			find_clause(ClauseList, AtLine, VarName, DefaultClause)
		; _ ->
			FoundForm
	end.


find_clause([], _, _, Default) ->
	Default;
find_clause([Element | Tail], AtLine, VarName, Default) ->
	case erl_syntax:type(Element)
		of fun_expr ->
			Clauses = erl_syntax:fun_expr_clauses(Element),
			find_clause(Clauses, AtLine, VarName, Default)
		; clause ->
			IsShadow = lists:any(fun(FunVar) ->
					Type = erl_syntax:type(FunVar),
					if Type == variable ->
						VarName == erl_syntax:variable_literal(FunVar)
					; true ->
						false
					end
				end, erl_syntax:clause_patterns(Element)),
			LastFound = if IsShadow ->
				Element
			; true ->
				Default
			end,
			Clauses = erl_syntax:clause_body(Element),
			case find_clause(Clauses, AtLine, VarName, LastFound)
				of LastFound ->
					find_clause(Tail, AtLine, VarName, LastFound)
				; Found ->
					Found
			end
		; _ ->
			find_clause(Tail, AtLine, VarName, Default)
	end.

%% @doc Replace elements of given type
-spec update(syntax(), atom(), fun((syntax()) -> syntax())) -> syntax().
update(Node, Type, ReplaceFun) ->
    case erl_syntax:subtrees(Node)
		of [] ->
			case erl_syntax:type(Node)
				of Type ->
					ReplaceFun(Node)
				; _ ->
					Node
			end
		; NodeTree ->
			NewSubTree = [
				[update(Element, Type, ReplaceFun) || Element <- SubTree] ||
				SubTree <- NodeTree],
			NewNode = erl_syntax:make_tree(erl_syntax:type(Node), NewSubTree),
			erl_syntax:copy_pos(Node, NewNode)
    end.

%% @doc Replace subtree of tree
-spec replace(syntax(), syntax(), syntax()) -> syntax().
replace(Tree, ThatReplace, ToReplace) ->
    case erl_syntax:subtrees(Tree)
		of [] ->
			Tree
		; NodeTree ->
			NewSubTree = [
				[replace_element(Element, ThatReplace, ToReplace) || Element <- SubTree] ||
				SubTree <- NodeTree],
			NewNode = erl_syntax:make_tree(erl_syntax:type(Tree), NewSubTree),
			erl_syntax:copy_pos(Tree, NewNode)
	end.

replace_element(Element, Element, NewElement) ->
	NewElement;
replace_element(Element, Replace, NewElement) ->
	replace(Element, Replace, NewElement).
