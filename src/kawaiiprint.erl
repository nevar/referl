%% @author Slava Yurin <YurinVV@ya.ru>
%% @doc Kawaii printer for erlang syntax tree
-module(kawaiiprint).

-export([syntax_tree/2, file/2]).

-define(PAPER, 80).
-define(RIBBON, 75).

%% @doc Kawaii printer
-spec syntax_tree(erl_syntax:syntaxTree(), term()) -> iolist().
syntax_tree(SyntaxTree, Options) ->
	Document = document:make_document(SyntaxTree, Options),

    W = proplists:get_value(paper, Options, ?PAPER),
    L = proplists:get_value(ribbon, Options, ?RIBBON),

    prettypr:format(Document, W, L).

%% @doc Kawaii printer
-spec file(file:filename(), list(tuple() | atom())) -> iolist().
file(FileName, Options) ->
	{ok, Form} = epp_dodger:parse_file(FileName, [nofail]),
	%% TODO: if exists error fail
	Comment = erl_comment_scan:file(FileName),
	SyntaxTree = erl_recomment:recomment_forms(Form, Comment),

	syntax_tree(SyntaxTree, Options).
