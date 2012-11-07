%% @author Slava Yurin <YurinVV@ya.ru>
%% @doc Kawaii printer for erlang syntax tree
-module(printer).

-define(PAPER, 80).
-define(RIBBON, 75).

%% External API
-export([file/2]).

%% Chain element
-export([document2string/1]).

%% @doc document -> string
-spec document2string([term()]) -> no_return().
document2string(Opt) ->
	case chainer:get()
		of {document, Doc} ->
			W = proplists:get_value(paper, Opt, ?PAPER),
			L = proplists:get_value(ribbon, Opt, ?RIBBON),
			String = prettypr:format(Doc, W, L),
			ok = chainer:send({string, String}, next, Opt),
			document2string(Opt)
		; eof ->
			ok = chainer:send(eof, next, Opt)
	end.

%% @doc String to file
-spec string2file([term()]) -> no_return().
string2file(Opt) ->
	case proplists:get_value(out, Opt)
		of true ->
			write_loop(standard_io)
		; undefined ->
			collect_strings([], Opt)
		; FileName ->
			{ok, FD} = file:open(FileName, [raw, write]),
			write_loop(FD)
	end.

collect_strings(Acc, Opt) ->
	case chainer:get()
		of {string, String} ->
			collect_strings(Opt, [String | Acc])
		; eof ->
			String = lists:flatten(lists:reverse(Acc)),
			chainer:send({result, String}, Opt),
			ok
		; Error ->
			Error
	end.

write_loop(FD) ->
	case chainer:get()
		of {string, String} ->
			file:write(FD, String),
			write_loop(FD)
		; eof ->
			file:close(FD)
	end.

%% @doc Pretty print file
-spec file(file:filename(), [term()]) -> ok | {error, term()}.
file(FileName, Opt) ->
	{In, Out} = chainer:create(chainer:chain([
		chainer:element(fun tokenizer:source2token/1, Opt)
		, chainer:element(fun formalize:tokens2syntax/1, Opt)
		, chainer:element(fun documenter:syntax2document/1, Opt)
		, chainer:element(fun document2string/1, Opt)
		, chainer:element(fun string2file/1, Opt)
		])),
	chainer:in({file, FileName}, In),
	case chainer:out(Out)
		of {result, String} ->
			chainer:in(stop, In),
			chainer:out(Out),
			String
		; chain_end ->
			ok
		; Error ->
			print_or_return(FileName, Error, Opt)
	end.

print_or_return(FileName, Error, Opt) ->
	case proplists:get_bool(print_error, Opt)
		of true ->
			case Error
				of {error, {_, file, Reason}} ->
					io:format(standard_error, "~s: ~s~n",
						[FileName, file:format_error(Reason)])
				; {error, {Line, Module, Reason}} ->
					io:format(standard_error, "~s:~w ~s~n",
						[FileName, Line, Module:format_error(Reason)])
				; {error, Reason} ->
					io:format(standard_error, "~s: ~p~n", [FileName, Reason])
			end,
			error
		; false ->
			Error
	end.
