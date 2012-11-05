%% @author Slava Yurin <YurinVV@ya.ru>
%% @doc Module for parse erlang source file
%%
%% Parse source file or string with source file and give tokens of form to
%% {@link, formalize}.
-module(tokenizer).

-export([source2token/1]).
-export([format_error/1]).

%% @doc Start filter for read and tokenize erlang code
-spec source2token([term()]) -> no_return().
source2token(Opt) ->
	case chainer:get()
		of {file, FileName} ->
			read_file(FileName, Opt)
		; {string, String} ->
			read_string(String, Opt)
		; Unknown ->
			exit({?MODULE, {unknown, Unknown}})
	end.

read_file(FileName, Opt) ->
	Size = proplists:get_value(block_size, Opt, 4096),
	case file:open(FileName, [raw, read, {read_ahead, Size}])
		of {ok, FD} ->
			read_and_tokenize(FD, [], Opt, 1),
			file:close(FD)
		; {error, Reason} ->
			exit({file, Reason})
	end.

read_and_tokenize(FD, LastTokens, Opt, Line) ->
	case file:read_line(FD)
		of {ok, Data} ->
			case erl_scan:string(Data, Line, [return_comments])
				of {ok, Tokens, _} ->
					{TokenLeft, TokenFormList} =
						split_by_dot(Tokens, LastTokens, []),
					parse_token(TokenFormList, Opt),
					read_and_tokenize(FD, TokenLeft, Opt, Line + 1)
				; {error, Error, _} ->
					exit({erl_scan, Error})
			end
		; eof ->
			chainer:send(eof, next, Opt),
			ok
		; {error, Reason} ->
			exit({file, Reason})
	end.

split_by_dot([], Acc, TokensList) ->
	{Acc, lists:reverse(TokensList)};
split_by_dot([{dot, _} = Dot | Tail], Acc, TokensList) ->
	split_by_dot(Tail, [], [lists:reverse([Dot | Acc]) | TokensList]);
split_by_dot([Token | Tail], Acc, TokensList) ->
	split_by_dot(Tail, [Token | Acc], TokensList).

parse_token(FormList, Opt) ->
	%% TODO: flow controll
	lists:foreach(fun(Form) ->
			chainer:send({tokens_form, Form}, next, Opt)
		end, FormList).

read_string(String, Opt) ->
	case erl_scan:string(String, 1, [return_comments])
		of {ok, Tokens, _} ->
			{Last, TokenFormList} = split_by_dot(Tokens, [], []),
			parse_token(TokenFormList, Opt),
			Last /= [] andalso
				parse_token([lists:reverse(Last)], Opt),
			chainer:send(eof, next, Opt),
			ok
		; {error, Error, _} ->
			exit({erl_scan, Error})
	end.

%% @doc Format error message
-spec format_error(term()) -> string().
format_error({unknown, Message}) ->
	lists:flaten(io_lib:format("unknown source format ~p", [Message])).
