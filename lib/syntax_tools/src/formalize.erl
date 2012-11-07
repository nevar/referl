%% @author Slava Yurin <YurinV@ya.ru>
%% @doc Tokens form to formal tuple
-module(formalize).

-export([tokens2syntax/1]).

%% @doc Tokens -> formal syntax
-spec tokens2syntax([term()]) -> no_return().
tokens2syntax(Opt) ->
	case chainer:get()
		of {tokens_form, TokenList} ->
			{TokensForm, _Comments} = filter_tokens(TokenList, [], []),
			%% Okay. We get tokens for form. Lats start out great parser
			case extend_parser:parse_form(TokensForm)
				of {ok, Form} ->
					chainer:send({form, Form}, next, Opt),
					tokens2syntax(Opt)
				; {error, Error} ->
					exit(Error)
			end
		; eof ->
			chainer:send(eof, next, Opt),
			ok
	end.

filter_tokens([], Tokens, Comments) ->
	{lists:reverse(Tokens), lists:reverse(Comments)};
filter_tokens([Token | Tail], Tokens, Comments) ->
	case element(1, Token)
		of comment ->
			filter_tokens(Tail, Tokens, [Token | Comments])
		; _ ->
			filter_tokens(Tail, [Token | Tokens], Comments)
	end.
