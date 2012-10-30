%% @author Slava Yurin <YurinV@ya.ru>
%% @doc Tokens form to formal tuple
-module(formalize).

-export([tokens2syntax/1]).

%% @doc Tokens -> formal syntax
-spec tokens2syntax([term()]) -> no_return().
tokens2syntax(Opt) ->
	receive {tokens_form, TokenList} ->
		{TokensForm, Comments} = filter_tokens(TokenList, [], []),
		%% Okay. We get tokens for form. Lats start out great parser
		case extend_parser:parse_form(TokensForm)
			of {ok, Form} ->
				chain:send({form, Form, Comments}, next, Opt),
				tokens2syntax(Opt)
			; {error, Error} ->
				exit({extend_parser, Error})
		end
	; eof ->
		chain:send(eof, next, Opt),
		ok
	; _Unknown ->
		tokens2syntax(Opt)
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
