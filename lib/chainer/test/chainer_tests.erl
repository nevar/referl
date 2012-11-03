-module(chainer_tests).

-include_lib("test_helper/include/eunit.hrl").

-define(element(Char), {{'fun', fun letter/1}, [{char, Char}]}).
-define(element_fial(), {{'fun', fun letter_fail/1}, []}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Help-full function %%%%%%%%%%%%%%%%%%%%%%%%%%%
letter(Opt) ->
	Char = proplists:get_value(char, Opt),
	case chainer:get()
		of eof ->
			chainer:send(eof, Opt),
			ok
		; String ->
			chainer:send([Char | String], Opt),
			letter(Opt)
	end.

letter_fail(Opt) ->
	case chainer:get()
		of _ ->
			exit(test_exit)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Moc      function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Setup/cleanup function %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test suite    function %%%%%%%%%%%%%%%%%%%%%%%
main_test_() ->
	{inparallel, [
		?EUNIT_TEST(fun test_simple_chain/0)
		, ?EUNIT_TEST(fun test_many_in/0)
		, ?EUNIT_TEST(fun test_many_out/0)
		, ?EUNIT_TEST(fun test_crazy_chain/0)
		, ?EUNIT_TEST(fun test_all_to_all/0)
		, ?EUNIT_TEST(fun test_fail_chain/0)
		]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test     function %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% (A) - (B) - (C) ->
%%
test_simple_chain() ->
	Chain = {chain, [
		?element($A)
		, ?element($B)
		, ?element($C)
		]},
	{In, Out} = chainer:create(Chain),
	chainer:in("", In),
	?assertEqual("CBA", chainer:out(Out)),
	chainer:in(eof, In),
	?assertEqual(eof, chainer:out(Out)),
	?assertEqual(chain_end, chainer:out(Out)).

%%
%% (A)
%%    \
%%     (B) - (C) ->
%%    /
%% (X)
%%
test_many_in() ->
	Chain = {chain, [
		{in, [?element($A), ?element($X)]}
		, ?element($B)
		, ?element($C)
		]},
	{In, Out} = chainer:create(Chain),
	[A, _] = In,
	erlang:suspend_process(A),
	chainer:in("", In),
	?assertEqual("CBX", chainer:out(Out)),
	erlang:resume_process(A),
	?assertEqual("CBA", chainer:out(Out)),
	chainer:in(eof, In),
	?assertEqual(eof, chainer:out(Out)),
	?assertEqual(chain_end, chainer:out(Out)).

%%
%%           (A) ->
%%          /
%% (B) - (C)
%%          \
%%           (X) ->
%%
test_many_out() ->
	Chain = {chain, [
		?element($B)
		, ?element($C)
		, {in, [?element($A), ?element($X)]}
		]},
	{In, Out} = chainer:create(Chain),
	chainer:in("", In),
	S1 = chainer:out(Out),
	S2 = chainer:out(Out),
	?assert("ACB" == S1 orelse "XCB" == S1),
	?assert("ACB" == S2 orelse "XCB" == S2),
	chainer:in(eof, In),
	?assertEqual(eof, chainer:out(Out)),
	?assertEqual(eof, chainer:out(Out)),
	?assertEqual(chain_end, chainer:out(Out)).
%%
%%             (a) - (b) ->
%%            /
%%          (1)
%%          / \
%% (-) - (0)   (A) - (B) ->
%%          \
%%           (2) ->
%%
test_crazy_chain() ->
	SubSubChain1 = {chain, [?element($a), ?element($b)]},
	SubSubChain2 = {chain, [?element($A), ?element($B)]},
	Sub2Chain = {in, [SubSubChain1, SubSubChain2]},
	Chain1 = {chain, [?element($1), Sub2Chain]},
	Chain2 = ?element($2),
	Chain = {in, [Chain1, Chain2]},
	MainChain = {chain, [?element($-), ?element($0), Chain]},
	{In, Out} = chainer:create(MainChain),
	chainer:in("", In),
	S1 = chainer:out(Out),
	S2 = chainer:out(Out),
	S3 = chainer:out(Out),
	?assert("BA10-" == S1 orelse "ba10-" == S1 orelse "20-" == S1),
	?assert("BA10-" == S2 orelse "ba10-" == S2 orelse "20-" == S2),
	?assert("BA10-" == S3 orelse "ba10-" == S3 orelse "20-" == S3),
	chainer:in(eof, In),
	[?assertEqual(eof, chainer:out(Out)) || _I <- lists:seq(1, 3)],
	?assertEqual(chain_end, chainer:out(Out)).

%%
%% (1) - - -
%%    \      \
%%     \   - -(A) ->
%%      \ /  /
%% (2) - - -
%%      / \  \
%%     /   - -(B) ->
%%    /      /
%% (3) - - -
%%
test_all_to_all() ->
	Layer1 = {in, [?element($1), ?element($2), ?element($3)]},
	Layer2 = {in, [?element($A), ?element($B)]},
	Chain = {chain, [Layer1, Layer2]},
	{In, Out} = chainer:create(Chain),
	chainer:in("", In),
	Result = [chainer:out(Out) || _I <- lists:seq(1, 6)],
	Expect = [[X, Y] || X <- [$A, $B], Y <- [$1, $2, $3]],
	?assertEqual(lists:sort(Expect), lists:sort(Result)),

	chainer:in(eof, In),
	[?assertEqual(eof, chainer:out(Out)) || _I <- lists:seq(1, 2)],
	?assertEqual(chain_end, chainer:out(Out)).

%%
%% (A) - >B< - (C) ->
%%
test_fail_chain() ->
	Chain = {chain, [
		?element($A)
		, ?element_fial()
		, ?element($C)
		]},
	{In, Out} = chainer:create(Chain),
	[In1] = In,
	chainer:in("", In),
	?assertEqual({error, test_exit}, chainer:out(Out)),
	?assertNot(is_process_alive(In1)).
