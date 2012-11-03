%% @author Slava Yurin <YurinVV@ya.ru>
%% @doc Chain manager
-module(chainer).

%% External API
-export([create/1]).
-export([in/2, out/1]).

%% Internal API
-export([send/2, send/3]).
-export([get/0]).

%% Internal function
-export([monitor/2, starter/3]).

-type chain_pid() :: {'fun', fun(([term()]) -> no_return())}.
-type chain_option() :: [term()].
-type chain() :: {chain, [{chain_pid(), chain_option()} | chain()]}.

%% @doc Create new chain
-spec create(chain(), [term()]) -> {pid(), term()}.
create(Chain) ->
	CountOut = count_out(Chain),
	{ok, NoopPid} = proc_lib:start(chainer, monitor, [CountOut, self()]),
	ChainOut = erlang:monitor(process, NoopPid),
	ChainIn = create(Chain, [NoopPid]),
	{ChainIn, ChainOut}.

%% @doc Calculates count of exits from chain
count_out({in, InList}) ->
	lists:sum([count_out(In) || In <- InList]);
count_out({chain, List}) ->
	count_out(lists:last(List));
count_out(_) ->
	1.

%% @doc Monitor for exit from chain
%%
%% This is last process in chain, that added by chainer. It use for
%% monitor it. He link with process that generate output of chain. It simple
%% pass message to process that start chain.
%%
%% If in chain happen collapse, then this process catching exit signal and send
%% error to process that created chain
-spec monitor(integer(), pid()) -> no_return().
monitor(CountOut, CallerPid) ->
	process_flag(trap_exit, true),
	proc_lib:init_ack(CallerPid, {ok, self()}),
	receive_loop(CallerPid, CountOut, 1).

%% @doc Loop for pass message
receive_loop(CallerPid, CountOut, Acc) ->
	receive
		{'$chain_message', Message} ->
			CallerPid ! {'$chain_message', Message},
			receive_loop(CallerPid, CountOut, Acc)
		; {'EXIT', _, Reason} when Reason /= normal ->
			exit(Reason)
		; {'EXIT', _, normal} when Acc < CountOut ->
			receive_loop(CallerPid, CountOut, Acc + 1)
		; {'EXIT', _, normal} ->
			exit(normal)
	end.

%% @doc Create chain element
%%
%% Start all need process and set it next chain pids to `NextPid'.
create_chain([], NextPid) ->
	lists:flatten(NextPid);
create_chain([ChainElement | Tail], NextPid) when is_tuple(ChainElement) ->
	NewNextPid = create(ChainElement, NextPid),
	create_chain(Tail, NewNextPid).

%% @doc Start chain element
%%
%% If chain element is simple proces, then start it with confirmation of
%% run.
create({{'fun', Fun}, Opt}, NextPid) ->
	[proc_lib:start(chainer, starter, [self(), {Fun, Opt}, NextPid])];
create({in, InList}, NextPid) ->
	lists:flatten([create(In, NextPid) || In <- InList]);
create({chain, Chain}, NextPid) ->
	create_chain(lists:reverse(Chain), NextPid).

%% @doc Start with feedback to parent
starter(Parent, {Fun, Opt}, NextPid) ->
	lists:foreach(fun erlang:link/1, NextPid),
	proc_lib:init_ack(Parent, self()),
	Fun([{next, NextPid} | Opt]).

%% @doc Send message to next pids in chain
-spec send(term(), [term()]) -> ok.
send(Message, Options) ->
	send(Message, next, Options).

%% @doc Send message to pids in chain
-spec send(term(), atom(), [term()]) -> ok.
send(Message, ChainLink, Options) ->
	Pids = proplists:append_values(ChainLink, Options),
	[Pid ! {'$chain_message', Message} ||
		Pid <- Pids, is_pid(Pid) or is_port(Pid) or is_atom(Pid)],
	ok.

%% @doc Receive message from chain
%%
%% Using for receive message from one chain element in other chain element.
-spec get() -> term().
get() ->
	receive
		{'$chain_message', Message} ->
			Message
	end.

%% @doc Send message to input elements in chain
-spec in(term(), [pid()]) -> ok.
in(Message, Pids) ->
	SMessage = {'$chain_message', Message},
	lists:foreach(fun(Pid) -> Pid ! SMessage end, Pids).

%% @doc Receive message from chain
%%
%% Using for receive result or error message from all chain. Result is message
%% sended last chain element.
-spec out(reference()) -> ok | {error, term()} | term().
out(ChainID) ->
	receive
		{'$chain_message', Message} ->
			Message
		; {'DOWN', ChainID, process, _, Reason} when Reason /= normal ->
			{error, Reason}
		; {'DOWN', ChainID, process, _, normal} ->
			chain_end
	end.
