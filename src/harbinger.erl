-module(harbinger).

%% API
-export([
	subscribe/1,
	subscribe/2,
	unsubscribe/1,
	unsubscribe/0,
	subscriptions/1,
	subscriptions/0,
	send/2,
	send_local/2,
	start/0,
	stop/0
]).

-export_type([chan_id/0, chan_filter/0]).

%% Helpers
-export([always_true/1]).

-include("../include/harbinger.hrl").

%% ===================================================================
%% API
%% ===================================================================

-type chan_id() :: term().
-type chan_filter() :: fun((term()) -> boolean()).
-type chanreg() :: {p, l, {chan, chan_id()}}.

-spec chan(chan_id()) -> chanreg().
chan(Name) -> {p, l, {chan, Name}}.

chan_msg(Chan, Msg) -> ?NOTIFICATION(Chan, Msg).

start() ->
	application:start(gproc),
	application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

-spec subscribe(chan_id()) -> true.
subscribe(Chan) ->
	subscribe(Chan, fun ?MODULE:always_true/1).

-spec subscribe(chan_id(), chan_filter()) -> true.
subscribe(Chan, FilterFun) ->
	chan_bcast_master:start_chan_bcast(Chan),
	gproc:reg(chan(Chan), FilterFun).

-spec unsubscribe(chan_id()) -> true.
unsubscribe(Chan) -> 
	gproc:unreg(chan(Chan)).

-spec unsubscribe() -> true.
unsubscribe() -> 
	[unsubscribe(C) || C <- subscriptions()],
	true.

-spec subscriptions() -> [chan_id()].
subscriptions() -> 
	subscriptions(self()).

-spec subscriptions(pid()) -> [chan_id()].
subscriptions(Pid) ->
	gproc:select({l, p}, [{
				{{p, l, {chan, '$1'}}, Pid, '_'}, 
				[], 
				['$1']}]).

-spec send(chan_id(), term()) -> true.
send(Chan, Msg) -> 
	chan_bcast_master:resend(Chan, Msg),
	send_local(Chan, Msg).

send_local(Chan, Msg) -> 
	K = chan(Chan),
	M = chan_msg(Chan, Msg),
	T = gproc:lookup_values(K),
	%R = [P || {P, F} <- T, apply_check_f(F, Msg)],
	lists:foreach(fun({P, F}) -> 
				S = apply_check_f(F, Msg),
				S andalso (P ! M)
		end, T),
	true.

always_true(_) -> true.

apply_check_f(F, M) ->
	try F(M) of
		true -> true;
		_    -> false
	catch
		_:_ -> false
	end.


%%% @doc
%%% Client calls harbinger:send(foo, {bar, [1,2,3]}).
%%% Msg goes to the local broker
%%% which in turn looks up for local subscribers and send Msg to them.
%%% Then broker looks up for a other subscribed nodes and notifies known peers
%%%
%%% Gosssip for PEX.
%%% Lamport stamp for delivery guarantee to fresh peers (Provide better desc.)

