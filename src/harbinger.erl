-module(harbinger).

%% Public API
-export([
	subscribe/1,
	subscribe/2,
	unsubscribe/1,
	unsubscribe/0,
	send/2,
	start/0,
	stop/0
]).

-export_type([chan_id/0, chan_filter/0]).

%% Internal API
-export([always_true/2, send_local/2, try_send_hipri/2]).

-include("../include/harbinger.hrl").

%% ===================================================================
%% API
%% ===================================================================

-type chan_id() :: term().
-type chan_filter() :: fun((chan_id(), term()) -> boolean()).
-type chanreg() :: {chan, chan_id()}.

start() ->
	application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

-spec subscribe(chan_id()) -> ok.
subscribe(Chan) ->
	subscribe(Chan, fun ?MODULE:always_true/2).

-spec subscribe(chan_id(), chan_filter()) -> ok.
subscribe(Chan, FilterFun) ->
	harbinger_reg_srv:checkin(chan(Chan), FilterFun).

-spec unsubscribe(chan_id()) -> ok.
unsubscribe(Chan) -> 
	harbinger_reg_srv:leave(chan(Chan)).

-spec unsubscribe() -> ok.
unsubscribe() -> 
	harbinger_reg_srv:leave().

-spec send(chan_id(), term()) -> true.
send(Chan, Msg) -> 
	harbinger_external:resend(Chan, Msg),
	send_local(Chan, Msg).

%%% @doc
%%% Client calls harbinger:send(foo, {bar, [1,2,3]}).
%%% Msg goes to the local broker
%%% which in turn looks up for local subscribers and send Msg to them.
%%% Then broker looks up for a other subscribed nodes and notifies known peers
%%%
%%% Gosssip for PEX.
%%% Lamport stamp for delivery guarantee to fresh peers (Provide better desc.)


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec chan(chan_id()) -> chanreg().
chan(Name) -> {chan, Name}.

chan_msg(Chan, Msg) -> ?NOTIFICATION(Chan, Msg).

try_send_hipri(Chan, Msg) -> 
	try harbinger_hipri:resend(Chan, Msg)
	catch
		_:_ -> send_local(Chan, Msg)
	end.

send_local(Chan, Msg) -> 
    R = subscribers_for_ch(Chan),
	M = chan_msg(Chan, Msg),
	T = [P || {_K,F,P} <- R, apply_check_f(F, Chan, Msg)],
	lists:foreach(fun(P) -> erlang:send(P, M) end, T),
	true.

subscribers_for_ch(Chan) ->
	harbinger_reg_srv:q(chan(Chan)).

always_true(_,_) -> true.

apply_check_f(F, K, M) ->
	try F(K, M) of
		true -> true;
		_    -> false
	catch
		_:_ -> false
	end.

