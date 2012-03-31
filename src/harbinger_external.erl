-module(harbinger_external).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([
	start_link/0,
	resend/2
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
}).

-include("../include/harbinger.hrl").
-define(CHAN_BCASTER(Chan), {n, l, {?MODULE, chan, Chan}}).

%%%===================================================================
%%% API
%%%===================================================================

resend(Chan, Msg) ->
	gen_server:abcast(nodes(connected), ?SERVER, ?RESEND(Chan, Msg)).

-spec start_link() -> {ok, pid()} | ignore | {error, Why::term()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	erlang:process_flag(priority, high),
	%folsom_metrics:new_histogram(g_msg_latency),
	%folsom_metrics:new_histogram(g_queue_len),
	{ok, #state{}}.

handle_call(_Msg, _From, S) ->
	{noreply, S}.

handle_cast(?RESEND(Chan, Msg), S) ->
	%{message_queue_len, L} = erlang:process_info(self(),message_queue_len),
	%folsom_metrics:notify(g_queue_len, L),
	%case Msg of
		%{ping,_,T} ->
			%D = timer:now_diff(erlang:now(), T) div 1000,
			%folsom_metrics:notify(g_msg_latency, D);
		%_ -> error_logger:info_msg("Msg:~p", [Msg])
	%end,
	catch harbinger:try_send_hipri(Chan, Msg),
	{noreply, S};

handle_cast(_Msg, S) ->
	{noreply, S}.

handle_info(_Msg, S) ->
	{noreply, S}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
