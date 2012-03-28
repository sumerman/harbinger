-module(chan_bcast_master).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([
	start_link/0,
	resend/2,
	register_chan_bcast/1,
	start_chan_bcast/1
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

-spec start_chan_bcast(harbinger:chan_id()) -> {ok, pid()}.
start_chan_bcast(Chan) ->
	try {ok, gproc:lookup_pid(?CHAN_BCASTER(Chan))}
	catch
		error:badarg -> 
			chan_bcast_sup:start_chan_bcast(Chan)
	end.

register_chan_bcast(Chan) -> 
	gproc:reg(?CHAN_BCASTER(Chan)).

-spec start_link() -> {ok, pid()} | ignore | {error, Why::term()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	{ok, #state{}}.

handle_call(_Msg, _From, S) ->
	{noreply, S}.

handle_cast(?RESEND(Chan, _) = M, S) ->
	{ok, P} = start_chan_bcast(Chan),
	gen_server:cast(P, M),	
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
