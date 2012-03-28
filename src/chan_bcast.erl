-module(chan_bcast).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([
	start_link/1
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
	id
}).

-include("../include/harbinger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(term()) -> {ok, pid()} | ignore | {error, Why::term()}.
start_link(Chan) ->
	gen_server:start_link(?MODULE, [{chan_id, Chan}], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opt) ->
	Chan = proplists:get_value(chan_id, Opt),
	chan_bcast_master:register_chan_bcast(Chan),
	{ok, #state{
			id = Chan
		}}.

handle_call(_Msg, _From, S) ->
	{noreply, S}.

handle_cast(?RESEND(Chan, Msg), #state{ id=Chan } = S) ->
	harbinger:send_local(Chan, Msg),
	{noreply, S};

handle_cast(Msg, S) ->
	error_logger:error_msg("Unexpected ~p", [Msg]),
	{noreply, S}.

handle_info(_Msg, S) ->
	{noreply, S}.


terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
