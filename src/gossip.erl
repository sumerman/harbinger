-module(gossip).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([
	start_link/0
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

%%%===================================================================
%%% API
%%%===================================================================

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

handle_cast(_Msg, S) ->
	{noreply, S}.

handle_info(_Msg, S) ->
	{noreply, S}.


terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
