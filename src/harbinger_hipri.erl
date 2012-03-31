-module(harbinger_hipri).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([
	start_link/1,
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
		max_ql
	}).

-include("../include/harbinger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(atom()) -> {ok, pid()} | ignore | {error, Why::term()}.
start_link(Name) ->
	gen_server:start_link({local, Name}, ?MODULE, [], []).

resend(Chan, Msg) ->
	HPs = harbinger_reg_srv:q(signature()),
	{_K,_I,P} = lists:nth(random:uniform(length(HPs)), HPs),
	gen_server:cast(P, ?RESEND(Chan, Msg)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Opt) ->
	erlang:process_flag(priority, high),
	harbinger_reg_srv:checkin(signature()),
	Env  = application:get_all_env(),
	MaxL = proplists:get_value(max_hipri_queue_len, Env, ?MAX_HIPRI_QL),
	{ok, #state{
			max_ql = MaxL
		}}.

handle_call(_Msg, _From, S) ->
	{noreply, S}.

handle_cast(?RESEND(Chan, Msg), #state{ max_ql=MaxL } = S) ->
	case erlang:process_info(self(), message_queue_len) of
		{_,MQL} when MQL > MaxL -> 
			harbinger_reg_srv:leave();
		{_,0} -> 
			harbinger_reg_srv:checkin(signature());
		_ -> ok
	end,
	catch harbinger:send_local(Chan, Msg),
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

signature() -> hipri_worker.

