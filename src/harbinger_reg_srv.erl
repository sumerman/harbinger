-module(harbinger_reg_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(TAB, ?SERVER).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	start_link/0,
	subscribe/2,
	unsubscribe/1,
	unsubscribe/0,
	subscribers/1
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

subscribe(K, Info) ->
	gen_server:call(?SERVER, {subscribe, self(), K, Info}).

unsubscribe(K) ->
	gen_server:call(?SERVER, {unsubscribe, self(), K}).

unsubscribe() ->
	gen_server:call(?SERVER, {unsubscribe, self()}).

subscribers(K) ->
	ets:lookup(?TAB, K).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {
		reg_tab,
		ref_tab
	}).

init(_Args) ->
	TabOpt = [named_table, bag, 
			protected, {read_concurrency, true}],
	Reg = ets:new(?TAB, TabOpt),
	Ref = ets:new(ref_srv_mref, []),
	{ok, #state{
			reg_tab = Reg,
			ref_tab = Ref
		}}.

handle_call({subscribe, Pid, K, I}, _From, State) ->
	add_subscribtion(State, Pid, K, I),
    {reply, ok, State};

handle_call({unsubscribe, Pid, K}, _From, State) ->
	remove_subscription(State, Pid, K),
    {reply, ok, State};

handle_call({unsubscribe, Pid}, _From, State) ->
	remove_all_subscriptions(State, Pid),
    {reply, ok, State};

handle_call(Request, _From, State) ->
	error_logger:error_msg("[~p] Unexpected call ~p", [?MODULE, Request]),
    {noreply, ok, State}.

handle_cast(Request, State) ->
	error_logger:error_msg("[~p] Unexpected cast ~p", [?MODULE, Request]),
    {noreply, State}.

handle_info({'DOWN', _MRef, process, Pid, _}, State) ->
	remove_all_subscriptions(State, Pid),
    {noreply, State};

handle_info(Info, State) ->
	error_logger:error_msg("[~p] Unexpected message ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

add_subscribtion(St, P, K, I) when is_pid(P) ->
	ets:insert(?TAB, {K, I, P}),
	case ets:lookup(St#state.ref_tab, P) of
		[] ->
			Ref = monitor(process, P),
			ets:insert(St#state.ref_tab, {P, Ref}),
			{ok, new};
		_  -> 
			{ok, already_reg}
	end.

remove_subscription(_St, P, K) when is_pid(P) ->
	ets:select_delete(?TAB, remove_ms(P, K)).

remove_all_subscriptions(St, P) when is_pid(P) ->
	[demonitor(Ref) || {_P, Ref} 
			<- ets:lookup(St#state.ref_tab, P)],
	ets:select_delete(?TAB, remove_all_ms(P)).

remove_ms(P, K) ->
	[{{'$1', '_', '$2'}, [{'==', '$1', K}, {'==', '$2', P}], [true]}].
remove_all_ms(P) ->
	[{{'$1', '_', '$2'}, [{'==', '$2', P}], [true]}].



