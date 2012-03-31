-module(harbinger_hipri_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("../include/harbinger.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

hipri_worker_spec(K) ->
	I = harbinger_hipri,
	N = list_to_atom(atom_to_list(I) ++ integer_to_list(K)),
	{N, {I, start_link, [N]}, transient, 5000, worker, [I]}.

init([]) ->
	Env = application:get_all_env(),
	Cnt = proplists:get_value(max_hipri_workers, Env, ?MAX_HIPRI_WRKS),
	Wrk = lists:map(fun hipri_worker_spec/1, lists:seq(1, Cnt)),
    {ok, { {one_for_one, 5, 10}, Wrk} }.

