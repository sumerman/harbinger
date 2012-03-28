-module(chan_bcast_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_chan_bcast/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILDW(I, Opts), {I, {I, start_link, Opts}, temporary, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_chan_bcast(Chan) ->
	supervisor:start_child(?MODULE, [Chan]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
				?CHILDW(chan_bcast, [])
			]} }.

