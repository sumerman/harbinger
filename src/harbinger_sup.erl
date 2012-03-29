-module(harbinger_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILDW(I, Opts), {I, {I, start_link, Opts}, transient, 5000, worker, [I]}).
-define(CHILDS(I, Opts), {I, {I, start_link, Opts}, permanent, infinity, supervisor, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {rest_for_one, 5, 10}, [
				?CHILDS(chan_bcast_sup, []),
				?CHILDW(harbinger_reg_srv, []),
				?CHILDW(chan_bcast_master, [])
				]} }.

