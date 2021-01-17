%%%-------------------------------------------------------------------
%% @doc kibom top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(kibom_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ElliOpts = [{callback, http_cb}, {port, 8021}],
    ElliSpec = #{
        id => elli_http,
        start => {elli, start_link, [ElliOpts]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [elli]
    },
    {ok, { {one_for_one, 5, 10}, [ElliSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================
