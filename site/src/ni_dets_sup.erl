-module(ni_dets_sup).
-behaviour(supervisor).
-export([start/0,
         start_link/0,
         init/1
        ]).

start() ->
    {ok, Pid} = start_link(),
    unlink(Pid),
    {ok, Pid}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
    },
    Child = #{
        id => ni_dets,
        start => {ni_dets, start_link, []},
        shutdown => 2000,
        modules => [ni_dets]
    },
    {ok, {SupFlags, [Child]}}.
