-module(rtpplay_sup).

-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_) ->
    {ok, { {one_for_one, 5, 10}, []} }.

