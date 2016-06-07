-module(quorum).
-behaviour(gen_statem).

-export([start/2, stop/1]).
-export([init/1, handle_event/4, code_change/4, terminate/3]).

%% RAFT FSM states
-export([follower/3, candidate/3, leader/3]).

%-callback commit(Command) -> {ok, State} | error.
% TODO: test if it is possible to extend the behaviour.

-record(state, {current_term=0, % (persistent)
                voted_for=none, % (persistent)
                log=[], % (persistent) remember that the first index must be 1
                other_nodes=[],
                commit_index=0,
                last_applied=0,
                next_index=[], % (only for the leader)
                match_index=[] % (only for the leader)
               }
       ).

%% API functions

start(NodeName, ClusterConfig) ->
    gen_statem:start({local, NodeName}, 
                     ?MODULE,
                     #{cluster_config => ClusterConfig},
                     []).

stop(ProcessName) ->
    gen_statem:stop(ProcessName).

follower({call, From}, _EventContent, _Data) ->
    {keep_state_and_data, [{reply, From, sending_vote}]}.

candidate(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

leader(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

%% Behaviour callbacks

init(#{cluster_config := []} = Options) ->
    {stop, cluster_configuration_error};
init(#{cluster_config := ClusterConfig} = Options) when (erlang:length(ClusterConfig)) rem 2 == 1 ->
    {stop, cluster_configuration_error};
init(#{cluster_config := ClusterConfig} = Options) ->
    {state_functions, follower, #{cluster_config => ClusterConfig}}.

handle_event(_EventType, _EventContent, _State, _Data) ->
    keep_state_and_data.

code_change(_OldVsn, OldState, OldData, _Extra) ->
    {state_functions, OldState, OldData}.

terminate(_Reason, _State, _Data) ->
    ignored.
