-module(quorum).
-behaviour(gen_statem).

-export([start/2,
         stop/1
        ]).

-export([init/1,
         handle_event/4,
         code_change/4,
         terminate/3
        ]).

%% RAFT FSM states
-export([follower/3,
         candidate/3,
         leader/3
        ]).

-record(state, {current_term=0, % (persistent)
                voted_for=none, % (persistent)
                log=[], % (persistent) remember that the first index must be 1
                
                other_nodes=[],
                commit_index=0,
                last_applied=0,

                % only used by the leader
                next_index=[],
                match_index=[]
               }
       ).

%% API functions

start(ProcessName, OtherNodes) ->
    gen_statem:start({local, ProcessName}, 
                     ?MODULE,
                     OtherNodes,
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

init(OtherNodes) ->
    {state_functions, follower, #state{other_nodes=OtherNodes}}.

handle_event(_EventType, _EventContent, _State, _Data) ->
    keep_state_and_data.

code_change(_OldVsn, OldState, OldData, _Extra) ->
    {state_functions, OldState, OldData}.

terminate(_Reason, _State, _Data) ->
    ignored.