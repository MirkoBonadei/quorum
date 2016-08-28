-module(quorum).
-behaviour(gen_statem).

-export([start/2, stop/1]).
-export([init/1, handle_event/4, code_change/4, terminate/3]).

%% RAFT FSM states
-export([follower/3, candidate/3, leader/3]).

-define(META_FILE_NAME, "/meta").
-define(LOG_FILE_NAME, "/log").
-define(LOWER_TIMEOUT_MS, 150).
-define(HIGHER_TIMEOUT_MS, 300).

%% API functions
start(NodeName, Options) ->
    gen_statem:start({local, NodeName}, ?MODULE, Options, []).

stop(ProcessName) ->
    gen_statem:stop(ProcessName).

follower({call, From}, _EventContent, _Data) ->
    {keep_state_and_data, [{reply, From, sending_vote}]}.

candidate(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

leader(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

%% Behaviour callbacks

init(#{cluster_config := []} = _Options) ->
    {stop, cluster_configuration_error};
init(#{cluster_config := ClusterConfig} = _Options) when (erlang:length(ClusterConfig)) rem 2 == 1 ->
    {stop, cluster_configuration_error};
init(Options) ->
    State = initialize_state(Options),
    TRef = initialize_follower_timer(Options),
    {state_functions, follower, maps:put(current_timer => TRef, State)}.

handle_event(_EventType, _EventContent, _State, _Data) ->
    keep_state_and_data.

code_change(_OldVsn, OldState, OldData, _Extra) ->
    {state_functions, OldState, OldData}.

terminate(_Reason, _State, _Data) ->
    ignored.

initialize_follower_timer(Options) ->
    Timeout = case maps:get(timeout_after, Options) of
                  {badkey, _Key} -> randomized_timeout();
                  ConfTimeout -> ConfTimeout
              end,
    timer:send_after(Timeout, {timeout, follower_timeout}).

randomized_timeout() ->
    

initialize_state(Options) ->
    DefaultState = #{cluster_config => maps:get(cluster_config, Options),
                     voted_for => none,
                     current_term => 0,
                     log => none,
                     storage_path => volatile},
    case maps:find(storage_path, Options) of
        error ->
            DefaultState;
        {ok, StoragePath} ->
            {ok, LogFd} = file:open(log_file_name(StoragePath), [append]), % TODO: binary?
            case filelib:is_regular(meta_file_name(StoragePath)) of
                true ->
                    {ok, VotedFor, CurrentTerm} = load_meta(StoragePath),
                    DefaultState#{storage_path := StoragePath,
                                  log := LogFd,
                                  voted_for := VotedFor,
                                  current_term := CurrentTerm};
                false ->
                    VotedFor = none,
                    CurrentTerm = 0,
                    store_meta(StoragePath, VotedFor, CurrentTerm),
                    DefaultState#{storage_path := StoragePath,
                                    log := LogFd,
                                    voted_for := VotedFor,
                                    current_term := CurrentTerm}
            end
    end.

log_file_name(StoragePath) ->
    StoragePath ++ ?LOG_FILE_NAME.

meta_file_name(StoragePath) ->
    StoragePath ++ ?META_FILE_NAME.

store_meta(StoragePath, VotedFor, CurrentTerm) ->
    FilePath = meta_file_name(StoragePath),
    {ok, Fd} = file:open(FilePath, [write, binary]),
    file:write(Fd, erlang:term_to_binary({voted_for, VotedFor})),
    insert_new_line(Fd),
    file:write(Fd, erlang:term_to_binary({current_term, CurrentTerm})),
    ok = file:close(Fd).

load_meta(StoragePath) ->
    FilePath = meta_file_name(StoragePath),
    {ok, Fd} = file:open(FilePath, [read, binary]),
    {ok, VotedForBinary} = file:read_line(Fd),
    {ok, CurrentTermBinary} = file:read_line(Fd),
    {voted_for, VotedFor} = erlang:binary_to_term(VotedForBinary),
    {current_term, CurrentTerm} = erlang:binary_to_term(CurrentTermBinary),
    {ok, VotedFor, CurrentTerm}.

insert_new_line(Fd) ->
    file:write(Fd, erlang:term_to_binary("\n")).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(TEST_STORAGE_PATH, "/tmp").

%% This are integrations tests because they are integrating quorum 
%% with the filesystem. I prefer to test them here because they are 
%% private functions and exporting them only for testing purpose it 
%% is not a good idea.

when_initialized_without_storage_it_is_volatile_test() ->
    ExpectedState = #{cluster_config => [second_node, third_node],
                      voted_for => none,
                      current_term => 0,
                      log => none,
                      storage_path => volatile},
    ActualState = initialize_state(#{cluster_config => [second_node, third_node]}),
    ?assertEqual(ExpectedState, ActualState).

when_initialized_with_storage_for_the_first_time_it_creates_persistent_state_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) ->
             file:delete(meta_file_name(?TEST_STORAGE_PATH)),
             file:delete(log_file_name(?TEST_STORAGE_PATH))
     end,
     fun() ->
             Options = #{cluster_config => [second_node, third_node],
                         storage_path => ?TEST_STORAGE_PATH},
             State = initialize_state(Options),
             ?assert(filelib:is_regular(log_file_name(?TEST_STORAGE_PATH))),
             ?assert(filelib:is_regular(meta_file_name(?TEST_STORAGE_PATH))),
             ?assertEqual(none, maps:get(voted_for, State)),
             ?assertEqual(?TEST_STORAGE_PATH, maps:get(storage_path, State)),
             ?assertEqual(0, maps:get(current_term, State)),
             ?assert(erlang:is_pid(maps:get(log, State)))
     end}.

when_initialized_with_storage_it_is_loaded_test_() ->
    {setup,
     fun() ->
             {ok, MetaFd} = file:open(meta_file_name(?TEST_STORAGE_PATH), [write, read, binary]),
             {ok, LogFd} = file:open(log_file_name(?TEST_STORAGE_PATH), [write, read, binary]),
             file:write(MetaFd, erlang:term_to_binary({voted_for, second_node})),
             insert_new_line(MetaFd),
             file:write(MetaFd, erlang:term_to_binary({current_term, 100})),
             file:close(MetaFd),
             file:close(LogFd)
     end,
     fun(_) ->
             file:delete(meta_file_name(?TEST_STORAGE_PATH)),
             file:delete(log_file_name(?TEST_STORAGE_PATH))
     end,
     fun() ->
             Options = #{cluster_config => [second_node, third_node],
                         storage_path => ?TEST_STORAGE_PATH},
             State = initialize_state(Options),
             ?assertEqual(second_node, maps:get(voted_for, State)),
             ?assertEqual(?TEST_STORAGE_PATH, maps:get(storage_path, State)),
             ?assertEqual(100, maps:get(current_term, State)),
             ?assert(erlang:is_pid(maps:get(log, State)))
     end}.

teardown(_) ->
    file:delete(meta_file_name(?TEST_STORAGE_PATH)),
    file:delete(log_file_name(?TEST_STORAGE_PATH)).

-endif.
