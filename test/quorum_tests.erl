-module(quorum_tests).
-include_lib("eunit/include/eunit.hrl").

starting_with_only_one_node_results_in_error_test() ->
    ?assertEqual({stop, cluster_configuration_error},
                 quorum:init(#{cluster_config => []})).

starting_with_an_even_number_of_nodes_results_in_error_test() ->
    ?assertEqual({stop, cluster_configuration_error},
                 quorum:init(#{cluster_config => [the_second_node]})).

starting_with_an_odd_number_and_without_presistent_storage_test() ->
    ClusterConfig = [second_node, third_node],
    OneMinute = 60000, %% really high for testing purposes
    Options = #{cluster_config => ClusterConfig, 
                timeout_after => OneMinute},
    {state_functions, follower, State} = quorum:init(Options),
    ?assertEqual(ClusterConfig, maps:get(cluster_config, State)),
    ?assertEqual(0, maps:get(current_term, State)),
    ?assertEqual(none, maps:get(voted_for, State)),
    ?assertEqual(none, maps:get(log, State)),
    ?assertEqual(volatile, maps:get(storage_path, State)),
    ?assert(erlang:is_reference(maps:get(current_timer, State))),
    timer:cancel(maps:get(current_timer, State)).


    
