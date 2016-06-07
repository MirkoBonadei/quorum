-module(quorum_tests).
-include_lib("eunit/include/eunit.hrl").

starting_quorum_with_only_one_node_results_in_error_test() ->
    ?assertEqual({stop, cluster_configuration_error},
                 quorum:init(#{cluster_config => []})).

starting_quorum_with_an_even_number_of_nodes_results_in_error_test() ->
    ?assertEqual({stop, cluster_configuration_error},
                 quorum:init(#{cluster_config => [the_second_node]})).

starting_quorum_with_an_odd_number_of_nodes_starts_the_process_in_state_functions_mode_test() ->
    ClusterConfig = [second_node, third_node],
    ?assertEqual({state_functions, follower, #{cluster_config => ClusterConfig}},
                 quorum:init(#{cluster_config => ClusterConfig})).
