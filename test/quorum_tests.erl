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
    ExpectedState = #{cluster_config => ClusterConfig,
                      current_term => 0,
                      voted_for => none,
                      log => [],
                      storage_path => volatile},
    ?assertEqual({state_functions, follower, ExpectedState},
                 quorum:init(#{cluster_config => ClusterConfig})).

