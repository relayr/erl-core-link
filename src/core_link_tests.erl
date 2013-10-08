%%------------------------------------------------------------------------------
%% @author jodias
%% @copyright Proximetry Inc. 2013
%% @version 1.0
%% @doc core_link_tests
%% @end
%%------------------------------------------------------------------------------
-module(core_link_tests).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------
-include("../include/coap.hrl").
-include_lib("testutils/include/testing.hrl").

%% =============================================================================
%% Tests
%% =============================================================================
-ifdef(TEST).

?TEST_FUN().
build_resources_test() ->
	?assertEqual({ok, "<\"/dev/battery\">;rt=\"Battery\";if=\"Sensor\""},
				 core_link:build_resources([#coap_resource{uri = "/dev/battery",
														   type = "Battery",
														   interface = "Sensor"}])),
	?assertEqual({ok, "<\"/dev/temperature\">;rt=\"Temperature\";if=\"Sensor\";sz=2,<\"/dev/battery\">;rt=\"Battery\";if=\"Sensor\""},
				 core_link:build_resources([#coap_resource{uri = "/dev/temperature",
														   type = "Temperature",
														   interface = "Sensor",
														   size = 2},
								            #coap_resource{uri = "/dev/battery",
														   type = "Battery",
														   interface = "Sensor"}])).

%% =============================================================================
%% Property based tests
%% =============================================================================

%% =============================================================================
%% Local functions
%% =============================================================================

-endif.
