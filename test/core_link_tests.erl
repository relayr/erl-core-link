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

-define(BATTERY_SENSOR_RESOURCE(Id), #coap_resource{uri = "/dev/battery" ++ integer_to_list(Id),
													attributes = [
													 {"rt", "Battery"},
													 {"if", "Sensor"}
													]}).
-define(TEMPERATURE_SENSOR_RESOURCE, #coap_resource{uri = "/dev/temperature",
													attributes = [
													 {"rt", "Temperature"},
													 {"if", "Sensor"},
													 {"sz", 2}
													]}).
-define(BATTERY_SENSOR_CLF(Id), "</dev/battery" ++ integer_to_list(Id) ++ ">;rt=\"Battery\";if=\"Sensor\"").
-define(TEMPERATURE_SENSOR_CLF, "</dev/temperature>;rt=\"Temperature\";if=\"Sensor\";sz=2").

-define(NSP_ENDPOINTS_CLF, "<http://localhost:8080/xri/@domain*light-1>;if=\"Light\";stl=\"121\"" 
    ++ ",<http://localhost:8080/xri/@domain*access-point-1>;if=\"AccessPoint\"").
-define(NSP_ENDPOINTS, [
    #coap_resource{uri = "http://localhost:8080/xri/@domain*light-1", attributes = [{"if", "Light"}, {"stl", "121"}]},
    #coap_resource{uri = "http://localhost:8080/xri/@domain*access-point-1", attributes = [{"if", "AccessPoint"}]}
]).
-define(NSP_RESOURCES_CLF, "<http://localhost:8080/proxy/light-2.domain/gps/loc>;ct=\"text/plain\";rt=\"ns:gpsloc\";if=\"core#s\";obs,"
    ++ "<http://localhost:8080/proxy/light-2.domain/dev/mdl>;ct=\"text/plain\";rt=\"ipso:dev-mdl\","
    ++ "<http://localhost:8080/proxy/light-2.domain/sen/V>,"
    ++ "<http://localhost:8080/proxy/light-2.domain/sen/temp>;ct=\"text/plain\";rt=\"ucum:Cel\";obs").
-define(NSP_RESOURCES, [
    #coap_resource{uri = "http://localhost:8080/proxy/light-2.domain/gps/loc"
        , attributes = [{"ct", "text/plain"}, {"rt", "ns:gpsloc"}, {"if", "core#s"}, {"obs", undefined}]},
    #coap_resource{uri = "http://localhost:8080/proxy/light-2.domain/dev/mdl"
        , attributes = [{"ct", "text/plain"}, {"rt", "ipso:dev-mdl"}]},
    #coap_resource{uri = "http://localhost:8080/proxy/light-2.domain/sen/V", attributes = []},
    #coap_resource{uri = "http://localhost:8080/proxy/light-2.domain/sen/temp"
        , attributes = [{"ct", "text/plain"}, {"rt", "ucum:Cel"}, {"obs", undefined}]}
]).
-define(SOME_POWER_NODE_RESOURCES_CLF, "</nw/eripaddr>;if=\"core#s\";rt=\"ns:v6addr\";ct=\"0\",</sen/temp>;obs;if=\"\";rt=\"ucum:Cel\";ct=\"0\"").
-define(SOME_POWER_NODE_RESOURCES, [
	#coap_resource{uri = "/nw/eripaddr"
		, attributes = [{"if", "core#s"}, {"rt", "ns:v6addr"}, {"ct", "0"}]},
	#coap_resource{uri = "/sen/temp"
		, attributes = [{"obs", undefined}, {"if", ""}, {"rt", "ucum:Cel"}, {"ct", "0"}]}
]).

-define(EMPTY_CLF, "    ").

?TEST_FUN().
build_resources_test() ->
	?assertEqual({ok, ?BATTERY_SENSOR_CLF(1)}, core_link:build_resources([?BATTERY_SENSOR_RESOURCE(1)])),
	?assertEqual({ok, ?TEMPERATURE_SENSOR_CLF ++ "," ++ ?BATTERY_SENSOR_CLF(2)},
				 core_link:build_resources([?TEMPERATURE_SENSOR_RESOURCE, ?BATTERY_SENSOR_RESOURCE(2)])),
    ?assertEqual({ok, ?NSP_ENDPOINTS_CLF}, core_link:build_resources(?NSP_ENDPOINTS)),
    ?assertEqual({ok, ?NSP_RESOURCES_CLF}, core_link:build_resources(?NSP_RESOURCES)).

?TEST_FUN().
parse_resources_test() ->
	?assertEqual({ok, [?BATTERY_SENSOR_RESOURCE(1)]}, core_link:parse_resources(?BATTERY_SENSOR_CLF(1))),
	?assertEqual({ok, [?BATTERY_SENSOR_RESOURCE(2), ?TEMPERATURE_SENSOR_RESOURCE]},
				 core_link:parse_resources(?BATTERY_SENSOR_CLF(2) ++ "," ++ ?TEMPERATURE_SENSOR_CLF)),
	?assertEqual({ok, [?BATTERY_SENSOR_RESOURCE(3), ?BATTERY_SENSOR_RESOURCE(4)]},
				 core_link:parse_resources(?BATTERY_SENSOR_CLF(3) ++ ",\n" ++ ?BATTERY_SENSOR_CLF(4))),
    ?assertEqual({ok, ?NSP_ENDPOINTS}, core_link:parse_resources(?NSP_ENDPOINTS_CLF)),
    ?assertEqual({ok, ?NSP_RESOURCES}, core_link:parse_resources(?NSP_RESOURCES_CLF)),
	?assertEqual({ok, ?SOME_POWER_NODE_RESOURCES}, core_link:parse_resources(?SOME_POWER_NODE_RESOURCES_CLF)).

?TEST_FUN().
parse_empty_test() ->
    ?assertEqual({ok, []}, core_link:parse_resources(?EMPTY_CLF)).

%% =============================================================================
%% Property based tests
%% =============================================================================

%% =============================================================================
%% Local functions
%% =============================================================================

-endif.
