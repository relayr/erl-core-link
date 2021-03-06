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
-include("../include/core_link.hrl").
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Tests
%% =============================================================================
-ifdef(TEST).

-define(BATTERY_SENSOR_RESOURCE(Id), #core_resource{uri = "/dev/battery" ++ integer_to_list(Id),
                                                    attributes = [
                                                     {"rt", "Battery"},
                                                     {"if", "Sensor"}
                                                    ]}).
-define(TEMPERATURE_SENSOR_RESOURCE, #core_resource{uri = "/dev/temperature",
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
    #core_resource{uri = "http://localhost:8080/xri/@domain*light-1", attributes = [{"if", "Light"}, {"stl", "121"}]},
    #core_resource{uri = "http://localhost:8080/xri/@domain*access-point-1", attributes = [{"if", "AccessPoint"}]}
]).
-define(NSP_RESOURCES_CLF, "<http://localhost:8080/proxy/light-2.domain/gps/loc>;ct=\"text/plain\";rt=\"ns:gpsloc\";if=\"core#s\";obs,"
    ++ "<http://localhost:8080/proxy/light-2.domain/dev/mdl>;ct=\"text/plain\";rt=\"ipso:dev-mdl\","
    ++ "<http://localhost:8080/proxy/light-2.domain/sen/V>,"
    ++ "<http://localhost:8080/proxy/light-2.domain/sen/temp>;ct=\"text/plain\";rt=\"ucum:Cel\";obs").
-define(NSP_RESOURCES, [
    #core_resource{uri = "http://localhost:8080/proxy/light-2.domain/gps/loc"
        , attributes = [{"ct", "text/plain"}, {"rt", "ns:gpsloc"}, {"if", "core#s"}, {"obs", undefined}]},
    #core_resource{uri = "http://localhost:8080/proxy/light-2.domain/dev/mdl"
        , attributes = [{"ct", "text/plain"}, {"rt", "ipso:dev-mdl"}]},
    #core_resource{uri = "http://localhost:8080/proxy/light-2.domain/sen/V", attributes = []},
    #core_resource{uri = "http://localhost:8080/proxy/light-2.domain/sen/temp"
        , attributes = [{"ct", "text/plain"}, {"rt", "ucum:Cel"}, {"obs", undefined}]}
]).
-define(SOME_POWER_NODE_RESOURCES_CLF, "</nw/eripaddr>;if=\"core#s\";rt=\"ns:v6addr\";ct=\"0\",</sen/temp>;obs;if=\"\";rt=\"ucum:Cel\";ct=\"0\"").
-define(SOME_POWER_NODE_RESOURCES, [
    #core_resource{uri = "/nw/eripaddr"
        , attributes = [{"if", "core#s"}, {"rt", "ns:v6addr"}, {"ct", "0"}]},
    #core_resource{uri = "/sen/temp"
        , attributes = [{"obs", undefined}, {"if", ""}, {"rt", "ucum:Cel"}, {"ct", "0"}]}
]).

-define(EMPTY_CLF, "    ").

build_resources_test() ->
    ?assertEqual({ok, ?BATTERY_SENSOR_CLF(1)}, core_link:build_resources([?BATTERY_SENSOR_RESOURCE(1)])),
    ?assertEqual({ok, ?TEMPERATURE_SENSOR_CLF ++ "," ++ ?BATTERY_SENSOR_CLF(2)},
                 core_link:build_resources([?TEMPERATURE_SENSOR_RESOURCE, ?BATTERY_SENSOR_RESOURCE(2)])),
    ?assertEqual({ok, ?NSP_ENDPOINTS_CLF}, core_link:build_resources(?NSP_ENDPOINTS)),
    ?assertEqual({ok, ?NSP_RESOURCES_CLF}, core_link:build_resources(?NSP_RESOURCES)).

parse_resources_test() ->
    ?assertEqual({ok, [?BATTERY_SENSOR_RESOURCE(1)]}, core_link:parse_resources(?BATTERY_SENSOR_CLF(1))),
    ?assertEqual({ok, [?BATTERY_SENSOR_RESOURCE(2), ?TEMPERATURE_SENSOR_RESOURCE]},
                 core_link:parse_resources(?BATTERY_SENSOR_CLF(2) ++ "," ++ ?TEMPERATURE_SENSOR_CLF)),
    ?assertEqual({ok, [?BATTERY_SENSOR_RESOURCE(3), ?BATTERY_SENSOR_RESOURCE(4)]},
                 core_link:parse_resources(?BATTERY_SENSOR_CLF(3) ++ ",\n" ++ ?BATTERY_SENSOR_CLF(4))),
    ?assertEqual({ok, ?NSP_ENDPOINTS}, core_link:parse_resources(?NSP_ENDPOINTS_CLF)),
    ?assertEqual({ok, ?NSP_RESOURCES}, core_link:parse_resources(?NSP_RESOURCES_CLF)),
    ?assertEqual({ok, ?SOME_POWER_NODE_RESOURCES}, core_link:parse_resources(?SOME_POWER_NODE_RESOURCES_CLF)).

parse_empty_test() ->
    ?assertEqual({ok, []}, core_link:parse_resources(?EMPTY_CLF)).

parse_invalid_resources_test() ->
    ?assertMatch({error, _}, core_link:parse_resources("<>")),
    ?assertMatch({error, _}, core_link:parse_resources("/dev")), % invalid URI format
    ?assertMatch({error, _}, core_link:parse_resources("</dev>sz=2")), % missing ';' character between URI and attributes
    ?assertMatch({error, _}, core_link:parse_resources("</dev/temp>;st=1</dev/bat>")).  % missing ',' character between resources

%% =============================================================================
%% Property based tests
%% =============================================================================

%% =============================================================================
%% Local functions
%% =============================================================================

-endif.
