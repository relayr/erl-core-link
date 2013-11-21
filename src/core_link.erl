%%------------------------------------------------------------------------------
%% @author jodias
%% @copyright Proximetry Inc. 2013
%% @version 1.0
%% @doc Constrained RESTful Environments (CoRE) Link Format
%%		http://tools.ietf.org/html/rfc6690
%% @end
%%------------------------------------------------------------------------------
-module(core_link).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------
-include("../include/coap.hrl").
-include_lib("logger/include/logger.hrl").
-include_lib("testutils/include/testing.hrl").

%%------------------------------------------------------------------------------
%% Function exports
%%------------------------------------------------------------------------------
-export([
	resource_attributes/0,
	build_resources/1,
	parse_resources/1
]).

%% =============================================================================
%% Exported functions
%% =============================================================================

-spec resource_attributes() -> [coap_resource_attribute(), ...].
resource_attributes() ->
	[resource_type, interface, size].

-spec build_resources(Resources :: [#coap_resource{}, ...]) -> {ok, nonempty_string()}.
build_resources(Resources) ->
	build_resources(Resources, []).

-spec parse_resources(Data :: nonempty_string()) -> {ok, [#coap_resource{}, ...]}.
parse_resources(Data) ->
	{_, Body ,_} = erl_scan:string(Data),
	core_link_parser:parse(Body).

%% =============================================================================
%% Local functions
%% =============================================================================

-spec build_resources(Resources :: [#coap_resource{}, ...], Elements :: [nonempty_string()]) -> {ok, nonempty_string()}.
build_resources([Resource], Elements) ->
	NewElements = [build_resource(Resource) | Elements],
	{ok, lists:flatten(lists:reverse(NewElements))};
build_resources([Resource | RestOfResources], Elements) ->
	NewElements = [",", build_resource(Resource) | Elements],
	build_resources(RestOfResources, NewElements).

-spec build_resource(Resource :: #coap_resource{}) -> nonempty_string().
build_resource(Resource) ->
	#coap_resource{uri = URI, attributes = Attributes} = Resource,
	lists:flatten(io_lib:format("<~p>~s", [URI, build_resource_attributes(Attributes, [])])).

build_resource_attributes([], ConvertedAttributes) ->
	lists:flatten(lists:reverse(ConvertedAttributes));
build_resource_attributes([{AttributeName, AttributeValue} | RestOfAttributes], ConvertedAttributes) ->
	build_resource_attributes(RestOfAttributes, [build_resource_attribute(AttributeName, AttributeValue) | ConvertedAttributes]).
  
-spec build_resource_attribute(AttributeName :: atom(), AttributeValue :: string() | integer()) -> nonempty_string().
build_resource_attribute(resource_type, AttributeValue) ->
	io_lib:format(";rt=~p", [AttributeValue]);
build_resource_attribute(interface, AttributeValue) ->
	io_lib:format(";if=~p", [AttributeValue]);
build_resource_attribute(size, AttributeValue) ->
	io_lib:format(";sz=~p", [AttributeValue]).