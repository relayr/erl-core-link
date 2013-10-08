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
	build_resources/1,
	parse_resources/1
]).

%% =============================================================================
%% Exported functions
%% =============================================================================

-spec build_resources(Resources :: [#coap_resource{}, ...]) -> {ok, nonempty_string()}.
build_resources(Resources) ->
	build_resources(Resources, []).

-spec parse_resources(Data :: nonempty_string()) -> {ok, [#coap_resource{}, ...]}.
parse_resources(Data) ->
	{ok, []}.

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
	#coap_resource{uri = URI, type = Type, interface = Interface, size = Size} = Resource,
	lists:flatten(io_lib:format("<~p>;rt=~p;if=~p~s", [URI, Type, Interface, resource_attribute(sz, Size)])).

-spec resource_attribute(AttributeName :: atom(), AttributeValue :: nonempty_string() | undefined) -> string().
resource_attribute(_AttributeName, undefined) ->
	"";
resource_attribute(AttributeName, AttributeValue) ->
	lists:flatten(io_lib:format(";~p=~p", [AttributeName, AttributeValue])).