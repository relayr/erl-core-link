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
	{_, Body ,_} = erl_scan:string(Data),
	{ok, CoRELinks} = core_link_parser:parse(Body),
	parse_core_link_resources(CoRELinks, []).

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

-spec parse_core_link_resources(CoRELinks :: [{core_link, URI :: nonempty_string(), Params :: [{atom(), any()}]}],
								Resources :: [#coap_resource{}]) -> {ok, [#coap_resource{}, ...]}.
parse_core_link_resources([], Resources) ->
	{ok, lists:reverse(Resources)};
parse_core_link_resources([{core_link, URI, Params} | RestOfCoRELinks], Resources) ->
	NewResources = [parse_resource_params(#coap_resource{uri = URI}, Params) | Resources],
	parse_core_link_resources(RestOfCoRELinks, NewResources).

-spec parse_resource_params(Resource :: #coap_resource{}, Params :: [{atom(), any()}]) -> #coap_resource{}.
parse_resource_params(Resource, []) ->
	Resource;
parse_resource_params(Resource, [{rt, Type} | RestOfParams]) ->
	parse_resource_params(Resource#coap_resource{type = Type}, RestOfParams);
parse_resource_params(Resource, [{'if', Interface} | RestOfParams]) ->
	parse_resource_params(Resource#coap_resource{interface = Interface}, RestOfParams);
parse_resource_params(Resource, [{sz, Size} | RestOfParams]) ->
	parse_resource_params(Resource#coap_resource{size = Size}, RestOfParams).