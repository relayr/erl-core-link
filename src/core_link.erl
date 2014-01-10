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

-spec parse_resources(Data :: nonempty_string()) -> {ok, [#coap_resource{}, ...]} | {error, tuple()}.
parse_resources(Data) ->
    case core_link_lexer:string(Data) of
        {ok, Body , _Line} ->
            core_link_parser:parse(Body);
        {error, Reason, _Line} ->
            {error, Reason}
    end.
	

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
	lists:flatten(io_lib:format("<~s>~s", [URI, build_resource_attributes(Attributes, [])])).

build_resource_attributes([], ConvertedAttributes) ->
	lists:flatten(lists:reverse(ConvertedAttributes));
build_resource_attributes([{AttributeName, AttributeValue} | RestOfAttributes], ConvertedAttributes) ->
	build_resource_attributes(RestOfAttributes, [build_resource_attribute(AttributeName, AttributeValue) | ConvertedAttributes]).
  
-spec build_resource_attribute(AttributeName :: nonempty_string(), AttributeValue :: string() | integer() | undefined) -> nonempty_string().
build_resource_attribute(AttributeName, AttributeValue) ->
    DecodedName = case AttributeName of
        "resource_type" -> "rt";
        "interface" -> "if";
        "size" -> "sz";
        _ -> AttributeName
    end,
    format_attribute(DecodedName, AttributeValue).

-spec format_attribute(AttributeName :: nonempty_string(), AttributeValue :: string() | integer() | undefined) -> nonempty_string().
format_attribute(AttributeName, undefined) ->
    io_lib:format(";~s", [AttributeName]);
format_attribute(AttributeName, AttributeValue) ->
    io_lib:format(";~s=~p", [AttributeName, AttributeValue]).
    