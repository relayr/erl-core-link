%%------------------------------------------------------------------------------
%% @author jodias
%% @copyright Proximetry Inc. 2013
%% @version 1.0
%% @doc Constrained RESTful Environments (CoRE) Link Format
%%        http://tools.ietf.org/html/rfc6690
%% @end
%%------------------------------------------------------------------------------
-module(core_link).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------
-include("../include/core_link.hrl").

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

-spec resource_attributes() -> [atom()].
resource_attributes() ->
    [resource_type, interface, size].

-spec build_resources(Resources :: [#core_resource{}, ...]) -> {ok, nonempty_string()}.
build_resources(Resources) ->
    build_resources(Resources, []).

-spec parse_resources(Data :: nonempty_string()) -> {ok, [#core_resource{}, ...]} | {error, tuple()}.
parse_resources(Data) ->
    case string:len(string:strip(Data)) of
        0 ->
            {ok, []};
        _OtherLen ->
            case core_link_lexer:string(Data) of
                {ok, Body , _Line} ->
                    core_link_parser:parse(Body);
                {error, Reason, _Line} ->
                    {error, Reason}
            end
    end.
    

%% =============================================================================
%% Local functions
%% =============================================================================

-spec build_resources(Resources :: [#core_resource{}, ...], Elements :: [nonempty_string()]) -> {ok, nonempty_string()}.
build_resources([Resource], Elements) ->
    NewElements = [build_resource(Resource) | Elements],
    {ok, lists:flatten(lists:reverse(NewElements))};
build_resources([Resource | RestOfResources], Elements) ->
    NewElements = [",", build_resource(Resource) | Elements],
    build_resources(RestOfResources, NewElements).

-spec build_resource(Resource :: #core_resource{}) -> nonempty_string().
build_resource(Resource) ->
    #core_resource{uri = URI, attributes = Attributes} = Resource,
    lists:flatten(io_lib:format("<~s>~s", [URI, build_resource_attributes(Attributes, [])])).

-spec build_resource_attributes(Attributes :: [tuple()], ConvertedAttributes :: [nonempty_string()]) -> nonempty_string().
build_resource_attributes([], ConvertedAttributes) ->
    lists:flatten(lists:reverse(ConvertedAttributes));
build_resource_attributes([{AttributeName, AttributeValue} | RestOfAttributes], ConvertedAttributes) ->
    build_resource_attributes(RestOfAttributes, [build_resource_attribute(AttributeName, AttributeValue) | ConvertedAttributes]).
  
-spec build_resource_attribute(AttributeName :: nonempty_string(), AttributeValue :: string() | integer() | undefined) -> nonempty_string().
build_resource_attribute(AttributeName, undefined) ->
    io_lib:format(";~s", [AttributeName]);
build_resource_attribute(AttributeName, AttributeValue) ->
    io_lib:format(";~s=~p", [AttributeName, AttributeValue]).
