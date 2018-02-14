%%------------------------------------------------------------------------------
%% @author jodias
%% @copyright Proximetry Inc. 2013
%% @version 1.0
%% @doc Constrained Application Protocol definitions.
%% @end
%%------------------------------------------------------------------------------

-ifndef(coap_hrl).

-define(coap_hrl, true).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------
-include("../include/coap_options.hrl").
-include_lib("prox_misc/include/networking.hrl").
-include_lib("prox_misc/include/uuid.hrl").

%%------------------------------------------------------------------------------
%% Definitions
%%------------------------------------------------------------------------------
-define(COAP_CON_MSG, 0).
-define(COAP_NON_MSG, 1).
-define(COAP_ACK_MSG, 2).
-define(COAP_RST_MSG, 3).

-define(COAP_VERSION, 1).

-define(COAP_MSG_PAYLOAD_MARKER, 16#FF).
-define(COAP_MAX_MSG_ID, 65535).

-define(COAP_DEFAULT_PORT, 5683).

-define(DEFAULT_LIFETIME, "86400").

-define(DEFAULT_BLOCK2_SIZE, 128).

%%------------------------------------------------------------------------------
%% CoAP Method Codes
%%------------------------------------------------------------------------------
% Request 0.xx
-define(COAP_MSG_CODE_EMPTY,                    0).
-define(COAP_MTH_CODE_GET,                      1).
-define(COAP_MTH_CODE_POST,                     2).
-define(COAP_MTH_CODE_PUT,                      3).
-define(COAP_MTH_CODE_DELETE,                   4).

%%------------------------------------------------------------------------------
%% CoAP Response Codes
%%------------------------------------------------------------------------------
% Success 2.xx
-define(COAP_RSP_CODE_CREATED,                  65). %% 2.01
-define(COAP_RSP_CODE_DELETED,                  66). %% 2.02
-define(COAP_RSP_CODE_VALID,                    67). %% 2.03
-define(COAP_RSP_CODE_CHANGED,                  68). %% 2.04
-define(COAP_RSP_CODE_CONTENT,                  69). %% 2.05

% Client Error 4.xx
-define(COAP_RSP_CODE_BAD_REQUEST,              128). %% 4.00
-define(COAP_RSP_CODE_UNAUTHORIZED,             129). %% 4.01
-define(COAP_RSP_CODE_BAD_OPTION,               130). %% 4.02
-define(COAP_RSP_CODE_FORBIDDEN,                131). %% 4.03
-define(COAP_RSP_CODE_NOT_FOUND,                132). %% 4.04
-define(COAP_RSP_CODE_METHOD_NOT_ALLOWED,       133). %% 4.05
-define(COAP_RSP_CODE_NOT_ACCEPTABLE,           134). %% 4.06
-define(COAP_RSP_CODE_PRECONDITION_FAILED,      140). %% 4.12
-define(COAP_RSP_CODE_REQUEST_ENTITY_TOO_LARGE, 141). %% 4.13
-define(COAP_RSP_CODE_UNSUPPORTED_CONTENT_FORMAT,143). %% 4.15

% Server Error 5.xx
-define(COAP_RSP_CODE_INTERNAL_SERVER_ERROR,    160). %% 5.00
-define(COAP_RSP_CODE_NOT_IMPLEMENTED,          161). %% 5.01
-define(COAP_RSP_CODE_BAD_GATEWAY,              162). %% 5.02
-define(COAP_RSP_CODE_SERVICE_UNAVAILABLE,      163). %% 5.03
-define(COAP_RSP_CODE_GATEWAY_TIMEOUT,          164). %% 5.04
-define(COAP_RSP_CODE_PROXYING_NOT_SUPPORTED,   165). %% 5.05
%%------------------------------------------------------------------------------
    
%%------------------------------------------------------------------------------
%% CoAP Content-Formats
%%------------------------------------------------------------------------------
-define('CoAP-text/plain',                      0).
-define('CoAP-application/link-format',         40).
-define('CoAP-application/xml',                 41).
-define('CoAP-application/octet-stream',        42).
-define('CoAP-application/exi',                 47).
-define('CoAP-application/json',                50).
-define('CoAP-application/cbor',                60).
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% CoAP Other Constants
%%------------------------------------------------------------------------------
-define(ACK_TIMEOUT, 2000).
-define(ACK_RANDOM_FACTOR, 1.5).
-define(MAX_RETRANSMIT, 4).
-define(NSTART, 1).
-define(TOKEN_LEN, 4).
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% CoAP Discovery constants
%%------------------------------------------------------------------------------
-define(WELL_KNOWN, "/.well-known").
-define(WELL_KNOWN_CORE, "/.well-known/core").
-define(WELL_KNOWN_CORE_ATTRIBUTES, [{"rt", "core"}, {"ct", ?'CoAP-application/link-format'}]).
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type coap_version()            :: ?COAP_VERSION.
-type coap_msg_type()           :: ?COAP_CON_MSG | ?COAP_NON_MSG | ?COAP_ACK_MSG | ?COAP_RST_MSG.
-type coap_msg_token()          :: binary().
-type coap_msg_code()           :: non_neg_integer().
-type coap_msg_class()          :: non_neg_integer().
-type coap_msg_detail()         :: non_neg_integer().
-type coap_msg_id()             :: non_neg_integer().
-type coap_uri()                :: nonempty_string().
-type coap_resource_attribute() :: {nonempty_string(), string() | integer() | undefined}.
-type coap_handler_path()       :: coap_uri() | {prefix, coap_uri()}.

%%------------------------------------------------------------------------------
%% Records
%%------------------------------------------------------------------------------

-record(coap_msg, {
    version = ?COAP_VERSION :: coap_version(),
    type                    :: coap_msg_type(),
    token = <<>>            :: coap_msg_token(),
    code                    :: coap_msg_code() | undefined,
    id                      :: coap_msg_id(),
    options = []            :: [#coap_option{}],
    payload = <<>>          :: binary()
}).

-record(coap_resource, {
    uri            :: coap_uri(),
    attributes    :: [coap_resource_attribute()]
}).

-record(coap_socket, {

}).

-record(coap_parsed_endpoint, {
    id :: did(),
    adr :: ip_address(),
    port :: port_number(),
    device_model_name :: nonempty_string(),
    resources = [#coap_resource{}] %% resources are now known from the beginning
}).

-record(coap_resource_server_state, {
    onconnect :: fun((#coap_parsed_endpoint{}) -> ok),
    ondisconnect :: fun((#coap_parsed_endpoint{}) -> ok),
    endpoints = [#coap_parsed_endpoint{}],
    socket :: pid()
}).

-record(coap_net_if_state, {
    socket :: gen_udp:socket() | tuple(),
    connected_endpoints = dict:new() :: dict:dict()
}).

-record(net_if_endpoint, {
    client_pid :: pid(),
    client_monitor :: reference(),
    msg_ids :: sets:set(),
    tokens :: sets:set()
}).

-record(coap_client_state, {
    ip :: inet:ip_address(),
    port :: inet:port_number(),
    msg_id :: coap_msg_id(),
    waiting = queue:new() :: queue:queue(),
    active = maps:new() :: maps:map()
}).

-record(coap_request, {
    request :: tuple(),
    msg :: #coap_msg{},
    timeout :: non_neg_integer(),
    timeout_timer :: timer:tref(),
    retry_no = 0 :: non_neg_integer(),
    acked = false :: boolean(),
    block_no = 0 :: non_neg_integer(),
    block_size :: non_neg_integer(),
    response_ct :: non_neg_integer(),
    response = <<>> :: binary(),
    response_callback :: fun(),
    error_callback :: fun()
}).

-record(coap_server_resource, {
    handler :: module(),
    handler_opts :: list(),
    attributes :: [coap_resource_attribute()] | hide
}).

-record(coap_server_state, {
    resources = maps:new() :: maps:map(),
    msg_id :: coap_msg_id()
}).

-record(coap_handler_state, {
    handler :: module(),
    handler_opts :: list(),
    handler_callback :: atom(),
    req :: {inet:ip_address(), inet:port_number(), #coap_msg{}},
    rsp :: #coap_msg{} | undefined,
    method :: coap_msg_code() | undefined,
    accepted_format :: coap_option_value() | undefined,
    content_format :: coap_option_value() | undefined,
    is_separate = false :: boolean(),
    response_sent = false :: boolean(),
    state :: any()
}).

-endif.
