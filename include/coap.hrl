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
-define(COAPS_DEFAULT_PORT, 5684).

-define(DEFAULT_LIFETIME, "86400").

%%------------------------------------------------------------------------------
%% CoAP Method Codes
%%------------------------------------------------------------------------------
% Request 0.xx
-define(COAP_MTH_CODE_EMPTY,                    0).
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
-define(COAP_RSP_CODE_METHOD_NOT_ACCEPTABLE,    134). %% 4.06
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
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type coap_protocol()           :: coap | coaps.
-type coap_version()            :: ?COAP_VERSION.
-type coap_msg_type()           :: ?COAP_CON_MSG | ?COAP_NON_MSG | ?COAP_ACK_MSG | ?COAP_RST_MSG.
-type coap_msg_token()          :: binary().
-type coap_msg_code()           :: non_neg_integer().
-type coap_msg_class()          :: non_neg_integer().
-type coap_msg_detail()         :: non_neg_integer().
-type coap_msg_id()             :: non_neg_integer().
-type coap_uri()                :: nonempty_string().
-type coap_resource_attribute() :: {nonempty_string(), string() | integer() | undefined}.

%%------------------------------------------------------------------------------
%% Records
%%------------------------------------------------------------------------------
-record(coap_msg, {
    version = ?COAP_VERSION :: coap_version(),
    type                    :: coap_msg_type(),
    token = <<>>            :: coap_msg_token(),
    code                    :: coap_msg_code(),
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

-endif.
