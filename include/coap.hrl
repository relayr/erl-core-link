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

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type coap_version()			:: ?COAP_VERSION.
-type coap_msg_type()			:: ?COAP_CON_MSG | ?COAP_NON_MSG | ?COAP_ACK_MSG | ?COAP_RST_MSG.
-type coap_msg_token()			:: binary().
-type coap_msg_code()			:: non_neg_integer().
-type coap_msg_class()			:: non_neg_integer().
-type coap_msg_detail()			:: non_neg_integer().
-type coap_msg_id()				:: non_neg_integer().
-type coap_option_id()			:: non_neg_integer().
-type coap_resource_attribute() :: resource_type | interface | size.

%%------------------------------------------------------------------------------
%% Records
%%------------------------------------------------------------------------------
-record(coap_option, {
	id			:: coap_option_id(),
	value		:: binary()
}).

-record(coap_msg, {
	version		:: coap_version(),
	type		:: coap_msg_type(),
	token		:: coap_msg_token(),
	code		:: coap_msg_code(),
	id			:: coap_msg_id(),
	options		:: [#coap_option{}],
	payload		:: binary()
}).

-record(coap_resource, {
	uri			:: nonempty_string(),
	attributes	:: [{coap_resource_attribute(), string() | integer()}]
}).

-endif.
