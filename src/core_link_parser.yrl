%% <[RFC6690]>
%%
%% Link            = link-value-list
%% link-value-list = [ link-value *[ "," link-value ]]
%% link-value     = "<" URI-Reference ">" *( ";" link-param )
%% link-param     = ( ( "rel" "=" relation-types )
%%                / ( "anchor" "=" DQUOTE URI-Reference DQUOTE )
%%                / ( "rev" "=" relation-types )
%%                / ( "hreflang" "=" Language-Tag )
%%                / ( "media" "=" ( MediaDesc
%%                       / ( DQUOTE MediaDesc DQUOTE ) ) )
%%                / ( "title" "=" quoted-string )
%%                / ( "title*" "=" ext-value )
%%                / ( "type" "=" ( media-type / quoted-mt ) )
%%                / ( "rt" "=" relation-types )
%%                / ( "if" "=" relation-types )
%%                / ( "sz" "=" cardinal )
%%                / ( link-extension ) )
%% link-extension = ( parmname [ "=" ( ptoken / quoted-string ) ] )
%%                / ( ext-name-star "=" ext-value )
%% ext-name-star  = parmname "*" ; reserved for RFC-2231-profiled
%%                               ; extensions.  Whitespace NOT
%%                               ; allowed in between.
%% ptoken         = 1*ptokenchar
%% ptokenchar     = "!" / "#" / "$" / "%" / "&" / "'" / "("
%%                / ")" / "*" / "+" / "-" / "." / "/" / DIGIT
%%                / ":" / "<" / "=" / ">" / "?" / "@" / ALPHA
%%                / "[" / "]" / "^" / "_" / "`" / "{" / "|"
%%                / "}" / "~"
%% media-type     = type-name "/" subtype-name
%% quoted-mt      = DQUOTE media-type DQUOTE
%% relation-types = relation-type
%%                / DQUOTE relation-type *( 1*SP relation-type ) DQUOTE
%% relation-type  = reg-rel-type / ext-rel-type
%% reg-rel-type   = LOALPHA *( LOALPHA / DIGIT / "." / "-" )
%% ext-rel-type   = URI
%% cardinal       = "0" / ( %x31-39 *DIGIT )
%% LOALPHA        = %x61-7A   ; a-z
%% quoted-string  = <defined in [RFC2616]>
%% URI            = <defined in [RFC3986]>
%% URI-Reference  = <defined in [RFC3986]>
%% type-name      = <defined in [RFC4288]>
%% subtype-name   = <defined in [RFC4288]>
%% MediaDesc      = <defined in [W3C.HTML.4.01]>
%% Language-Tag   = <defined in [RFC5646]>
%% ext-value      = <defined in [RFC5987]>
%% parmname       = <defined in [RFC5987]>

Nonterminals link_value_list link_value uri link_params link_param link_param_value text number.
Terminals '<' '>' ';' ',' '=' '"' integer string.
Rootsymbol link_value_list.

link_value_list -> link_value : ['$1'].
link_value_list -> link_value ',' link_value_list : ['$1' | '$3'].

link_value -> '<' uri '>' : #core_resource{uri = '$2', attributes = []}.
link_value -> '<' uri '>' link_params : #core_resource{uri = '$2', attributes = '$4'}.

uri -> text : '$1'.

link_params -> ';' link_param : ['$2'].
link_params -> ';' link_param link_params : ['$2' | '$3'].

link_param -> text : {'$1', undefined}.
link_param -> text '=' link_param_value : {'$1', '$3'}.

link_param_value -> number : '$1'.
link_param_value -> '"' text '"' : '$2'.
link_param_value -> '"' number '"' : integer_to_list('$2').
link_param_value -> '"' '"' : "".

text -> string : value_of('$1').
number -> integer : value_of('$1').

Erlang code.

-include("../include/core_link.hrl").

value_of(Token) -> 
    element(3, Token).
