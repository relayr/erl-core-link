Definitions.

SPECIAL_TOKEN = (<|>|,|;|=|")
DIGIT = [0-9]
TEXT = ([A-Z]|[a-z]|[0-9]|!|#|\$|%|&|'|\(|\)|\*|\+|-|\.|/|:|\?|@|\[|\]|\^|_|`|\{|\||}|~)
WS = ([\000-\s]|%.*)

Rules.

{SPECIAL_TOKEN} : {token, {list_to_atom(TokenChars), TokenLine}}.
{DIGIT}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{TEXT}+ : {token, {string, TokenLine, TokenChars}}. 
{WS}+ : skip_token.

Erlang code.