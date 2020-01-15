# core_link

## Constrained RESTful Environments (CoRE) Link Format

[![Build Status](https://travis-ci.org/relayr/erl-core-link.svg?branch=master)](https://travis-ci.org/relayr/erl-core-link) [![Hex.pm](https://img.shields.io/hexpm/v/core_link.svg?style=flat)](https://hex.pm/packages/core_link) [![Coverage Status](https://coveralls.io/repos/github/relayr/erl-core-link/badge.svg?branch=master)](https://coveralls.io/github/relayr/erl-core-link?branch=master)

This application can be used to process CoRE resources as defined in [RFC 6690](http://tools.ietf.org/html/rfc6690).

There's the following `#core_resource{}` record defined in `core_link/include/core_link.hrl`:
```
-record(core_resource, {
    uri           :: nonempty_string(),
    attributes    :: {nonempty_string(), string() | integer() | undefined}
}).
```

---
### Examples

#### core_link:build_resources/1
Build resources in CoRE Link Format from Erlang records.
```
1> rr(core_link).
[core_resource]
2> core_link:build_resources([#core_resource{uri = "/dev/temperature",
                                             attributes = [
                                                {"rt", "Temperature"},
                                                {"if", "Sensor"},
                                                {"sz", 2}
                                             ]}]).

{ok,"</dev/temperature>;rt=\"Temperature\";if=\"Sensor\";sz=2"}
```

#### core_link:parse_resources/1
Parse resources in CoRE Link Format to Erlang records.
```
3> core_link:parse_resources("</dev/temperature>;rt=\"Temperature\";if=\"Sensor\";sz=2,</dev/battery2>;rt=\"Battery\";if=\"Sensor\"").
{ok,[#core_resource{uri = "/dev/temperature",
                    attributes = [{"rt","Temperature"},
                                  {"if","Sensor"},
                                  {"sz",2}]},
     #core_resource{uri = "/dev/battery2",
                    attributes = [{"rt","Battery"},{"if","Sensor"}]}]}
```
