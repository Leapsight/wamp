-module(wamp_uri_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("wamp.hrl").
-compile(export_all).


all() ->
    [
        exact_match,
        exact_match_error,
        prefix_match,
        prefix_match_error,
        wildcard_match
    ].



init_per_suite(Config) ->
    _ = application:ensure_all_started(wamp),
    ok = wamp_config:init(),
    Config.

end_per_suite(_) ->
    ok.


exact_match(_) ->
    List = [
        <<"a">>,
        <<"a.foo">>,
        <<"a.foo.c">>,
        <<"a.foo.c.1">>,
        <<"a.foo.c.1.1">>
    ],
    lists:foreach(
        fun(URI) ->
            ?assertEqual(
                URI,
                wamp_uri:validate(URI, ?EXACT_MATCH)
            )
        end,
        List
    ).

exact_match_error(_) ->
    List = [
        <<"a.">>,
        <<"a.foo.">>,
        <<"a.foo.c.">>,
        <<".">>,
        <<"..">>,
        <<"...">>,
        <<".foo">>,
        <<"a..">>,
        <<"a.foo..">>,
        <<".foo..">>
    ],
    lists:foreach(
        fun(URI) ->
            ?assertError(
                {invalid_uri, URI},
                wamp_uri:validate(URI, ?EXACT_MATCH)
            )
        end,
        List
    ).


prefix_match(_) ->
    List = [
        <<"a">>,
        <<"a.">>,
        <<"a.foo.">>,
        <<"a.foo.c">>,
        <<"a.foo.c.">>
    ],
    lists:foreach(
        fun(URI) ->
            ?assertEqual(
                URI,
                wamp_uri:validate(URI, ?PREFIX_MATCH)
            )
        end,
        List
    ).

prefix_match_error(_) ->
    List = [
        <<".">>,
        <<"..">>,
        <<"...">>,
        <<".foo">>,
        <<"a..">>,
        <<"a.foo..">>,
        <<".foo..">>
    ],
    lists:foreach(
        fun(URI) ->
            ?assertError(
                {invalid_uri, URI},
                wamp_uri:validate(URI, ?PREFIX_MATCH)
            )
        end,
        List
    ).


wildcard_match(_) ->
    List = [
        <<".">>,
        <<"..">>,
        <<"...">>,
        <<".foo">>,
        <<"a..">>,
        <<"a.foo.">>,
        <<"a.foo..">>,
        <<".foo..">>
    ],
    lists:foreach(
        fun(URI) ->
            ?assertEqual(
                URI,
                wamp_uri:validate(URI, ?WILDCARD_MATCH)
            )
        end,
        List
    ).
