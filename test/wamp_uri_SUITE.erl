-module(wamp_uri_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("wamp.hrl").
-compile(export_all).


all() ->
    [
        test_strict,
        validate_exact,
        validate_exact_error,
        validate_prefix,
        validate_prefix_error,
        validate_wildcard,
        validate_wildcard_error,
        empty_uri,
        match
    ].



init_per_suite(Config) ->
    _ = application:ensure_all_started(wamp),
    ok = wamp_config:init(),
    Config.

end_per_suite(_) ->
    ok.


test_strict(_) ->
    List = [
        <<"foo.b-a-r">>,
        <<"foo.b a r">>
    ],
    lists:foreach(
        fun(URI) ->
            ?assertEqual(
                false,
                wamp_uri:is_valid(URI, strict)
            )
        end,
        List
    ).

validate_exact(_) ->
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

validate_exact_error(_) ->
    List = [
        <<>>,
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


validate_prefix(_) ->
    List = [
        <<>>,
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

validate_prefix_error(_) ->
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


validate_wildcard(_) ->
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


validate_wildcard_error(_) ->
    List = [
        <<>>
    ],
    lists:foreach(
        fun(URI) ->
          ?assertError(
                {invalid_uri, URI},
                wamp_uri:validate(URI, ?WILDCARD_MATCH)
            )
        end,
        List
    ).


empty_uri(_) ->
    wamp_config:set(uri_strictness, strict),
    ?assertEqual(false, wamp_uri:is_valid(<<>>)),
    ?assertEqual(false, wamp_uri:is_valid(<<>>, ?PREFIX_MATCH)),
    ?assertEqual(false, wamp_uri:is_valid(<<>>, ?EXACT_MATCH)),
    ?assertEqual(false, wamp_uri:is_valid(<<>>, ?WILDCARD_MATCH)),

    wamp_config:set(uri_strictness, loose),
    ?assertEqual(false, wamp_uri:is_valid(<<>>)),
    ?assertEqual(true, wamp_uri:is_valid(<<>>, ?PREFIX_MATCH)),
    ?assertEqual(false, wamp_uri:is_valid(<<>>, ?EXACT_MATCH)),
    ?assertEqual(false, wamp_uri:is_valid(<<>>, ?WILDCARD_MATCH)),

    %% Irrespective of uri_strictness setting
    ?assertEqual(true, wamp_uri:is_valid(<<>>, loose_prefix)),
    ?assertEqual(false, wamp_uri:is_valid(<<>>, loose)),
    ?assertEqual(false, wamp_uri:is_valid(<<>>, strict)),
    ?assertEqual(false, wamp_uri:is_valid(<<>>, strict_prefix)),
    ?assertEqual(false, wamp_uri:is_valid(<<>>, loose_allow_empty)),
    ?assertEqual(false, wamp_uri:is_valid(<<>>, strict_allow_empty)),

    ok.



match(Config) ->
    wamp_config:set(uri_strictness, strict),
    do_match(Config),

    wamp_config:set(uri_strictness, loose),
    do_match(Config).


do_match(_) ->
    ?assertError(badarg, wamp_uri:match(<<>>, <<"a">>, ?EXACT_MATCH)),
    ?assertError(badarg, wamp_uri:match(<<>>, <<"a">>, ?WILDCARD_MATCH)),
    ?assertEqual(false, wamp_uri:match(<<>>, <<"a">>, ?PREFIX_MATCH)),
    ?assertEqual(true, wamp_uri:match(<<>>, <<>>, ?PREFIX_MATCH)),
    ?assertEqual(false, wamp_uri:match(<<"a">>, <<"b">>, ?EXACT_MATCH)),
    ?assert(wamp_uri:match(<<"a">>, <<"a">>, ?EXACT_MATCH)),
    ?assert(wamp_uri:match(<<"a">>, <<"a">>, ?PREFIX_MATCH)),
    ?assert(wamp_uri:match(<<"a.">>, <<"a">>, ?PREFIX_MATCH)),
    ?assert(wamp_uri:match(<<"a.b">>, <<"a">>, ?PREFIX_MATCH)),

    ?assert(wamp_uri:match(<<"a">>, <<"a">>, ?WILDCARD_MATCH)),
    ?assert(wamp_uri:match(<<"a.b">>, <<".">>, ?WILDCARD_MATCH)),
    ?assert(wamp_uri:match(<<"a.b">>, <<"a.">>, ?WILDCARD_MATCH)),
    ?assert(wamp_uri:match(<<"a.b">>, <<".b">>, ?WILDCARD_MATCH)),
    ?assert(wamp_uri:match(<<"a.b">>, <<"a.b">>, ?WILDCARD_MATCH)).

