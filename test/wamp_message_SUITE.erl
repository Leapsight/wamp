-module(wamp_message_SUITE).
-include("wamp.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).



all() ->
    common:all().

groups() ->
    [{main, [parallel], common:tests(?MODULE)}].




%% =============================================================================
%% JSON
%% =============================================================================


init_per_suite(Config) ->
    _ = application:ensure_all_started(wamp),
    ok = wamp_config:init(),
    Config.

end_per_suite(_) ->
    ok.


abort_test(_) ->
    Uri = <<"com.example.foo">>,
    Details = #{bar => baz},
    Expected = #abort{reason_uri = Uri, details = Details},

    ?assertEqual(Expected, wamp_message:abort(Details, Uri)).



call_test(_) ->
    Uri = <<"com.example.foo">>,

    Opts0 = #{},
    ?assertEqual(
        #call{
            request_id = 1,
            options = Opts0,
            procedure_uri = Uri,
            args = undefined,
            kwargs = undefined
        },
        wamp_message:call(1, Opts0, Uri, [])
    ),

    Opts1 = #{ppt_scheme => <<"foo">>},
    ?assertError(
        badarg,
        wamp_message:call(1, Opts1, Uri, [], #{}),
        "We should have Args = [Payload :: binary()]"
    ),
    ?assertError(
        badarg,
        wamp_message:call(1, Opts1, Uri, [], #{a => 100}),
        "KWArgs should be undefined"
    ),
    ?assertError(
        badarg,
        wamp_message:call(1, Opts1, Uri, [1]),
        "Args should be a single binary"
    ),
    ?assertEqual(
        #call{
            request_id = 1,
            options = Opts1,
            procedure_uri = Uri,
            args = [<<>>],
            kwargs = undefined
        },
        wamp_message:call(1, Opts1, Uri, [<<>>])
    ).
