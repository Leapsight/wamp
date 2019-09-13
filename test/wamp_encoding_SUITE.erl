-module(wamp_encoding_SUITE).
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




hello_json_test(_) ->
    M = wamp_message:hello(<<"realm1">>, #{
        <<"roles">> => #{
            <<"caller">> => #{}
        }}),
    Bin = wamp_encoding:encode(M, json),
    hello = wamp_encoding:decode_message_name({ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

welcome_json_test(_) ->
    M = wamp_message:welcome(1, #{
        <<"roles">> => #{
            <<"dealer">> => #{},
            <<"broker">> => #{}
        }}),
    Bin = wamp_encoding:encode(M, json),
    welcome = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

abort_json_test(_) ->
    M = wamp_message:abort(#{message => <<"foo">>}, <<"wamp.error.foo">>),
    Bin = wamp_encoding:encode(M, json),
    abort = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

challenge_json_test(_) ->
    M = wamp_message:challenge(<<"foo">>, #{}),
    Bin = wamp_encoding:encode(M, json),
    challenge = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

authenticate_json_test(_) ->
    M = wamp_message:authenticate(<<"foo">>, #{}),
    Bin = wamp_encoding:encode(M, json),
    authenticate = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

goodbye_json_test(_) ->
    M = wamp_message:goodbye(
        #{message => <<"The host is shutting down now.">>},
        <<"wamp.error.system_shutdown">>
    ),
    Bin = wamp_encoding:encode(M, json),
    goodbye = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).


error_json_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>),
    Bin = wamp_encoding:encode(M, json),
    error = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

error_json_2_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, []),
    Bin = wamp_encoding:encode(M, json),
    error = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

error_json_3_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, [], #{}),Bin = wamp_encoding:encode(M, json),
    error = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

publish_json_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>),Bin = wamp_encoding:encode(M, json),
    publish = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

publish_json_2_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, []),
    Bin = wamp_encoding:encode(M, json),
    publish = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

publish_json_3_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, [], #{}),
    Bin = wamp_encoding:encode(M, json),
    publish = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

published_json_test(_) ->
    M = wamp_message:published(1, 2),
    Bin = wamp_encoding:encode(M, json),
    published = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

subscribe_json_test(_) ->
    M = wamp_message:subscribe(1, #{}, <<"com.leapsight.topic1">>),
    Bin = wamp_encoding:encode(M, json),
    subscribe = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

subscribed_json_test(_) ->
    M = wamp_message:subscribed(1, 3),
    Bin = wamp_encoding:encode(M, json),
    subscribed = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

unsubscribe_json_test(_) ->
    M = wamp_message:unsubscribe(1, 3),
    Bin = wamp_encoding:encode(M, json),
    unsubscribe = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

unsubscribed_json_test(_) ->
    M = wamp_message:unsubscribed(1),
    Bin = wamp_encoding:encode(M, json),
    unsubscribed = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

event_json_test(_) ->
    M = wamp_message:event(3, 2, #{}),
    Bin = wamp_encoding:encode(M, json),
    event = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

event_json_2_test(_) ->
    M = wamp_message:event(3, 2, #{}, []),
    Bin = wamp_encoding:encode(M, json),
    event = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

event_json_3_test(_) ->
    M = wamp_message:event(3, 2, #{}, [], #{}),
    Bin = wamp_encoding:encode(M, json),
    event = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

call_json_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>),
    Bin = wamp_encoding:encode(M, json),
    call = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

call_json_2_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, []),
    Bin = wamp_encoding:encode(M, json),
    call = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

call_json_3_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, [], #{}),
    Bin = wamp_encoding:encode(M, json),
    call = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

cancel_json_test(_) ->
    M = wamp_message:cancel(1, #{}),
    Bin = wamp_encoding:encode(M, json),
    cancel = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

result_json_test(_) ->
    M = wamp_message:result(1, #{}),
    Bin = wamp_encoding:encode(M, json),
    result = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

result_json_2_test(_) ->
    M = wamp_message:result(1, #{}, []),
    Bin = wamp_encoding:encode(M, json),
    result = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

result_json_3_test(_) ->
    M = wamp_message:result(1, #{}, [], #{}),
    Bin = wamp_encoding:encode(M, json),
    result = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

register_json_test(_) ->
    M = wamp_message:register(1, #{}, <<"com.leapsight.myprocedure1">>),
    Bin = wamp_encoding:encode(M, json),
    register = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

registered_json_2_test(_) ->
    M = wamp_message:registered(1, 4),
    Bin = wamp_encoding:encode(M, json),
    registered = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

unregister_json_3_test(_) ->
    M = wamp_message:unregister(1, 4),
    Bin = wamp_encoding:encode(M, json),
    unregister = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

unregistered_json_test(_) ->
    M = wamp_message:unregistered(1),
    Bin = wamp_encoding:encode(M, json),
    unregistered = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

invocation_json_test(_) ->
    M = wamp_message:invocation(1, 4, #{}),
    Bin = wamp_encoding:encode(M, json),
    invocation = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

invocation_json_2_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, []),
    Bin = wamp_encoding:encode(M, json),
    invocation = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

invocation_json_3_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, [], #{}),
    Bin = wamp_encoding:encode(M, json),
    invocation = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

interrupt_json_test(_) ->
    M = wamp_message:interrupt(1, #{}),
    Bin = wamp_encoding:encode(M, json),
    interrupt = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

yield_json_test(_) ->
    M = wamp_message:yield(1, #{}),
    Bin = wamp_encoding:encode(M, json),
    yield = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

yield_json_2_test(_) ->
    M = wamp_message:yield(1, #{}, []),
    Bin = wamp_encoding:encode(M, json),
    yield = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).

yield_json_3_test(_) ->
    M = wamp_message:yield(1, #{}, [], #{}),
    Bin = wamp_encoding:encode(M, json),
    yield = wamp_encoding:decode_message_name(
        {ws, text, json}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, Bin).



%% =============================================================================
%% MSGPACK
%% =============================================================================




hello_msgpack_test(_) ->
    M = wamp_message:hello(<<"realm1">>, #{
        <<"roles">> => #{
            <<"caller">> => #{}
        }}),
    Bin = wamp_encoding:encode(M, msgpack),
    hello = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode(
        {ws, binary, msgpack}, Bin).

welcome_msgpack_test(_) ->
    M = wamp_message:welcome(1, #{
        <<"roles">> => #{
            <<"dealer">> => #{},
            <<"broker">> => #{}
        }}),
    Bin = wamp_encoding:encode(M, msgpack),
    welcome = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

abort_msgpack_test(_) ->
    M = wamp_message:abort(#{message => <<"foo">>}, <<"wamp.error.foo">>),
    Bin = wamp_encoding:encode(M, msgpack),
    abort = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

challenge_msgpack_test(_) ->
    M = wamp_message:challenge(<<"foo">>, #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    challenge = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

authenticate_msgpack_test(_) ->
    M = wamp_message:authenticate(<<"foo">>, #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    authenticate = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

goodbye_msgpack_test(_) ->
    M = wamp_message:goodbye(
        #{message => <<"The host is shutting down now.">>},
        <<"wamp.error.system_shutdown">>
    ),
    Bin = wamp_encoding:encode(M, msgpack),
    goodbye = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).


error_msgpack_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>),
    Bin = wamp_encoding:encode(M, msgpack),
    error = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

error_msgpack_2_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, []),
    Bin = wamp_encoding:encode(M, msgpack),
    error = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

error_msgpack_3_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, [], #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    error = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

publish_msgpack_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>),
    Bin = wamp_encoding:encode(M, msgpack),
    publish = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

publish_msgpack_2_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, []),
    Bin = wamp_encoding:encode(M, msgpack),
    publish = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

publish_msgpack_3_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, [], #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    publish = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

published_msgpack_test(_) ->
    M = wamp_message:published(1, 2),
    Bin = wamp_encoding:encode(M, msgpack),
    published = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

subscribe_msgpack_test(_) ->
    M = wamp_message:subscribe(1, #{}, <<"com.leapsight.topic1">>),
    Bin = wamp_encoding:encode(M, msgpack),
    subscribe = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

subscribed_msgpack_test(_) ->
    M = wamp_message:subscribed(1, 3),
    Bin = wamp_encoding:encode(M, msgpack),
    subscribed = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

unsubscribe_msgpack_test(_) ->
    M = wamp_message:unsubscribe(1, 3),
    Bin = wamp_encoding:encode(M, msgpack),
    unsubscribe = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

unsubscribed_msgpack_test(_) ->
    M = wamp_message:unsubscribed(1),
    Bin = wamp_encoding:encode(M, msgpack),
    unsubscribed = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

event_msgpack_test(_) ->
    M = wamp_message:event(3, 2, #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    event = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

event_msgpack_2_test(_) ->
    M = wamp_message:event(3, 2, #{}, []),
    Bin = wamp_encoding:encode(M, msgpack),
    event = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

event_msgpack_3_test(_) ->
    M = wamp_message:event(3, 2, #{}, [], #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    event = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

call_msgpack_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>),
    Bin = wamp_encoding:encode(M, msgpack),
    call = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

call_msgpack_2_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, []),
    Bin = wamp_encoding:encode(M, msgpack),
    call = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

call_msgpack_3_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, [], #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    call = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

cancel_msgpack_test(_) ->
    M = wamp_message:cancel(1, #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    cancel = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

result_msgpack_test(_) ->
    M = wamp_message:result(1, #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    result = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

result_msgpack_2_test(_) ->
    M = wamp_message:result(1, #{}, []),
    Bin = wamp_encoding:encode(M, msgpack),
    result = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

result_msgpack_3_test(_) ->
    M = wamp_message:result(1, #{}, [], #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    result = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

register_msgpack_test(_) ->
    M = wamp_message:register(1, #{}, <<"com.leapsight.myprocedure1">>),
    Bin = wamp_encoding:encode(M, msgpack),
    register = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

registered_msgpack_2_test(_) ->
    M = wamp_message:registered(1, 4),
    Bin = wamp_encoding:encode(M, msgpack),
    registered = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

unregister_msgpack_3_test(_) ->
    M = wamp_message:unregister(1, 4),
    Bin = wamp_encoding:encode(M, msgpack),
    unregister = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

unregistered_msgpack_test(_) ->
    M = wamp_message:unregistered(1),
    Bin = wamp_encoding:encode(M, msgpack),
    unregistered = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

invocation_msgpack_test(_) ->
    M = wamp_message:invocation(1, 4, #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    invocation = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

invocation_msgpack_2_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, []),
    Bin = wamp_encoding:encode(M, msgpack),
    invocation = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

invocation_msgpack_3_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, [], #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    invocation = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

interrupt_msgpack_test(_) ->
    M = wamp_message:interrupt(1, #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    interrupt = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

yield_msgpack_test(_) ->
    M = wamp_message:yield(1, #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    yield = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

yield_msgpack_2_test(_) ->
    M = wamp_message:yield(1, #{}, []),
    Bin = wamp_encoding:encode(M, msgpack),
    yield = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).

yield_msgpack_3_test(_) ->
    M = wamp_message:yield(1, #{}, [], #{}),
    Bin = wamp_encoding:encode(M, msgpack),
    yield = wamp_encoding:decode_message_name(
        {ws, binary, msgpack}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, Bin).



%% =============================================================================
%% BERT
%% =============================================================================




hello_bert_test(_) ->
    M = wamp_message:hello(<<"realm1">>, #{
        <<"roles">> => #{
            <<"caller">> => #{}
        }}),
    {[M], <<>>} = wamp_encoding:decode(
        {ws, binary, bert}, wamp_encoding:encode(M, bert)).

welcome_bert_test(_) ->
    M = wamp_message:welcome(1, #{
        <<"roles">> => #{
            <<"dealer">> => #{},
            <<"broker">> => #{}
        }}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

abort_bert_test(_) ->
    M = wamp_message:abort(#{message => <<"foo">>}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

challenge_bert_test(_) ->
    M = wamp_message:challenge(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

authenticate_bert_test(_) ->
    M = wamp_message:authenticate(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

goodbye_bert_test(_) ->
    M = wamp_message:goodbye(
        #{message => <<"The host is shutting down now.">>},
        <<"wamp.error.system_shutdown">>
    ),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).


error_bert_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

error_bert_2_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

error_bert_3_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

publish_bert_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

publish_bert_2_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

publish_bert_3_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

published_bert_test(_) ->
    M = wamp_message:published(1, 2),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

subscribe_bert_test(_) ->
    M = wamp_message:subscribe(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

subscribed_bert_test(_) ->
    M = wamp_message:subscribed(1, 3),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

unsubscribe_bert_test(_) ->
    M = wamp_message:unsubscribe(1, 3),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

unsubscribed_bert_test(_) ->
    M = wamp_message:unsubscribed(1),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

event_bert_test(_) ->
    M = wamp_message:event(3, 2, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

event_bert_2_test(_) ->
    M = wamp_message:event(3, 2, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

event_bert_3_test(_) ->
    M = wamp_message:event(3, 2, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

call_bert_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

call_bert_2_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

call_bert_3_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

cancel_bert_test(_) ->
    M = wamp_message:cancel(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

result_bert_test(_) ->
    M = wamp_message:result(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

result_bert_2_test(_) ->
    M = wamp_message:result(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

result_bert_3_test(_) ->
    M = wamp_message:result(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

register_bert_test(_) ->
    M = wamp_message:register(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

registered_bert_2_test(_) ->
    M = wamp_message:registered(1, 4),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

unregister_bert_3_test(_) ->
    M = wamp_message:unregister(1, 4),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

unregistered_bert_test(_) ->
    M = wamp_message:unregistered(1),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

invocation_bert_test(_) ->
    M = wamp_message:invocation(1, 4, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

invocation_bert_2_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

invocation_bert_3_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

interrupt_bert_test(_) ->
    M = wamp_message:interrupt(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

yield_bert_test(_) ->
    M = wamp_message:yield(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

yield_bert_2_test(_) ->
    M = wamp_message:yield(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

yield_bert_3_test(_) ->
    M = wamp_message:yield(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, bert}, wamp_encoding:encode(M, bert)).

%% =============================================================================
%% ERL
%% =============================================================================


hello_erl_test(_) ->
    M = wamp_message:hello(<<"realm1">>, #{
        <<"roles">> => #{
            <<"caller">> => #{}
        }}),
    Bin = wamp_encoding:encode(M, erl),
    hello = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

welcome_erl_test(_) ->
    M = wamp_message:welcome(1, #{
        <<"roles">> => #{
            <<"dealer">> => #{},
            <<"broker">> => #{}
        }}),
    Bin = wamp_encoding:encode(M, erl),
    welcome = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

abort_erl_test(_) ->
    M = wamp_message:abort(#{message => <<"foo">>}, <<"wamp.error.foo">>),
    Bin = wamp_encoding:encode(M, erl),
    abort = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

challenge_erl_test(_) ->
    M = wamp_message:challenge(<<"foo">>, #{}),
    Bin = wamp_encoding:encode(M, erl),
    challenge = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

authenticate_erl_test(_) ->
    M = wamp_message:authenticate(<<"foo">>, #{}),
    Bin = wamp_encoding:encode(M, erl),
    authenticate = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

goodbye_erl_test(_) ->
    M = wamp_message:goodbye(
        #{message => <<"The host is shutting down now.">>},
        <<"wamp.error.system_shutdown">>
    ),
    Bin = wamp_encoding:encode(M, erl),
    goodbye = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).


error_erl_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>),
    Bin = wamp_encoding:encode(M, erl),
    error = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

error_erl_2_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, []),
    Bin = wamp_encoding:encode(M, erl),
    error = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

error_erl_3_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, [], #{}),
    Bin = wamp_encoding:encode(M, erl),
    error = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

publish_erl_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>),
    Bin = wamp_encoding:encode(M, erl),
    publish = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

publish_erl_2_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, []),
    Bin = wamp_encoding:encode(M, erl),
    publish = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

publish_erl_3_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, [], #{}),
    Bin = wamp_encoding:encode(M, erl),
    publish = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

published_erl_test(_) ->
    M = wamp_message:published(1, 2),
    Bin = wamp_encoding:encode(M, erl),
    published = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

subscribe_erl_test(_) ->
    M = wamp_message:subscribe(1, #{}, <<"com.leapsight.topic1">>),
    Bin = wamp_encoding:encode(M, erl),
    subscribe = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

subscribed_erl_test(_) ->
    M = wamp_message:subscribed(1, 3),
    Bin = wamp_encoding:encode(M, erl),
    subscribed = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

unsubscribe_erl_test(_) ->
    M = wamp_message:unsubscribe(1, 3),
    Bin = wamp_encoding:encode(M, erl),
    unsubscribe = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

unsubscribed_erl_test(_) ->
    M = wamp_message:unsubscribed(1),
    Bin = wamp_encoding:encode(M, erl),
    unsubscribed = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

event_erl_test(_) ->
    M = wamp_message:event(3, 2, #{}),
    Bin = wamp_encoding:encode(M, erl),
    event = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

event_erl_2_test(_) ->
    M = wamp_message:event(3, 2, #{}, []),
    Bin = wamp_encoding:encode(M, erl),
    event = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

event_erl_3_test(_) ->
    M = wamp_message:event(3, 2, #{}, [], #{}),
    Bin = wamp_encoding:encode(M, erl),
    event = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

call_erl_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>),
    Bin = wamp_encoding:encode(M, erl),
    call = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

call_erl_2_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, []),
    Bin = wamp_encoding:encode(M, erl),
    call = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

call_erl_3_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, [], #{}),
    Bin = wamp_encoding:encode(M, erl),
    call = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

cancel_erl_test(_) ->
    M = wamp_message:cancel(1, #{}),
    Bin = wamp_encoding:encode(M, erl),
    cancel = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

result_erl_test(_) ->
    M = wamp_message:result(1, #{}),
    Bin = wamp_encoding:encode(M, erl),
    result = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

result_erl_2_test(_) ->
    M = wamp_message:result(1, #{}, []),
    Bin = wamp_encoding:encode(M, erl),
    result = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

result_erl_3_test(_) ->
    M = wamp_message:result(1, #{}, [], #{}),
    Bin = wamp_encoding:encode(M, erl),
    result = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

register_erl_test(_) ->
    M = wamp_message:register(1, #{}, <<"com.leapsight.myprocedure1">>),
    Bin = wamp_encoding:encode(M, erl),
    register = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

registered_erl_2_test(_) ->
    M = wamp_message:registered(1, 4),
    Bin = wamp_encoding:encode(M, erl),
    registered = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

unregister_erl_3_test(_) ->
    M = wamp_message:unregister(1, 4),
    Bin = wamp_encoding:encode(M, erl),
    unregister = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

unregistered_erl_test(_) ->
    M = wamp_message:unregistered(1),
    Bin = wamp_encoding:encode(M, erl),
    unregistered = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

invocation_erl_test(_) ->
    M = wamp_message:invocation(1, 4, #{}),
    Bin = wamp_encoding:encode(M, erl),
    invocation = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

invocation_erl_2_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, []),
    Bin = wamp_encoding:encode(M, erl),
    invocation = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

invocation_erl_3_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, [], #{}),
    Bin = wamp_encoding:encode(M, erl),
    invocation = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

interrupt_erl_test(_) ->
    M = wamp_message:interrupt(1, #{}),
    Bin = wamp_encoding:encode(M, erl),
    interrupt = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

yield_erl_test(_) ->
    M = wamp_message:yield(1, #{}),
    Bin = wamp_encoding:encode(M, erl),
    yield = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

yield_erl_2_test(_) ->
    M = wamp_message:yield(1, #{}, []),
    Bin = wamp_encoding:encode(M, erl),
    yield = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).

yield_erl_3_test(_) ->
    M = wamp_message:yield(1, #{}, [], #{}),
    Bin = wamp_encoding:encode(M, erl),
    yield = wamp_encoding:decode_message_name(
        {ws, binary, erl}, Bin),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, Bin).



%% =============================================================================
%% EXTENSION KEYS VALIDATION
%% =============================================================================


validate_extension_key_1_test(_) ->
    Keys = [
        '_xyz1',
        '_xyz2',
        '_xyz3',
        %% This should be ignored
        '_foo',
        'bar'
    ],
    Allowed =[{yield, Keys}],
    ok = app_config:set(wamp, extended_options, Allowed),
    Keys = app_config:get(wamp, [extended_options, yield]),

    Expected = #{
        '_xyz1' => 1,
        '_xyz2' => 2,
        '_xyz3' => 3
    },
    Opts = #{
        <<"_xyz1">> => 1,
        <<"_xyz2">> => 2,
        <<"_xyz3">> => 3,
        %% This should be removed
        <<"foo">> => 1,
        <<"_bar">> => 1,
        <<"_x">> => 1
    },

    M1 = wamp_message:yield(1, Opts),
    ?assertEqual(Expected, wamp_message:options(M1)),

    M2 = wamp_message:cancel(1, Opts),
    ?assertEqual(maps:new(), wamp_message:options(M2)).