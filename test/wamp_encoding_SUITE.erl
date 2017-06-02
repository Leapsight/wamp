-module(wamp_encoding_SUITE).
-include_lib("common_test/include/ct.hrl").
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
    {[M], <<>>} = wamp_encoding:decode(
        wamp_encoding:encode(M, json), text, json).

welcome_json_test(_) ->
    M = wamp_message:welcome(1, #{
        <<"roles">> => #{
            <<"dealer">> => #{},
            <<"broker">> => #{}
        }}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

abort_json_test(_) ->
    M = wamp_message:abort(#{message => <<"foo">>}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

challenge_json_test(_) ->
    M = wamp_message:challenge(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

authenticate_json_test(_) ->
    M = wamp_message:authenticate(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

goodbye_json_test(_) ->
    M = wamp_message:goodbye(
        #{message => <<"The host is shutting down now.">>},
        <<"wamp.error.system_shutdown">>
    ),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).


error_json_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

error_json_2_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

error_json_3_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

publish_json_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

publish_json_2_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

publish_json_3_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

published_json_test(_) ->
    M = wamp_message:published(1, 2),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

subscribe_json_test(_) ->
    M = wamp_message:subscribe(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

subscribed_json_test(_) ->
    M = wamp_message:subscribed(1, 3),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

unsubscribe_json_test(_) ->
    M = wamp_message:unsubscribe(1, 3),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

unsubscribed_json_test(_) ->
    M = wamp_message:unsubscribed(1),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

event_json_test(_) ->
    M = wamp_message:event(3, 2, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

event_json_2_test(_) ->
    M = wamp_message:event(3, 2, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

event_json_3_test(_) ->
    M = wamp_message:event(3, 2, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

call_json_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

call_json_2_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

call_json_3_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

cancel_json_test(_) ->
    M = wamp_message:cancel(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

result_json_test(_) ->
    M = wamp_message:result(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

result_json_2_test(_) ->
    M = wamp_message:result(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

result_json_3_test(_) ->
    M = wamp_message:result(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

register_json_test(_) ->
    M = wamp_message:register(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

registered_json_2_test(_) ->
    M = wamp_message:registered(1, 4),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

unregister_json_3_test(_) ->
    M = wamp_message:unregister(1, 4),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

unregistered_json_test(_) ->
    M = wamp_message:unregistered(1),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

invocation_json_test(_) ->
    M = wamp_message:invocation(1, 4, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

invocation_json_2_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

invocation_json_3_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

interrupt_json_test(_) ->
    M = wamp_message:interrupt(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

yield_json_test(_) ->
    M = wamp_message:yield(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

yield_json_2_test(_) ->
    M = wamp_message:yield(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).

yield_json_3_test(_) ->
    M = wamp_message:yield(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, json), text, json).



%% =============================================================================
%% MSGPACK
%% =============================================================================




hello_msgpack_test(_) ->
    M = wamp_message:hello(<<"realm1">>, #{
        <<"roles">> => #{
            <<"caller">> => #{}
        }}),
    {[M], <<>>} = wamp_encoding:decode(
        wamp_encoding:encode(M, msgpack), binary, msgpack).

welcome_msgpack_test(_) ->
    M = wamp_message:welcome(1, #{
        <<"roles">> => #{
            <<"dealer">> => #{},
            <<"broker">> => #{}
        }}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

abort_msgpack_test(_) ->
    M = wamp_message:abort(#{message => <<"foo">>}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

challenge_msgpack_test(_) ->
    M = wamp_message:challenge(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

authenticate_msgpack_test(_) ->
    M = wamp_message:authenticate(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

goodbye_msgpack_test(_) ->
    M = wamp_message:goodbye(
        #{message => <<"The host is shutting down now.">>},
        <<"wamp.error.system_shutdown">>
    ),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).


error_msgpack_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

error_msgpack_2_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

error_msgpack_3_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

publish_msgpack_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

publish_msgpack_2_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

publish_msgpack_3_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

published_msgpack_test(_) ->
    M = wamp_message:published(1, 2),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

subscribe_msgpack_test(_) ->
    M = wamp_message:subscribe(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

subscribed_msgpack_test(_) ->
    M = wamp_message:subscribed(1, 3),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

unsubscribe_msgpack_test(_) ->
    M = wamp_message:unsubscribe(1, 3),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

unsubscribed_msgpack_test(_) ->
    M = wamp_message:unsubscribed(1),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

event_msgpack_test(_) ->
    M = wamp_message:event(3, 2, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

event_msgpack_2_test(_) ->
    M = wamp_message:event(3, 2, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

event_msgpack_3_test(_) ->
    M = wamp_message:event(3, 2, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

call_msgpack_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

call_msgpack_2_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

call_msgpack_3_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

cancel_msgpack_test(_) ->
    M = wamp_message:cancel(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

result_msgpack_test(_) ->
    M = wamp_message:result(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

result_msgpack_2_test(_) ->
    M = wamp_message:result(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

result_msgpack_3_test(_) ->
    M = wamp_message:result(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

register_msgpack_test(_) ->
    M = wamp_message:register(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

registered_msgpack_2_test(_) ->
    M = wamp_message:registered(1, 4),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

unregister_msgpack_3_test(_) ->
    M = wamp_message:unregister(1, 4),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

unregistered_msgpack_test(_) ->
    M = wamp_message:unregistered(1),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

invocation_msgpack_test(_) ->
    M = wamp_message:invocation(1, 4, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

invocation_msgpack_2_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

invocation_msgpack_3_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

interrupt_msgpack_test(_) ->
    M = wamp_message:interrupt(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

yield_msgpack_test(_) ->
    M = wamp_message:yield(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

yield_msgpack_2_test(_) ->
    M = wamp_message:yield(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).

yield_msgpack_3_test(_) ->
    M = wamp_message:yield(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, msgpack), binary, msgpack).



%% =============================================================================
%% BERT
%% =============================================================================




hello_bert_test(_) ->
    M = wamp_message:hello(<<"realm1">>, #{
        <<"roles">> => #{
            <<"caller">> => #{}
        }}),
    {[M], <<>>} = wamp_encoding:decode(
        wamp_encoding:encode(M, bert), binary, bert).

welcome_bert_test(_) ->
    M = wamp_message:welcome(1, #{
        <<"roles">> => #{
            <<"dealer">> => #{},
            <<"broker">> => #{}
        }}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

abort_bert_test(_) ->
    M = wamp_message:abort(#{message => <<"foo">>}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

challenge_bert_test(_) ->
    M = wamp_message:challenge(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

authenticate_bert_test(_) ->
    M = wamp_message:authenticate(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

goodbye_bert_test(_) ->
    M = wamp_message:goodbye(
        #{message => <<"The host is shutting down now.">>},
        <<"wamp.error.system_shutdown">>
    ),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).


error_bert_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

error_bert_2_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

error_bert_3_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

publish_bert_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

publish_bert_2_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

publish_bert_3_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

published_bert_test(_) ->
    M = wamp_message:published(1, 2),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

subscribe_bert_test(_) ->
    M = wamp_message:subscribe(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

subscribed_bert_test(_) ->
    M = wamp_message:subscribed(1, 3),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

unsubscribe_bert_test(_) ->
    M = wamp_message:unsubscribe(1, 3),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

unsubscribed_bert_test(_) ->
    M = wamp_message:unsubscribed(1),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

event_bert_test(_) ->
    M = wamp_message:event(3, 2, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

event_bert_2_test(_) ->
    M = wamp_message:event(3, 2, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

event_bert_3_test(_) ->
    M = wamp_message:event(3, 2, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

call_bert_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

call_bert_2_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

call_bert_3_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

cancel_bert_test(_) ->
    M = wamp_message:cancel(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

result_bert_test(_) ->
    M = wamp_message:result(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

result_bert_2_test(_) ->
    M = wamp_message:result(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

result_bert_3_test(_) ->
    M = wamp_message:result(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

register_bert_test(_) ->
    M = wamp_message:register(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

registered_bert_2_test(_) ->
    M = wamp_message:registered(1, 4),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

unregister_bert_3_test(_) ->
    M = wamp_message:unregister(1, 4),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

unregistered_bert_test(_) ->
    M = wamp_message:unregistered(1),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

invocation_bert_test(_) ->
    M = wamp_message:invocation(1, 4, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

invocation_bert_2_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

invocation_bert_3_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

interrupt_bert_test(_) ->
    M = wamp_message:interrupt(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

yield_bert_test(_) ->
    M = wamp_message:yield(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

yield_bert_2_test(_) ->
    M = wamp_message:yield(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

yield_bert_3_test(_) ->
    M = wamp_message:yield(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, bert), binary, bert).

%% =============================================================================
%% ERL
%% =============================================================================


hello_erl_test(_) ->
    M = wamp_message:hello(<<"realm1">>, #{
        <<"roles">> => #{
            <<"caller">> => #{}
        }}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

welcome_erl_test(_) ->
    M = wamp_message:welcome(1, #{
        <<"roles">> => #{
            <<"dealer">> => #{},
            <<"broker">> => #{}
        }}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

abort_erl_test(_) ->
    M = wamp_message:abort(#{message => <<"foo">>}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

challenge_erl_test(_) ->
    M = wamp_message:challenge(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

authenticate_erl_test(_) ->
    M = wamp_message:authenticate(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

goodbye_erl_test(_) ->
    M = wamp_message:goodbye(
        #{message => <<"The host is shutting down now.">>},
        <<"wamp.error.system_shutdown">>
    ),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).


error_erl_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

error_erl_2_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

error_erl_3_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

publish_erl_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

publish_erl_2_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

publish_erl_3_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

published_erl_test(_) ->
    M = wamp_message:published(1, 2),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

subscribe_erl_test(_) ->
    M = wamp_message:subscribe(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

subscribed_erl_test(_) ->
    M = wamp_message:subscribed(1, 3),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

unsubscribe_erl_test(_) ->
    M = wamp_message:unsubscribe(1, 3),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

unsubscribed_erl_test(_) ->
    M = wamp_message:unsubscribed(1),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

event_erl_test(_) ->
    M = wamp_message:event(3, 2, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

event_erl_2_test(_) ->
    M = wamp_message:event(3, 2, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

event_erl_3_test(_) ->
    M = wamp_message:event(3, 2, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

call_erl_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

call_erl_2_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

call_erl_3_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

cancel_erl_test(_) ->
    M = wamp_message:cancel(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

result_erl_test(_) ->
    M = wamp_message:result(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

result_erl_2_test(_) ->
    M = wamp_message:result(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

result_erl_3_test(_) ->
    M = wamp_message:result(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

register_erl_test(_) ->
    M = wamp_message:register(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

registered_erl_2_test(_) ->
    M = wamp_message:registered(1, 4),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

unregister_erl_3_test(_) ->
    M = wamp_message:unregister(1, 4),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

unregistered_erl_test(_) ->
    M = wamp_message:unregistered(1),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

invocation_erl_test(_) ->
    M = wamp_message:invocation(1, 4, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

invocation_erl_2_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

invocation_erl_3_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

interrupt_erl_test(_) ->
    M = wamp_message:interrupt(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

yield_erl_test(_) ->
    M = wamp_message:yield(1, #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

yield_erl_2_test(_) ->
    M = wamp_message:yield(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).

yield_erl_3_test(_) ->
    M = wamp_message:yield(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode(wamp_encoding:encode(M, erl), binary, erl).
