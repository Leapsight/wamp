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
        {ws, text, json}, wamp_encoding:encode(M, json)).

welcome_json_test(_) ->
    M = wamp_message:welcome(1, #{
        <<"roles">> => #{
            <<"dealer">> => #{},
            <<"broker">> => #{}
        }}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

abort_json_test(_) ->
    M = wamp_message:abort(#{message => <<"foo">>}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

challenge_json_test(_) ->
    M = wamp_message:challenge(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

authenticate_json_test(_) ->
    M = wamp_message:authenticate(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

goodbye_json_test(_) ->
    M = wamp_message:goodbye(
        #{message => <<"The host is shutting down now.">>},
        <<"wamp.error.system_shutdown">>
    ),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).


error_json_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

error_json_2_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, []),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

error_json_3_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

publish_json_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

publish_json_2_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, []),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

publish_json_3_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

published_json_test(_) ->
    M = wamp_message:published(1, 2),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

subscribe_json_test(_) ->
    M = wamp_message:subscribe(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

subscribed_json_test(_) ->
    M = wamp_message:subscribed(1, 3),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

unsubscribe_json_test(_) ->
    M = wamp_message:unsubscribe(1, 3),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

unsubscribed_json_test(_) ->
    M = wamp_message:unsubscribed(1),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

event_json_test(_) ->
    M = wamp_message:event(3, 2, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

event_json_2_test(_) ->
    M = wamp_message:event(3, 2, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

event_json_3_test(_) ->
    M = wamp_message:event(3, 2, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

call_json_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

call_json_2_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, []),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

call_json_3_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

cancel_json_test(_) ->
    M = wamp_message:cancel(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

result_json_test(_) ->
    M = wamp_message:result(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

result_json_2_test(_) ->
    M = wamp_message:result(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

result_json_3_test(_) ->
    M = wamp_message:result(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

register_json_test(_) ->
    M = wamp_message:register(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

registered_json_2_test(_) ->
    M = wamp_message:registered(1, 4),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

unregister_json_3_test(_) ->
    M = wamp_message:unregister(1, 4),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

unregistered_json_test(_) ->
    M = wamp_message:unregistered(1),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

invocation_json_test(_) ->
    M = wamp_message:invocation(1, 4, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

invocation_json_2_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

invocation_json_3_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

interrupt_json_test(_) ->
    M = wamp_message:interrupt(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

yield_json_test(_) ->
    M = wamp_message:yield(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

yield_json_2_test(_) ->
    M = wamp_message:yield(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).

yield_json_3_test(_) ->
    M = wamp_message:yield(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, text, json}, wamp_encoding:encode(M, json)).



%% =============================================================================
%% MSGPACK
%% =============================================================================




hello_msgpack_test(_) ->
    M = wamp_message:hello(<<"realm1">>, #{
        <<"roles">> => #{
            <<"caller">> => #{}
        }}),
    {[M], <<>>} = wamp_encoding:decode(
        {ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

welcome_msgpack_test(_) ->
    M = wamp_message:welcome(1, #{
        <<"roles">> => #{
            <<"dealer">> => #{},
            <<"broker">> => #{}
        }}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

abort_msgpack_test(_) ->
    M = wamp_message:abort(#{message => <<"foo">>}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

challenge_msgpack_test(_) ->
    M = wamp_message:challenge(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

authenticate_msgpack_test(_) ->
    M = wamp_message:authenticate(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

goodbye_msgpack_test(_) ->
    M = wamp_message:goodbye(
        #{message => <<"The host is shutting down now.">>},
        <<"wamp.error.system_shutdown">>
    ),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).


error_msgpack_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

error_msgpack_2_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

error_msgpack_3_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

publish_msgpack_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

publish_msgpack_2_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

publish_msgpack_3_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

published_msgpack_test(_) ->
    M = wamp_message:published(1, 2),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

subscribe_msgpack_test(_) ->
    M = wamp_message:subscribe(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

subscribed_msgpack_test(_) ->
    M = wamp_message:subscribed(1, 3),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

unsubscribe_msgpack_test(_) ->
    M = wamp_message:unsubscribe(1, 3),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

unsubscribed_msgpack_test(_) ->
    M = wamp_message:unsubscribed(1),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

event_msgpack_test(_) ->
    M = wamp_message:event(3, 2, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

event_msgpack_2_test(_) ->
    M = wamp_message:event(3, 2, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

event_msgpack_3_test(_) ->
    M = wamp_message:event(3, 2, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

call_msgpack_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

call_msgpack_2_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

call_msgpack_3_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

cancel_msgpack_test(_) ->
    M = wamp_message:cancel(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

result_msgpack_test(_) ->
    M = wamp_message:result(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

result_msgpack_2_test(_) ->
    M = wamp_message:result(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

result_msgpack_3_test(_) ->
    M = wamp_message:result(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

register_msgpack_test(_) ->
    M = wamp_message:register(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

registered_msgpack_2_test(_) ->
    M = wamp_message:registered(1, 4),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

unregister_msgpack_3_test(_) ->
    M = wamp_message:unregister(1, 4),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

unregistered_msgpack_test(_) ->
    M = wamp_message:unregistered(1),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

invocation_msgpack_test(_) ->
    M = wamp_message:invocation(1, 4, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

invocation_msgpack_2_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

invocation_msgpack_3_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

interrupt_msgpack_test(_) ->
    M = wamp_message:interrupt(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

yield_msgpack_test(_) ->
    M = wamp_message:yield(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

yield_msgpack_2_test(_) ->
    M = wamp_message:yield(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).

yield_msgpack_3_test(_) ->
    M = wamp_message:yield(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, msgpack}, wamp_encoding:encode(M, msgpack)).



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
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

welcome_erl_test(_) ->
    M = wamp_message:welcome(1, #{
        <<"roles">> => #{
            <<"dealer">> => #{},
            <<"broker">> => #{}
        }}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

abort_erl_test(_) ->
    M = wamp_message:abort(#{message => <<"foo">>}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

challenge_erl_test(_) ->
    M = wamp_message:challenge(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

authenticate_erl_test(_) ->
    M = wamp_message:authenticate(<<"foo">>, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

goodbye_erl_test(_) ->
    M = wamp_message:goodbye(
        #{message => <<"The host is shutting down now.">>},
        <<"wamp.error.system_shutdown">>
    ),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).


error_erl_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

error_erl_2_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

error_erl_3_test(_) ->
    M = wamp_message:error(0, 1, #{}, <<"wamp.error.foo">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

publish_erl_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

publish_erl_2_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

publish_erl_3_test(_) ->
    M = wamp_message:publish(1, #{}, <<"com.leapsight.topic1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

published_erl_test(_) ->
    M = wamp_message:published(1, 2),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

subscribe_erl_test(_) ->
    M = wamp_message:subscribe(1, #{}, <<"com.leapsight.topic1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

subscribed_erl_test(_) ->
    M = wamp_message:subscribed(1, 3),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

unsubscribe_erl_test(_) ->
    M = wamp_message:unsubscribe(1, 3),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

unsubscribed_erl_test(_) ->
    M = wamp_message:unsubscribed(1),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

event_erl_test(_) ->
    M = wamp_message:event(3, 2, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

event_erl_2_test(_) ->
    M = wamp_message:event(3, 2, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

event_erl_3_test(_) ->
    M = wamp_message:event(3, 2, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

call_erl_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

call_erl_2_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

call_erl_3_test(_) ->
    M = wamp_message:call(1, #{}, <<"com.leapsight.myprocedure1">>, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

cancel_erl_test(_) ->
    M = wamp_message:cancel(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

result_erl_test(_) ->
    M = wamp_message:result(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

result_erl_2_test(_) ->
    M = wamp_message:result(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

result_erl_3_test(_) ->
    M = wamp_message:result(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

register_erl_test(_) ->
    M = wamp_message:register(1, #{}, <<"com.leapsight.myprocedure1">>),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

registered_erl_2_test(_) ->
    M = wamp_message:registered(1, 4),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

unregister_erl_3_test(_) ->
    M = wamp_message:unregister(1, 4),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

unregistered_erl_test(_) ->
    M = wamp_message:unregistered(1),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

invocation_erl_test(_) ->
    M = wamp_message:invocation(1, 4, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

invocation_erl_2_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

invocation_erl_3_test(_) ->
    M = wamp_message:invocation(1, 4, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

interrupt_erl_test(_) ->
    M = wamp_message:interrupt(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

yield_erl_test(_) ->
    M = wamp_message:yield(1, #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

yield_erl_2_test(_) ->
    M = wamp_message:yield(1, #{}, []),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).

yield_erl_3_test(_) ->
    M = wamp_message:yield(1, #{}, [], #{}),
    {[M], <<>>} = wamp_encoding:decode({ws, binary, erl}, wamp_encoding:encode(M, erl)).
