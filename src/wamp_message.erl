%% -----------------------------------------------------------------------------
%% Copyright (C) Ngineo Limited 2015 - 2017. All rights reserved.
%% -----------------------------------------------------------------------------

%% =============================================================================
%% @doc
%%
%% @end
%% =============================================================================
-module(wamp_message).
-include("wamp.hrl").

-export([abort/2]).
-export([authenticate/2]).
-export([call/3, call/4, call/5]).
-export([cancel/2]).
-export([challenge/2]).
-export([error/4, error/5, error/6]).
-export([event/3, event/4, event/5]).
-export([goodbye/2]).
-export([hello/2]).
-export([interrupt/2]).
-export([invocation/3, invocation/4, invocation/5]).
-export([is_message/1]).
-export([publish/3, publish/4, publish/5]).
-export([published/2]).
-export([register/3]).
-export([registered/2]).
-export([result/2, result/3, result/4]).
-export([subscribe/3]).
-export([subscribed/2]).
-export([unregister/2]).
-export([unregistered/1]).
-export([unsubscribe/2]).
-export([unsubscribed/1]).
-export([welcome/2]).
-export([yield/2, yield/3, yield/4]).






%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec is_message(any()) -> boolean().
is_message(Term) when is_record(Term, hello) -> true;
is_message(Term) when is_record(Term, welcome) -> true;
is_message(Term) when is_record(Term, abort) -> true;
is_message(Term) when is_record(Term, challenge) -> true;
is_message(Term) when is_record(Term, authenticate) -> true;
is_message(Term) when is_record(Term, goodbye) -> true;
is_message(Term) when is_record(Term, error) -> true;
is_message(Term) when is_record(Term, publish) -> true;
is_message(Term) when is_record(Term, published) -> true;
is_message(Term) when is_record(Term, subscribe) -> true;
is_message(Term) when is_record(Term, subscribed) -> true;
is_message(Term) when is_record(Term, unsubscribe) -> true;
is_message(Term) when is_record(Term, unsubscribed) -> true;
is_message(Term) when is_record(Term, event) -> true;
is_message(Term) when is_record(Term, call) -> true;
is_message(Term) when is_record(Term, result) -> true;
is_message(Term) when is_record(Term, register) -> true;
is_message(Term) when is_record(Term, registered) -> true;
is_message(Term) when is_record(Term, unregister) -> true;
is_message(Term) when is_record(Term, unregistered) -> true;
is_message(Term) when is_record(Term, invocation) -> true;
is_message(Term) when is_record(Term, yield) -> true;
is_message(_) -> false.


%% -----------------------------------------------------------------------------
%% @doc
%% If Details argument is not valid fails with an exception
%% @end
%% -----------------------------------------------------------------------------
-spec hello(uri(), map()) -> #hello{} | no_return().
hello(RealmUri, Details) when is_binary(RealmUri) ->
    #hello{
        realm_uri = RealmUri,
        details = validate_map(Details, ?HELLO_DETAILS_SPEC)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec welcome(id(), map()) -> #welcome{} | no_return().
welcome(SessionId, Details)   ->
    #welcome{
        session_id = SessionId,
        details = validate_map(Details, ?WELCOME_DETAILS_SPEC)
    }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
%% "ABORT" gets sent only _before_ a _Session_ is established, while
-spec abort(map(), uri()) -> #abort{} | no_return().
abort(Details, ReasonUri) ->
    #abort{
        details = validate_map(Details, ?ABORT_DETAILS_SPEC),
        reason_uri = ReasonUri
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec challenge(binary(), map()) -> #challenge{} | no_return().
challenge(AuthMethod, Extra) ->
    #challenge{
        auth_method = AuthMethod,
        extra = Extra
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec authenticate(binary(), map()) -> #authenticate{} | no_return().
authenticate(Signature, Extra) ->
    #authenticate{
        signature = Signature,
        extra = Extra
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
%% "GOODBYE" is sent only _after_ a _Session_ is already established.
-spec goodbye(map(), uri()) -> #goodbye{} | no_return().
goodbye(Details, ReasonUri) ->
    #goodbye{
        details = validate_map(Details, ?GOODBYE_DETAILS_SPEC),
        reason_uri = ReasonUri
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error(pos_integer(), id(), map(), uri()) -> #error{} | no_return().
error(ReqType, ReqId, Details, ErrorUri) ->
    error(ReqType, ReqId, Details, ErrorUri, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error(pos_integer(), id(), map(), uri(), list()) -> #error{} | no_return().
error(ReqType, ReqId, Details, ErrorUri, Args) ->
    error(ReqType, ReqId, Details, ErrorUri, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error(pos_integer(), id(), map(), uri(), list(), map()) -> #error{} | no_return().
error(ReqType, ReqId, Details, ErrorUri, Args, Payload) ->
    #error{
        request_type = ReqType,
        request_id = ReqId,
        details = Details, % any
        error_uri = ErrorUri,
        arguments = Args,
        payload = Payload
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec publish(id(), map(), uri()) -> #publish{} | no_return().
publish(ReqId, Options, TopicUri) ->
    publish(ReqId, Options, TopicUri, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec publish(id(), map(), uri(), list()) -> #publish{} | no_return().
publish(ReqId, Options, TopicUri, Args) ->
    publish(ReqId, Options, TopicUri, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec publish(id(), map(), uri(), list(), map()) -> #publish{} | no_return().
publish(ReqId, Options, TopicUri, Args, Payload) ->
    #publish{
        request_id = ReqId,
        options = validate_map(Options, ?PUBLISH_OPTS_SPEC),
        topic_uri = TopicUri,
        arguments = Args,
        payload = Payload
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec published(id(), id()) -> #published{} | no_return().
published(ReqId, PubId) ->
    #published{
        request_id = ReqId,
        publication_id = PubId
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec subscribe(id(), map(), uri()) -> #subscribe{} | no_return().
subscribe(ReqId, Options, TopicUri) ->
    #subscribe{
        request_id = ReqId,
        options = Options,
        topic_uri = TopicUri
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec subscribed(id(), id()) -> #subscribed{} | no_return().
subscribed(ReqId, SubsId) ->
    #subscribed{
        request_id = ReqId,
        subscription_id = SubsId
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unsubscribe(id(), id()) -> #unsubscribe{} | no_return().
unsubscribe(ReqId, SubsId) ->
    #unsubscribe{
        request_id = ReqId,
        subscription_id = SubsId
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unsubscribed(id()) -> #unsubscribed{} | no_return().
unsubscribed(ReqId) ->
    #unsubscribed{
        request_id = ReqId
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec event(id(), id(), map()) -> #event{} | no_return().
event(SubsId, PubId, Details) ->
    event(SubsId, PubId, Details, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec event(id(), id(), map(), list()) -> #event{} | no_return().
event(SubsId, PubId, Details, Args) ->
    event(SubsId, PubId, Details, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec event(id(), id(), map(), list(), map()) -> #event{} | no_return().
event(SubsId, PubId, Details, Args, Payload) ->
    #event{
        subscription_id = SubsId,
        publication_id = PubId,
        details = Details,
        arguments = Args,
        payload = Payload
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec call(id(), map(), uri()) -> #call{} | no_return().
call(ReqId, Options, ProcedureUri) ->
    call(ReqId, Options, ProcedureUri, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec call(id(), map(), uri(), list()) -> #call{} | no_return().
call(ReqId, Options, ProcedureUri, Args) ->
    call(ReqId, Options, ProcedureUri, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec call(id(), map(), uri(), list(), map()) -> #call{} | no_return().
call(ReqId, Options, ProcedureUri, Args, Payload) ->
    #call{
        request_id = ReqId,
        options = validate_map(Options, ?CALL_OPTS_SPEC),
        procedure_uri = ProcedureUri,
        arguments = Args,
        payload = Payload
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec cancel(id(), map()) -> #cancel{} | no_return().
cancel(ReqId, Options) ->
    #cancel{
        request_id = ReqId,
        options = validate_map(Options, ?CALL_CANCELLING_OPTS_SPEC)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec result(id(), map()) -> #result{} | no_return().
result(ReqId, Details) ->
    result(ReqId, Details, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec result(id(), map(), list()) -> #result{} | no_return().
result(ReqId, Details, Args) ->
    result(ReqId, Details, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec result(id(), map(), list(), map()) -> #result{} | no_return().
result(ReqId, Details, Args, Payload) ->
    #result{
        request_id = ReqId,
        details = Details,
        arguments = Args,
        payload = Payload
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec register(id(), map(), uri()) -> #register{} | no_return().
register(ReqId, Options, ProcedureUri) ->
    #register{
        request_id = ReqId,
        options = validate_map(Options, ?REGISTER_OPTS_SPEC),
        procedure_uri = ProcedureUri
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec registered(id(), id()) -> #registered{} | no_return().
registered(ReqId, RegId) ->
    #registered{
        request_id = ReqId,
        registration_id = RegId
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unregister(id(), id()) -> #unregister{} | no_return().
unregister(ReqId, RegId) ->
    #unregister{
        request_id = ReqId,
        registration_id = RegId
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unregistered(id()) -> #unregistered{} | no_return().
unregistered(ReqId) ->
    #unregistered{
        request_id = ReqId
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec invocation(id(), id(), map()) -> #invocation{} | no_return().
invocation(ReqId, RegId, Details) ->
    invocation(ReqId, RegId, Details, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec invocation(id(), id(), map(), list()) -> #invocation{} | no_return().
invocation(ReqId, RegId, Details, Args) ->
    invocation(ReqId, RegId, Details, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec invocation(id(), id(), map(), list(), map()) -> #invocation{} | no_return().
invocation(ReqId, RegId, Details, Args, Payload) ->
    #invocation{
        request_id = ReqId,
        registration_id = RegId,
        details = validate_map(Details, ?INVOCATION_DETAILS_SPEC),
        arguments = Args,
        payload = Payload
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec interrupt(id(), map()) -> #interrupt{} | no_return().
interrupt(ReqId, Options) ->
    #interrupt{
        request_id = ReqId,
        options = validate_map(Options, ?CALL_CANCELLING_OPTS_SPEC)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec yield(id(), map()) -> #yield{} | no_return().
yield(ReqId, Options) ->
    yield(ReqId, Options, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec yield(id(), map(), list()) -> #yield{} | no_return().
yield(ReqId, Options, Args) ->
    yield(ReqId, Options, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec yield(id(), map(), list(), map()) -> #yield{} | no_return().
yield(ReqId, Options, Args, Payload) ->
    #yield{
        request_id = ReqId,
        options = Options,
        arguments = Args,
        payload = Payload
    }.





%% =============================================================================
%% PRIVATE
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc
%% Calls maps_utils with options:
%%
%% * atomic = true
%%
%% @end
%% -----------------------------------------------------------------------------
validate_map(Map, Spec) ->
    maps_utils:validate(Map, Spec, #{atomic => true}).
