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
-spec hello(uri(), map()) -> wamp_hello() | no_return().
hello(RealmUri, Details) when is_binary(RealmUri) ->
    #hello{
        realm_uri = validate_uri(RealmUri),
        details = validate_map(Details, ?HELLO_DETAILS_SPEC)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec welcome(id(), map()) -> wamp_welcome() | no_return().
welcome(SessionId, Details)   ->
    #welcome{
        session_id = validate_id(SessionId),
        details = validate_map(Details, ?WELCOME_DETAILS_SPEC)
    }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
%% "ABORT" gets sent only _before_ a _Session_ is established, while
-spec abort(map(), uri()) -> wamp_abort() | no_return().
abort(Details, ReasonUri) ->
    #abort{
        details = validate_map(Details, ?ABORT_DETAILS_SPEC),
        reason_uri = validate_uri(ReasonUri)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec challenge(binary(), map()) -> wamp_challenge() | no_return().
challenge(AuthMethod, Extra) when is_map(Extra) ->
    #challenge{
        auth_method = AuthMethod,
        extra = Extra
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec authenticate(binary(), map()) -> wamp_authenticate() | no_return().
authenticate(Signature, Extra) when is_map(Extra) ->
    #authenticate{
        signature = Signature,
        extra = Extra
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
%% "GOODBYE" is sent only _after_ a _Session_ is already established.
-spec goodbye(map(), uri()) -> wamp_goodbye() | no_return().
goodbye(Details, ReasonUri) ->
    #goodbye{
        details = validate_map(Details, ?GOODBYE_DETAILS_SPEC),
        reason_uri = validate_uri(ReasonUri)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error(pos_integer(), id(), map(), uri()) -> wamp_error() | no_return().
error(ReqType, ReqId, Details, ErrorUri) ->
    error(ReqType, ReqId, Details, ErrorUri, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error(pos_integer(), id(), map(), uri(), list()) ->
    wamp_error() | no_return().
error(ReqType, ReqId, Details, ErrorUri, Args) ->
    error(ReqType, ReqId, Details, ErrorUri, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error(
    pos_integer(),
    id(),
    map(),
    uri(),
    list() | undefined,
    map() | undefined) ->
    wamp_error() | no_return().
error(ReqType, ReqId, Details, ErrorUri, Args, Payload)
when is_map(Details) ->
    #error{
        request_type = ReqType,
        request_id = validate_id(ReqId),
        details = Details, % any
        error_uri = validate_uri(ErrorUri),
        arguments = Args,
        arguments_kw = Payload
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec publish(id(), map(), uri()) -> wamp_publish() | no_return().
publish(ReqId, Options, TopicUri) ->
    publish(ReqId, Options, TopicUri, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec publish(id(), map(), uri(), list()) -> wamp_publish() | no_return().
publish(ReqId, Options, TopicUri, Args) ->
    publish(ReqId, Options, TopicUri, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec publish(id(), map(), uri(), list() | undefined, map() | undefined) ->
    wamp_publish() | no_return().
publish(ReqId, Options, TopicUri, Args, Payload) ->
    #publish{
        request_id = validate_id(ReqId),
        options = validate_map(Options, ?PUBLISH_OPTS_SPEC),
        topic_uri = validate_uri(TopicUri),
        arguments = Args,
        arguments_kw = Payload
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec published(id(), id()) -> wamp_published() | no_return().
published(ReqId, PubId) ->
    #published{
        request_id = validate_id(ReqId),
        publication_id = validate_id(PubId)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec subscribe(id(), map(), uri()) -> wamp_subscribe() | no_return().
subscribe(ReqId, Options, TopicUri) when is_map(Options) ->
    #subscribe{
        request_id = validate_id(ReqId),
        options = validate_map(Options, ?SUBSCRIBE_OPTS_SPEC),
        topic_uri = validate_uri(TopicUri)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec subscribed(id(), id()) -> wamp_subscribed() | no_return().
subscribed(ReqId, SubsId) ->
    #subscribed{
        request_id = validate_id(ReqId),
        subscription_id = validate_id(SubsId)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unsubscribe(id(), id()) -> wamp_unsubscribe() | no_return().
unsubscribe(ReqId, SubsId) ->
    #unsubscribe{
        request_id = validate_id(ReqId),
        subscription_id = validate_id(SubsId)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unsubscribed(id()) -> wamp_unsubscribed() | no_return().
unsubscribed(ReqId) ->
    #unsubscribed{
        request_id = validate_id(ReqId)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec event(id(), id(), map()) -> wamp_event() | no_return().
event(SubsId, PubId, Details) ->
    event(SubsId, PubId, Details, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec event(id(), id(), map(), list()) -> wamp_event() | no_return().
event(SubsId, PubId, Details, Args) ->
    event(SubsId, PubId, Details, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec event(id(), id(), map(), list() | undefined, map() | undefined) ->
    wamp_event() | no_return().
event(SubsId, PubId, Details, Args, Payload) when is_map(Details) ->
    #event{
        subscription_id = validate_id(SubsId),
        publication_id = validate_id(PubId),
        details = Details,
        arguments = Args,
        arguments_kw = Payload
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec call(id(), map(), uri()) -> wamp_call() | no_return().
call(ReqId, Options, ProcedureUri) ->
    call(ReqId, Options, ProcedureUri, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec call(id(), map(), uri(), list()) -> wamp_call() | no_return().
call(ReqId, Options, ProcedureUri, Args) ->
    call(ReqId, Options, ProcedureUri, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec call(id(), map(), uri(), list() | undefined, map() | undefined) ->
    wamp_call() | no_return().
call(ReqId, Options, ProcedureUri, Args, Payload) ->
    #call{
        request_id = validate_id(ReqId),
        options = validate_map(Options, ?CALL_OPTS_SPEC),
        procedure_uri = validate_uri(ProcedureUri),
        arguments = Args,
        arguments_kw = Payload
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec cancel(id(), map()) -> wamp_cancel() | no_return().
cancel(ReqId, Options) ->
    #cancel{
        request_id = validate_id(ReqId),
        options = validate_map(Options, ?CALL_CANCELLING_OPTS_SPEC)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec result(id(), map()) -> wamp_result() | no_return().
result(ReqId, Details) ->
    result(ReqId, Details, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec result(id(), map(), list()) -> wamp_result() | no_return().
result(ReqId, Details, Args) ->
    result(ReqId, Details, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec result(id(), map(), list() | undefined, map() | undefined) ->
    wamp_result() | no_return().
result(ReqId, Details, Args, Payload) when is_map(Details) ->
    #result{
        request_id = validate_id(ReqId),
        details = Details,
        arguments = Args,
        arguments_kw = Payload
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec register(id(), map(), uri()) -> wamp_register() | no_return().
register(ReqId, Options, ProcedureUri) ->
    #register{
        request_id = validate_id(ReqId),
        options = validate_map(Options, ?REGISTER_OPTS_SPEC),
        procedure_uri = validate_uri(ProcedureUri)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec registered(id(), id()) -> wamp_registered() | no_return().
registered(ReqId, RegId) ->
    #registered{
        request_id = validate_id(ReqId),
        registration_id = validate_id(RegId)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unregister(id(), id()) -> wamp_unregister() | no_return().
unregister(ReqId, RegId) ->
    #unregister{
        request_id = validate_id(ReqId),
        registration_id = validate_id(RegId)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unregistered(id()) -> wamp_unregistered() | no_return().
unregistered(ReqId) ->
    #unregistered{
        request_id = validate_id(ReqId)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec invocation(id(), id(), map()) -> wamp_invocation() | no_return().
invocation(ReqId, RegId, Details) ->
    invocation(ReqId, RegId, Details, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec invocation(id(), id(), map(), list()) -> wamp_invocation() | no_return().
invocation(ReqId, RegId, Details, Args) ->
    invocation(ReqId, RegId, Details, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec invocation(id(), id(), map(), list() | undefined, map() | undefined) ->
    wamp_invocation() | no_return().
invocation(ReqId, RegId, Details, Args, Payload) ->
    #invocation{
        request_id = validate_id(ReqId),
        registration_id = validate_id(RegId),
        details = validate_map(Details, ?INVOCATION_DETAILS_SPEC),
        arguments = Args,
        arguments_kw = Payload
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec interrupt(id(), map()) -> wamp_interrupt() | no_return().
interrupt(ReqId, Options) ->
    #interrupt{
        request_id = validate_id(ReqId),
        options = validate_map(Options, ?CALL_CANCELLING_OPTS_SPEC)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec yield(id(), map()) -> wamp_yield() | no_return().
yield(ReqId, Options) ->
    yield(ReqId, Options, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec yield(id(), map(), list()) -> wamp_yield() | no_return().
yield(ReqId, Options, Args) ->
    yield(ReqId, Options, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec yield(id(), map(), list() | undefined, map() | undefined) ->
    wamp_yield() | no_return().
yield(ReqId, Options, Args, Payload) ->
    #yield{
        request_id = validate_id(ReqId),
        options = Options,
        arguments = Args,
        arguments_kw = Payload
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
    maps_utils:validate(Map, Spec, #{
        atomic => true, % Fail atomically for the whole map
        labels => atom  % This will only turn the defined keys to atoms
    }).


%% @private
validate_id(Id) ->
    wamp_utils:is_valid_id(Id) == true orelse error({invalid_id, Id}),
    Id.


%% @private
validate_uri(Uri) ->
    wamp_uri:is_valid(Uri) == true orelse error({invalid_uri, Uri}),
    Uri.