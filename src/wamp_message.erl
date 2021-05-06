%% -----------------------------------------------------------------------------
%%  Copyright (c) 2015-2021 Leapsight. All rights reserved.
%% -----------------------------------------------------------------------------

%% =============================================================================
%% @doc
%%
%% @end
%% =============================================================================
-module(wamp_message).
-include("wamp.hrl").

-type error_source()    ::  wamp_subscribe()
                            | wamp_unsubscribe()
                            | wamp_publish()
                            | wamp_register()
                            | wamp_unregister()
                            | wamp_call()
                            | wamp_invocation()
                            | wamp_cancel().

-export_type([error_source/0]).


-export([abort/2]).
-export([authenticate/2]).
-export([call/3, call/4, call/5]).
-export([cancel/2]).
-export([challenge/2]).
-export([error/4]).
-export([error/5]).
-export([error/6]).
-export([error_from/3]).
-export([error_from/4]).
-export([error_from/5]).
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
-export([options/1]).
-export([details/1]).



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
        realm_uri = wamp_uri:validate(RealmUri, strict),
        details = wamp_details:new(hello, Details)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec welcome(id(), map()) -> wamp_welcome() | no_return().

welcome(SessionId, Details)   ->
    #welcome{
        session_id = wamp_utils:validate_id(SessionId),
        details = wamp_details:new(welcome, Details)
    }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
%% "ABORT" gets sent only _before_ a _Session_ is established, while
-spec abort(map(), uri()) -> wamp_abort() | no_return().

abort(Details, ReasonUri) ->
    #abort{
        reason_uri = wamp_uri:validate(ReasonUri, strict),
        details = wamp_details:new(abort, Details)
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
        reason_uri = wamp_uri:validate(ReasonUri, strict),
        details = wamp_details:new(goodbye, Details)
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
        request_id = wamp_utils:validate_id(ReqId),
        details = Details, % any
        error_uri = wamp_uri:validate(ErrorUri, strict),
        arguments = Args,
        arguments_kw = Payload
    }.



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error_from(error_source(), map(), uri()) -> wamp_error() | no_return().

error_from(M, Details, ErrorUri) ->
    error_from(M, Details, ErrorUri, undefined, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error_from(error_source(), map(), uri(), list()) ->
    wamp_error() | no_return().

error_from(M, Details, ErrorUri, Args) ->
    error_from(M, Details, ErrorUri, Args, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec error_from(
    error_source(),
    map(),
    uri(),
    list() | undefined,
    map() | undefined) ->
    wamp_error() | no_return().


error_from(#subscribe{request_id = ReqId}, Details, ErrorUri, Args, Payload) ->
    error(?SUBSCRIBE, ReqId, Details, ErrorUri, Args, Payload);

error_from(
    #unsubscribe{request_id = ReqId}, Details, ErrorUri, Args, Payload) ->
    error(?UNSUBSCRIBE, ReqId, Details, ErrorUri, Args, Payload);

error_from(#publish{request_id = ReqId}, Details, ErrorUri, Args, Payload) ->
    error(?PUBLISH, ReqId, Details, ErrorUri, Args, Payload);

error_from(#register{request_id = ReqId}, Details, ErrorUri, Args, Payload) ->
    error(?REGISTER, ReqId, Details, ErrorUri, Args, Payload);

error_from(#unregister{request_id = ReqId}, Details, ErrorUri, Args, Payload) ->
    error(?UNREGISTER, ReqId, Details, ErrorUri, Args, Payload);

error_from(#call{request_id = ReqId}, Details, ErrorUri, Args, Payload) ->
    error(?CALL, ReqId, Details, ErrorUri, Args, Payload);

error_from(#invocation{request_id = ReqId}, Details, ErrorUri, Args, Payload) ->
    error(?INVOCATION, ReqId, Details, ErrorUri, Args, Payload);

error_from(#cancel{request_id = ReqId}, Details, ErrorUri, Args, Payload) ->
    error(?CANCEL, ReqId, Details, ErrorUri, Args, Payload).


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
        request_id = wamp_utils:validate_id(ReqId),
        options = wamp_options:new(publish, Options),
        topic_uri = wamp_uri:validate(TopicUri, strict),
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
        request_id = wamp_utils:validate_id(ReqId),
        publication_id = wamp_utils:validate_id(PubId)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec subscribe(id(), map(), uri()) -> wamp_subscribe() | no_return().

subscribe(ReqId, Options0, TopicUri) when is_map(Options0) ->
    Options = wamp_options:new(subscribe, Options0),
    Match = maps:get(match, Options, ?EXACT_MATCH),

    #subscribe{
        request_id = wamp_utils:validate_id(ReqId),
        options = Options,
        topic_uri = wamp_uri:validate_match(TopicUri, Match)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec subscribed(id(), id()) -> wamp_subscribed() | no_return().

subscribed(ReqId, SubsId) ->
    #subscribed{
        request_id = wamp_utils:validate_id(ReqId),
        subscription_id = wamp_utils:validate_id(SubsId)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unsubscribe(id(), id()) -> wamp_unsubscribe() | no_return().

unsubscribe(ReqId, SubsId) ->
    #unsubscribe{
        request_id = wamp_utils:validate_id(ReqId),
        subscription_id = wamp_utils:validate_id(SubsId)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unsubscribed(id()) -> wamp_unsubscribed() | no_return().

unsubscribed(ReqId) ->
    #unsubscribed{
        request_id = wamp_utils:validate_id(ReqId)
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

event(SubsId, PubId, Details, Args, Payload) ->
    #event{
        subscription_id = wamp_utils:validate_id(SubsId),
        publication_id = wamp_utils:validate_id(PubId),
        details = wamp_details:new(event, Details),
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
        request_id = wamp_utils:validate_id(ReqId),
        options = wamp_options:new(call, Options),
        procedure_uri = wamp_uri:validate(ProcedureUri, strict),
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
        request_id = wamp_utils:validate_id(ReqId),
        options = wamp_options:new(cancel, Options)
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
        request_id = wamp_utils:validate_id(ReqId),
        details = wamp_details:new(result, Details),
        arguments = Args,
        arguments_kw = Payload
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec register(id(), map(), uri()) -> wamp_register() | no_return().

register(ReqId0, Options0, ProcedureUri) ->
    ReqId = wamp_utils:validate_id(ReqId0),
    Options = wamp_options:new(register, Options0),
    Match = maps:get(match, Options, ?EXACT_MATCH),

    #register{
        request_id = ReqId,
        options = Options,
        procedure_uri = wamp_uri:validate_match(ProcedureUri, Match)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec registered(id(), id()) -> wamp_registered() | no_return().

registered(ReqId, RegId) ->
    #registered{
        request_id = wamp_utils:validate_id(ReqId),
        registration_id = wamp_utils:validate_id(RegId)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unregister(id(), id()) -> wamp_unregister() | no_return().

unregister(ReqId, RegId) ->
    #unregister{
        request_id = wamp_utils:validate_id(ReqId),
        registration_id = wamp_utils:validate_id(RegId)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
% -spec registration_revocation(id(), id()) -> wamp_unregister() | no_return().

% registration_revocation(RegId, Reason) when is_binary(Reason) ->
%     Id = wamp_utils:validate_id(RegId),
%     #unregister_ext{
%         request_id = 0,
%         details = #{registration => Id, reason => Reason}
%     }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec unregistered(id()) -> wamp_unregistered() | no_return().

unregistered(ReqId) ->
    #unregistered{
        request_id = wamp_utils:validate_id(ReqId)
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
        request_id = wamp_utils:validate_id(ReqId),
        registration_id = wamp_utils:validate_id(RegId),
        details = wamp_details:new(invocation, Details),
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
        request_id = wamp_utils:validate_id(ReqId),
        options = wamp_options:new(interrupt, Options)
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
        request_id = wamp_utils:validate_id(ReqId),
        options = wamp_options:new(yield, Options),
        arguments = Args,
        arguments_kw = Payload
    }.


-spec options(
    wamp_call()
    | wamp_cancel()
    | wamp_interrupt()
    | wamp_publish()
    | wamp_register()
    | wamp_subscribe()
    | wamp_yield()
    ) -> ok | no_return().

options(#call{options = Val}) -> Val;
options(#cancel{options = Val}) -> Val;
options(#interrupt{options = Val}) -> Val;
options(#publish{options = Val}) -> Val;
options(#register{options = Val}) -> Val;
options(#subscribe{options = Val}) -> Val;
options(#yield{options = Val}) -> Val;
options(_) ->
    error(badarg).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec details(
    wamp_hello()
    | wamp_welcome()
    | wamp_abort()
    | wamp_goodbye()
    | wamp_result()
    | wamp_invocation()
    ) -> ok | no_return().

details(#hello{details = Val}) -> Val;
details(#welcome{details = Val}) -> Val;
details(#abort{details = Val}) -> Val;
details(#goodbye{details = Val}) -> Val;
details(#result{details = Val}) -> Val;
details(#invocation{details = Val}) -> Val;
details(_) ->
    error(badarg).

