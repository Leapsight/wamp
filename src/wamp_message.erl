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
-export([options/1]).
-export([details/1]).
-export([validate_options/2]).
-export([validate_details/2]).



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
        details = validate_details(hello, Details, ?HELLO_DETAILS_SPEC)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec welcome(id(), map()) -> wamp_welcome() | no_return().

welcome(SessionId, Details)   ->
    #welcome{
        session_id = validate_id(SessionId),
        details = validate_details(welcome, Details, ?WELCOME_DETAILS_SPEC)
    }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
%% "ABORT" gets sent only _before_ a _Session_ is established, while
-spec abort(map(), uri()) -> wamp_abort() | no_return().

abort(Details, ReasonUri) ->
    #abort{
        reason_uri = validate_uri(ReasonUri),
        details = validate_details(abort, Details, ?ABORT_DETAILS_SPEC)
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
        reason_uri = validate_uri(ReasonUri),
        details = validate_details(goodbye, Details, ?GOODBYE_DETAILS_SPEC)
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
        options = validate_options(publish, Options, ?PUBLISH_OPTS_SPEC),
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
        options = validate_options(subscribe, Options, ?SUBSCRIBE_OPTS_SPEC),
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
        options = validate_options(call, Options, ?CALL_OPTS_SPEC),
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
        options = validate_options(cancel, Options, ?CALL_CANCELLING_OPTS_SPEC)
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
        details = validate_details(result, Details, ?RESULT_DETAILS_SPEC),
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
        options = validate_options(register, Options, ?REGISTER_OPTS_SPEC),
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

invocation(ReqId, RegId, Details0, Args, Payload) ->
    Details1 = validate_details(invocation, Details0, ?INVOCATION_DETAILS_SPEC),
    #invocation{
        request_id = validate_id(ReqId),
        registration_id = validate_id(RegId),
        details = Details1,
        arguments = Args,
        arguments_kw = Payload
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec interrupt(id(), map()) -> wamp_interrupt() | no_return().

interrupt(ReqId, Opts0) ->
    Opts1 = validate_options(interrupt, Opts0, ?CALL_CANCELLING_OPTS_SPEC),
    #interrupt{
        request_id = validate_id(ReqId),
        options = Opts1
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

yield(ReqId, Opts0, Args, Payload) ->
    Opts1 = validate_options(yield, Opts0, ?YIELD_OPTIONS_SPEC),
    #yield{
        request_id = validate_id(ReqId),
        options = Opts1,
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
%% @doc Fails with an exception if the Options maps is not valid.
%% A Options map is valid if all its properties (keys) are valid. A property is
%% valid if it is a key defined by the WAMP Specification for the message type
%% and its value is valid according to the same specification or when:
%%
%% 1. the key is found in the list of extended options configured in the
%% application option `extended_options` or the keyword `any` was used instead
%% of a list of keys;
%% 2. there is no definition of the extended_options` application option for
%% the given key e.g. the default is to allow any unknown property.
%%
%% Example:
%%
%% ```
%% application:set_env(wamp, extended_details, [{result, [<<"_x">>, <<"_y">>]}).
%% ```
%% Using this configuration all messages but `result' would accept the
%% properties `<<"_x">>', `<<"_y">>', and `<<"_z">>'; and result would accept only `<<"_x">>' and `<<"_y">>'.
%%
%% This is equivalente to:
%% ```
%% application:set_env(wamp, extended_details, [
%%  {result, [<<"_x">>, y]},
%%  {hello, any},
%%  {welcome, any},
%%  {abort, any},
%%  {goodbye, any},
%%  {result, any},
%%  {invocation, any}
%% ]).
%% ```

%% -----------------------------------------------------------------------------
-spec validate_options(MessageType :: atom(), Opts :: map()) ->
    ok | no_return().

validate_options(publish, Opts) ->
    validate_options(publish, Opts, ?PUBLISH_OPTS_SPEC);

validate_options(subscribe, Opts) ->
    validate_options(subscribe, Opts, ?SUBSCRIBE_OPTS_SPEC);

validate_options(call, Opts) ->
    validate_options(call, Opts, ?CALL_OPTS_SPEC);

validate_options(cancel, Opts) ->
    validate_options(cancel, Opts, ?CALL_CANCELLING_OPTS_SPEC);

validate_options(register, Opts) ->
    validate_options(register, Opts, ?REGISTER_OPTS_SPEC);

validate_options(interrupt, Opts) ->
    validate_options(interrupt, Opts, ?CALL_CANCELLING_OPTS_SPEC);

validate_options(yield, Opts) ->
    validate_options(yield, Opts, ?YIELD_OPTIONS_SPEC).


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


%% -----------------------------------------------------------------------------
%% @doc Fails with an exception if the Details maps is not valid.
%% A Details map is valid if all its properties (keys) are valid. A property is
%% valid if it is a key defined by the WAMP Specification for the message type
%% and its value is valid according to the same specification or when:
%%
%% 1. the key is found in the list of extended details configured in the
%% application option `extended_details` or the keyword `any` was used instead
%% of a list of keys;
%% 2. there is no definition of the extended_details` application option for
%% the given key e.g. the default is to allow any unknown property.
%% @end
%% -----------------------------------------------------------------------------
-spec validate_details(MessageType :: atom(), Details :: map()) ->
    ok | no_return().

validate_details(hello, Details) ->
    validate_details(hello, Details, ?HELLO_DETAILS_SPEC);

validate_details(welcome, Details) ->
    validate_details(welcome, Details, ?WELCOME_DETAILS_SPEC);

validate_details(abort, Details) ->
    validate_details(abort, Details, ?ABORT_DETAILS_SPEC);

validate_details(goodbye, Details) ->
    validate_details(goodbye, Details, ?GOODBYE_DETAILS_SPEC);

validate_details(result, Details) ->
    validate_details(result, Details, ?RESULT_DETAILS_SPEC);

validate_details(invocation, Details) ->
    validate_details(invocation, Details, ?INVOCATION_DETAILS_SPEC).



%% =============================================================================
%% PRIVATE
%% =============================================================================




%% private
validate_options(Type, Map, Spec) ->
    Config = app_config:get(wamp, [extended_options, Type], any),
    {NewSpec, Opts} = parse_config(Config, {Spec, maps:new()}),
    validate_map(Map, NewSpec, Opts).

%% private
validate_details(Type, Map, Spec) ->
    ExtensionKeys = app_config:get(wamp, [extended_details, Type], any),
    {NewSpec, Opts} = parse_config(ExtensionKeys, {Spec, maps:new()}),
    validate_map(Map, NewSpec, Opts).


%% private
parse_config(any, {Spec, Opts0}) ->
    Opts = Opts0#{
        keep_unknown => true,
        unknown_label_validator => fun(X) -> is_valid_extension_key(X) end
    },
    {Spec, Opts};

parse_config(ExtensionKeys, {Spec, Opts}) when is_list(ExtensionKeys) ->
    Fun = fun(Key, Acc) ->
        %% We ignore all invalid keys
        case is_valid_extension_key(Key) of
            true ->
                %% We declare a new key for the configured ExtensionKeys
                OptionSpec = #{
                    alias => atom_to_binary(Key, utf8),
                    required => false
                },
                maps:put(Key, OptionSpec, Acc);
            false ->
                Acc
        end
    end,
    NewSpec = lists:foldl(Fun, Spec, ExtensionKeys),
    {NewSpec, Opts#{keep_unknown => false}};

parse_config(DeltaSpec, {Spec, Opts}) when is_map(Spec) ->
    {maps:merge(DeltaSpec, Spec), Opts#{keep_unknown => false}}.


%% @private
is_valid_extension_key(Key) when is_atom(Key) ->
    is_valid_extension_key(atom_to_binary(Key, utf8));

is_valid_extension_key(Key) when is_binary(Key) ->
    re:run(Key, extension_key_pattern()) =/= nomatch.


%% @private
extension_key_pattern() ->
    CompiledPattern = persistent_term:get({?MODULE, ekey_pattern}, undefined),
    extension_key_pattern(CompiledPattern).


%% @private
extension_key_pattern(undefined) ->
    {ok, Pattern} = re:compile("_[a-z0-9_]{3,}"),
    ok = persistent_term:put({?MODULE, ekey_pattern}, Pattern),
    Pattern;

extension_key_pattern(CompiledPattern) ->
    CompiledPattern.


%% private
validate_map(Map, Spec, Opts0) ->
    Opts1 = Opts0#{
        atomic => true, % Fail atomically for the whole map
        labels => atom  % This will only turn the defined keys to atoms
    },
    maps_utils:validate(Map, Spec, Opts1).


%% @private
validate_id(Id) ->
    wamp_utils:is_valid_id(Id) == true orelse error({invalid_id, Id}),
    Id.


%% @private
validate_uri(Uri) ->
    wamp_uri:is_valid(Uri) == true orelse error({invalid_uri, Uri}),
    Uri.