


-define(JUNO_VERSION_STRING, <<"JUNO-0.0.1">>).
-define(WS_SUBPROTOCOL_HEADER_NAME, <<"sec-websocket-protocol">>).
-define(WAMP2_JSON, <<"wamp.2.json">>).
-define(WAMP2_MSGPACK, <<"wamp.2.msgpack">>).
-define(WAMP2_MSGPACK_BATCHED,<<"wamp.2.msgpack.batched">>).
-define(WAMP2_JSON_BATCHED,<<"wamp.2.json.batched">>).
-define(MAX_ID, 9007199254740993).

-define(ANON_AUTH, <<"anonymous">>).
-define(COOKIE_AUTH, <<"cookie">>).
-define(TICKET_AUTH, <<"ticket">>).
-define(TLS_AUTH, <<"tls">>).
-define(WAMPCRA_AUTH, <<"wampcra">>).

-define(WAMP_AUTH_METHODS, [
    ?ANON_AUTH,
    ?COOKIE_AUTH,
    ?TICKET_AUTH,
    ?TLS_AUTH,
    ?WAMPCRA_AUTH
]).


%% Adictionary describing *features* supported by the peer for that role.
%% This MUST be empty for WAMP Basic Profile implementations, and MUST
%% be used by implementations implementing parts of the Advanced Profile
%% to list the specific set of features they support.
-type role_features() :: dict().
-type dict()    ::  map().
-type uri()     ::  binary().
-type id()      ::  0..9007199254740993.
-type payload() ::  map().

%% A _Client_ can support any combination of the following roles but must
%% support at least one role.
-type client_role() ::  caller | callee | subscriber | publisher.

-type subprotocol() ::  #{
    id => binary(),
    frame_type => text | binary,
    encoding => json | msgpack | json_batched | msgpack_batched
}.

-define(HELLO, 1).
-define(WELCOME, 2).
-define(ABORT, 3).
-define(CHALLENGE, 4).
-define(AUTHENTICATE, 5).
-define(GOODBYE, 6).
-define(ERROR, 8).
-define(PUBLISH, 16).
-define(PUBLISHED, 17).
-define(SUBSCRIBE, 32).
-define(SUBSCRIBED, 33).
-define(UNSUBSCRIBE, 34).
-define(UNSUBSCRIBED, 35).
-define(EVENT, 36).
-define(CALL, 48).
-define(CANCEL, 49).
-define(RESULT, 50).
-define(REGISTER, 64).
-define(REGISTERED, 65).
-define(UNREGISTER, 66).
-define(UNREGISTERED, 67).
-define(INVOCATION, 68).
-define(INTERRUPT, 69).
-define(YIELD, 70).




%% DO NOT CHANGE THE ORDER OF THE RECORD FIELDS as it maps
%% to the order in WAMP messages
-record (hello, {
    realm_uri       ::  uri(),
    details         ::  map()
}).
-type wamp_hello()       ::  #hello{}.

-record (challenge, {
    auth_method      ::  binary(),
    extra            ::  map()
}).
-type wamp_challenge()       ::  #challenge{}.

-record (authenticate, {
    signature       ::  binary(),
    extra           ::  map()
}).
-type wamp_authenticate()       ::  #authenticate{}.

-record (welcome, {
    session_id      ::  id(),
    details         ::  map()
}).
-type wamp_welcome()       ::  #welcome{}.

-record (abort, {
    details         ::  map(),
    reason_uri      ::  uri()
}).
-type wamp_abort()       ::  #abort{}.

-record (goodbye, {
    details         ::  map(),
    reason_uri      ::  uri()
}).
-type wamp_goodbye()       ::  #goodbye{}.

-record (error, {
    request_type    ::  pos_integer(),
    request_id      ::  id(),
    details         ::  map(),
    error_uri       ::  uri(),
    arguments       ::  list(),
    payload         ::  map()
}).
-type wamp_error()       ::  #error{}.

-record (publish, {
    request_id      ::  id(),
    options         ::  map(),
    topic_uri       ::  uri(),
    arguments       ::  list(),
    payload         ::  map()
}).
-type wamp_publish()       ::  #publish{}.

-record (published, {
    request_id      ::  id(),
    publication_id  ::  id()
}).
-type wamp_published()       ::  #published{}.

-record (subscribe, {
    request_id      ::  id(),
    options         ::  map(),
    topic_uri       ::  uri()
}).
-type wamp_subscribe()       ::  #subscribe{}.

-record (subscribed, {
    request_id      ::  id(),
    subscription_id ::  id()
}).
-type wamp_subscribed()       ::  #subscribed{}.

-record (unsubscribe, {
    request_id      ::  id(),
    subscription_id ::  id()
}).
-type wamp_unsubscribe()       ::  #unsubscribe{}.

-record (unsubscribed, {
    request_id      ::  id()
}).
-type wamp_unsubscribed()       ::  #unsubscribed{}.

-record (event, {
    subscription_id ::  id(),
    publication_id  ::  id(),
    details         ::  map(),
    arguments       ::  list(),
    payload         ::  map()
}).
-type wamp_event()       ::  #event{}.

-record (call, {
    request_id      ::  id(),
    options         ::  map(),
    procedure_uri   ::  uri(),
    arguments       ::  list(),
    payload         ::  map()
}).
-type wamp_call()       ::  #call{}.

-record (cancel, {
    request_id      ::  id(),
    options         ::  map()
}).
-type wamp_cancel()       ::  #cancel{}.

-record (result, {
    request_id      ::  id(),
    details         ::  map(),
    arguments       ::  list(),
    payload         ::  map()
}).
-type wamp_result()       ::  #result{}.

-record (register, {
    request_id      ::  id(),
    options         ::  map(),
    procedure_uri   ::  uri()
}).
-type wamp_register()       ::  #register{}.

-record (registered, {
    request_id      ::  id(),
    registration_id ::  id()
}).
-type wamp_registered()       ::  #registered{}.

-record (unregister, {
    request_id      ::  id(),
    registration_id ::  id()
}).
-type wamp_unregister()       ::  #unregister{}.

-record (unregistered, {
    request_id      ::  id()
}).
-type wamp_unregistered()       ::  #unregistered{}.

-record (invocation, {
    request_id      ::  id(),
    registration_id ::  id(),
    details         ::  map(),
    arguments       ::  list(),
    payload         ::  map()
}).
-type wamp_invocation()       ::  #invocation{}.

-record (interrupt, {
    request_id      ::  id(),
    options         ::  map()
}).
-type wamp_interrupt()       ::  #interrupt{}.

-record (yield, {
    request_id      ::  id(),
    options         ::  map(),
    arguments       ::  list(),
    payload         ::  map()
}).
-type wamp_yield()       ::  #yield{}.

-type message()     ::  wamp_hello() 
                        | wamp_challenge() 
                        | wamp_authenticate() 
                        | wamp_welcome()
                        | wamp_abort()
                        | wamp_goodbye()
                        | wamp_error()
                        | wamp_publish() 
                        | wamp_published()
                        | wamp_subscribe() 
                        | wamp_subscribed()
                        | wamp_unsubscribe() 
                        | wamp_unsubscribed()
                        | wamp_event()
                        | wamp_call()
                        | wamp_cancel()
                        | wamp_result()
                        | wamp_register() 
                        | wamp_registered()
                        | wamp_unregister() 
                        | wamp_unregistered()
                        | wamp_invocation()
                        | wamp_interrupt()
                        | wamp_yield().

-type subscriber_features()     ::  event_history
                                    | pattern_based_subscription
                                    | pattern_based_subscription
                                    | publication_trustlevels
                                    | publisher_identification
                                    | sharded_registration
                                    | sharded_subscription
                                    | subscriber_blackwhite_listing.
-type publisher_features()      ::  publisher_exclusion
                                    | publisher_identification
                                    | sharded_subscription
                                    | subscriber_blackwhite_listing.
-type broker_features()         ::  subscriber_features()
                                    | publisher_features()
                                    | session_meta_api
                                    | subscription_meta_api
                                    | topic_reflection.
-type caller_features()         ::  call_cancelling
                                    | call_timeout
                                    | caller_identification
                                    | procedure_reflection
                                    | progressive_call_results
                                    | sharded_registration
                                    | sharded_registration.
-type callee_features()         ::  caller_features()
                                    | call_trustlevels
                                    | pattern_based_registration.
-type dealer_features()         ::  callee_features() | session_meta_api.
-type hello_details()           ::  roles
                                    | agent
                                    | transport
                                    | authmethods
                                    | authid.
-type result_details()          ::  progress.
-type challenge_details()       ::  challenge | salt | keylen | iterations.
-type invocation_details()      ::  caller | trustlevel | procedure.
-type event_details()           ::  publisher | trustlevel | topic.

%% <<"exact">> | <<"prefix">> | <<"wildcard">>.
-type match_policy()            ::  binary().

%% <<"single">> | <<"roundrobin">> | <<"random">> | <<"first">> | <<"last">>.
-type invocation_policy()       ::  binary().

-type subscribe_options()       ::  match
                                    | nkey
                                    | disclose_caller.
-type register_options()        ::  match | invoke | disclose_caller.
-type call_runmode()            ::  partition.
-type call_options()            ::  disclose_me
                                    | runmode
                                    | rkey
                                    | receive_progress
                                    | timeout.
-type cancel_options()          ::  mode.
-type yeild_options()           ::  progress.
-type publish_options()         ::  acknowledge
                                    | exclude
                                    | exclude_authid
                                    | exclude_authrole
                                    | exclude_me
                                    | disclose_me
                                    | eligible
                                    | eligible_authid
                                    | eligible_authrole
                                    | rkey.

-define(WAMP_ERROR_AUTHORIZATION_FAILED, <<"wamp.error.authorization_failed">>).
-define(WAMP_ERROR_CANCELED, <<"wamp.error.canceled">>).
-define(WAMP_ERROR_CLOSE_REALM, <<"wamp.error.close_realm">>).
-define(WAMP_ERROR_DISCLOSE_ME_NOT_ALLOWED, <<"wamp.error.disclose_me.not_allowed">>).
-define(WAMP_ERROR_GOODBYE_AND_OUT, <<"wamp.error.goodbye_and_out">>).
-define(WAMP_ERROR_INVALID_ARGUMENT, <<"wamp.error.invalid_argument">>).
-define(WAMP_ERROR_INVALID_URI, <<"wamp.error.invalid_uri">>).
-define(WAMP_ERROR_NET_FAILURE, <<"wamp.error.network_failure">>).
-define(WAMP_ERROR_NOT_AUTHORIZED, <<"wamp.error.not_authorized">>).
-define(WAMP_ERROR_NO_ELIGIBLE_CALLE, <<"wamp.error.no_eligible_callee">>).
-define(WAMP_ERROR_NO_SUCH_PROCEDURE, <<"wamp.error.no_such_procedure">>).
-define(WAMP_ERROR_NO_SUCH_REALM, <<"wamp.error.no_such_realm">>).
-define(WAMP_ERROR_NO_SUCH_REGISTRATION, <<"wamp.error.no_such_registration">>).
-define(WAMP_ERROR_NO_SUCH_ROLE, <<"wamp.error.no_such_role">>).
-define(WAMP_ERROR_NO_SUCH_SESSION, <<"wamp.error.no_such_session">>).
-define(WAMP_ERROR_NO_SUCH_SUBSCRIPTION, <<"wamp.error.no_such_subscription">>).
-define(WAMP_ERROR_OPTION_DISALLOWED_DISCLOSE_ME, <<"wamp.error.option_disallowed.disclose_me">>).
-define(WAMP_ERROR_OPTION_NOT_ALLOWED, <<"wamp.error.option_not_allowed">>).
-define(WAMP_ERROR_PROCEDURE_ALREADY_EXISTS, <<"wamp.error.procedure_already_exists">>).
-define(WAMP_ERROR_SYSTEM_SHUTDOWN, <<"wamp.error.system_shutdown">>).

-define(JUNO_ERROR_NOT_IN_SESSION, <<"juno.error.not_in_session">>).
-define(JUNO_SESSION_ALREADY_EXISTS, <<"juno.error.session_already_exists">>).
