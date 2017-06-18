%% -----------------------------------------------------------------------------
%% Copyright (C) Ngineo Limited 2015 - 2017. All rights reserved.
%% -----------------------------------------------------------------------------



-define(WAMP2_JSON, <<"wamp.2.json">>).
-define(WAMP2_MSGPACK, <<"wamp.2.msgpack">>).
-define(WAMP2_BERT, <<"wamp.2.bert">>).
-define(WAMP2_ERL, <<"wamp.2.erl">>).
-define(WAMP2_MSGPACK_BATCHED,<<"wamp.2.msgpack.batched">>).
-define(WAMP2_JSON_BATCHED,<<"wamp.2.json.batched">>).
-define(WAMP2_BERT_BATCHED,<<"wamp.2.bert.batched">>).
-define(WAMP2_ERL_BATCHED,<<"wamp.2.erl.batched">>).
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



-define(WAMP_ENCODINGS, [
    json,
    msgpack,
    bert,
    erl,
    json_batched,
    msgpack_batched,
    bert_batched,
    erl_batched
]).

-type encoding()        ::  json
                            | msgpack
                            | bert
                            | erl
                            | json_batched
                            | msgpack_batched
                            | bert_batched
                            | erl_batched.


-type frame_type()          ::  text | binary.
-type transport()           ::  ws | raw.


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

-define(MSG_TYPES,[
    ?HELLO,
    ?WELCOME,
    ?ABORT,
    ?CHALLENGE,
    ?AUTHENTICATE,
    ?GOODBYE,
    ?ERROR,
    ?PUBLISH,
    ?PUBLISHED,
    ?SUBSCRIBE,
    ?SUBSCRIBED,
    ?UNSUBSCRIBE,
    ?UNSUBSCRIBED,
    ?EVENT,
    ?CALL,
    ?CANCEL,
    ?RESULT,
    ?REGISTER,
    ?REGISTERED,
    ?UNREGISTER,
    ?UNREGISTERED,
    ?INVOCATION,
    ?INTERRUPT,
    ?YIELD
]).



%% =============================================================================
%% FEATURE ANNOUNCEMENT
%% =============================================================================


-define(DEALER_FEATURES_SPEC, #{
    <<"progressive_call_results">> => #{
        required => false, datatype => boolean},
    <<"progressive_calls">> => #{
        required => false, datatype => boolean},
    <<"call_timeout">> => #{
        required => false, datatype => boolean},
    <<"call_canceling">> => #{
        required => false, datatype => boolean},
    <<"caller_identification">>=> #{
        required => false, datatype => boolean},
    <<"call_trustlevels">> => #{
        required => false, datatype => boolean},
    <<"registration_meta_api">> => #{
        required => false, datatype => boolean},
    <<"registration_revocation">> => #{
        required => false, datatype => boolean},
    <<"session_meta_api">> => #{
        required => false, datatype => boolean},
    <<"pattern_based_registration">> => #{
        required => false, datatype => boolean},
    <<"procedure_reflection">> => #{
        required => false, datatype => boolean},
    <<"shared_registration">> => #{
        required => false, datatype => boolean},
    <<"sharded_registration">> => #{
        required => false, datatype => boolean}
}).

-define(BROKER_FEATURES_SPEC, #{
    <<"event_history">> => #{
        required => false, datatype => boolean},
    <<"pattern_based_subscription">> => #{
        required => false, datatype => boolean},
    <<"publication_trustlevels">> => #{
        required => false, datatype => boolean},
    <<"publisher_exclusion">> => #{
        required => false, datatype => boolean},
    <<"publisher_identification">> => #{
        required => false, datatype => boolean},
    <<"session_meta_api">> => #{
        required => false, datatype => boolean},
    <<"sharded_subscription">> => #{
        required => false, datatype => boolean},
    <<"subscriber_blackwhite_listing">> => #{
        required => false, datatype => boolean},
    <<"subscription_meta_api">> => #{
        required => false, datatype => boolean},
    <<"topic_reflection">> => #{
        required => false, datatype => boolean}
}).

-define(BROKER_ROLE_SPEC, #{
    <<"features">> => #{
        required => false,
        datatype => map,
        validator => ?BROKER_FEATURES_SPEC
    }
}).

-define(DEALER_ROLE_SPEC, #{
    <<"features">> => #{
        required => false,
        datatype => map,
        validator => ?DEALER_FEATURES_SPEC
    }
}).

-define(ROUTER_ROLES_SPEC, #{
    <<"broker">> => #{
        required => false, 
        datatype => map,
        validator => ?BROKER_ROLE_SPEC},
    <<"dealer">> => #{
        required => false, 
        datatype => map,
        validator => ?DEALER_ROLE_SPEC}
}).


-define(CALLEE_FEATURES_SPEC, #{
    <<"progressive_call_results">> => #{
        required => false, datatype => boolean},
    <<"progressive_calls">> => #{
        required => false, datatype => boolean},
    <<"call_timeout">> => #{
        required => false, datatype => boolean},
    <<"call_canceling">> => #{
        required => false, datatype => boolean},
    <<"caller_identification">> => #{
        required => false, datatype => boolean},
    <<"call_trustlevels">> => #{
        required => false, datatype => boolean},
    <<"registration_revocation">> => #{
        required => false, datatype => boolean},
    <<"session_meta_api">> => #{
        required => false, datatype => boolean},
    <<"pattern_based_registration">> => #{
        required => false, datatype => boolean},
    <<"shared_registration">> => #{
        required => false, datatype => boolean},
    <<"sharded_registration">> => #{
        required => false, datatype => boolean}
}).

-define(CALLER_FEATURES_SPEC, #{
    <<"progressive_call_results">> => #{
        required => false, datatype => boolean},
    <<"progressive_calls">> => #{
        required => false, datatype => boolean},
    <<"call_timeout">> => #{
        required => false, datatype => boolean},
    <<"call_canceling">> => #{
        required => false, datatype => boolean},
    <<"caller_identification">>=> #{
        required => false, datatype => boolean}
}).

-define(SUBSCRIBER_FEATURES_SPEC, #{
    <<"event_history">> => #{
        required => false, datatype => boolean},
    <<"pattern_based_subscription">> => #{
        required => false, datatype => boolean},
    <<"publication_trustlevels">> => #{
        required => false, datatype => boolean},
    <<"publisher_identification">> => #{
        required => false, datatype => boolean},
    <<"sharded_subscription">> => #{
        required => false, datatype => boolean}
}).

-define(PUBLISHER_FEATURES_SPEC, #{
    <<"publisher_exclusion">> => #{
        required => false, datatype => boolean},
    <<"publisher_identification">> => #{
        required => false, datatype => boolean},
    <<"subscriber_blackwhite_listing">> => #{
        required => false, datatype => boolean}
}).

-define(PUBLISHER_ROLE_SPEC, #{
    <<"features">> => #{
        required => false,
        datatype => map,
        validator => ?PUBLISHER_FEATURES_SPEC
    }
}).

-define(SUBSCRIBER_ROLE_SPEC, #{
    <<"features">> => #{
        required => false,
        datatype => map,
        validator => ?SUBSCRIBER_FEATURES_SPEC
    }
}).

-define(CALLER_ROLE_SPEC, #{
    <<"features">> => #{
        required => false,
        datatype => map,
        validator => ?CALLER_FEATURES_SPEC
    }
}).

-define(CALLEE_ROLE_SPEC, #{
    <<"features">> => #{
        required => false,
        datatype => map,
        validator => ?CALLEE_FEATURES_SPEC
    }
}).


-define(CLIENT_ROLES_SPEC, #{
    <<"publisher">> => #{
        required => false, 
        datatype => map,
        validator => ?PUBLISHER_ROLE_SPEC},
    <<"subscriber">> => #{
        required => false, 
        datatype => map,
        validator => ?SUBSCRIBER_ROLE_SPEC},
    <<"caller">> => #{
        required => false, 
        datatype => map,
        validator => ?CALLER_ROLE_SPEC},
    <<"callee">> => #{
        required => false, 
        datatype => map,
        validator => ?CALLEE_ROLE_SPEC}
}).




-define(HELLO_DETAILS_SPEC, #{
    <<"authmethods">> => #{
        % description => <<"Used by the client to announce the authentication methods it is prepared to perform.">>,
        required => false, 
        datatype => {in, ?WAMP_AUTH_METHODS}
    },
    <<"authid">> => #{
        % description => <<"Te authentication ID (e.g. username) the client wishes to authenticate as.">>,
        required => false,
        datatype => binary
    },
    <<"authrole">> => #{
        required => false,
        datatype => binary
    },
    <<"authextra">> => #{
        % description => <<"Not in RFC">>,
        required => false
    },
    <<"roles">> => #{
        required => true,
        datatype => map,
        validator => ?CLIENT_ROLES_SPEC
    },
    <<"agent">> => #{
        % description => <<"When a software agent operates in a network protocol, it often identifies itself, its application type, operating system, software vendor, or software revision, by submitting a characteristic identification string to its operating peer. Similar to what browsers do with the User-Agent HTTP header, both the HELLO and the WELCOME message MAY disclose the WAMP implementation in use to its peer">>,
        required => false,
        datatype => binary
    },
    <<"transport">> => #{
        % description => <<"When running WAMP over a TLS (either secure WebSocket
        % or raw TCP) transport, a peer may authenticate to the other via the TLS certificate mechanism. A server might authenticate to the client, and a client may authenticate to the server (TLS client-certificate based authentication). This transport-level authentication information may be forward to the WAMP level within HELLO.Details.transport.auth|any in both directions (if available).">>,
        required => false,
        datatype => map,
        validator => #{
            auth => #{required => true}
        }
    },
    <<"resumable">> => #{
        required => false,
        datatype => boolean
    },
    <<"resume_session">> => #{
        % description => <<"The session ID the client would like to resume.">>,
        required => false,
        datatype => binary
    },
    <<"resume_token">> => #{
        % description => <<"The secure token required to resume the session defined in 'resume_session'.">>,
        required => false,
        datatype => binary
    }
}).

-define(CHALLENGE_DETAILS_SPEC, #{
    <<"challenge">> => #{
        required => false,
        datatype => binary
    },
    <<"salt">> => #{
        required => false,
        datatype => binary
    },
    <<"keylen">> => #{
        required => false,
        datatype => integer
    },
    <<"iterations">> => #{
        required => false,
        datatype => integer
    }
}).


-define(WELCOME_DETAILS_SPEC, #{
    <<"authmethod">> => #{
        required => false,
        datatype => binary
    },
    <<"authid">> => #{
        % description => <<"The authentication ID (e.g. username) the client is authenticate as.">>,
        required => false,
        datatype => binary
    },
    <<"authrole">> => #{
        required => false,
        datatype => binary
    },
    <<"authprovider">> => #{
        % description => <<"Not in RFC">>,
        required => false,
        datatype => binary
    },
    <<"authextra">> => #{
        % description => <<"Not in RFC">>,
        required => false
    },
    <<"roles">> => #{
        required => true,
        datatype => map,
        validator => ?ROUTER_ROLES_SPEC
    },
    <<"agent">> => #{
        required => false,
        datatype => binary
    },
    <<"resumed">> => #{
        required => false,
        datatype => boolean
    },
    <<"resumable">> => #{
        required => false,
        datatype => boolean
    },
    <<"resume_token">> => #{
        % description => <<"The secure token required to resume the session defined in 'resume_session'.">>,
        required => false,
        datatype => binary
    }
}).

-define(GOODBYE_DETAILS_SPEC, #{
    <<"message">> => #{
        required => false,
        datatype => binary
    }
}).

-define(ABORT_DETAILS_SPEC, ?GOODBYE_DETAILS_SPEC).

-define(CALL_CANCELLING_OPTS_SPEC, #{
    <<"mode">> => #{
        required => false,
        datatype => {in, [<<"skip">>, <<"kill">>, <<"killnowait">>]}
    }
}).

-define(CALL_OPTS_SPEC, #{
    <<"timeout">> => #{
        required => false,
        default => 0,
        datatype => non_neg_integer
    },
    <<"receive_progress">> => #{
        required => false,
        datatype => boolean
    },
    <<"disclose_me">> => #{
        required => false,
        datatype => boolean
    },
    <<"runmode">> => #{
        required => false,
        datatype => {in, [<<"partition">>]}
    },
    <<"rkey">> => #{
        required => false,
        datatype => binary
    }
}).

-define(REGISTER_OPTS_SPEC, #{
    <<"disclose_caller">> => #{
        required => false,
        datatype => boolean
    },
    <<"match">> => #{
        required => false,
        datatype => {in, [
            <<"prefix">>, 
            <<"wildcard">>
        ]}
    },
    <<"invoque">> => #{
        required => false,
        default => <<"single">>,
        datatype => {in, [
            <<"single">>, 
            <<"roundrobin">>,
            <<"random">>, 
            <<"first">>,
            <<"last">>
        ]}
    }
}).

-define(SUBSCRIBE_OPTS_SPEC, #{
    <<"match">> => #{
        required => false,
        datatype => {in, [
            <<"prefix">>, 
            <<"wildcard">>
        ]}
    }
}).


-define(PUBLISH_OPTS_SPEC, #{
    %% resource key
   <<"rkey">> => #{
        required => false,
        datatype => binary
    },
    %% node key
    <<"nkey">> => #{
        required => false,
        datatype => binary
    },
    <<"disclose_me">> => #{
        required => false,
        datatype => boolean
    },
    <<"exclude_me">> => #{
        required => false,
        datatype => boolean
    },
    %% blacklisting
    <<"exclude">> => #{
        required => false,
        datatype => {list, integer}
    },
    <<"exclude_authid">> => #{
        required => false,
        datatype => {list, binary}
    },
    <<"exclude_authrole">> => #{
        required => false,
        datatype => {list, binary}
    },
    %% whitelisting
    <<"eligible">> => #{
        required => false,
        datatype => {list, integer}
    },
    <<"eligible_authid">> => #{
        required => false,
        datatype => {list, binary}
    },
    <<"eligible_authrole">> => #{
        required => false,
        datatype => {list, binary}
    }
}).

-define(INVOCATION_DETAILS_SPEC, #{
    <<"trustlevel">> => #{
        required => false,
        datatype => integer
    }
}).

-define(EVENT_DETAILS_SPEC, ?INVOCATION_DETAILS_SPEC).





%% =============================================================================
%% RECORD DEFINITIONS
%% =============================================================================



%% NOTICE: DO NOT CHANGE THE ORDER OF THE RECORD FIELDS as it maps
%% to the order in WAMP messages and we use list_to_tuple/1
-record(hello, {
    realm_uri       ::  uri(),
    details         ::  map()
}).
-type wamp_hello()       ::  #hello{}.

-record(challenge, {
    auth_method      ::  binary(),
    extra            ::  map()
}).
-type wamp_challenge()       ::  #challenge{}.

-record(authenticate, {
    signature       ::  binary(),
    extra           ::  map()
}).
-type wamp_authenticate()       ::  #authenticate{}.

-record(welcome, {
    session_id      ::  id(),
    details         ::  map()
}).
-type wamp_welcome()       ::  #welcome{}.

-record(abort, {
    details         ::  map(),
    reason_uri      ::  uri()
}).
-type wamp_abort()       ::  #abort{}.

-record(goodbye, {
    details         ::  map(),
    reason_uri      ::  uri()
}).
-type wamp_goodbye()       ::  #goodbye{}.

-record(error, {
    request_type    ::  pos_integer(),
    request_id      ::  id(),
    details         ::  map(),
    error_uri       ::  uri(),
    arguments       ::  list() | undefined,
    payload         ::  map() | undefined
}).
-type wamp_error()       ::  #error{}.

-record(publish, {
    request_id      ::  id(),
    options         ::  map(),
    topic_uri       ::  uri(),
    arguments       ::  list() | undefined,
    payload         ::  map() | undefined
}).
-type wamp_publish()       ::  #publish{}.

-record(published, {
    request_id      ::  id(),
    publication_id  ::  id()
}).
-type wamp_published()       ::  #published{}.

-record(subscribe, {
    request_id      ::  id(),
    options         ::  map(),
    topic_uri       ::  uri()
}).
-type wamp_subscribe()       ::  #subscribe{}.

-record(subscribed, {
    request_id      ::  id(),
    subscription_id ::  id()
}).
-type wamp_subscribed()       ::  #subscribed{}.

-record(unsubscribe, {
    request_id      ::  id(),
    subscription_id ::  id()
}).
-type wamp_unsubscribe()       ::  #unsubscribe{}.

-record(unsubscribed, {
    request_id      ::  id()
}).
-type wamp_unsubscribed()       ::  #unsubscribed{}.

-record(event, {
    subscription_id ::  id(),
    publication_id  ::  id(),
    details         ::  map(),
    arguments       ::  list() | undefined,
    payload         ::  map() | undefined
}).
-type wamp_event()       ::  #event{}.

-record(call, {
    request_id      ::  id(),
    options         ::  map(),
    procedure_uri   ::  uri(),
    arguments       ::  list() | undefined,
    payload         ::  map() | undefined
}).
-type wamp_call()       ::  #call{}.

-record(cancel, {
    request_id      ::  id(),
    options         ::  map()
}).
-type wamp_cancel()       ::  #cancel{}.

-record(result, {
    request_id      ::  id(),
    details         ::  map(),
    arguments       ::  list() | undefined,
    payload         ::  map() | undefined
}).
-type wamp_result()       ::  #result{}.

-record(register, {
    request_id      ::  id(),
    options         ::  map(),
    procedure_uri   ::  uri()
}).
-type wamp_register()       ::  #register{}.

-record(registered, {
    request_id      ::  id(),
    registration_id ::  id()
}).
-type wamp_registered()       ::  #registered{}.

-record(unregister, {
    request_id      ::  id(),
    registration_id ::  id()
}).
-type wamp_unregister()       ::  #unregister{}.

-record(unregistered, {
    request_id      ::  id()
}).
-type wamp_unregistered()       ::  #unregistered{}.

-record(invocation, {
    request_id      ::  id(),
    registration_id ::  id(),
    details         ::  map(),
    arguments       ::  list() | undefined,
    payload         ::  map() | undefined
}).
-type wamp_invocation()       ::  #invocation{}.

-record(interrupt, {
    request_id      ::  id(),
    options         ::  map()
}).
-type wamp_interrupt()       ::  #interrupt{}.

-record(yield, {
    request_id      ::  id(),
    options         ::  map(),
    arguments       ::  list() | undefined,
    payload         ::  map() | undefined
}).
-type wamp_yield()  ::  #yield{}.

-type wamp_message()     ::  wamp_hello() 
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



                                    
% -type hello_details()           ::  roles
%                                     | agent
%                                     | transport
%                                     | authmethods
%                                     | authid.
% -type result_details()          ::  progress.
% -type challenge_details()       ::  challenge 
%                                     | salt 
%                                     | keylen 
%                                     | iterations.
% -type invocation_details()      ::  caller
%                                     | trustlevel | procedure.
% -type event_details()           ::  publisher 
%                                     | trustlevel | topic.

% %% <<"exact">> | <<"prefix">> | <<"wildcard">>.
% -type match_policy()            ::  binary().

% %% <<"single">> | <<"roundrobin">> | <<"random">> | <<"first">> | <<"last">>.
% -type invocation_policy()       ::  binary().

% -type subscribe_options()       ::  match
%                                     | nkey
%                                     | disclose_caller.
% -type register_options()        ::  match | invoke | disclose_caller.
% -type call_runmode()            ::  partition.
% -type call_options()            ::  disclose_me
%                                     | runmode
%                                     | rkey
%                                     | receive_progress
%                                     | timeout.
% -type cancel_options()          ::  mode.
% -type yeild_options()           ::  progress.
% -type publish_options()         ::  acknowledge
%                                     | exclude
%                                     | exclude_authid
%                                     | exclude_authrole
%                                     | exclude_me
%                                     | disclose_me
%                                     | eligible
%                                     | eligible_authid
%                                     | eligible_authrole
%                                     | rkey.

-define(WAMP_ERROR_AUTHORIZATION_FAILED, <<"wamp.error.authorization_failed">>).
-define(WAMP_ERROR_CANCELLED, <<"wamp.error.cancelled">>).
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




%% Adictionary describing *features* supported by the peer for that role.
%% This MUST be empty for WAMP Basic Profile implementations, and MUST
%% be used by implementations implementing parts of the Advanced Profile
%% to list the specific set of features they support.
-type uri()             ::  binary().
-type id()              ::  0..?MAX_ID.



