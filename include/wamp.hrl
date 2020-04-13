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


%% =============================================================================
%% RAW
%% =============================================================================



-define(RAW_MAGIC, 16#7F).
-define(RAW_MSG_PREFIX, <<0:5, 0:3>>).
-define(RAW_PING_PREFIX, <<0:5, 1:3>>).
-define(RAW_PONG_PREFIX, <<0:5, 2:3>>).

%% 0: illegal (must not be used)
%% 1: serializer unsupported
%% 2: maximum message length unacceptable
%% 3: use of reserved bits (unsupported feature)
%% 4: maximum connection count reached
%% 5 - 15: reserved for future errors
-define(RAW_ERROR(Upper), <<?RAW_MAGIC:8, Upper:4, 0:20>>).
-define(RAW_FRAME(Bin),
    <<(?RAW_MSG_PREFIX)/binary, (byte_size(Bin)):24, Bin/binary>>
).

-type raw_error()           ::  invalid_response
                                | invalid_socket
                                | invalid_handshake
                                | maximum_connection_count_reached
                                | maximum_message_length_unacceptable
                                | maximum_message_length_exceeded
                                | serializer_unsupported
                                | use_of_reserved_bits.

%% =============================================================================
%% AUTH
%% =============================================================================



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
-type subprotocol()         ::  {transport(), frame_type(), encoding()}.



%% =============================================================================
%% FEATURE ANNOUNCEMENT
%% =============================================================================



%% maps_utils:map_spec()
-define(DEALER_FEATURES_SPEC, ?WAMP_DEALER_FEATURES_SPEC#{
    call_retries => #{
        alias => <<"call_retries">>,
        required => false,
        datatype => boolean}
}).


%% maps_utils:map_spec()
-define(WAMP_DEALER_FEATURES_SPEC, #{
    progressive_call_results => #{
        alias => <<"progressive_call_results">>,
        required => false,
        datatype => boolean},
    progressive_calls => #{
        alias => <<"progressive_calls">>,
        required => false,
        datatype => boolean},
    call_timeout => #{
        alias => <<"call_timeout">>,
        required => false,
        datatype => boolean},
    call_canceling => #{
        alias => <<"call_canceling">>,
        required => false,
        datatype => boolean},
    caller_identification=> #{
        alias => <<"caller_identification">>,
        required => false,
        datatype => boolean},
    call_trustlevels => #{
        alias => <<"call_trustlevels">>,
        required => false,
        datatype => boolean},
    registration_meta_api => #{
        alias => <<"registration_meta_api">>,
        required => false,
        datatype => boolean},
    registration_revocation => #{
        alias => <<"registration_revocation">>,
        required => false,
        datatype => boolean},
    session_meta_api => #{
        alias => <<"session_meta_api">>,
        required => false,
        datatype => boolean},
    pattern_based_registration => #{
        alias => <<"pattern_based_registration">>,
        required => false,
        datatype => boolean},
    procedure_reflection => #{
        alias => <<"procedure_reflection">>,
        required => false,
        datatype => boolean},
    shared_registration => #{
        alias => <<"shared_registration">>,
        required => false,
        datatype => boolean},
    sharded_registration => #{
        alias => <<"sharded_registration">>,
        required => false,
        datatype => boolean}
}).

%% maps_utils:map_spec()
-define(BROKER_FEATURES_SPEC, #{
    retained_events => #{
        alias => <<"retained_events">>,
        required => false,
        datatype => boolean},
    event_history => #{
        alias => <<"event_history">>,
        required => false,
        datatype => boolean},
    pattern_based_subscription => #{
        alias => <<"pattern_based_subscription">>,
        required => false,
        datatype => boolean},
    publication_trustlevels => #{
        alias => <<"publication_trustlevels">>,
        required => false,
        datatype => boolean},
    publisher_exclusion => #{
        alias => <<"publisher_exclusion">>,
        required => false,
        datatype => boolean},
    publisher_identification => #{
        alias => <<"publisher_identification">>,
        required => false,
        datatype => boolean},
    session_meta_api => #{
        alias => <<"session_meta_api">>,
        required => false,
        datatype => boolean},
    sharded_subscription => #{
        alias => <<"sharded_subscription">>,
        required => false,
        datatype => boolean},
    subscriber_blackwhite_listing => #{
        alias => <<"subscriber_blackwhite_listing">>,
        required => false,
        datatype => boolean},
    subscription_meta_api => #{
        alias => <<"subscription_meta_api">>,
        required => false,
        datatype => boolean},
    topic_reflection => #{
        alias => <<"topic_reflection">>,
        required => false,
        datatype => boolean}
}).

%% maps_utils:map_spec()
-define(BROKER_ROLE_SPEC, #{
    features => #{
        alias => <<"features">>,
        required => false,
        datatype => map,
        validator => ?BROKER_FEATURES_SPEC
    }
}).

%% maps_utils:map_spec()
-define(DEALER_ROLE_SPEC, #{
    features => #{
        alias => <<"features">>,
        required => false,
        datatype => map,
        validator => ?DEALER_FEATURES_SPEC
    }
}).

%% maps_utils:map_spec()
-define(ROUTER_ROLES_SPEC, #{
    broker => #{
        alias => <<"broker">>,
        required => false,
        datatype => map,
        validator => ?BROKER_ROLE_SPEC},
    dealer => #{
        alias => <<"dealer">>,
        required => false,
        datatype => map,
        validator => ?DEALER_ROLE_SPEC}
}).


%% maps_utils:map_spec()

-define(CALLEE_FEATURES_SPEC, #{
    progressive_call_results => #{
        alias => <<"progressive_call_results">>,
        required => false,
        datatype => boolean},
    progressive_calls => #{
        alias => <<"progressive_calls">>,
        required => false,
        datatype => boolean},
    call_timeout => #{
        alias => <<"call_timeout">>,
        required => false,
        datatype => boolean},
    call_canceling => #{
        alias => <<"call_canceling">>,
        required => false,
        datatype => boolean},
    caller_identification => #{
        alias => <<"caller_identification">>,
        required => false,
        datatype => boolean},
    call_trustlevels => #{
        alias => <<"call_trustlevels">>,
        required => false,
        datatype => boolean},
    registration_revocation => #{
        alias => <<"registration_revocation">>,
        required => false,
        datatype => boolean},
    session_meta_api => #{
        alias => <<"session_meta_api">>,
        required => false,
        datatype => boolean},
    pattern_based_registration => #{
        alias => <<"pattern_based_registration">>,
        required => false,
        datatype => boolean},
    shared_registration => #{
        alias => <<"shared_registration">>,
        required => false,
        datatype => boolean},
    sharded_registration => #{
        alias => <<"sharded_registration">>,
        required => false,
        datatype => boolean}
}).

%% maps_utils:map_spec()

-define(CALLER_FEATURES_SPEC, ?WAMP_CALLER_FEATURES_SPEC#{
    call_retries => #{
        alias => <<"call_retries">>,
        required => false,
        datatype => boolean}
}).

-define(WAMP_CALLER_FEATURES_SPEC, #{
    progressive_call_results => #{
        alias => <<"progressive_call_results">>,
        required => false,
        datatype => boolean},
    progressive_calls => #{
        alias => <<"progressive_calls">>,
        required => false,
        datatype => boolean},
    call_timeout => #{
        alias => <<"call_timeout">>,
        required => false,
        datatype => boolean},
    call_canceling => #{
        alias => <<"call_canceling">>,
        required => false,
        datatype => boolean},
    caller_identification=> #{
        alias => <<"caller_identification">>,
        required => false,
        datatype => boolean}
}).

%% maps_utils:map_spec()
-define(SUBSCRIBER_FEATURES_SPEC, #{
    event_history => #{
        alias => <<"event_history">>,
        required => false,
        datatype => boolean},
    pattern_based_subscription => #{
        alias => <<"pattern_based_subscription">>,
        required => false,
        datatype => boolean},
    publication_trustlevels => #{
        alias => <<"publication_trustlevels">>,
        required => false,
        datatype => boolean},
    publisher_identification => #{
        alias => <<"publisher_identification">>,
        required => false,
        datatype => boolean},
    sharded_subscription => #{
        alias => <<"sharded_subscription">>,
        required => false,
        datatype => boolean}
}).

%% maps_utils:map_spec()
-define(PUBLISHER_FEATURES_SPEC, #{
    retained_events => #{
        alias => <<"retained_events">>,
        required => false,
        datatype => boolean},
    publisher_exclusion => #{
        alias => <<"publisher_exclusion">>,
        required => false,
        datatype => boolean},
    publisher_identification => #{
        alias => <<"publisher_identification">>,
        required => false,
        datatype => boolean},
    subscriber_blackwhite_listing => #{
        alias => <<"subscriber_blackwhite_listing">>,
        required => false,
        datatype => boolean}
}).

%% maps_utils:map_spec()
-define(PUBLISHER_ROLE_SPEC, #{
    features => #{
        alias => <<"features">>,
        required => false,
        datatype => map,
        validator => ?PUBLISHER_FEATURES_SPEC
    }
}).

%% maps_utils:map_spec()
-define(SUBSCRIBER_ROLE_SPEC, #{
    features => #{
        alias => <<"features">>,
        required => false,
        datatype => map,
        validator => ?SUBSCRIBER_FEATURES_SPEC
    }
}).

%% maps_utils:map_spec()
-define(CALLER_ROLE_SPEC, #{
    features => #{
        alias => <<"features">>,
        required => false,
        datatype => map,
        validator => ?CALLER_FEATURES_SPEC
    }
}).

%% maps_utils:map_spec()
-define(CALLEE_ROLE_SPEC, #{
    features => #{
        alias => <<"features">>,
        required => false,
        datatype => map,
        validator => ?CALLEE_FEATURES_SPEC
    }
}).


%% maps_utils:map_spec()
-define(CLIENT_ROLES_SPEC, #{
    publisher => #{
        alias => <<"publisher">>,
        required => false,
        datatype => map,
        validator => ?PUBLISHER_ROLE_SPEC},
    subscriber => #{
        alias => <<"subscriber">>,
        required => false,
        datatype => map,
        validator => ?SUBSCRIBER_ROLE_SPEC},
    caller => #{
        alias => <<"caller">>,
        required => false,
        datatype => map,
        validator => ?CALLER_ROLE_SPEC},
    callee => #{
        alias => <<"callee">>,
        required => false,
        datatype => map,
        validator => ?CALLEE_ROLE_SPEC}
}).




%% =============================================================================
%% MESSAGES
%% =============================================================================




-define(MAX_ID, 9007199254740993).



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

-type message_name() :: hello
                        | welcome
                        | abort
                        | challenge
                        | authenticate
                        | goodbye
                        | error
                        | publish
                        | published
                        | subscribe
                        | subscribed
                        | unsubscribe
                        | unsubscribed
                        | event
                        | call
                        | cancel
                        | result
                        | register
                        | registered
                        | unregister
                        | unregistered
                        | invocation
                        | interrupt
                        | yield.


%% maps_utils:map_spec()
-define(HELLO_DETAILS_SPEC, #{
    authmethods => #{
        % description => Used by the client to announce the authentication methods it is prepared to perform.">>,
        alias => <<"authmethods">>,
        required => false,
        datatype => {list, {in, ?WAMP_AUTH_METHODS}}
    },
    authid => #{
        % description => <<"Te authentication ID (e.g. username) the client wishes to authenticate as.">>,
        alias => <<"authid">>,
        required => false,
        datatype => binary
    },
    authrole => #{
        alias => <<"authrole">>,
        required => false,
        datatype => binary
    },
    authextra => #{
        % description => <<"Not in RFC">>,
        alias => <<"authextra">>,
        required => false
    },
    roles => #{
        alias => <<"roles">>,
        required => true,
        datatype => map,
        validator => ?CLIENT_ROLES_SPEC
    },
    agent => #{
        % description => <<"When a software agent operates in a network protocol, it often identifies itself, its application type, operating system, software vendor, or software revision, by submitting a characteristic identification string to its operating peer. Similar to what browsers do with the User-Agent HTTP header, both the HELLO and the WELCOME message MAY disclose the WAMP implementation in use to its peer">>,
        alias => <<"agent">>,
        required => false,
        datatype => binary
    },
    transport => #{
        % description => <<"When running WAMP over a TLS (either secure WebSocket
        % or raw TCP) transport, a peer may authenticate to the other via the TLS certificate mechanism. A server might authenticate to the client, and a client may authenticate to the server (TLS client-certificate based authentication). This transport-level authentication information may be forward to the WAMP level within HELLO.Details.transport.auth|any in both directions (if available).">>,
        alias => <<"transport">>,
        required => false,
        datatype => map,
        validator => #{
            auth => #{required => true}
        }
    },
    resumable => #{
        alias => <<"resumable">>,
        required => false,
        datatype => boolean
    },
    resume_session => #{
        % description => <<"The session ID the client would like to resume.">>,
        alias => <<"resume_session">>,
        required => false,
        datatype => binary
    },
    resume_token => #{
        % description => <<"The secure token required to resume the session defined in 'resume_session'.">>,
        alias => <<"resume_token">>,
        required => false,
        datatype => binary
    }
}).

%% maps_utils:map_spec()
-define(CHALLENGE_DETAILS_SPEC, #{
    challenge => #{
        alias => <<"challenge">>,
        required => false,
        datatype => binary
    },
    salt => #{
        alias => <<"salt">>,
        required => false,
        datatype => binary
    },
    keylen => #{
        alias => <<"keylen">>,
        required => false,
        datatype => integer
    },
    iterations => #{
        alias => <<"iterations">>,
        required => false,
        datatype => integer
    }
}).


%% maps_utils:map_spec()
-define(WELCOME_DETAILS_SPEC, #{
    authmethod => #{
        alias => <<"authmethod">>,
        required => false,
        datatype => binary
    },
    authid => #{
        % description => <<"The authentication ID (e.g. username) the client is authenticate as.">>,
        alias => <<"authid">>,
        required => false,
        datatype => binary
    },
    authrole => #{
        alias => <<"authrole">>,
        required => false,
        datatype => binary
    },
    authprovider => #{
        alias => <<"authprovider">>,
        % description => <<"Not in RFC">>,
        required => false,
        datatype => binary
    },
    authextra => #{
        % description => <<"Not in RFC">>,
        alias => <<"authextra">>,
        required => false
    },
    roles => #{
        alias => <<"roles">>,
        required => true,
        datatype => map,
        validator => ?ROUTER_ROLES_SPEC
    },
    agent => #{
        alias => <<"agent">>,
        required => false,
        datatype => binary
    },
    resumed => #{
        alias => <<"resumed">>,
        required => false,
        datatype => boolean
    },
    resumable => #{
        alias => <<"resumable">>,
        required => false,
        datatype => boolean
    },
    resume_token => #{
        % description => <<"The secure token required to resume the session defined in 'resume_session'.">>,
        alias => <<"resume_token">>,
        required => false,
        datatype => binary
    }
}).

%% maps_utils:map_spec()
-define(GOODBYE_DETAILS_SPEC, #{
    message => #{
        alias => <<"message">>,
        required => false,
        datatype => binary
    }
}).

%% maps_utils:map_spec()
-define(ABORT_DETAILS_SPEC, ?GOODBYE_DETAILS_SPEC).

%% maps_utils:map_spec()
-define(CALL_CANCELLING_OPTS_SPEC, #{
    mode => #{
        alias => <<"mode">>,
        required => false,
        datatype => {in, [<<"skip">>, <<"kill">>, <<"killnowait">>]}
    }
}).

%% maps_utils:map_spec()

%% BONDY ALPHA
-define(CALL_OPTS_SPEC, ?WAMP_CALL_OPTS_SPEC#{
    retries => #{
        alias => <<"retries">>,
        required => false,
        datatype => map,
        validator => ?RETRIES_SPEC
    }
}).

-define(RETRIES_SPEC, #{
    allowance => #{
        alias => <<"allowance">>,
        required => true,
        datatype => map,
        validator => #{
            min_retries_per_sec => #{
                alias => <<"min_retries_frequency">>,
                required => true,
                default => 5,
                datatype => non_neg_integer
            },
            ratio => #{
                alias => <<"ratio">>,
                required => true,
                default => 0.5,
                datatype => float
            },
            ttl => #{
                alias => <<"ttl">>,
                required => true,
                default => 5000,
                datatype => timeout
            }
        }
    },
    backoff => #{
        alias => <<"backoff">>,
        required => true,
        datatype => map,
        validator => #{
            type => #{
                alias => <<"type">>,
                required => true,
                default => <<"normal">>,
                datatype => {in, [<<"normal">>, <<"jitter">>]}
            },
            min_duration => #{
                alias => <<"min_duration">>,
                required => true,
                default => 50,
                datatype => non_neg_integer
            },
            max_duration => #{
                alias => <<"max_duration">>,
                required => true,
                default => 5000,
                datatype => non_neg_integer
            }
        }
    }
}).

-define(WAMP_CALL_OPTS_SPEC, #{
    timeout => #{
        alias => <<"timeout">>,
        required => false,
        default => 0,
        datatype => timeout
    },
    receive_progress => #{
        alias => <<"receive_progress">>,
        required => false,
        datatype => boolean
    },
    disclose_me => #{
        alias => <<"disclose_me">>,
        required => false,
        datatype => boolean
    },
    runmode => #{
        alias => <<"runmode">>,
        required => false,
        datatype => {in, [<<"partition">>]}
    },
    %% if runmode is present then rkey should be present
    rkey => #{
        alias => <<"rkey">>,
        required => false,
        datatype => binary
    },
    %% 'all' invocation strategy (ALPHA)
    yields => #{
        alias => <<"yields">>,
        required => false,
        datatype => {in, [<<"first">>, <<"gather">>, <<"progressive">>]}
    }
}).

-define(EXACT_MATCH, <<"exact">>).
-define(PREFIX_MATCH, <<"prefix">>).
-define(WILDCARD_MATCH, <<"wildcard">>).
-define(MATCH_STRATEGIES, [
    ?EXACT_MATCH,
    ?PREFIX_MATCH,
    ?WILDCARD_MATCH
]).
-define(INVOKE_SINGLE, <<"single">>).
-define(INVOKE_ROUND_ROBIN, <<"roundrobin">>).
-define(INVOKE_RANDOM, <<"random">>).
-define(INVOKE_FIRST, <<"first">>).
-define(INVOKE_LAST, <<"last">>).
%% ALPHA
-define(INVOKE_ALL, <<"last">>).


%% maps_utils:map_spec()
-define(REGISTER_OPTS_SPEC, #{
    disclose_caller => #{
        alias => <<"disclose_caller">>,
        required => false,
        datatype => boolean
    },
    match => #{
        alias => <<"match">>,
        required => false,
        datatype => {in, ?MATCH_STRATEGIES}
    },
    invoke => #{
        alias => <<"invoke">>,
        required => false,
        default => ?INVOKE_SINGLE,
        datatype => {in, [
            ?INVOKE_SINGLE,
            ?INVOKE_ROUND_ROBIN,
            ?INVOKE_RANDOM,
            ?INVOKE_FIRST,
            ?INVOKE_LAST,
            ?INVOKE_ALL
        ]}
    }
}).


%% maps_utils:map_spec()
-define(SUBSCRIBE_OPTS_SPEC, #{
    match => #{
        alias => <<"match">>,
        required => false,
        datatype => {in, ?MATCH_STRATEGIES}
    },
    %% node key
    nkey => #{
        alias => <<"nkey">>,
        required => false,
        datatype => binary
    },
    get_retained => #{
        alias => <<"get_retained">>,
        required => false,
        datatype => boolean
    }
}).


%% maps_utils:map_spec()
-define(PUBLISH_OPTS_SPEC, #{
    %% resource key
    acknowledge => #{
        alias => <<"acknowledge">>,
        required => false,
        datatype => boolean
    },
    rkey => #{
        alias => <<"rkey">>,
        required => false,
        datatype => binary
    },
    disclose_me => #{
        alias => <<"disclose_me">>,
        required => false,
        datatype => boolean
    },
    %% blacklisting
    exclude => #{
        alias => <<"exclude">>,
        required => false,
        datatype => {list, integer}
    },
    exclude_authid => #{
        alias => <<"exclude_authid">>,
        required => false,
        datatype => {list, binary}
    },
    exclude_authrole => #{
        alias => <<"exclude_authrole">>,
        required => false,
        datatype => {list, binary}
    },
    exclude_me => #{
        alias => <<"exclude_me">>,
        required => false,
        datatype => boolean
    },
    %% whitelisting
    eligible => #{
        alias => <<"eligible">>,
        required => false,
        datatype => {list, integer}
    },
    eligible_authid => #{
        alias => <<"eligible_authid">>,
        required => false,
        datatype => {list, binary}
    },
    eligible_authrole => #{
        alias => <<"eligible_authrole">>,
        required => false,
        datatype => {list, binary}
    },
    retain => #{
        alias => <<"retain">>,
        required => false,
        datatype => boolean
    }
}).

%% maps_utils:map_spec()
-define(INVOCATION_DETAILS_SPEC, #{
    trustlevel => #{
        alias => <<"trustlevel">>,
        required => false,
        datatype => integer
    },
    procedure => #{
        alias => <<"procedure">>,
        required => false,
        datatype => binary
    },
    caller => #{
        alias => <<"caller">>,
        required => false,
        datatype => integer
    }
}).

-define(YIELD_OPTIONS_SPEC, #{
    progress => #{
        alias => <<"progress">>,
        required => false,
        datatype => boolean
    }
}).

-define(RESULT_DETAILS_SPEC, #{
    progress => #{
        alias => <<"progress">>,
        required => false,
        datatype => boolean
    }
}).

%% maps_utils:map_spec()
-define(EVENT_DETAILS_SPEC, ?PUBLISH_OPTS_SPEC).





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
    arguments_kw         ::  map() | undefined
}).
-type wamp_error()       ::  #error{}.

-record(publish, {
    request_id      ::  id(),
    options         ::  map(),
    topic_uri       ::  uri(),
    arguments       ::  list() | undefined,
    arguments_kw         ::  map() | undefined
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
    arguments_kw         ::  map() | undefined
}).
-type wamp_event()       ::  #event{}.

-record(call, {
    request_id      ::  id(),
    options         ::  map(),
    procedure_uri   ::  uri(),
    arguments       ::  list() | undefined,
    arguments_kw         ::  map() | undefined
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
    arguments_kw         ::  map() | undefined
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
    arguments_kw         ::  map() | undefined
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
    arguments_kw         ::  map() | undefined
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




%% =============================================================================
%% META EBENT URIS
%% =============================================================================


-define(WAMP_REGISTRATION_ON_CREATE, <<"wamp.registration.on_create">>).
-define(WAMP_REGISTRATION_ON_REGISTER, <<"wamp.registration.on_register">>).
-define(WAMP_REGISTRATION_ON_UNREGISTER, <<"wamp.registration.on_unregister">>).
-define(WAMP_REGISTRATION_ON_DELETE, <<"wamp.registration.on_delete">>).

-define(WAMP_SUBCRIPTION_ON_CREATE, <<"wamp.subscription.on_create">>).
-define(WAMP_SUBCRIPTION_ON_REGISTER, <<"wamp.subscription.on_subscribe">>).
-define(WAMP_SUBCRIPTION_ON_UNREGISTER, <<"wamp.subscription.on_unsubscribe">>).
-define(WAMP_SUBCRIPTION_ON_DELETE, <<"wamp.subscription.on_delete">>).

-define(WAMP_SESSION_ON_JOIN, <<"wamp.session.on_join">>).
-define(WAMP_SESSION_ON_LEAVE, <<"wamp.session.on_leave">>).



%% =============================================================================
%% ERROR URIS
%% =============================================================================



%% INCORRECT URIs
-define(WAMP_INVALID_URI, <<"wamp.error.invalid_uri">>).

%% INTERACTION URIs
-define(WAMP_NO_SUCH_PROCEDURE, <<"wamp.error.no_such_procedure">>).
-define(WAMP_PROCEDURE_ALREADY_EXISTS,
    <<"wamp.error.procedure_already_exists">>).
-define(WAMP_NO_SUCH_REGISTRATION, <<"wamp.error.no_such_registration">>).
-define(WAMP_NO_SUCH_SUBSCRIPTION, <<"wamp.error.no_such_subscription">>).
-define(WAMP_INVALID_ARGUMENT, <<"wamp.error.invalid_argument">>).

%% SESSION CLOSE
-define(WAMP_SYSTEM_SHUTDOWN, <<"wamp.close.system_shutdown">>).
-define(WAMP_CLOSE_REALM, <<"wamp.close.close_realm">>).
-define(WAMP_GOODBYE_AND_OUT, <<"wamp.close.goodbye_and_out">>).
-define(WAMP_PROTOCOL_VIOLATION, <<"wamp.error.protocol_violation">>).

%% AUTORIZATION
-define(WAMP_NOT_AUTHORIZED, <<"wamp.error.not_authorized">>).
-define(WAMP_AUTHORIZATION_FAILED, <<"wamp.error.authorization_failed">>).
-define(WAMP_NO_SUCH_REALM, <<"wamp.error.no_such_realm">>).
-define(WAMP_NO_SUCH_ROLE, <<"wamp.error.no_such_role">>).

%% ADVANCES
-define(WAMP_CANCELLED, <<"wamp.error.canceled">>).
-define(WAMP_OPTION_NOT_ALLOWED, <<"wamp.error.option_not_allowed">>).
-define(WAMP_NO_ELIGIBLE_CALLE, <<"wamp.error.no_eligible_callee">>).
-define(WAMP_OPTION_DISALLOWED_DISCLOSE_ME,
    <<"wamp.error.option_disallowed.disclose_me">>).

-define(WAMP_DISCLOSE_ME_NOT_ALLOWED, <<"wamp.error.disclose_me.not_allowed">>).
-define(WAMP_NET_FAILURE, <<"wamp.error.network_failure">>).

-define(WAMP_NO_SUCH_SESSION, <<"wamp.error.no_such_session">>).




%% Adictionary describing *features* supported by the peer for that role.
%% This MUST be empty for WAMP Basic Profile implementations, and MUST
%% be used by implementations implementing parts of the Advanced Profile
%% to list the specific set of features they support.
-type uri()             ::  binary().
-type id()              ::  0..?MAX_ID.



