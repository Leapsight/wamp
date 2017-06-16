%% -----------------------------------------------------------------------------
%% Copyright (C) Ngineo Limited 2015 - 2017. All rights reserved.
%% -----------------------------------------------------------------------------

%% =============================================================================
%% @doc
%% A Session lets you access information that defines the state of an
%% interaction.
%%
%% The Session is passed as an argument through the whole request-response
%%  loop to provide access to that information.
%% @end
%% =============================================================================
-module(wamp_session).
-include("wamp.hrl").

-define(REQ_TIMEOUT, 15000).

-record(session, {
    id                              ::  id(),
    pid = self()                    ::  pid(),
    realm_uri                       ::  uri(),
    %% Peer and roles
    peer                            ::  wamp_protocol:peer(),
    roles                           ::  map(),
    %% The authentication ID of the session that joined
    authid                          ::  binary(),
    %% The authentication role of the session that joined
    authrole                        ::  binary(),
    %% The method that was used for authentication 
    authmethod = ?TICKET_AUTH       ::  binary(),
    %% The provider that performed the authentication of the session that joined
    authprovider                    ::  binary(),
    %% Session resumption
    is_active = true                ::  boolean(),
    resumed = false                 ::  boolean(),
    resumable = false               ::  boolean(),
    resumed_token                   ::  binary(),
    %% State
    awaiting_calls = sets:new()     ::  sets:set(),
    request_timeout = ?REQ_TIMEOUT  ::  non_neg_integer(),
    request_details                 ::  map(),
    %% Our extensions
    counters_tab                    ::  ets:tab(),
    expires_in = infinity           ::  timeout(),
    seq = 0                         ::  non_neg_integer(),
    created                         ::  calendar:date_time(),
    last_updated                    ::  pos_integer(),
    %% Metadata map 
    metadata = #{}                  ::  map()
}).


-type session()                     :: #session{}.
-type session_opts()                ::  #{roles => map()}.

-export_type([session/0]).
-export_type([session_opts/0]).

%% API
-export([add_awaiting_call/2]).
-export([awaiting_calls/1]).
-export([close/1]).
-export([id/1]).
-export([is_feature_enabled/3]).
-export([new/3]).
-export([peer/1]).
-export([peer_id/1]).
-export([realm_uri/1]).
-export([set_realm_uri/2]).
-export([remove_awaiting_call/2]).
-export([request_timeout/1]).
-export([reset/1]).
-export([roles/1]).
-export([set_peer/2]).
-export([set_request_timeout/2]).
-export([set_roles/2]).
-export([get_id/2]).
%% API: Metadata utils
-export([get/2]).
-export([get/3]).
-export([get_path/2]).
-export([put/3]).
-export([put_path/3]).
-export([remove/2]).
-export([remove_path/2]).






%% =============================================================================
%% API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc
%% Creates a new client session.
%% 
%% 
%% Opts is a map containing the following keys:
%% - authid (required) - the user id of the client
%% - authmethod (required) - the method to be used for authentication 
%% - authrole (optional) - the authentication role for the session
%% - agent (optional) - a name identifying the client 
%% - roles (optional) - a map containing at least one entry with key Role and
%% for value a map of features (possibly empty) where Role is one of 'callee',
%%  'caller', 'publisher', 'subscriber'.
%% -----------------------------------------------------------------------------
-spec new(wamp_protocol:peer(), uri(), session_opts()) -> 
    session() | no_return().

new(Peer, RealmUri, Opts) when is_map(Opts) ->
    %% TODO replace with tuplespace
    %% This table is used to allow concurrent atomic updates to the 
    %% session scope IDs.
    Tab = ets:new(session_counters, [
        set,
        {keypos, 1},
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    
    Roles = maps:get(<<"roles">>, Opts),
    maps:size(Roles) > 0 orelse error({invalid_options, missing_client_role}),

    Now = erlang:universaltime(),
    
    #session{
        id = wamp_utils:rand_uniform(),
        peer = Peer,
        roles = Roles,
        realm_uri = RealmUri,
        counters_tab = Tab,
        created = Now,
        last_updated = Now
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% Resets the session. Returns a copy of Ctxt where the following attributes
%% have been reset: request_timeout.
%% @end
%% -----------------------------------------------------------------------------
-spec reset(session()) -> session().

reset(S) ->
    S#session{
        request_timeout = 0
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% Closes the session.
%% @end
%% -----------------------------------------------------------------------------
-spec close(session()) -> ok.

close(#session{counters_tab = Tab}) ->
    ets:delete(Tab).


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the peer of the provided session.
%% @end
%% -----------------------------------------------------------------------------
-spec peer(session()) -> wamp_protocol:peer().

peer(#session{peer = Val}) -> Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Set the peer to the provided session.
%% @end
%% -----------------------------------------------------------------------------
-spec set_peer(session(), wamp_protocol:peer()) -> session().

set_peer(S, {{_, _, _, _}, _} = Peer) ->
    S#session{peer = Peer}.





%% -----------------------------------------------------------------------------
%% @doc
%% Returns the sessionId of the provided session or 'undefined' 
%% if there is none.
%% @end
%% -----------------------------------------------------------------------------
-spec id(session()) -> id().

id(#session{id = Val}) -> Val.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec peer_id(session()) -> {id(), pid()}.

peer_id(#session{id = Id, pid = Pid}) ->
    {Id, Pid}.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the realm uri of the provided session.
%% @end
%% -----------------------------------------------------------------------------
-spec realm_uri(session()) -> uri().

realm_uri(#session{realm_uri = Val}) -> Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Set the realm_uri to the provided session.
%% @end
%% -----------------------------------------------------------------------------
-spec set_realm_uri(session(), uri()) -> session().

set_realm_uri(S, Uri) ->
    S#session{realm_uri = Uri}.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the roles of the provided session.
%% @end
%% -----------------------------------------------------------------------------
-spec roles(session()) -> map().

roles(#session{roles = Val}) -> Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Sets the roles to the provided session.
%% @end
%% -----------------------------------------------------------------------------
-spec set_roles(session(), map()) -> session().

set_roles(S, Roles) when is_map(Roles) ->
    S#session{roles = Roles}.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the current request timeout.
%% @end
%% -----------------------------------------------------------------------------
-spec request_timeout(session()) -> non_neg_integer().

request_timeout(#session{request_timeout = Val}) ->
    Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Sets the current request timeout to the provided session.
%% @end
%% -----------------------------------------------------------------------------
-spec set_request_timeout(session(), non_neg_integer()) -> session().

set_request_timeout(S, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    S#session{request_timeout = Timeout}.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns true if the feature Feature is enabled for role Role.
%% @end
%% -----------------------------------------------------------------------------
-spec is_feature_enabled(session(), atom(), binary()) -> boolean().

is_feature_enabled(#session{roles = Roles}, Role, Feature) 
when is_binary(Feature) ->
    maps_utils:get_path([Role, Feature], Roles, false).



%% -----------------------------------------------------------------------------
%% @doc
%% Returns a list containing the identifiers for the calls the peer performed 
%% and it is still awaiting a response for.  This is used by the internal rpc
%% mechanism which is based on promises.
%% @end
%% -----------------------------------------------------------------------------
-spec awaiting_calls(session()) -> [id()].

awaiting_calls(#session{awaiting_calls = S}) ->
    sets:to_list(S).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec add_awaiting_call(session(), id()) -> session().

add_awaiting_call(#session{awaiting_calls = Set} = S, Id) ->
    S#session{awaiting_calls = sets:add_element(Id, Set)}.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec remove_awaiting_call(session(), id()) -> session().

remove_awaiting_call(#session{awaiting_calls = Set} = S, Id) ->
    S#session{awaiting_calls = sets:del_element(Id, Set)}.





%% -----------------------------------------------------------------------------
%% @doc
%% Returns value Value associated with Key if the Session metadata
%% map contains Key.
%% The call fails with a `{badkey,Key}` exception if no value is associated 
%% with Key.
%% @end
%% -----------------------------------------------------------------------------
-spec get(Session :: session(), Key :: any()) -> Value :: any().

get(#session{metadata = M}, K) ->
    maps:get(K, M).



%% -----------------------------------------------------------------------------
%% @doc
%% Returns value Value associated with Key if the Session metadata
%% map contains Key.
%% If no value is associated with Key, Default is returned.
%% @end
%% -----------------------------------------------------------------------------
-spec get(Session :: session(), Key :: any(), Default :: any()) -> 
    Value :: any().

get(#session{metadata = M}, K, Default) ->
    maps:get(K, M, Default).


%% -----------------------------------------------------------------------------
%% @doc
%% Returns value Value associated with path Path if the Session metadata
%% map contains Path.
%% The call fails with a `{badkey,Key}` exception where Key is any component of 
%% Path missing or if no value is associated with Path.
%% @end
%% -----------------------------------------------------------------------------
-spec get_path(Session :: session(), Path :: list()) -> Value :: any().

get_path(#session{metadata = M}, P) ->
    maps_utils:get_path(P, M).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec put(Session :: session(), Key :: any(), Value :: any()) -> 
    NewSession :: session().

put(#session{metadata = M}, K, V) ->
    maps_utils:put(K, V, M).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec put_path(Session :: session(), Path :: list(), Value :: any()) -> 
    NewSession :: session().

put_path(#session{metadata = M}, P, V) ->
    maps_utils:put_path(P, V, M).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec remove(Session :: session(), Key :: any())-> NewSession :: session().

remove(#session{metadata = M}, K) ->
    maps_utils:remove(K, M).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec remove_path(Session :: session(), Path :: list()) -> 
    NewSession :: session().

remove_path(#session{metadata = M}, P) ->
    maps_utils:remove_path(P, M).



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_id(id() | session(), MsgId :: pos_integer()) -> map().

get_id(#session{} = S, X) ->
    case id_type(X) of 
        global ->
            crypto:rand_uniform(0, ?MAX_ID);
        router ->
            crypto:rand_uniform(0, ?MAX_ID);
        session ->
            ets:update_counter(
                S#session.counters_tab, X, {2, 1, ?MAX_ID, 1}, {X, 0})
    end.


%% @private
id_type(?WELCOME) ->        global;
id_type(?ERROR) ->          session;
id_type(?PUBLISH) ->        session;
id_type(?PUBLISHED) ->      global;
id_type(?SUBSCRIBE) ->      session;
id_type(?SUBSCRIBED) ->     router;
id_type(?UNSUBSCRIBE) ->    router;
id_type(?UNSUBSCRIBED) ->   session;
id_type(?EVENT) ->          global;
id_type(?CALL) ->           session;
id_type(?CANCEL) ->         session;
id_type(?RESULT) ->         session;
id_type(?REGISTER) ->       session;
id_type(?REGISTERED) ->     router;
id_type(?UNREGISTER) ->     router;
id_type(?UNREGISTERED) ->   session;
id_type(?INVOCATION) ->     router;
id_type(?INTERRUPT) ->      session;
id_type(?YIELD) ->          session.


%% =============================================================================
%% PRIVATE
%% =============================================================================



