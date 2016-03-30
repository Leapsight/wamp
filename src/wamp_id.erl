%% -----------------------------------------------------------------------------
%% Copyright (C) Ngineo Limited 2015 - 2016. All rights reserved.
%% -----------------------------------------------------------------------------

%% =============================================================================
%% @doc
%% WAMP needs to identify the following ephemeral entities each in the
%% scope noted:
%%
%% 1. Sessions (_global scope_)
%% 2. Publications (_global scope_)
%% 3. Subscriptions (_router scope_)
%% 4. Registrations (_router scope_)
%% 5. Requests (_session scope_)
%% @end
%% =============================================================================
-module(wamp_id).
-include("wamp.hrl").


-export ([new/1]).
-export ([is_valid/1]).




%% =============================================================================
%% API
%% =============================================================================



-spec new(Scope :: global | {router, uri()} | {session, id()}) -> id().
new(global) ->
    %% IDs in the _global scope_ MUST be drawn _randomly_ from a _uniform
    %% distribution_ over the complete range [0, 2^53]
    crypto:rand_uniform(0, ?MAX_ID);

new({router, _}) ->
    new(global);

new({session, SessionId}) ->
    %% IDs in the _session scope_ SHOULD be incremented by 1 beginning
    %% with 1 (for each direction - _Client-to-Router_ and _Router-to-
    %% Client_)
    juno_session:incr_seq(SessionId).


-spec is_valid(id()) -> boolean().
is_valid(N) when is_integer(N) andalso N >= 0 andalso N =< ?MAX_ID ->
    true;
is_valid(_) ->
    false.


%% =============================================================================
%% PRIVATE
%% =============================================================================
