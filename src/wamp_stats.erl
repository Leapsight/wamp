%% =============================================================================
%% Copyright (C) NGINEO LIMITED 2016. All rights reserved.
%% =============================================================================


-module(wamp_stats).

-export([update/1]).
-export([update/2]).
-export([get_stats/0]).
-export([create_metrics/0]).



%% =============================================================================
%% API
%% =============================================================================




%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec create_metrics() -> ok.

create_metrics() ->
    %% TODO Process aliases
    lists:foreach(
      fun({Name, Type , Opts, _Aliases}) ->
              exometer:new(Name, Type, Opts)
      end,
      static_specs()
     ),
    ok.



%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_stats() -> any().

get_stats() ->
    exometer:get_values([wamp]).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec update(tuple()) -> ok.

update(Event) ->
    do_update(Event).



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec update(wamp_message:message(), wamp_session:session()) -> ok | overload.

update(M, S) ->
    MsgType = element(1, M),
    Size = erts_debug:flat_size(M) * 8,
    {IP, _} = wamp_session:peer(S),
    case {wamp_session:is_active(S), wamp_session:realm_uri(S)} of
        {false, undefined} ->
            do_update({message, IP, MsgType, Size});
        {false, Uri} ->
            do_update({message, Uri, IP, MsgType, Size});
        {true, Uri} ->
            Id = wamp_session:id(S),
            do_update({message, Uri, Id, IP, MsgType, Size})
    end.



%% =============================================================================
%% PRIVATE
%% =============================================================================

%% @private
baddress(T) when is_tuple(T), tuple_size(T) == 4 ->
  list_to_binary(inet_parse:ntoa(T));
baddress(T) when is_list(T) ->
  list_to_binary(T);
baddress(T) when is_binary(T) ->
  T.


%% @private
do_update({session_opened, Realm, _SessionId, IP}) ->
    BIP = baddress(IP),
    exometer:update([wamp, sessions], 1),
    exometer:update([wamp, sessions, active], 1),

    exometer:update_or_create(
      [wamp, realm, sessions, Realm], 1, spiral, []),
    exometer:update_or_create(
      [wamp, realm, sessions, active, Realm], 1, counter, []),

    exometer:update_or_create(
      [wamp, ip, sessions, BIP], 1, spiral, []),
    exometer:update_or_create(
      [wamp, ip, sessions, active, BIP], 1, counter, []);

do_update({session_closed, Realm, SessionId, IP, Secs}) ->
    BIP = baddress(IP),
    exometer:update([wamp, sessions, active], -1),
    exometer:update([wamp, sessions, duration], Secs),

    exometer:update_or_create(
      [wamp, realm, sessions, active, Realm], -1, []),
    exometer:update_or_create(
      [wamp, realm, sessions, duration, Realm], Secs, []),

    exometer:update_or_create(
      [wamp, ip, sessions, active, BIP], -1, []),
    exometer:update_or_create(
      [wamp, ip, sessions, duration, BIP], Secs, []),

    %% Cleanup
    exometer:delete([wamp, session, messages, SessionId]),
    lists:foreach(
      fun({Name, _, _}) ->
              exometer:delete(Name)
      end,
      exometer:find_entries([wamp, session, messages, '_', SessionId])
     ),
    ok;

do_update({message, IP, Type, Sz}) ->
    BIP = baddress(IP),
    exometer:update([wamp, messages], 1),
    exometer:update([wamp, messages, size], Sz),
    exometer:update_or_create([wamp, messages, Type], 1, spiral, []),

    exometer:update_or_create(
      [wamp, ip, messages, BIP], 1, counter, []),
    exometer:update_or_create(
      [wamp, ip, messages, size, BIP], Sz, histogram, []),
    exometer:update_or_create(
      [wamp, ip, messages, Type, BIP], 1, spiral, []);


do_update({message, Realm, IP, Type, Sz}) ->
    BIP = baddress(IP),
    exometer:update([wamp, messages], 1),
    exometer:update([wamp, messages, size], Sz),
    exometer:update_or_create([wamp, messages, Type], 1, spiral, []),

    exometer:update_or_create(
      [wamp, ip, messages, BIP], 1, counter, []),
    exometer:update_or_create(
      [wamp, ip, messages, size, BIP], Sz, histogram, []),
    exometer:update_or_create(
      [wamp, ip, messages, Type, BIP], 1, spiral, []),

    exometer:update_or_create(
      [wamp, realm, messages, Realm], 1, counter, []),
    exometer:update_or_create(
      [wamp, realm, messages, size, Realm], Sz, histogram, []),
    exometer:update_or_create(
      [wamp, realm, messages, Type, Realm], 1, spiral, []);

do_update({message, Realm, Session, IP, Type, Sz}) ->
    BIP = baddress(IP),
    exometer:update([wamp, messages], 1),
    exometer:update([wamp, messages, size], Sz),
    exometer:update_or_create([wamp, messages, Type], 1, spiral, []),

    exometer:update_or_create(
      [wamp, ip, messages, BIP], 1, counter, []),
    exometer:update_or_create(
      [wamp, ip, messages, size, BIP], Sz, histogram, []),
    exometer:update_or_create(
      [wamp, ip, messages, Type, BIP], 1, spiral, []),

    exometer:update_or_create(
      [wamp, realm, messages, Realm], 1, counter, []),
    exometer:update_or_create(
      [wamp, realm, messages, size, Realm], Sz, histogram, []),
    exometer:update_or_create(
      [wamp, realm, messages, Type, Realm], 1, spiral, []),

    exometer:update_or_create(
      [wamp, session, messages, Session], 1, counter, []),
    exometer:update_or_create(
      [wamp, session, messages, size, Session], Sz, histogram, []),
    exometer:update_or_create(
      [wamp, session, messages, Type, Session], 1, spiral, []).




%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%% -----------------------------------------------------------------------------
static_specs() ->
    [
     {[wamp, sessions],
      spiral, [], [
                   {one, sessions},
                   {count, sessions_total}]},
     {[wamp, messages],
      spiral, [], [
                   {one, messages},
                   {count, messages_total}]},
     {[wamp, sessions, active],
      counter, [], [
                    {value, sessions_active}]},
     {[wamp, sessions, duration],
      histogram, [], [
                      {mean, sessions_duration_mean},
                      {median, sessions_duration_median},
                      {95, sessions_duration_95},
                      {99, sessions_duration_99},
                      {max, sessions_duration_100}]}
    ].
