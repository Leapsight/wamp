%% 
%%  Untitled-1 -
%% 
%%  Copyright (c) 2016-2017 Ngineo Limited t/a Leapsight. All rights reserved.
%% 
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-module(wamp_protocol).
-include("wamp.hrl").

-define(IS_TRANSPORT(X), (T =:= ws orelse T =:= raw)).
%% Custom error messages
-define(ERROR_NOT_IN_SESSION, <<"com.leapsight.error.not_in_session">>).
-define(SESSION_ALREADY_EXISTS, 
    <<"com.leapsight.error.session_already_exists">>).
-define(INVALID_MESSAGE, <<"com.leapsight.error.invalid_message">>).

%% RAW SOCKET SUBPROTOCOL SUPPORT
%% -define(MAGIC, 16#7F).
%% -define(MSG_PREFIX, <<0:5, 0:3>>).
%% -define(PING_PREFIX, <<0:5, 1:3>>).
%% -define(PONG_PREFIX, <<0:5, 2:3>>).
%% %% 0: illegal (must not be used)
%% %% 1: serializer unsupported
%% %% 2: maximum message length unacceptable
%% %% 3: use of reserved bits (unsupported feature)
%% %% 4: maximum connection count reached
%% %% 5 - 15: reserved for future errors
%% -define(ERROR(Upper), <<?MAGIC:8, Upper:4, 0:4, 0:8, 0:8>>).
%% -define(FRAME(Bin), <<0:5, 0:3, (byte_size(Bin)):24, Bin/binary>>).


-record(wamp_state, {
    transport               ::  transport(),
    frame_type              ::  frame_type(),
    encoding                ::  encoding(),
    raw_encoding            ::  1..15,
    raw_max_len             ::  0..15,
    max_len_bytes           ::  pos_integer(),
    buffer = <<>>           ::  binary(),
    peer_type               ::  peer_type(),
    mod                     ::  module(),
    state_name              ::  state_name(),
    session                 ::  wamp_session:session() | undefined
}).


-type state_name()          ::  closed
                                | establishing
                                | established
                                | shutting_down
                                | closing
                                | failed
                                | challenging
                                | authenticating.

-type peer_type()           ::  router | client.
-type peer()                ::  {inet:ip_address(), inet:port_number()}.
-type subprotocol()         ::  {transport(), frame_type(), encoding()}.
-type state()               ::  #wamp_state{} | undefined.
-type auth_details()        ::  #{}.
-type type()                ::  in | out.

-export_type([encoding/0]).
-export_type([frame_type/0]).
-export_type([peer/0]).
-export_type([state/0]).
-export_type([subprotocol/0]).


-export([init/5]).
-export([handle_inbound_data/2]).
-export([handle_inbound_message/2]).
-export([handle_outbound_data/2]).
-export([handle_outbound_message/2]).
-export([terminate/1]).
-export([validate_subprotocol/1]).

%% STATES
-export([authenticating/3]).
-export([challenging/3]).
-export([closed/3]).
-export([closing/2]).
-export([established/3]).
-export([establishing/3]).
-export([failed/3]).
-export([shutting_down/2]).


%% =============================================================================
%% WAMP CALLBACKS
%% =============================================================================




%% -----------------------------------------------------------------------------
%% @doc
%% To be implemented by the router
%% @end
%% -----------------------------------------------------------------------------
-callback forward(M :: wamp_message(), Session :: wamp_session:session()) ->
    {ok, wamp_session:session()}
    | {reply, Reply :: wamp_message(), wamp_session:session()}
    | {stop, Reply :: wamp_message(), wamp_session:session()}.

-optional_callbacks([forward/2]).


%% -----------------------------------------------------------------------------
%% @doc
%% To be implemented by the client
%% @end
%% -----------------------------------------------------------------------------
-callback deliver(M :: wamp_message(), Session :: wamp_session:session()) ->
    {ok, wamp_session:session()}
    | {reply, Reply :: wamp_message(), wamp_session:session()}
    | {stop, Reply :: wamp_message(), wamp_session:session()}.

-optional_callbacks([deliver/2]).


%% -----------------------------------------------------------------------------
%% @doc
%% To be implemented by the router
%% @end
%% -----------------------------------------------------------------------------
-callback authenticate(
    RealmUri :: uri(), 
    Details :: auth_details(), 
    Session :: wamp_session:session()) ->
    {ok, wamp_welcome(), Session :: any(), wamp_session:session()}
    | {ok, wamp_challenge(), wamp_session:session()}
    | {error, any(), wamp_session:session()}.

-optional_callbacks([authenticate/3]).


%% -----------------------------------------------------------------------------
%% @doc
%% To be implemented by routers and peers.
%% @end
%% -----------------------------------------------------------------------------
-callback roles() -> #{binary() => #{binary() => boolean()}}.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-callback code_change(
    OldVsn :: term() | {'down', term()},
    OldState :: state(),
    OldData :: term(),
    Extra :: term()) ->
    {ok, NewState :: state(), NewData :: term()} |
    (Reason :: term()).

-optional_callbacks([code_change/4]).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-callback terminate(wamp_session:session()) -> ok.




%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec init(
    binary() | subprotocol(), {peer_type(), module()}, peer(), uri(), map()) -> 
    {ok, state()}
    | {ok, binary(), state()} 
    | {stop, state()}
    | {stop, binary(), state()}
    | {reply, binary(), state()}.

init(Subproto0, {PeerType, Mod}, Peer, RealmUri, Opts) ->
    case validate_subprotocol(Subproto0) of
        {ok, Subproto1} ->
            do_init(Subproto1, {PeerType, Mod}, Peer, RealmUri, Opts);
        {error, Reason} ->
            {error, Reason, undefined}
    end.



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec terminate(state()) -> ok.

terminate(St) ->
    (St#wamp_state.mod):terminate(St#wamp_state.session).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec validate_subprotocol(subprotocol()) -> ok | {error, invalid_subprotocol}.

validate_subprotocol(T) when is_binary(T) ->
    {ok, subprotocol(T)};
validate_subprotocol({ws, text, json_batched} = S) ->  
    {ok, S};
validate_subprotocol({ws, binary, msgpack_batched} = S) ->
    {ok, S};
validate_subprotocol({ws, binary, bert_batched} = S) ->
    {ok, S};
validate_subprotocol({ws, binary, erl_batched} = S) -> 
    {ok, S};
validate_subprotocol({T, text, json} = S) when ?IS_TRANSPORT(T) ->          
    {ok, S};
validate_subprotocol({T, binary, msgpack} = S) when ?IS_TRANSPORT(T) ->     
    {ok, S};
validate_subprotocol({T, binary, bert} = S) when ?IS_TRANSPORT(T) ->        
    {ok, S};
validate_subprotocol({T, binary, erl} = S) when ?IS_TRANSPORT(T) ->         
    {ok, S};
validate_subprotocol(_) ->                             
    {error, invalid_subprotocol}.



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec handle_inbound_data({frame_type(), binary()} | binary(), state()) ->
    {ok, state()}
    | {ok, [binary()], state()} 
    | {stop, state()}
    | {stop, [binary()], state()}
    | {reply, [binary()], state()}.

handle_inbound_data({T, Data}, #wamp_state{frame_type = T} = St) ->
    handle_data(in, Data, St);

handle_inbound_data(Data, #wamp_state{frame_type = binary} = St) ->
    handle_data(in, Data, St).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec handle_outbound_data({frame_type(), binary()} | binary(), state()) ->
    {ok, state()}
    | {ok, [binary()], state()} 
    | {stop, state()}
    | {stop, [binary()], state()}
    | {reply, [binary()], state()}.

handle_outbound_data({T, Data}, #wamp_state{frame_type = T} = St) ->
    handle_data(out, Data, St);

handle_outbound_data(Data, #wamp_state{frame_type = binary} = St) ->
    handle_data(out, Data, St).

%% -----------------------------------------------------------------------------
%% @doc
%% Handles wamp frames, decoding 1 or more messages. 
%% The messages are then either forwarded to the router callback module
%% in case the peer type is `router` by calling the `forward/2` 
%% callback function.
%% Otherwise in case the peer type is `client` it will call the client's
%% callback module using the `deliver/2` function.
%%
%% In each case, the the callback module might reply synchronously by returning
%% the `{reply, [binary()], state()}`.
%% @end
%% -----------------------------------------------------------------------------
-spec handle_data(type(), binary(), state()) ->
    {ok, state()}
    | {ok, [binary()], state()} 
    | {stop, state()}
    | {stop, [binary()], state()}
    | {reply, [binary()], state()}.

handle_data(Type, Data0, St) ->
    Data1 = <<(St#wamp_state.buffer)/binary, Data0/binary>>,
    {Messages, Buffer} = wamp_encoding:decode(
        Data1, St#wamp_state.frame_type, St#wamp_state.encoding),
    handle_messages(Type, Messages, St#wamp_state{buffer = Buffer}, []).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec handle_inbound_message(wamp_message:message(), state()) ->
    {ok, state()}
    | {ok, binary(), state()} 
    | {stop, state()}
    | {stop, binary(), state()}
    | {reply, binary(), state()}.

handle_inbound_message(M, St) ->
    ok = wamp_stats:update(M, St#wamp_state.session),
    handle_message(in, M, St).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec handle_outbound_message(wamp_message:message(), state()) ->
    {ok, state()}
    | {ok, binary(), state()} 
    | {stop, state()}
    | {stop, binary(), state()}
    | {reply, binary(), state()}.

handle_outbound_message(M, St) ->
    ok = wamp_stats:update(M, St#wamp_state.session),
    handle_message(out, M, St).



%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec handle_message(type(), wamp_message:message(), state()) ->
    {ok, state()}
    | {ok, binary(), state()} 
    | {stop, state()}
    | {stop, binary(), state()}
    | {reply, binary(), state()}.

handle_message(Type, M, St0) ->
    ok = wamp_stats:update(M, St0#wamp_state.session),
    %% We call the current state's name function
    case (St0#wamp_state.state_name)(Type, M, St0) of
        {ok, _}  = OK->
            OK;
        {ok, R, St1} ->
            Bin = wamp_encoding:encode(R, St1#wamp_state.encoding),
            {ok, Bin, St1};
        {reply, R, St1} ->
            Bin = wamp_encoding:encode(R, St1#wamp_state.encoding),
            {reply, Bin, St1};
        {stop, _} = Stop ->
            Stop;
        {stop, R, St1} ->
            Bin = wamp_encoding:encode(R, St1#wamp_state.encoding),
            {stop, Bin, St1}
    end.


%% =============================================================================
%% PRIVATE STATES
%% http://wamp-proto.org/static/rfc/draft-oberstet-hybi-crossbar-wamp.html#rfc.section.7
%% =============================================================================





%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec closed(type(), wamp_message(), state()) ->
    {ok, state()}
    | {ok, wamp_message(), state()} 
    | {stop, state()}
    | {stop, wamp_message(), state()}
    | {reply, wamp_message(), state()}.

closed(out, #hello{} = M, #wamp_state{peer_type = client} = St) ->
    %% The client is sending a Hello to the router to open a session
    {ok, M, next_state(establishing, St)};

closed(in, #hello{realm_uri = Uri} = M, #wamp_state{peer_type = router} = St0) ->
    %% The router receives a Hello message
    %% We will reply with wamp_welcome() | wamp_challenge() | wamp_abort()
    Session0 = St0#wamp_state.session,
    ok = wamp_stats:update(M, Session0),
    Session1 = Session0#{realm_uri => Uri},
    %% This transition is irrelevant as we do the auth synchonously
    %% but we keep it for completeness
    St1 = next_state(establishing, Session1, St0),

    case 
        (St1#wamp_state.mod):authenticate(Uri, M#hello.details, Session1) 
    of
        {ok, #welcome{} = R, Session, Session2} ->
            Session3 = maps:put(session, Session, Session2),
            {reply, R, next_state(established, Session3, St1)};
        
        {ok, #challenge{auth_method = Method} = R, Session2} ->
            Session3 = maps:put(auth_method, Method, Session2),
            {reply, R, next_state(challenging, Session3, St1)};
        
        {error, Reason, Session2} ->
            {stop, abort(Reason), next_state(closed, Session2, St1)}
    end;

closed(in, _, #wamp_state{peer_type = router} = St) ->
    R = abort(
        ?ERROR_NOT_IN_SESSION, <<"You need to establish a session first.">>),
    {stop, R, next_state(closed, St)}.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec establishing(type(), wamp_message(), state()) ->
    {ok, state()}
    | {ok, wamp_message(), state()} 
    | {stop, state()}
    | {stop, wamp_message(), state()}
    | {reply, wamp_message(), state()}.

establishing(in, #welcome{} = M, #wamp_state{peer_type = client} = St) ->
    %% We have a session
    case (St#wamp_state.mod):deliver(M, St#wamp_state.session) of
        {ok, Session} ->
            {ok, next_state(established, Session, St)};
        {reply, R, Session} ->
            {reply, R, next_state(established, Session, St)};
        {stop, R, Session} ->
            {stop, R, next_state(established, Session, St)}
    end;

establishing(in, #abort{} = M, #wamp_state{peer_type = client} = St) ->
    %% My session request has been denied
    case (St#wamp_state.mod):deliver(M, St#wamp_state.session) of
        {ok, Session} ->
            {stop, next_state(closed, Session, St)};
        {_, _, Session} ->
            %% We ignore any reply or stop, the client should shutdown
            %% an an abort message without replying.
            {stop, next_state(closed, Session, St)}
    end;

establishing(
    in, 
    #challenge{auth_method = ?TICKET_AUTH}, #wamp_state{peer_type = client} = St) ->
    %% We've been challenged, we need to reply with an authenticate
    %% message
    % next_state(authenticating, St)
    %% Get auth data from state
    Pass = <<>>,
    R = #authenticate{signature = Pass, extra = #{}},
    {reply, R, next_state(authenticating, St)};

establishing(
    in, 
    #challenge{auth_method = ?WAMPCRA_AUTH}, #wamp_state{peer_type = client} = _St) ->
    %% We've been challenged, we need to reply with an authenticate
    %% message
    % next_state(authenticating, St)
    %% Get auth data from state
    error(wampcra_not_yet_implemented);

establishing(in, #hello{}, #wamp_state{peer_type = router} = St) ->
    %% client already sent a HELLO previously.
    %% This case will never happen as we synchronously transition from
    %% closed to challenging | authenticating
    R = abort(
        ?WAMP_ERROR_CANCELLED, 
        <<"You've sent a HELLO message more than once.">>),
    {stop, R, next_state(closed, St)};

establishing(_, _, #wamp_state{peer_type = client} = St) ->
    %% The router sent us an invalid message
    R = abort(
        ?INVALID_MESSAGE,
        <<"Invalid message. I was expecting CHALLENGE or WELCOME.">>
    ),
    {stop, R, next_state(failed, St)};

establishing(_, _, #wamp_state{peer_type = router} = St) ->
    R = abort(
        ?ERROR_NOT_IN_SESSION, 
        <<"You need to request a session first by sending a HELLO message.">>),
    {stop, R, next_state(failed, St)}.


%% -----------------------------------------------------------------------------
%% @doc
%% The state the router is in when it has sent a challenge message and is 
%% awaiting for an authenticate response.
%% @end
%% -----------------------------------------------------------------------------
-spec challenging(type(), wamp_message(), state()) ->
    {ok, state()}
    | {ok, wamp_message(), state()} 
    | {stop, state()}
    | {stop, wamp_message(), state()}
    | {reply, wamp_message(), state()}.

challenging(in, #hello{}, #wamp_state{peer_type = router} = St) ->
    %% client already sent a HELLO previously and we have replied
    %% with a challenge message
    R = abort(
        ?WAMP_ERROR_CANCELLED, 
        <<"You've sent a HELLO message again and you have not yet replied to our previous CHALLENGE message. Connection closing.">>),
    {stop, R, next_state(closed, St)};

challenging(in, #authenticate{}, #wamp_state{peer_type = router} = St) ->
    %% TODO authenticate, return welcome -> established  
    %% or abort -> closed

    %% ok = wamp_stats:update(M, St#wamp_state.session),
    %% %% Client is responding to a challenge
    %% #authenticate{signature = Sign} = M,
    %% Session0 = St#wamp_state.session,
    %% Realm = maps:get(realm_uri, Session0),
    %% Peer = maps:get(client, Session0),
    %% AuthId = maps:get(authid, Session0),
    %% case 
    %%     bondy_security_utils:authenticate(
    %%         AuthMethod, {AuthMethod, AuthId, Sign}, Realm, Peer) 
    %% of
    %%     {ok, _AuthSession} ->
    %%         %% We already stored the authid (username) in the Session
    %%         open_session(St);
    %%     {error, Reason} ->
    %%         abort(?WAMP_ERROR_AUTHORIZATION_FAILED, Reason, St)
    %% end;
    
    {stop, St};

challenging(_, _, #wamp_state{peer_type = router} = St) ->
    %% Any other message is an error
    %% TODO MAYBE AN ERROR? SPEC is not clear
    R = abort(
        ?ERROR_NOT_IN_SESSION, 
        <<"You need to establish a session first. Connection closing.">>
    ),
    {stop, R, next_state(failed, St)}.


%% -----------------------------------------------------------------------------
%% @doc
%% State the peer is in 
%% @end
%% -----------------------------------------------------------------------------
-spec authenticating(type(), wamp_message(), state()) ->
    {ok, state()}
    | {ok, wamp_message(), state()} 
    | {stop, state()}
    | {stop, wamp_message(), state()}
    | {reply, wamp_message(), state()}.

authenticating(in, #welcome{}, #wamp_state{peer_type = client} = St) ->
    %% We've got a session
    %% TODO Capture info in Session
    {ok, St};

authenticating(_, _, #wamp_state{peer_type = client} = St) ->
    %% The router sent us an invalid message
    R = abort(
        ?INVALID_MESSAGE, 
        <<"Invalid message. I was expecting ABORT or WELCOME.">>
    ),
    {stop, R, next_state(failed, St)}.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec failed(type(), wamp_message(), state()) ->
    {ok, state()}
    | {ok, wamp_message(), state()} 
    | {stop, state()}
    | {stop, wamp_message(), state()}
    | {reply, wamp_message(), state()}.

failed(in, _M, #wamp_state{peer_type = router} = St) ->
    {stop, St}.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec established(type(), wamp_message(), state()) ->
    {ok, state()}
    | {stop, state()}
    | {stop, wamp_message(), state()}
    | {reply, wamp_message(), state()}.

established(out, #result{} = M, #wamp_state{peer_type = client} = St0) ->
    %% The client is receiving a result for a call
    CallId = M#result.request_id,
    Session0 = St0#wamp_state.session,
    Session1 = wamp_session:remove_awaiting_call(Session0, CallId),
    St1 = update_session(wamp_session:reset(Session1), St0),
    {reply, M, St1};

established(out, #error{request_type = ?CALL} = M, #wamp_state{peer_type = client} = St0) ->
    CallId = M#error.request_id,
    Session0 = St0#wamp_state.session,
    Session1 = wamp_session:remove_awaiting_call(Session0, CallId),
    St1 = update_session(wamp_session:reset(Session1), St0),
    {reply, M, St1};

established(in, #hello{}, #wamp_state{peer_type = router} = St) ->
    %% Peer already has a session
    %% RFC:
    %% It is a protocol error to receive a second "HELLO" message during the
    %% lifetime of the session and the _Peer_ must fail the session if that
    %% happens
    %% @TODO Abort | Error or Silent?
    R = abort(
        ?SESSION_ALREADY_EXISTS, 
        <<"You've sent a HELLO message more than once.">> 
    ),
    {stop, R, next_state(failed, St)};

established(in, #authenticate{}, #wamp_state{peer_type = router} = St) ->
    %% Peer already has a session
    %% @TODO Error?
    R = abort(
        ?SESSION_ALREADY_EXISTS, 
        <<"You've sent an AUTHENTICATE message more than once.">>
    ),
    {stop, R, next_state(failed, St)};

established(in, #goodbye{}, #wamp_state{peer_type = router} = St0) ->
    %% The peer initiated a goodbye, so we will not process
    %% any subsequent messages
    %% We reply with a goodbye and stop
    R = wamp_message:goodbye(#{}, ?WAMP_ERROR_GOODBYE_AND_OUT),
    {stop, R, next_state(closed, next_state(closing, St0))};

established(in, #goodbye{}, #wamp_state{peer_type = client} = St0) ->
    %% The router is closing the session
    %% We reply with a goodbye and stop
    %% TODO Log reason for goodbye
    R = wamp_message:goodbye(#{}, ?WAMP_ERROR_GOODBYE_AND_OUT),
    {stop, R, next_state(closed, next_state(shutting_down, St0))};

established(in, M, #wamp_state{peer_type = router} = St) ->
    case (St#wamp_state.mod):forward(M, St#wamp_state.session) of
        {ok, Session} ->
            {ok, update_session(Session, St)};
        {reply, M, Session} ->
            {reply, M, update_session(Session, St)};
        {stop, M, Session} ->
            {stop, M, update_session(Session, St)}
    end;

established(in, M, #wamp_state{peer_type = client} = St) ->
    case (St#wamp_state.mod):deliver(M, St#wamp_state.session) of
        {ok, Session} ->
            {ok, update_session(Session, St)};
        {reply, M, Session} ->
            {reply, M, update_session(Session, St)};
        {stop, M, Session} ->
            {stop, M, update_session(Session, St)}
    end;

established(out, M, St) ->
    {ok, M, St}.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec shutting_down(wamp_message(), state()) ->
    {ok, state()}
    | {ok, wamp_message(), state()} 
    | {stop, state()}
    | {stop, wamp_message(), state()}
    | {reply, wamp_message(), state()}.

shutting_down(#goodbye{}, #wamp_state{peer_type = router} = St) ->
    %% The client responded to our goodbye with his goodbye
    {ok, next_state(closed, St)};

shutting_down(_M, #wamp_state{peer_type = router} = St) ->
    %% We ignore all other messages
    {ok, St}.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec closing(wamp_message(), state()) ->
    {ok, state()}
    | {ok, wamp_message(), state()} 
    | {stop, state()}
    | {stop, wamp_message(), state()}
    | {reply, wamp_message(), state()}.

closing(_M, #wamp_state{peer_type = router} = St) ->
    {stop, St}.




%% =============================================================================
%% PRIVATE: HANDLING INBOUND MESSAGES
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Handles one or more messages, routing them and returning a reply
%% when required.
%% @end
%% -----------------------------------------------------------------------------
-spec handle_messages(
    type(), [wamp_message()], state(), Acc :: [wamp_message()]) ->
    {ok, state()}
    | {ok, [binary()], state()} 
    | {stop, state()}
    | {stop, [binary()], state()}
    | {reply, [binary()], state()}.

handle_messages(_, [], St, []) ->
    %% We have no replies
    {ok, St};

handle_messages(_, [], St, Acc) ->
    {reply, lists:reverse(Acc), St};    

handle_messages(Type, [H|T], St0, Acc) ->
    case (St0#wamp_state.state_name)(Type, H, St0) of
        {ok, St1} ->
            handle_messages(Type, T, St1, Acc);
        {reply, M, St1} ->
            Bin = wamp_encoding:encode(M, St1#wamp_state.encoding),
            handle_messages(Type, T, St1, [Bin | Acc]);
        {stop, St1} ->
            {stop, lists:reverse(Acc), St1};
        {stop, M, St1} ->
            Bin = wamp_encoding:encode(M, St1#wamp_state.encoding),
            {stop, lists:reverse([Bin|Acc]), St1}
    end.




%% =============================================================================
%% PRIVATE: AUTH & SESSION
%% =============================================================================




%% @private
abort({realm_not_found, Uri}) ->
    abort(
        ?WAMP_ERROR_NO_SUCH_REALM,
        <<"Realm '", Uri/binary, "' does not exist.">>
    );

abort({missing_param, Param}) ->
    abort(
        ?WAMP_ERROR_CANCELLED,
        <<"Missing value for required parameter '", 
        Param/binary, "'.">>
    );

abort({user_not_found, AuthId}) ->
    abort(
        ?WAMP_ERROR_CANCELLED,
        <<"User '", AuthId/binary, "' does not exist.">>
    );

abort({invalid_options, missing_client_role}) ->
    abort(
        <<"wamp.error.missing_client_role">>, 
        <<"Please provide at least one client role.">>
    ).


%% @private
abort(Type, Reason) ->
    Details = #{
        <<"message">> => Reason,
        <<"timestamp">> => erlang:system_time(seconds)
    },
    wamp_message:abort(Details, Type).




%% =============================================================================
%% PRIVATE: UTILS
%% =============================================================================
    
%% @private
do_init({T, FrameType, Enc}, {PeerType, Mod}, Peer, Uri, Opts) 
when PeerType == client orelse PeerType == router ->
    State = #wamp_state{
        peer_type = PeerType,
        mod = Mod,
        transport = T,
        frame_type = FrameType,
        encoding = Enc,
        session = wamp_session:new(Peer, Uri, Opts)
    },
    case PeerType of
        router ->
            {ok, next_state(closed, State)};
        client ->
            M = wamp_message:hello(Uri, Opts),
            handle_outbound_message(M, next_state(closed, State))
    end.


%% @private
update_session(Session, St) ->
    St#wamp_state{session = Session}.



%% @private
next_state(Name, Session, St) ->
    next_state(Name, update_session(Session, St)).

%% @private
next_state(closed = N, #wamp_state{state_name = undefined} = St) ->
    St#wamp_state{state_name = N};

next_state(closed = N, #wamp_state{state_name = closed} = St) ->
    St#wamp_state{state_name = N};
next_state(establishing = N, #wamp_state{state_name = closed} = St) ->
    St#wamp_state{state_name = N};

next_state(closed = N, #wamp_state{state_name = establishing} = St) ->
    St#wamp_state{state_name = N};
next_state(failed = N, #wamp_state{state_name = establishing} = St) ->
    St#wamp_state{state_name = N};
next_state(challenging = N, #wamp_state{state_name = establishing} = St) ->
    St#wamp_state{state_name = N};
next_state(authenticating = N, #wamp_state{state_name = establishing} = St) ->
    St#wamp_state{state_name = N};
next_state(established = N, #wamp_state{state_name = establishing} = St) ->
    St#wamp_state{state_name = N};

next_state(closed = N, #wamp_state{state_name = challenging} = St) ->
    St#wamp_state{state_name = N};
next_state(established = N, #wamp_state{state_name = challenging} = St) ->
    St#wamp_state{state_name = N};

next_state(closed = N, #wamp_state{state_name = authenticating} = St) ->
    St#wamp_state{state_name = N};
next_state(established = N, #wamp_state{state_name = authenticating} = St) ->
    St#wamp_state{state_name = N};


next_state(shutting_down = N, #wamp_state{state_name = established} = St) ->
    St#wamp_state{state_name = N};
next_state(failed = N, #wamp_state{state_name = established} = St) ->
    St#wamp_state{state_name = N};
next_state(closing = N, #wamp_state{state_name = established} = St) ->
    St#wamp_state{state_name = N};

next_state(shutting_down = N, #wamp_state{state_name = shutting_down} = St) ->
    St#wamp_state{state_name = N};
next_state(closed = N, #wamp_state{state_name = shutting_down} = St) ->
    St#wamp_state{state_name = N};

next_state(closed = N, #wamp_state{state_name = closing} = St) ->
    St#wamp_state{state_name = N}.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec subprotocol(binary()) -> subprotocol().

subprotocol(?WAMP2_JSON) ->                 {ws, text, json};
subprotocol(?WAMP2_MSGPACK) ->              {ws, binary, msgpack};
subprotocol(?WAMP2_JSON_BATCHED) ->         {ws, text, json_batched};
subprotocol(?WAMP2_MSGPACK_BATCHED) ->      {ws, binary, msgpack_batched};
subprotocol(?WAMP2_BERT) ->                 {ws, binary, bert};
subprotocol(?WAMP2_ERL) ->                  {ws, binary, erl};
subprotocol(?WAMP2_BERT_BATCHED) ->         {ws, binary, bert_batched};
subprotocol(?WAMP2_ERL_BATCHED) ->          {ws, binary, erl_batched};
subprotocol(_) ->                           {error, invalid_subprotocol}. 





%% %% =============================================================================
%% %% PRIVATE: RAW SOCKET SUBPROTOCOL 
%% %% =============================================================================





%% %% -----------------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% The possible values for "LENGTH" are:
%% %%
%% %% 0: 2**9 octets 
%% %% 1: 2**10 octets ...
%% %% 15: 2**24 octets
%% %%
%% %% This means a _Client_ can choose the maximum message length between *512* 
%% %% and *16M* octets.
%% %% @end
%% %% -----------------------------------------------------------------------------
%% validate_max_len(N) when N >= 0, N =< 15 ->
%%     math:pow(2, 9 + N);

%% validate_max_len(_) ->
%%     %% TODO define correct error return
%%     throw(maximum_message_length_unacceptable).

%% %% -----------------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% 0: illegal
%% %% 1: JSON
%% %% 2: MessagePack
%% %% 3 - 15: reserved for future serializers
%% %% @end
%% %% -----------------------------------------------------------------------------
%% validate_encoding({text, json}) ->          1;
%% validate_encoding({binary, msgpack}) ->     2;
%% %% validate_encoding({binary, cbor}) ->     3;
%% validate_encoding({binary, bert}) ->        4;
%% validate_encoding({binary, erl}) ->         15;
%% validate_encoding(_) ->
%%     throw(serializer_unsupported).


%% %% -----------------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% 0: illegal (must not be used)
%% %% 1: serializer unsupported
%% %% 2: maximum message length unacceptable
%% %% 3: use of reserved bits (unsupported feature)
%% %% 4: maximum connection count reached
%% %% 5 - 15: reserved for future errors
%% %% @end
%% %% -----------------------------------------------------------------------------
%% error_number(serializer_unsupported) ->?ERROR(1);
%% error_number(maximum_message_length_unacceptable) ->?ERROR(2);
%% error_number(use_of_reserved_bits) ->?ERROR(3);
%% error_number(maximum_connection_count_reached) ->?ERROR(4).


%% %% -----------------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% 0: illegal (must not be used)
%% %% 1: serializer unsupported
%% %% 2: maximum message length unacceptable
%% %% 3: use of reserved bits (unsupported feature)
%% %% 4: maximum connection count reached
%% %% 5 - 15: reserved for future errors
%% %% @end
%% %% -----------------------------------------------------------------------------
%% error_reason(1) -> serializer_unsupported;
%% error_reason(2) -> maximum_message_length_unacceptable;
%% error_reason(3) -> use_of_reserved_bits;
%% error_reason(4) -> maximum_connection_count_reached.

