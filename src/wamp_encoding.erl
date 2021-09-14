%% -----------------------------------------------------------------------------
%%  Copyright (c) 2015-2021 Leapsight. All rights reserved.
%% -----------------------------------------------------------------------------

%% =============================================================================
%% @doc
%% Handles the packing/unpacking and encoding/decoding of WAMP messages.
%% @end
%% =============================================================================
-module(wamp_encoding).
-include("wamp.hrl").


%% ASCII record separator
-define(JSON_BATCHED_SEPARATOR, <<$\30>>).
-define(JSON_BATCH_FRAME(Bin), <<Bib/binary, $\30>>).
-define(MSGPACK_BATCH_FRAME(Bin), <<Bib/binary, $\30>>).



%% -export([frame/2]).
%% -export([unframe/2]).
-export([pack/1]).
-export([unpack/1]).
-export([encode/2]).
-export([encode/3]).
-export([decode/2]).
-export([decode/3]).
-export([is_encoding/1]).

-export([message_name/1]).
-export([decode_message_name/2]).




%% =============================================================================
%% API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec is_encoding(encoding()) -> boolean().

is_encoding(bert) -> true;
is_encoding(erl) -> true;
is_encoding(json) -> true;
is_encoding(msgpack) -> true;
%% is_encoding(bert_batched) -> true;
%% is_encoding(erl_batched) -> true;
%% is_encoding(json_batched) -> true;
%% is_encoding(msgpack_batched) -> true;
is_encoding(_) -> false.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec decode_message_name(subprotocol(), Data :: binary()) ->
    message_name() | no_return().

decode_message_name({_, _, Enc}, Data) ->
    do_decode_message_name(Data, Enc).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec decode(subprotocol(), Data :: binary()) ->
    {Messages :: [wamp_message()], Rest :: binary()} | no_return().

decode({ws, text, json = Enc}, Data) ->
    decode({ws, text, json}, Data, opts(Enc, decode));

decode({ws, text, Enc}, _) ->
    error({unsupported_encoding, Enc});

decode({ws, binary, Enc}, Data) ->
    decode_binary(Data, Enc, opts(Enc, decode), []);

decode({raw, binary, Enc}, Data) ->
    decode_binary(Data, Enc, opts(Enc, decode), []).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec decode(subprotocol(), Data :: binary(), Opts :: list()) ->
    {Messages :: [wamp_message()], Rest :: binary()} | no_return().

decode({ws, text, json}, Data, Opts) ->
    decode_text(Data, json, Opts, []);

decode({ws, text, Enc}, _, _) ->
    error({unsupported_encoding, Enc});

decode({ws, binary, Enc}, Data, Opts) ->
    decode_binary(Data, Enc, Opts, []);

decode({raw, binary, Enc}, Data, Opts) ->
    decode_binary(Data, Enc, Opts, []).



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec encode(wamp_message() | list(), encoding()) -> binary() | no_return().

encode(Message, Enc) when is_tuple(Message) ->
    encode(pack(Message), Enc);

encode(Message, erl = Enc) when is_list(Message) ->
    encode(Message, Enc, opts(Enc, encode));

encode(Message, json = Enc) when is_list(Message) ->
    encode(Message, Enc, opts(Enc, encode));

encode(Message, msgpack = Enc) when is_list(Message) ->
    %% We want binary keys always
    encode(Message, msgpack, opts(Enc, encode));

encode(Message, bert = Enc) when is_list(Message) ->
    encode(Message, bert, opts(Enc, encode));

encode(Message, Format) when is_list(Message) ->
    error({unsupported_encoding, Format}).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec encode(wamp_message() | list(), encoding(), Opts :: list()) ->
    binary() | no_return().


encode(Message, Encoding, Opts) when is_tuple(Message) ->
    encode(pack(Message), Encoding, Opts);

encode(Message, erl, _) when is_list(Message) ->
    term_to_binary(Message);

encode(Message, bert, _) when is_list(Message) ->
    bert:encode(Message);

encode(Message, json, Opts) when is_list(Message) ->
    jsone:encode(Message, Opts);

encode(Message, msgpack, Opts) when is_list(Message) ->
    msgpack:pack(Message, Opts);

encode(Message, Format, _) when is_list(Message) ->
    error({unsupported_encoding, Format}).


%% -----------------------------------------------------------------------------
%% @doc
%% Returns a message in WAMP list format.
%% @end
%% -----------------------------------------------------------------------------
-spec pack(wamp_message()) -> list() | no_return().

pack(#error{} = M) ->
    #error{
        request_type = ReqType,
        request_id = ReqId,
        details = Details,
        error_uri = ErrorUri,
        arguments = Args,
        arguments_kw = Payload
    } = M,
    T = pack_optionals(Args, Payload),
    [?ERROR, ReqType, ReqId, Details, ErrorUri | T];

pack(#publish{} = M) ->
    #publish{
        request_id = ReqId,
        options = Options,
        topic_uri = TopicUri,
        arguments = Args,
        arguments_kw = Payload
    } = M,
    T = pack_optionals(Args, Payload),
    [?PUBLISH, ReqId, Options, TopicUri | T];

pack(#event{} = M) ->
    #event{
        subscription_id = SubsId,
        publication_id = PubId,
        details = Details,
        arguments = Args,
        arguments_kw = Payload
    } = M,
    T = pack_optionals(Args, Payload),
    [?EVENT, SubsId, PubId, Details | T];

pack(#call{} = M) ->
    #call{
        request_id = ReqId,
        options = Options,
        procedure_uri = ProcedureUri,
        arguments = Args,
        arguments_kw = Payload
    } = M,
    T = pack_optionals(Args, Payload),
    [?CALL, ReqId, Options, ProcedureUri | T];

pack(#result{} = M) ->
    #result{
        request_id = ReqId,
        details = Details,
        arguments = Args,
        arguments_kw = Payload
    } = M,
    T = pack_optionals(Args, Payload),
    [?RESULT, ReqId, Details | T];

pack(#invocation{} = M) ->
    #invocation{
        request_id = ReqId,
        registration_id = RegId,
        details = Details,
        arguments = Args,
        arguments_kw = Payload
    } = M,
    T = pack_optionals(Args, Payload),
    [?INVOCATION, ReqId, RegId, Details | T];

pack(#yield{} = M) ->
    #yield{
        request_id = ReqId,
        options = Options,
        arguments = Args,
        arguments_kw = Payload
    } = M,
    T = pack_optionals(Args, Payload),
    [?YIELD, ReqId, Options | T];

pack(#hello{} = M) -> pack_generic(?HELLO, M);
pack(#welcome{} = M) -> pack_generic(?WELCOME, M);
pack(#abort{} = M) -> pack_generic(?ABORT, M);
pack(#challenge{} = M) -> pack_generic(?CHALLENGE, M);
pack(#authenticate{} = M) -> pack_generic(?AUTHENTICATE, M);
pack(#goodbye{} = M) -> pack_generic(?GOODBYE, M);
pack(#published{} = M) -> pack_generic(?PUBLISHED, M);
pack(#subscribe{} = M) -> pack_generic(?SUBSCRIBE, M);
pack(#subscribed{} = M) -> pack_generic(?SUBSCRIBED, M);
pack(#unsubscribe{} = M) -> pack_generic(?UNSUBSCRIBE, M);
pack(#unsubscribed{} = M) -> pack_generic(?UNSUBSCRIBED, M);
pack(#cancel{} = M) -> pack_generic(?CANCEL, M);
pack(#register{} = M) -> pack_generic(?REGISTER, M);
pack(#registered{} = M) -> pack_generic(?REGISTERED, M);
pack(#unregister{} = M) -> pack_generic(?UNREGISTER, M);
pack(#unregistered{} = M) -> pack_generic(?UNREGISTERED, M);
pack(#interrupt{} = M) -> pack_generic(?INTERRUPT, M);

pack(_) ->
    error(badarg).


pack_generic(Type, M) when is_tuple(M) ->
    %% All other message types are straight forward
    [_H|T] = tuple_to_list(M),
    [Type | T].



%% -----------------------------------------------------------------------------
%% @doc
%% Converts a message from a WAMP list external format to
%% an internal format (erlang record).
%% See {@link wamp_message} for all message types.
%% @end
%% -----------------------------------------------------------------------------
-spec unpack(list()) -> wamp_message() | no_return().

unpack([?HELLO, RealmUri, Details]) ->
    wamp_message:hello(RealmUri, Details);

unpack([?WELCOME, SessionId, Details]) ->
    wamp_message:welcome(SessionId, Details);

unpack([?CHALLENGE, AuthMethod, Extra]) ->
    wamp_message:challenge(AuthMethod, Extra);

unpack([?AUTHENTICATE, Signature, Extra]) ->
    wamp_message:authenticate(Signature, Extra);

unpack([?ABORT, Details, ReasonUri]) ->
    wamp_message:abort(Details, ReasonUri);

unpack([?GOODBYE, Details, ReasonUri]) ->
    wamp_message:goodbye(Details, ReasonUri);

unpack([?ERROR, ReqType, ReqId, Details, ErrorUri]) ->
    wamp_message:error(
        ReqType,
        ReqId,
        Details,
        ErrorUri
    );

unpack([?ERROR, ReqType, ReqId, Details, ErrorUri, Args]) when is_list(Args) ->
    wamp_message:error(
        ReqType,
        ReqId,
        Details,
        ErrorUri,
        Args
    );

unpack([?ERROR, ReqType, ReqId, Details, ErrorUri, Args, Payload])
 when is_list(Args), is_map(Payload) ->
    wamp_message:error(
        ReqType,
        ReqId,
        Details,
        ErrorUri,
        Args,
        Payload
    );

unpack([?PUBLISH, ReqId, Options, TopicUri]) ->
    wamp_message:publish(
        ReqId, Options, TopicUri);

unpack([?PUBLISH, ReqId, Options, TopicUri, Args]) ->
    wamp_message:publish(
        ReqId,
        Options,
        TopicUri,
        Args
    );

unpack([?PUBLISH, ReqId, Options, TopicUri, Args, Payload]) ->
    wamp_message:publish(
        ReqId,
        Options,
        TopicUri,
        Args,
        Payload
    );

unpack([?PUBLISHED, ReqId, PubId]) ->
    wamp_message:published(ReqId, PubId);

unpack([?SUBSCRIBE, ReqId, Options, TopicUri]) ->
    wamp_message:subscribe(
        ReqId, Options, TopicUri);

unpack([?SUBSCRIBED, ReqId, SubsId]) ->
    wamp_message:subscribed(ReqId, SubsId);

unpack([?UNSUBSCRIBE, ReqId, SubsId]) ->
    wamp_message:unsubscribe(ReqId, SubsId);

unpack([?UNSUBSCRIBED, ReqId]) ->
    wamp_message:unsubscribed(ReqId);

unpack([?EVENT, SubsId, PubId, Details]) ->
    wamp_message:event(
        SubsId,
        PubId,
        Details
    );

unpack([?EVENT, SubsId, PubId, Details, Args]) ->
    wamp_message:event(
        SubsId,
        PubId,
        Details,
        Args
    );

unpack([?EVENT, SubsId, PubId, Details, Args, Payload]) ->
    wamp_message:event(
        SubsId,
        PubId,
        Details,
        Args,
        Payload
    );

unpack([?CALL, ReqId, Options, ProcedureUri]) ->
    wamp_message:call(
        ReqId,
        Options,
        ProcedureUri
    );

unpack([?CALL, ReqId, Options, ProcedureUri, Args]) ->
    wamp_message:call(
        ReqId,
        Options,
        ProcedureUri,
        Args
    );

unpack([?CALL, ReqId, Options, ProcedureUri, Args, Payload]) ->
    wamp_message:call(
        ReqId,
        Options,
        ProcedureUri,
        Args,
        Payload
    );

unpack([?CANCEL, ReqId, Options]) ->
    wamp_message:cancel(ReqId, Options);

unpack([?INTERRUPT, ReqId, Options]) ->
    wamp_message:interrupt(ReqId, Options);

unpack([?RESULT, ReqId, Details]) ->
    wamp_message:result(ReqId, Details);

unpack([?RESULT, ReqId, Details, Args]) ->
    wamp_message:result(ReqId, Details, Args);

unpack([?RESULT, ReqId, Details, Args, Payload]) ->
    wamp_message:result(
        ReqId, Details, Args, Payload);


unpack([?REGISTER, ReqId, Options, ProcedureUri]) ->
    wamp_message:register(
        ReqId, Options, ProcedureUri);

unpack([?REGISTERED, ReqId, RegId]) ->
    wamp_message:registered(ReqId, RegId);

unpack([?UNREGISTER, ReqId, RegId]) ->
    wamp_message:unregister(ReqId, RegId);

unpack([?UNREGISTERED, ReqId]) ->
    wamp_message:unregistered(ReqId);

unpack([?INVOCATION, ReqId, RegId, Details]) ->
    wamp_message:invocation(
        ReqId,
        RegId,
        Details
    );

unpack([?INVOCATION, ReqId, RegId, Details, Args]) ->
    wamp_message:invocation(
        ReqId,
        RegId,
        Details,
        Args
    );

unpack([?INVOCATION, ReqId, RegId, Details, Args, Payload]) ->
    wamp_message:invocation(
        ReqId,
        RegId,
        Details,
        Args,
        Payload
    );

unpack([?YIELD, ReqId, Options]) ->
    wamp_message:yield(
        ReqId,
        Options
    );

unpack([?YIELD, ReqId, Options, Args]) ->
    wamp_message:yield(
        ReqId,
        Options,
        Args
    );

unpack([?YIELD, ReqId, Options, Args, Payload]) ->
    wamp_message:yield(
        ReqId,
        Options,
        Args,
        Payload
    );

unpack(M) ->
    error({invalid_message, M}).



%% =============================================================================
%% PRIVATE: DECODING
%% =============================================================================



opts(erl, _) ->
    [];

opts(bert, _) ->
    [];

opts(json, encode) ->
    wamp_config:get([serialization, json, encode]);

opts(json, decode) ->
    wamp_config:get([serialization, json, decode]);

opts(msgpack, encode) ->
    [{map_format, map}, {pack_str, from_binary}];

opts(msgpack, decode) ->
    [{map_format, map}, {unpack_str, as_binary}].



%% @private
do_decode_message_name(<<131, 108, _:32, 97, Type, _/binary>>, erl) ->
    message_name(Type);

do_decode_message_name(<<131, 107, _Len:16, Type, _/binary>>, erl) ->
    %% When all elements in the list are integers, erlang encodes it
    %% with 107 :: string type
    message_name(Type);

do_decode_message_name(_, erl) ->
    error(badarg);

do_decode_message_name(Data, bert) ->
    do_decode_message_name(Data, erl);

do_decode_message_name(<<"[", Rest/binary>>, json) ->
    case binary:match(Rest, [<<$,>>], []) of
        nomatch ->
            error(badarg);
        {Pos, 1} ->
            Type = binary:part(Rest, {0, Pos}),
            message_name(binary_to_integer(Type))
    end;

do_decode_message_name(_, json) ->
    error(badarg);

do_decode_message_name(<<2#101:3, _:5, 0:1, V:7, _/binary>>, msgpack) ->
    message_name(V);

do_decode_message_name(<<2#101:3, _:5, 16#CD, V:8, _/binary>>, msgpack) ->
    message_name(V);

do_decode_message_name(<<2#1001:4, _:4, 0:1, V:7, _/binary>>, msgpack) ->
    message_name(V);

do_decode_message_name(
    <<2#1001:4, _:4, 16#CD, V:8/unsigned-integer, _/binary>>, msgpack) ->
    %% We use msgpack list with len < 16
    message_name(V);

do_decode_message_name(_, msgpack) ->
    error(badarg);

do_decode_message_name(_, Enc) ->
    error({unsupported_encoding, Enc}).


%% @private
-spec decode_text(
    binary(), encoding(), Opts :: list(), Acc0 :: [wamp_message()]) ->
    {Acc1 :: [wamp_message()], Buffer :: binary()} | no_return().

decode_text(Data, json, Opts, Acc) ->
    {decode_message(Data, json, Opts, Acc), <<>>}.


%% @private
-spec decode_binary(
    binary(), encoding(), Opts :: list(), Acc0 :: [wamp_message()]) ->
    {Acc1 :: [wamp_message()], Buffer :: binary()} | no_return().

decode_binary(Data, Enc, Opts, Acc) ->
    %% At the moment we do not support batched encoding
    %% so Data is a single message
    {decode_message(Data, Enc, Opts, Acc), <<>>}.



%% @private
-spec decode_message(binary(), encoding(), Opts :: list(), [wamp_message()]) ->
    [wamp_message()] | no_return().

decode_message(Data, json, Opts, Acc) ->
    %% Decode might failed with badarg exception if not a proper JSON
    M = jsone:decode(Data, Opts),
    unpack(M, Acc);

decode_message(Data, msgpack, Opts, Acc) ->
    {ok, M} = msgpack:unpack(Data, Opts),
    unpack(M, Acc);

decode_message(Data, bert, _, Acc) ->
    M = bert:decode(Data),
    unpack(M, Acc);

decode_message(Bin, erl, _, Acc) ->
    M = binary_to_term(Bin),
    unpack(M, Acc);

decode_message(_Data, Enc, _, _Acc) ->
    error({unsupported_encoding, Enc}).



unpack(M, Acc) ->
    try
        [unpack(M) | Acc]
    catch
        error:{validation_failed, Reason} ->
            error({validation_failed, Reason, request_info(M)});
        error:{invalid_uri, Uri} ->
            error({invalid_uri, Uri, request_info(M)})
    end.




request_info([?HELLO, _, _]) ->
    #{request_type => ?HELLO, request_id => undefined};

request_info([?WELCOME, _, _]) ->
    #{request_type => ?WELCOME, request_id => undefined};

request_info([?CHALLENGE, _, _]) ->
    #{request_type => ?CHALLENGE, request_id => undefined};

request_info([?AUTHENTICATE, _, _]) ->
    #{request_type => ?AUTHENTICATE, request_id => undefined};

request_info([?ABORT, _, _]) ->
    #{request_type => ?ABORT, request_id => undefined};

request_info([?GOODBYE, _, _]) ->
    #{request_type => ?GOODBYE, request_id => undefined};

request_info([?ERROR | _])->
    #{request_type => ?ERROR, request_id => undefined};

request_info([?PUBLISH, ReqId | _]) ->
    #{request_type => ?PUBLISH, request_id => ReqId};

request_info([?PUBLISHED, ReqId | _]) ->
    #{request_type => ?PUBLISHED, request_id => ReqId};

request_info([?SUBSCRIBE, ReqId | _]) ->
    #{request_type => ?SUBSCRIBE, request_id => ReqId};

request_info([?SUBSCRIBED, ReqId | _]) ->
    #{request_type => ?SUBSCRIBED, request_id => ReqId};

request_info([?UNSUBSCRIBE, ReqId | _]) ->
    #{request_type => ?UNSUBSCRIBE, request_id => ReqId};

request_info([?UNSUBSCRIBED, ReqId]) ->
    #{request_type => ?UNSUBSCRIBED, request_id => ReqId};

request_info([?EVENT | _]) ->
    #{request_type => ?EVENT, request_id => undefined};

request_info([?CALL, ReqId | _]) ->
    #{request_type => ?CALL, request_id => ReqId};

request_info([?CANCEL, ReqId | _]) ->
    #{request_type => ?CANCEL, request_id => ReqId};

request_info([?INTERRUPT, ReqId | _]) ->
    #{request_type => ?INTERRUPT, request_id => ReqId};

request_info([?RESULT, ReqId | _]) ->
    #{request_type => ?RESULT, request_id => ReqId};

request_info([?REGISTER, ReqId | _]) ->
    #{request_type => ?REGISTERED, request_id => ReqId};

request_info([?REGISTERED, ReqId | _]) ->
    #{request_type => ?REGISTERED, request_id => ReqId};

request_info([?UNREGISTER, ReqId | _]) ->
    #{request_type => ?UNREGISTER, request_id => ReqId};

request_info([?UNREGISTERED, ReqId]) ->
    #{request_type => ?UNREGISTERED, request_id => ReqId};

request_info([?INVOCATION, ReqId | _]) ->
    #{request_type => ?INVOCATION, request_id => ReqId};

request_info([?YIELD, ReqId | _]) ->
    #{request_type => ?YIELD, request_id => ReqId}.




%% =============================================================================
%% PRIVATE: UTILS
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% RFC: Implementations SHOULD avoid sending empty Arguments lists.
%% RFC: Implementations SHOULD avoid sending empty ArgumentsKw dictionaries.
%% @end
%% -----------------------------------------------------------------------------
pack_optionals(undefined, undefined) -> [];
pack_optionals(Args, undefined) -> [Args];
pack_optionals(Args, Payload) -> [Args, Payload].


-spec message_name(1..255) -> atom().

message_name(?HELLO) -> hello;
message_name(?WELCOME) -> welcome;
message_name(?ABORT) -> abort;
message_name(?CHALLENGE) -> challenge;
message_name(?AUTHENTICATE) -> authenticate;
message_name(?GOODBYE) -> goodbye;
message_name(?ERROR) -> error;
message_name(?PUBLISH) -> publish;
message_name(?PUBLISHED) -> published;
message_name(?SUBSCRIBE) -> subscribe;
message_name(?SUBSCRIBED) -> subscribed;
message_name(?UNSUBSCRIBE) -> unsubscribe;
message_name(?UNSUBSCRIBED) -> unsubscribed;
message_name(?EVENT) -> event;
message_name(?CALL) -> call;
message_name(?CANCEL) -> cancel;
message_name(?RESULT) -> result;
message_name(?REGISTER) -> register;
message_name(?REGISTERED) -> registered;
message_name(?UNREGISTER) -> unregister;
message_name(?UNREGISTERED) -> unregistered;
message_name(?INVOCATION) -> invocation;
message_name(?INTERRUPT) -> interrupt;
message_name(?YIELD) -> yield.