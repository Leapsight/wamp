%% -----------------------------------------------------------------------------
%% Copyright (C) Ngineo Limited 2015 - 2020. All rights reserved.
%% -----------------------------------------------------------------------------

%% =============================================================================
%% @doc
%%
%% @end
%% =============================================================================
-module(wamp_options).
-include("wamp.hrl").

-type type()    ::  publish
                    | subscribe
                    | call
                    | cancel
                    | register
                    | interrupt
                    | yield.


-export_type([type/0]).


-export([new/2]).




%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc Fails with an exception if the Options maps is not valid.
%% A Options map is valid if all its properties (keys) are valid. A property is
%% valid if it is a key defined by the WAMP Specification for the message type
%% or when the key is found in the list of extended_options configured in the
%% application environment and in both cases the key is valid according to the
%% WAMP regex specification.
%%
%% Example:
%%
%% ```
%% application:set_env(wamp, extende_options, [{call, [<<"_x">>, <<"_y">>]}).
%% ```
%%
%% Using this configuration only `call' messages would accept `<<"_x">>'
%% and `<<"_y">>' properties.
%% -----------------------------------------------------------------------------
-spec new(MessageType :: type(), Opts :: map()) -> ok | no_return().

new(Type, Opts) ->
    Spec = spec(Type),
    Extensions = app_config:get(wamp, [extended_options, Type], []),
    wamp_utils:validate_map(Opts, Spec, Extensions).




%% =============================================================================
%% PRIVATE
%% =============================================================================



%% @private
spec(call) ->
    ?CALL_OPTS_SPEC;
spec(cancel) ->
    ?CALL_CANCELLING_OPTS_SPEC;
spec(interrupt) ->
    ?CALL_CANCELLING_OPTS_SPEC;
spec(publish) ->
    ?PUBLISH_OPTS_SPEC;
spec(register) ->
    ?REGISTER_OPTS_SPEC;
spec(subscribe) ->
    ?SUBSCRIBE_OPTS_SPEC;
spec(yield) ->
    ?YIELD_OPTIONS_SPEC;
spec(_) ->
    error(badarg).