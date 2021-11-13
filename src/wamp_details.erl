%% -----------------------------------------------------------------------------
%%  Copyright (c) 2015-2021 Leapsight. All rights reserved.
%% -----------------------------------------------------------------------------

%% =============================================================================
%% @doc
%%
%% @end
%% =============================================================================
-module(wamp_details).
-include("wamp.hrl").

-type type()    ::  hello
                    | welcome
                    | abort
                    | goodbye
                    | event
                    | result
                    | invocation
                    | event_received
                    | subscriber_received.


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
-spec new(MessageType :: type(), Details :: map()) -> map() | no_return().

new(Type, Details) ->
    Extensions = app_config:get(wamp, [extended_details, Type], []),
    case spec(Type) of
        undefined ->
            Opts = #{
                keep_unknown => true
            },
            wamp_utils:validate_map(Details, #{}, Extensions, Opts);
        Spec ->

            wamp_utils:validate_map(Details, Spec, Extensions)
    end.




%% =============================================================================
%% PRIVATE
%% =============================================================================



%% @private
spec(hello) ->
    ?HELLO_DETAILS_SPEC;
spec(welcome) ->
    ?WELCOME_DETAILS_SPEC;
spec(abort) ->
    undefined;
spec(goodbye) ->
    undefined;
spec(error) ->
    ?ERROR_DETAILS_SPEC;
spec(event) ->
    ?EVENT_DETAILS_SPEC;
spec(event_received) ->
    ?EVENT_RECEIVED_DETAILS_SPEC;
spec(subscriber_received) ->
    ?SUBSCRIBER_RECEIVED_DETAILS_SPEC;
spec(result) ->
    ?RESULT_DETAILS_SPEC;
spec(invocation) ->
    ?INVOCATION_DETAILS_SPEC;
spec(_) ->
    error(badarg).