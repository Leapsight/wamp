%% -----------------------------------------------------------------------------
%% Copyright (C) Ngineo Limited 2015 - 2016. All rights reserved.
%% -----------------------------------------------------------------------------

%% =============================================================================
%% @doc
%%
%% @end
%% =============================================================================
-module (wamp_uri).
-include("wamp.hrl").

-type rule()    ::  loose | loose_allow_empty | strict | strict_allow_empty.

-export_type([rule/0]).

-export([is_valid/1]).
-export([is_valid/2]).
-export([components/1]).



%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc Sames as `is_valid(Uri, loose_allow_empty)'.
%% @end
%% -----------------------------------------------------------------------------
-spec is_valid(uri()) -> boolean().

is_valid(Uri) ->
    is_valid(Uri, loose_allow_empty).



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec is_valid(Uri :: uri(), Rule :: rule()) -> boolean().

is_valid(<<>>, _) ->
    false;

is_valid(Uri, Rule) when is_binary(Uri) ->
    re:run(Uri, uri_pattern(Rule)) =/= nomatch;

is_valid(_, _) ->
    false.


%% -----------------------------------------------------------------------------
%% @doc
%% Example:
%% components(<<"com.mycompany.foo.bar">>) ->
%% [<<"com.mycompany">>, <<"foo">>, <<"bar">>].
%% @end
%% -----------------------------------------------------------------------------
-spec components(uri()) -> [binary()] | no_return().

components(Uri) ->
    case binary:split(Uri, <<".">>, [global]) of
        [TopLevelDomain, AppName | Rest] when length(Rest) > 0 ->
            Domain = <<TopLevelDomain/binary, $., AppName/binary>>,
            [Domain | Rest];
        _Other ->
            %% Invalid Uri
            error({?WAMP_INVALID_URI, Uri})
    end.



%% =============================================================================
%% PRIVATE
%% =============================================================================



%% @private
uri_pattern(Rule) ->
    CompiledPattern = persistent_term:get({?MODULE, Rule}, undefined),
    uri_pattern(Rule, CompiledPattern).


%% @private
uri_pattern(loose = Rule, undefined) ->
    {ok, Pattern} = re:compile("^([^\s\.#]+\.)*([^\s\.#]+)$"),
    ok = persistent_term:put({?MODULE, Rule}, Pattern),
    Pattern;

uri_pattern(loose_allow_empty = Rule, undefined) ->
    {ok, Pattern} = re:compile("^(([^\s\.#]+\.)|\.)*([^\s\.#]+)?$"),
    ok = persistent_term:put({?MODULE, Rule}, Pattern),
    Pattern;

uri_pattern(strict = Rule, undefined) ->
    {ok, Pattern} = re:compile("^([0-9a-z_]+\.)*([0-9a-z_]+)$"),
    ok = persistent_term:put({?MODULE, Rule}, Pattern),
    Pattern;

uri_pattern(strict_allow_empty = Rule, undefined) ->
    {ok, Pattern} = re:compile("^(([0-9a-z_]+\.)|\.)*([0-9a-z_]+)?$"),
    ok = persistent_term:put({?MODULE, Rule}, Pattern),
    Pattern;

uri_pattern(_, CompiledPattern) ->
    CompiledPattern.
