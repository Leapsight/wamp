%% -----------------------------------------------------------------------------
%%  Copyright (c) 2015-2021 Leapsight. All rights reserved.
%% -----------------------------------------------------------------------------

%% =============================================================================
%% @doc
%%
%% @end
%% =============================================================================
-module (wamp_uri).
-include("wamp.hrl").

-type t()       ::  binary().
-type rule()    ::  loose | loose_prefix | loose_allow_empty
                    | strict | strict_prefix | strict_allow_empty.

-export_type([t/0]).
-export_type([rule/0]).

-export([is_valid/1]).
-export([is_valid/2]).
-export([validate/1]).
-export([validate/2]).
-export([validate_match/2]).
-export([components/1]).



%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc Sames as `is_valid(Uri, loose_allow_empty)'.
%% @end
%% -----------------------------------------------------------------------------
-spec is_valid(uri()) -> boolean().

is_valid(Uri) when is_binary(Uri) andalso byte_size(Uri) > 0 ->
    is_valid(Uri, loose_allow_empty);

is_valid(_) ->
    false.



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec is_valid(Uri :: uri(), Rule :: rule()) -> boolean().

is_valid(Uri, Rule) when is_binary(Uri) andalso byte_size(Uri) > 0 ->
    re:run(Uri, uri_regex(Rule)) =/= nomatch;

is_valid(_, _) ->
    false.



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec validate(Uri :: binary()) -> Uri :: t().

validate(Uri) when is_binary(Uri) andalso byte_size(Uri) > 0 ->
    maybe_error(is_valid(Uri), Uri);

validate(Uri) ->
    maybe_error(false, Uri).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec validate(Uri :: binary(), Rule :: rule()) -> Uri :: t().

validate(Uri, Rule) when is_binary(Uri) andalso byte_size(Uri) > 0  ->
    maybe_error(is_valid(Uri, Rule), Uri);

validate(Uri, _) ->
    maybe_error(false, Uri).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec validate_match(Uri :: binary(), Strategy :: binary()) -> Uri :: t().

validate_match(Uri, ?EXACT_MATCH) ->
    maybe_error(is_valid(Uri, strict), Uri);

validate_match(Uri, ?PREFIX_MATCH) ->
    maybe_error(is_valid(Uri, strict_prefix), Uri);

validate_match(Uri, ?WILDCARD_MATCH) ->
    maybe_error(is_valid(Uri, strict_allow_empty), Uri).



%% -----------------------------------------------------------------------------
%% @doc
%% Example:
%% components(<<"com.mycompany.foo.bar">>) ->
%% [<<"com.mycompany">>, <<"foo">>, <<"bar">>].
%% @end
%% -----------------------------------------------------------------------------
-spec components(t()) -> [binary()] | no_return().

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

%% URI components (the parts between two .s, the head part up to the first .,
%% the tail part after the last .) MUST NOT contain a ., # or whitespace
%% characters and MUST NOT be empty (zero-length strings).



%% @private
uri_regex(Rule) ->
    Regex = persistent_term:get({?MODULE, Rule}, undefined),
    uri_regex(Rule, Regex).


%% @private
uri_regex(loose = Rule, undefined) ->
    {ok, Regex} = re:compile("^([^\s\.#]+\.)*([^\s\.#]+)$"),
    ok = persistent_term:put({?MODULE, Rule}, Regex),
    Regex;

uri_regex(loose_prefix = Rule, undefined) ->
    {ok, Regex} = re:compile("^([^\s\.#]+\.)*([^\s\.#]+)[.]?$"),
    ok = persistent_term:put({?MODULE, Rule}, Regex),
    Regex;

uri_regex(loose_allow_empty = Rule, undefined) ->
    {ok, Regex} = re:compile("^(([^\s\.#]+\.)|\.)*([^\s\.#]+)?$"),
    ok = persistent_term:put({?MODULE, Rule}, Regex),
    Regex;

uri_regex(strict = Rule, undefined) ->
    {ok, Regex} = re:compile("^([0-9a-z_]+\.)*([0-9a-z_]+)$"),
    ok = persistent_term:put({?MODULE, Rule}, Regex),
    Regex;

uri_regex(strict_prefix = Rule, undefined) ->
    {ok, Regex} = re:compile("^([0-9a-z_]+\.)*([0-9a-z_]+)[.]?$"),
    ok = persistent_term:put({?MODULE, Rule}, Regex),
    Regex;

uri_regex(strict_allow_empty = Rule, undefined) ->
    {ok, Regex} = re:compile("^(([0-9a-z_]+\.)|\.)*([0-9a-z_]+)?$"),
    ok = persistent_term:put({?MODULE, Rule}, Regex),
    Regex;

uri_regex(_, Regex) ->
    Regex.



maybe_error(true, Uri) ->
    Uri;

maybe_error(false, Uri) ->
    error({invalid_uri, Uri}).
