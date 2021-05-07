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

-type t()               ::  binary().
-type rule()            ::  loose | loose_prefix | loose_allow_empty
                            | strict | strict_prefix | strict_allow_empty.
-type match_strategy()  ::  binary().

-export_type([t/0]).
-export_type([rule/0]).

-export([is_valid/1]).
-export([is_valid/2]).
-export([validate/1]).
-export([validate/2]).
-export([components/1]).
-export([match/3]).



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
-spec is_valid(Uri :: uri(), RuleOrStrategy :: rule() | match_strategy()) ->
    boolean() | no_return().

is_valid(Uri, Strategy) when is_binary(Strategy) ->
    is_valid(Uri, rule_for_strategy(Strategy));

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
-spec validate(Uri :: binary(), RuleOrStrategy :: rule() | match_strategy()) ->
    Uri :: t().

validate(Uri, Rule) when is_binary(Uri) andalso byte_size(Uri) > 0  ->
    maybe_error(is_valid(Uri, Rule), Uri);

validate(Uri, _) ->
    maybe_error(false, Uri).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec match(Uri :: t(), Pattern :: t(), Strategy :: match_strategy()) -> any().

match(Uri, Uri, ?EXACT_MATCH)
when is_binary(Uri) andalso byte_size(Uri) > 0 ->
    true;

match(Uri, Pattern, ?PREFIX_MATCH)
when is_binary(Uri) andalso byte_size(Uri) > 0 ->
    binary:longest_common_prefix([Uri, Pattern]) >= byte_size(Pattern);

match(Uri, Pattern, ?WILDCARD_MATCH)
when is_binary(Uri) andalso byte_size(Uri) > 0 ->
    subsumes(
        binary:split(Pattern, [<<$.>>], [global]),
        binary:split(Uri, [<<$.>>], [global])
    );

match(Uri, _, _)
when is_binary(Uri) andalso byte_size(Uri) > 0 ->
    false;

match(_, _, _) ->
    error(badarg).






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

uri_regex(_, undefined) ->
    error(badrule);

uri_regex(_, Regex) ->
    Regex.


%% @private
-spec subsumes(Pattern :: binary(), Uri :: binary()) -> boolean().

subsumes(Term, Term) ->
    true;

subsumes(Term1, Term2) when length(Term1) =/= length(Term2) ->
    false;

subsumes([H|T1], [H|T2]) ->
    subsumes(T1, T2);

subsumes([<<>>|T1], [_|T2]) ->
    subsumes(T1, T2);

subsumes([], []) ->
    true;

subsumes(_, _) ->
    false.


%% @private
maybe_error(true, Uri) ->
    Uri;

maybe_error(false, Uri) ->
    error({invalid_uri, Uri}).


%% @private
rule_for_strategy(?EXACT_MATCH) -> strict;
rule_for_strategy(?PREFIX_MATCH) -> strict_prefix;
rule_for_strategy(?WILDCARD_MATCH) -> strict_allow_empty;
rule_for_strategy(_) -> error(badstrategy).