-module(wamp_utils).
-include("wamp.hrl").


-export([rand_uniform/0]).
-export([is_valid_id/1]).

%% -----------------------------------------------------------------------------
%% @doc
%% Returns a random number from a _uniform distribution_ over the range [0, 2^53]
%% @end
%% -----------------------------------------------------------------------------
-spec rand_uniform() -> integer().

rand_uniform() ->
    crypto:rand_uniform(0, ?MAX_ID).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec is_valid_id(id()) -> boolean().
is_valid_id(N) when is_integer(N) andalso N >= 0 andalso N =< ?MAX_ID ->
    true;
is_valid_id(_) ->
    false.


