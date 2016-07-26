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

-export([parse/1]).
-export([is_valid/1]).
-export([components/1]).

%% URI components SHOULD only contain letters, digits and "_".
%% and allow empty uri components
%% re.compile("^(([0-9a-z_]+\.)|\.)*([0-9a-z_]+)?$")

%% Further, application URIs MUST NOT use "wamp" as a first URI
%% component, since this is reserved for URIs predefined with the WAMP
%% protocol itself.

%% The URIs are understood to form a single, global, hierarchical
%% namespace for WAMP.
%%
%% The namespace is unified for topics, procedures and errors - these
%% different resource types do NOT have separate namespaces.

%% Further, application URIs MUST NOT use "wamp" as a first URI
%% component, since this is reserved for URIs predefined with the WAMP
%% protocol itself.

parse(URI) when is_list(URI) ->
    %% TODO
    {ok, URI};
parse(URI) when is_binary(URI) ->
    %% TODO
    {ok, URI}.

is_valid(Uri) ->
    case catch parse(Uri) of
        {ok, _} -> true;
        _ -> false
    end.


%% -----------------------------------------------------------------------------
%% @doc
%% Example:
%% components(<<"com.mycompany.foo.bar">>) ->
%% [<<"com.mycompany">>, <<"foo">>, <<"bar">>].
%% @end
%% -----------------------------------------------------------------------------
-spec components(uri()) -> [binary()].
components(Uri) ->
    case binary:split(Uri, <<".">>, [global]) of
        [TopLevelDomain, AppName | Rest] when length(Rest) > 0 ->
            Domain = <<TopLevelDomain/binary, $., AppName/binary>>,
            [Domain | Rest];
        _Other ->
            %% Invalid Uri
            error({badarg, Uri})
    end.
