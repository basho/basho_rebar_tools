%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%%
%% @doc This module performs operations using the GitHub HTTP API.
%%
%% @see https://developer.github.com/v3/
%%
-module(brt_github).

% API
-export([
    branch_head/3
]).

-export_type([
    commit/0
]).

-include("brt.hrl").

-type commit_info() ::  {sha,       git_hash()}
                    |   {time,      timestamp()}
                    |   {email,     string()}
                    |   {message,   string()}
                    |   {name,      string()}
                    |   {user,      string()}
                    |   {userid,    pos_integer()}
                    |   {api_key(), api_value()}.

-type api_key()     ::  atom().
-type api_value()   ::  atom() | integer() | list() | string() | timestamp().
-type commit()      ::  [commit_info()].
-type git_hash()    ::  string().
-type path_seg()    ::  atom() | binary() | string().
-type timestamp()   ::  calendar:datetime().

-type head()        ::  [{string(), string()}].
-type body()        ::  [list()] | [{api_key(), api_value()}].

-define(GH_HOST,    "api.github.com").
-define(GH_API,     "https://" ?GH_HOST).

%% ===================================================================
%% API
%% ===================================================================

-spec branch_head(
    Owner :: path_seg(), Repo :: path_seg(), Branch :: path_seg())
        -> commit() | brt:err_result().
%%
%% @doc Returns information about the latest commit on the specified branch.
%%
%% Information is returned as a `proplist' whose Keys are atoms with
%% appropriately typed Values.
%%
%% The result contains at least:
%%
%% - `{sha, <full-hash>}'
%%
%% - `{time, {{Year, Mon, Day}, {Hour, Min, Sec}}}'
%%
branch_head(Owner, Repo, Branch) ->
    URL = lists:flatten([
        ?GH_API, "/repos/", brt:to_string(Owner), $/, brt:to_string(Repo),
        "/branches/", brt:to_string(Branch)
    ]),
    case gh_api_get(URL) of
        {ok, _, Body} ->
            Commit  = brt:get_key_list(commit, Body),
            Detail  = brt:get_key_list(commit, Commit),
            User    = brt:get_key_list(committer, Commit),
            History = brt:get_key_list(committer, Detail),
            [
                brt:get_key_tuple(email, History),
                brt:get_key_tuple(message, Detail),
                brt:get_key_tuple(name, History),
                brt:get_key_tuple(sha, Commit),
                {time, brt:get_key_value(date, History)},
                {user, brt:get_key_value(login, User)},
                {userid, brt:get_key_value(id, User)}
            ];
        GetErr ->
            GetErr
    end.

%% ===================================================================
%% GitHub Operations
%% ===================================================================

-spec gh_api_get(URL :: string()) -> {ok, head(), body()} | brt:err_result().
%
% Perform an HTTP GET on the specified URL using the default MediaType.
%
gh_api_get(URL) ->
    gh_api_get(URL, "application/vnd.github.loki-preview+json").

-spec gh_api_get(URL :: string(), MediaType :: string())
        -> {ok, head(), body()} | brt:err_result().
%
% Perform an HTTP GET on the specified URL using the specified MediaType.
%
gh_api_get(URL, MediaType) ->
    ReqHead = [{"Accept", MediaType}, {"User-Agent", "Basho-Rebar-Tools"}],
    GetOpts = [{relaxed, true} | ssl_opts()],
    RetOpts = [{body_format, binary}],
    case httpc:request(get, {URL, ReqHead}, GetOpts, RetOpts) of
        {ok, {{_Version, 200, _Reason}, Head, BinBody}} ->
            {ok, Body, []} = parse_body(BinBody),
            {ok, Head, Body};
        {ok, {Result, _, _}} ->
            {error, Result};
        Error ->
            Error
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec ssl_opts() -> [{ssl, [ssl:ssl_option()]}].
%
% Assume we're running in rebar3, which includes the `certifi' and
% `ssl_verify_fun' packages.
%
ssl_opts() ->
    Key = {?MODULE, ssl_opts},
    case erlang:get(Key) of
        undefined ->
            Opts = [{ssl, [
                {depth, 2}, % currently only need 1, but allow wiggle room
                {verify, verify_peer},
                {cacerts, certifi:cacerts()},
                {verify_fun,
                    {fun ssl_verify_hostname:verify_fun/3,
                        [{check_hostname, ?GH_HOST}]}}
            ]}],
            _ = erlang:put(Key, Opts),
            Opts;
        Val ->
            Val
    end.

%% ===================================================================
%% JSON
%% ===================================================================
%
% GitHub's JSON is extremely simple, and because we control the retrieval we
% know exactly how it's encoded, so it's easy to parse it ourselves rather
% than pulling in an external package.
%
% Because it's tailored to the API, there are no error returns. If the HTTP
% operation was successful and we run across something unexpected, we assume
% it's a programming error and raise a `badarg' exception.
%

-spec parse_body(Json :: binary() | list()) -> {ok, list(), [char()]}.
%
% Returns either a list of values or name/value pairs, depending on the API
% call used.
%
parse_body(Json) ->
    parse_value(skip_space(unicode:characters_to_list(Json, utf8))).

-spec parse_array([char()], [term()]) -> {ok, list(), [char()]}.
parse_array([$] | Rest], Result) ->
    {ok, lists:reverse(Result), skip_space(Rest)};
parse_array([$, | Rest], Result) ->
    % TODO: validate commas actually divide values, or do we really care?
    parse_array(skip_space(Rest), Result);
parse_array(Json, Result) ->
    {ok, Value, Rest} = parse_value(Json),
    parse_array(skip_space(Rest), [Value | Result]).

-spec parse_string([char()], [char()]) -> {ok, [char()], [char()]}.
parse_string([$\" | Rest], Result) ->
    {ok, lists:reverse(Result), skip_space(Rest)};
parse_string([$\\ | Rest], Result) ->
    parse_string(Rest, Result);
parse_string([Ch | Rest], Result) ->
    parse_string(Rest, [Ch | Result]);
parse_string(Arg, _) ->
    erlang:error(badarg, [Arg]).

-spec parse_struct([char()], [term()])
        -> {ok, [{api_key(), term()}], [char()]}.
parse_struct([$} | Rest], Result) ->
    {ok, Result, skip_space(Rest)};
parse_struct([$, | Rest], Result) ->
    % TODO: validate commas actually divide values, or do we really care?
    parse_struct(skip_space(Rest), Result);
parse_struct([$\" | Rest], Result) ->
    {ok, Name, More1} = parse_string(Rest, []),
    case skip_space(More1) of
        [$: | More2] ->
            {ok, Value, Remain} = parse_value(skip_space(More2)),
            parse_struct(skip_space(Remain),
                [{erlang:list_to_atom(Name), Value} | Result]);
        _ ->
            erlang:error(badarg, [More1])
    end;
parse_struct(Arg, _) ->
    erlang:error(badarg, [Arg]).

-spec parse_value([char()]) -> {ok, term(), [char()]}.
parse_value("true" ++ Rest) ->
    {ok, true, skip_space(Rest)};
parse_value("false" ++ Rest) ->
    {ok, false, skip_space(Rest)};
parse_value("null" ++ Rest) ->
    % not sure if we'll ever see this, but it's alluded to in the API schema
    {ok, null, skip_space(Rest)};
parse_value([
    $\", Y1, Y2, Y3, Y4, $-, M1, M2, $-, D1, D2,
    $T, H1, H2, $:, N1, N2, $:, S1, S2, $Z, $\" | Rest]) ->
    % timestamps should only arrive in strict ISO-8601 format
    TS = {{
        erlang:list_to_integer([Y1, Y2, Y3, Y4]),
        erlang:list_to_integer([M1, M2]),
        erlang:list_to_integer([D1, D2]) }, {
        erlang:list_to_integer([H1, H2]),
        erlang:list_to_integer([N1, N2]),
        erlang:list_to_integer([S1, S2]) }},
    {ok, TS, skip_space(Rest)};
parse_value([$\" | Rest]) ->
    parse_string(Rest, []);
parse_value([$[ | Rest]) ->
    parse_array(skip_space(Rest), []);
parse_value([${ | Rest]) ->
    parse_struct(skip_space(Rest), []);
parse_value([N | _] = Json) when N >= $0 andalso N =< $9 ->
    {Value, Rest} = string:to_integer(Json),
    {ok, Value, skip_space(Rest)};
parse_value(Arg) ->
    erlang:error(badarg, [Arg]).

-spec skip_space([char()]) -> [char()].
skip_space([Ch | Rest]) when Ch =< $\s ->
    skip_space(Rest);
skip_space(Str) ->
    Str.

