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
%% @doc BRT provider for the 'brt-repo' command.
%%
-module(brt_prv_repo).

%% provider behavior
-ifndef(brt_validate).
-behaviour(brt).
-endif.
-export([do/1, format_error/1, init/1, spec/0]).

-include("brt.hrl").

%
% Thoughts on what you should be able to do:
%
% - If you want to restrict an operation to the current project, or any other
%   single repo, you can just perform the operation from the command line.
%
% - All repos in the checkouts directory IS NOT limited to true dependencies,
%   as that would require compilation.
%
% - No potentially destructive commands at this point, as automating mass
%   commits and pushes have some scary aspects to them.
%

-define(PROVIDER_ATOM,  'brt-repo').
-define(PROVIDER_STR,   "brt-repo").
-define(PROVIDER_DEPS,  ['lock']).
-define(PROVIDER_OPTS,  [
%%    {'ahead', $a, "ahead", 'boolean',
%%        "Check whether any repositories contain unpushed commits on the "
%%        "current branch."},
    {'dirty', $d, "dirty", 'boolean',
        "Check whether any repositories contain uncommitted changes or "
        "untracked files."},
    {'pull', $p, "pull", 'boolean',
        "Pull all target repositories."},
    {'version', $v, "version", 'boolean',
        "Display the versions of all target repositories."},
    {'long', $l, "long", 'boolean',
        "Where available, display command output."},
    {'short', $s, "short", 'boolean',
        "List only repository names/paths, as appropriate [default]."},
    ?BRT_VERBOSITY_OPTS
]).

%% ===================================================================
%% Behavior
%% ===================================================================

-spec init(State :: brt:rebar_state()) -> {'ok', brt:rebar_state()}.
%%
%% @doc Adds the command provider to rebar's state.
%%
init(State) ->
    Provider = providers:create(spec()),
    {'ok', rebar_state:add_provider(State, Provider)}.

-spec do(State :: brt:rebar_state()) -> {'ok', brt:rebar_state()}.
%%
%% @doc Display provider information.
%%
do(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    handle_command(Opts, State).

-spec format_error(Error :: term()) -> iolist().
%%
%% @doc Placeholder to fill out the 'provider' API, should never be called.
%%
format_error(Error) ->
    brt:format_error(Error).

-spec spec() -> [{atom(), term()}].
%%
%% @doc Return the proplist that will be supplied to providers:create/1.
%%
spec() ->
    [
        {'name',        ?PROVIDER_ATOM},
        {'module',      ?MODULE},
        {'bare',        'true'},
        {'deps',        ?PROVIDER_DEPS},
        {'example',     "rebar3 " ?PROVIDER_STR},
        {'short_desc',  short_desc()},
        {'desc',        long_desc()},
        {'opts',        ?PROVIDER_OPTS}
    ].

%%====================================================================
%% Help Text
%%====================================================================

-spec short_desc() -> string().
short_desc() ->
    "Perform operations on project source repositories.".

-spec long_desc() -> string().
long_desc() ->
    short_desc() ++ "\n"
    "\n"
    "Operations are applied to the current project repository and all "
    "configured dependency repositories in its checkouts directory.\n"
    "\n"
    "Note that repositories in the checkouts directory are processed based "
    "on Rebar configuration, not true dependencies, to avoid the need for "
    "successful compilation beforehand.\n"
    "\n"
    "!!! THIS BEHAVIOR IS LIKELY TO CHANGE !!!\n"
    "This provider is in active development - options and commands may not "
    "be implemented and are expected to change based on user feedback.\n".

%%====================================================================
%% Internal
%%====================================================================

-type context() ::  {boolean(), boolean()}.

-spec handle_command(
    Opts :: [proplists:property()], State :: brt:rebar_state())
        -> {'ok', brt:rebar_state()} | brt:prv_error().
handle_command(Opts, State) ->
    InitCtx = {proplists:get_value('long', Opts, 'false'), 'true'},
    Select  = fun brt_rebar:in_prj_or_checkouts/2,
    case fold_func(Opts, InitCtx) of
        {'error', _} = Error ->
            Error;
        {Func, Ctx} ->
            case brt_rebar:fold(Select, Func, Ctx, State) of
                {'ok', {_, 'true'}} ->
                    {'ok', State};
                {'ok', _} ->
                    {'error', {?MODULE, 'repo_fail'}};
                Error ->
                    Error
            end
    end.

-spec fold_func(Opts :: [proplists:property()], Context :: context())
        -> {brt_rebar:fold_func(), context()} | brt:prv_error().
fold_func(Opts, Context) ->
    fold_func(['version', 'dirty', 'ahead', 'pull'], Opts, Context).

-spec fold_func(
    Ops :: [atom()], Opts :: [proplists:property()], Context :: context())
        -> {brt_rebar:fold_func(), context()} | brt:prv_error().

fold_func(['ahead' = Op | Ops], Opts, Context) ->
    case proplists:get_value(Op, Opts) of
        'true' ->
            {fun ahead/3, Context};
        _ ->
            fold_func(Ops, Opts, Context)
    end;
fold_func(['dirty' = Op | Ops], Opts, Context) ->
    case proplists:get_value(Op, Opts) of
        'true' ->
            {fun dirty/3, Context};
        _ ->
            fold_func(Ops, Opts, Context)
    end;
fold_func(['pull' = Op | Ops], Opts, Context) ->
    case proplists:get_value(Op, Opts) of
        'true' ->
            {fun pull/3, Context};
        _ ->
            fold_func(Ops, Opts, Context)
    end;
fold_func(['version' = Op | Ops], Opts, Context) ->
    case proplists:get_value(Op, Opts) of
        'true' ->
            {fun version/3, Context};
        _ ->
            fold_func(Ops, Opts, Context)
    end;
fold_func([], _Opts, _Context) ->
    {'error', {?MODULE, 'no_operation'}}.

-spec format_error(App :: brt:app_spec(), Error :: term()) -> brt:prv_error().
%
% For use by the fold functions to generate appropriate results, primarily
% from the detailed error results returned from git commands.
%
format_error({Name, _, _}, {'error', {_, _, _, ErrLines}}) ->
    {'error', string:join(
        [lists:flatten(["Repository error: ", brt:to_string(Name)]) | ErrLines],
        "\n")};
format_error(_, Error) ->
    format_error(Error).

-spec ahead(
    App :: brt:app_spec(), Ctx :: context(), State :: brt:rebar_state())
        -> {'ok', context()} | brt:prv_error().
ahead(App, {_Long, _Result} = Ctx, _State) ->
    rebar_api:debug(
        "~s:~s/3: Ctx: ~p~nApp: ~p", [?MODULE, 'ahead', Ctx, App]),
    {'error', {?MODULE, 'not_implemented'}}.

-spec dirty(
    App :: brt:app_spec(), Ctx :: context(), State :: brt:rebar_state())
        -> {'ok', context()} | brt:prv_error().
dirty({Name, Repo, _} = App, {Long, _} = Ctx, _State) ->
    rebar_api:debug(
        "~s:~s/3: Ctx: ~p~nApp: ~p", [?MODULE, 'dirty', Ctx, App]),
    case brt_repo:dirty(Repo) of
        'false' ->
            rebar_api:info("Repository clean: ~s", [Name]),
            {'ok', Ctx};
        {'true', OutLines} ->
            case Long of
                'true' ->
                    rebar_api:warn("Repository dirty: ~s~n~s",
                        [Name, string:join(OutLines, "\n")]),
                    {'ok', {Long, 'false'}};
                _ ->
                    rebar_api:warn("Repository dirty: ~s", [Name]),
                    {'ok', {Long, 'false'}}
            end;
        Error ->
            format_error(App, Error)
    end.

-spec pull(
    App :: brt:app_spec(), Ctx :: context(), State :: brt:rebar_state())
        -> {'ok', context()} | brt:prv_error().
pull({Name, Repo, _} = App, {Long, _Result} = Ctx, _State) ->
    rebar_api:debug(
        "~s:~s/3: Ctx: ~p~nApp: ~p", [?MODULE, 'pull', Ctx, App]),
    case brt_repo:pull(Repo) of
        {'ok', OutLines} ->
            case Long of
                'true' ->
                    rebar_api:info("Repository pulled: ~s~n~s",
                        [Name, string:join(OutLines, "\n")]),
                    {'ok', Ctx};
                _ ->
                    rebar_api:info("Repository pulled: ~s", [Name]),
                    {'ok', Ctx}
            end;
        Error ->
            format_error(App, Error)
    end.

-spec version(
    App :: brt:app_spec(), Ctx :: context(), State :: brt:rebar_state())
        -> {'ok', context()} | brt:prv_error().
version({Name, Repo, _} = App, {Long, _} = Ctx, _State) ->
    rebar_api:debug(
        "~s:~s/3: Ctx: ~p~nApp: ~p", [?MODULE, 'version', Ctx, App]),
    case brt_repo:version(Repo) of
        {'error', 'noversion'} ->
            rebar_api:warn("Repository ~s: No commits", [Name]),
            {'ok', {Long, 'false'}};
        {'error', _} = Error ->
            format_error(App, Error);
        {Type, Vsn} ->
            rebar_api:info("Repository ~s: {~s, \"~s\"}", [Name, Type, Vsn]),
            {'ok', Ctx}
    end.
