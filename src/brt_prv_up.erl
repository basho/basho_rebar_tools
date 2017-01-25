%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016-2017 Basho Technologies, Inc.
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
%% @doc BRT provider for the `brt-up' command.
%%
-module(brt_prv_up).

%% provider behavior
-ifndef(BRT_VALIDATE).
-behaviour(brt).
-endif.
-export([do/1, format_error/1, spec/0]).

-include("brt.hrl").

-define(PROVIDER_ATOM,  'brt-up').
-define(PROVIDER_STR,   "brt-up").
-define(PROVIDER_DEPS,  [lock]).
-define(PROVIDER_OPTS,  [
    {sync, $s, "sync", boolean,
        "Synchronize mirrored branches [default]."},
    {push, $p, "push", boolean,
        "Push mirrored branches after --sync."},
    {long, $l, "long", boolean,
        "Where available, display command output."},
    ?BRT_RECURSIVE_OPT,
    ?BRT_CHECKOUTS_OPT,
    ?BRT_VERBOSITY_OPTS
]).

%% ===================================================================
%% Behavior
%% ===================================================================

-spec do(State :: brt:rebar_state()) -> {ok, brt:rebar_state()}.
%%
%% @doc Execute the provider command logic.
%%
do(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    ?LOG_DEBUG("~s:do/1: Opts = ~p", [?MODULE, Opts]),
    handle_command(Opts, State).

-spec format_error(Error :: term()) -> iolist().
%%
%% @doc Format errors for display.
%%
format_error(Error) ->
    brt:format_error(Error).

-spec spec() -> [{atom(), term()}].
%%
%% @doc Return the proplist that will be supplied to providers:create/1.
%%
spec() ->
    [
        {name,          ?PROVIDER_ATOM},
        {module,        ?MODULE},
        {bare,          true},
        {deps,          ?PROVIDER_DEPS},
        {example,       "rebar3 " ?PROVIDER_STR},
        {short_desc,    short_desc()},
        {desc,          long_desc()},
        {opts,          ?PROVIDER_OPTS}
    ].

%%====================================================================
%% Help Text
%%====================================================================

-spec short_desc() -> string().
short_desc() ->
    "Manage upstream repositories.".

-spec long_desc() -> string().
long_desc() ->
    short_desc() ++ "\n"
    "\n"
    "Synchronized branches are identified by 'mirror' action clauses in the "
    "'brt_upstream' section of rebar.config.\n".

%%====================================================================
%% Internal
%%====================================================================

-record(ctx, {
    push        :: boolean(),
    long        :: boolean(),
    status = ok :: term()
}).
-type context() ::  #ctx{}.

-record(mirror, {
    alias   :: atom(),
    type    :: atom(),
    url     :: string(),
    mirrors :: [atom()]
}).
-type mirror()  :: #mirror{}.

-spec handle_command(
    Opts :: [proplists:property()], State :: brt:rebar_state())
        -> {ok, brt:rebar_state()} | brt:prv_error().

handle_command(Opts, State) ->
    case proplists:get_value(sync, Opts, true) of
        true ->
            Select = case proplists:get_value(recurse, Opts) of
                true ->
                    case proplists:get_value(checkouts, Opts) of
                        true ->
                            fun brt_rebar:in_prj_or_checkouts/2;
                        _ ->
                            all
                    end;
                _ ->
                    brt_rebar:prj_app_specs(State)
            end,
            Context = #ctx{
                push    = proplists:get_value(push, Opts, false),
                long    = proplists:get_value(long, Opts, false)
            },
            case brt_rebar:fold(Select, fun update/3, Context, State) of
                {ok, #ctx{status = ok}} ->
                    {ok, State};
                {ok, #ctx{status = warn}} ->
                    {error, {?MODULE, config_warning}};
                {ok, _} ->
                    {error, {?MODULE, {internal, ?MODULE, ?LINE}}};
                Err ->
                    Err
            end;
        false ->
            ?LOG_WARN("Nothing to do.", []),
            {ok, State}
    end.

-spec update(
    App     :: brt:app_spec(),
    Context :: context(),
    State   :: brt:rebar_state())
        -> {ok, context()} | brt:prv_error().

update({Name, Path, _} = App, Context, State) ->
    ?LOG_DEBUG("~s:update/3: App = ~p", [?MODULE, App]),
    AppInfo = brt_rebar:app_info(Name, State),
    case dict:find(brt_upstream, rebar_app_info:opts(AppInfo)) of
        {ok, Upstream} ->
            {Mirrors, Status} = map_mirrors(Upstream, Name),
            case process_mirrors(Mirrors, Context, Status, Path, Name) of
                ok ->
                    {ok, Context};
                warn ->
                    {ok, Context#ctx{status = warn}};
                Error ->
                    Error
            end;
        _ ->
            ?LOG_INFO("~s: No upstream repositories", [Name]),
            {ok, Context}
    end.

-spec map_mirrors(Section :: [tuple()], AppName :: brt:app_name())
        -> {[mirror()], ok | warn}.

map_mirrors(Section, AppName) ->
    map_mirrors(Section, [], ok, Section, AppName).

-spec map_mirrors(
    Remain  :: [tuple()],
    Map     :: [mirror()],
    Status  :: ok | warn,
    Section :: [tuple()],
    App     :: brt:app_name())
        -> {[mirror()], ok | warn}.

map_mirrors([{mirror, Upstream, Branch} | Remain], Map, Status, Section, App) ->
    case lists:keyfind(Upstream, #mirror.alias, Map) of
        #mirror{mirrors = Branches} = M ->
            case lists:member(Branch, Branches) of
                true ->
                    map_mirrors(Remain, Map, Status, Section, App);
                _ ->
                    MRecs = lists:keystore(
                        Upstream, #mirror.alias, Map,
                        M#mirror{mirrors = [Branch | Branches]}),
                    map_mirrors(Remain, MRecs, Status, Section, App)
            end;
        false ->
            case lists:keyfind(Upstream, 1, Section) of
                {_, {Type, URL}} ->
                    MRec = #mirror{
                        alias   = Upstream,
                        type    = Type,
                        url     = URL,
                        mirrors = [Branch] },
                    map_mirrors(Remain, [MRec | Map], Status, Section, App);
                false ->
                    ?LOG_WARN(
                        "~s: skipping unmatched alias for mirror '~s-~s'.",
                        [App, Upstream, Branch]),
                    map_mirrors(Remain, Map, warn, Section, App)
            end
    end;

map_mirrors([_ | Remain], Map, Status, Section, App) ->
    map_mirrors(Remain, Map, Status, Section, App);

map_mirrors([], Map, Status, _, _) ->
    {Map, Status}.

-spec process_mirrors(
    Remain  :: [mirror()],
    Context :: context(),
    Status  :: ok | warn,
    Repo    :: brt:fs_path(),
    App     :: brt:app_name())
        -> ok | warn | brt:prv_error().

process_mirrors([#mirror{mirrors = []} | Remain], Context, Status, Repo, App) ->
    process_mirrors(Remain, Context, Status, Repo, App);

process_mirrors([MRec | Remain], Context, Status, Repo, App) ->
    #mirror{
        alias   = Alias,
        type    = Type,
        url     = URL,
        mirrors = [UpBranch | Mirrors]
    } = MRec,
    #ctx{
        push    = Push,
        long    = Long
    } = Context,
    SyncBranch = lists:flatten(io_lib:format("~s-~s", [Alias, UpBranch])),
    case brt_repo:sync_upstream(Repo, Type, URL, Push, UpBranch, SyncBranch) of
        {ok, OutLines} ->
            case OutLines of
                [_|_] when Long == true ->
                    ?LOG_INFO("Synced ~s:~s\n\t~s",
                        [App, SyncBranch, string:join(OutLines, "\n\t")]);
                _ ->
                    ?LOG_INFO("Synced ~s:~s", [App, SyncBranch])
            end,
            Next = MRec#mirror{mirrors = Mirrors},
            process_mirrors([Next | Remain], Context, Status, Repo, App);
        {error, What} ->
            {error, format_error(What)}
    end;

process_mirrors([], _, Status, _, _) ->
    Status.
