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

-module(brt_config).

% API
-export([
    config/0,
    config_file/0,
    dep_erl_opts/2,
    dep_plugins/2,
    init/0,
    init/1,
    merge/1,
    overwrite_blocked/1,
    pkg_dep/1,
    pkg_dep/2,
    pkg_repo/2,
    pkg_version/2,
    reset/1
]).

-export_type([
    state/0
]).

-include("brt.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(saved, {
    file    :: brt:fs_path(),
    conf    :: config()
}).

-type state()   ::  #saved{}.
-type config()  ::  [{atom(), term()}].

-define(DEFAULT_CONFIG_FILE,    "brt.config").
-define(OS_ENV_CONFIG_FILE,     "BRT_CONFIG").

-define(CONFIG_TERMS_KEY,       {?MODULE, terms}).
-define(CONFIG_TERMS(),         erlang:get(?CONFIG_TERMS_KEY)).
-define(CONFIG_TERMS(New),      erlang:put(?CONFIG_TERMS_KEY, New)).

-define(CONFIG_FILE_KEY,        {?MODULE, file}).
-define(CONFIG_FILE(),          erlang:get(?CONFIG_FILE_KEY)).
-define(CONFIG_FILE(New),       erlang:put(?CONFIG_FILE_KEY, New)).

-define(BRT_CFG_NAMES_KEY,      {?MODULE, brt_config_names}).
-define(BRT_CFG_NAMES(),        erlang:get(?BRT_CFG_NAMES_KEY)).
-define(BRT_CFG_NAMES(New),     erlang:put(?BRT_CFG_NAMES_KEY, New)).

%
% Implementation Notes:
%
%   It's tempting to use `proplists' operations on the configuration, but it
%   silently returns the default if there's a matching tuple with other than
%   2 elements, so a munged up element might not be flagged as such.
%
%   Instead, brt:get_key_list/2, brt:get_key_tuple/2, and brt:get_key_value/2
%   are used, ensuring that we'll fail on bogus data.
%

%% ===================================================================
%% API
%% ===================================================================

-spec config() -> [{atom(), term()}].
%%
%% @doc Returns the current runtime configuration.
%%
config() ->
    ?CONFIG_TERMS().

-spec config_file() -> brt:fs_path() | undefined.
%%
%% @doc Returns the file from which the initial configuration was loaded.
%%
config_file() ->
    ?CONFIG_FILE().

-spec dep_erl_opts(
        Deps    :: [brt:app_name() | brt:dep_spec()],
        Opts    :: [atom() | tuple()])
        -> [atom() | tuple()].
%%
%% @doc Returns Opts with Erlang compiler options needed by Deps included.
%%
dep_erl_opts(Deps, Opts) ->
    dep_erl_opts(Deps, Opts,
        brt:get_key_list(dep_erl_opts, ?CONFIG_TERMS())).

-spec dep_plugins(
        Deps    :: [brt:app_name() | brt:dep_spec()],
        Plugins :: [brt:dep_spec()])
        -> [brt:dep_spec()].
%%
%% @doc Returns Plugins with rebar plugins needed by Deps included.
%%
%% This plugin is always included in the result.
%%
dep_plugins(Deps, Plugins) ->
    Config  = ?CONFIG_TERMS(),
    Conf    = brt:get_key_list(dep_plugins, Config),
    Raw     = brt:get_key_list(raw, Config),
    case brt:dep_list_member(?APP_NAME_ATOM, Plugins) of
        true ->
            dep_plugins(Deps, Plugins, Conf, Raw);
        _ ->
            dep_plugins(Deps, [?APP_NAME_ATOM | Plugins], Conf, Raw)
    end.

-spec init() -> ok | brt:err_result().
%%
%% @doc Initializes the runtime configuration.
%%
init() ->
    {ok, CWD} = file:get_cwd(),
    init([CWD]).

-spec init(Dirs :: [file:name()]) -> ok | brt:err_result().
%%
%% @doc Initializes the runtime configuration.
%%
init(Dirs) ->
    DefaultFile = ?DEFAULT_CONFIG_FILE,
    FileNames = case os:getenv(?OS_ENV_CONFIG_FILE) of
        false ->
            [DefaultFile];
        Val ->
            [Val, DefaultFile]
    end,
    ?BRT_CFG_NAMES(FileNames),
    case init(Dirs, brt_defaults:file_terms(DefaultFile)) of
        ok ->
            brt_rebar:init();
        Err ->
            Err
    end.

-spec merge(AppDir :: brt:fs_path())
        -> {ok, state()} | brt:err_result().
%%
%% @doc Merges the BRT config file from Dir, if any, and returns a reset object.
%%
%% The state before the call can be restored by calling reset/1.
%%
merge(Dir) ->
    Config  = config(),
    State   = #saved{file = config_file(), conf = Config},
    Exclude = brt:get_key_list(no_inherit, Config),
    Inherit = lists:filter(
        fun({Key, _}) -> not lists:member(Key, Exclude) end, Config),
    case config_file(Dir) of
        false ->
            ?CONFIG_TERMS(Inherit),
            {ok, State};
        File ->
            case file:consult(File) of
                {ok, Local} ->
                    ?CONFIG_TERMS(merge(Inherit, Local)),
                    ?CONFIG_FILE(File),
                    {ok, State};
                {error, What} ->
                    brt:file_error(File, What)
            end
    end.

-spec overwrite_blocked(File :: brt:fs_path()) -> boolean().
%%
%% @doc Reports whether automated overwrite of the specified file is blocked.
%%
overwrite_blocked(File) ->
    % At present, the `brt.protect' flag is recognized, but that's deprecated.
    % `brt_protect == true` is the preferred pattern, allowing it to be a
    % simple attribute at the top of the file, while the meatier BRT stuff is
    % stashed at the end.
    case file:consult(File) of
        {ok, Terms} ->
            proplists:get_value(brt_protect, Terms, false) orelse
            proplists:get_value(protect, brt:get_key_list(brt, Terms), false);
        _ ->
            false
    end.

-spec pkg_dep(Package :: brt:app_name() | brt:dep_spec()) -> brt:dep_spec().
%%
%% @doc Returns a package's dependency specification.
%%
pkg_dep(PkgSpec) when ?is_rebar_dep(PkgSpec) ->
    PkgSpec;
pkg_dep(PkgName) ->
    get_pkg_dep(?CONFIG_TERMS(), brt:to_atom(PkgName)).

-spec pkg_dep(
        Package :: brt:app_name(), Spec :: brt:rsrc_spec() | brt:dep_spec())
        -> brt:dep_spec().
%%
%% @doc Stores a package's dependency specification override.
%%
pkg_dep(Package, Spec)
        when erlang:is_tuple(Spec) andalso erlang:tuple_size(Spec) > 1 ->
    PkgName = brt:to_atom(Package),
    DepSpec = case erlang:element(1, Spec) of
        PkgName ->
            Spec;
        _ ->
            {PkgName, Spec}
    end,
    OldConf = ?CONFIG_TERMS(),
    OldDeps = brt:get_key_list(deps, OldConf),
    NewDeps = lists:keystore(PkgName, 1, OldDeps, DepSpec),
    NewConf = lists:keystore(deps, 1, OldConf, {deps, NewDeps}),
    ?CONFIG_TERMS(NewConf),
    DepSpec;
pkg_dep(Package, Spec) ->
    erlang:error(badarg, [Package, Spec]).

-spec pkg_repo(
    Package :: brt:app_name(), Repo :: brt:app_name() | brt:dep_loc())
        -> {brt:app_name(), brt:app_name() | brt:dep_loc()}.
%%
%% @doc Stores a package's repository override.
%%
pkg_repo(Package, Repo) ->
    PkgName = brt:to_atom(Package),
    PkgRepo = case Repo of
        Atom when erlang:is_atom(Atom) ->
            {PkgName, Atom};
        List when erlang:is_list(List) ->
            URL = lists:flatten(List),
            case io_lib:printable_latin1_list(URL) of
                true ->
                    {PkgName, URL};
                _ ->
                    erlang:error(badarg, [Package, Repo])
            end;
        _ ->
            erlang:error(badarg, [Package, Repo])
    end,
    OldConf = ?CONFIG_TERMS(),
    OldReps = brt:get_key_list(repos, OldConf),
    NewReps = lists:keystore(PkgName, 1, OldReps, PkgRepo),
    NewConf = lists:keystore(repos, 1, OldConf, {repos, NewReps}),
    ?CONFIG_TERMS(NewConf),
    PkgRepo.

-spec pkg_version(Package :: brt:app_name(), Version :: brt:dep_vsn())
        -> {brt:app_name(), {atom(), string()}}.
%%
%% @doc Stores a package's version override.
%%
pkg_version(Package, {Type, Label} = Version)
        when erlang:is_atom(Type) andalso erlang:is_list(Label) ->
    PkgName = brt:to_atom(Package),
    FlatVsn = lists:flatten(Label),
    PkgVers = case io_lib:char_list(FlatVsn) of
        true ->
            {PkgName, {Type, FlatVsn}};
        _ ->
            erlang:error(badarg, [Package, Version])
    end,
    OldConf = ?CONFIG_TERMS(),
    OldVers = brt:get_key_list(versions, OldConf),
    NewVers = lists:keystore(PkgName, 1, OldVers, PkgVers),
    NewConf = lists:keystore(versions, 1, OldConf, {versions, NewVers}),
    ?CONFIG_TERMS(NewConf),
    PkgVers;
pkg_version(Package, Version) when erlang:is_list(Version) ->
    pkg_version(Package, {ref, Version});
pkg_version(Package, Version) ->
    erlang:error(badarg, [Package, Version]).

-spec reset(State :: state()) -> ok.
%%
%% @doc Restores the BRT state returned by a previous merge/1 call.
%%
reset(#saved{file = File, conf = Conf}) ->
    ?CONFIG_FILE(File),
    ?CONFIG_TERMS(Conf),
    ok.

%% ===================================================================
%% Internal
%% ===================================================================

-spec config_file(Dir :: brt:fs_path()) -> brt:fs_path() | false.
config_file(Dir) ->
    config_file(?BRT_CFG_NAMES(), Dir).

-spec config_file(Names :: [brt:fs_path()], Dir :: brt:fs_path())
        -> brt:fs_path() | false.
config_file([Name | Names], Dir) ->
    File = filename:absname(Name, Dir),
    case filelib:is_regular(File) of
        true ->
            File;
        _ ->
            config_file(Names, Dir)
    end;
config_file([], _) ->
    false.

-spec dep_erl_opts(
        Deps    :: [brt:app_name() | brt:dep_spec()],
        Opts    :: [atom() | tuple()],
        Conf    :: [{brt:app_name(), [atom() | tuple()]}])
        -> [atom() | tuple()].

dep_erl_opts([AppName | Deps], Opts, Conf) when ?is_app_name(AppName) ->
    case brt:get_key_list(AppName, Conf) of
        [] ->
            dep_erl_opts(Deps, Opts, Conf);
        Add ->
            dep_erl_opts(Deps, Add ++ Opts, Conf)
    end;

dep_erl_opts([DepSpec | Deps], Opts, Conf) when ?is_rebar_dep(DepSpec) ->
    dep_erl_opts([erlang:element(1, DepSpec) | Deps], Opts, Conf);

dep_erl_opts([Dep | _], _, _) ->
    erlang:error(badarg, [Dep]);

dep_erl_opts([], Opts, _) ->
    lists:usort(Opts).

-spec dep_plugins(
        Deps    :: [brt:app_name() | brt:dep_spec()],
        Plugins :: [brt:dep_spec()],
        Conf    :: [{brt:app_name(), [brt:app_name()]}],
        Raw     :: [brt:app_name()])
        -> [brt:dep_spec()].

dep_plugins([AppName | Deps], Plugins, Conf, Raw) when ?is_app_name(AppName) ->
    AppConf = brt:get_key_list(AppName, Conf),
    RawConf = case lists:member(AppName, Raw)
            andalso not lists:member(rebar_raw_resource, AppConf) of
        true ->
            [rebar_raw_resource | AppConf];
        _ ->
            AppConf
    end,
    Next = lists:foldl(
        fun(Dep, Result) ->
            case brt:dep_list_member(Dep, Result) of
                true ->
                    Result;
                _ ->
                    [Dep | Result]
            end
        end, Plugins, RawConf),
    dep_plugins(Deps, Next, Conf, Raw);

dep_plugins([DepSpec | Deps], Plugins, Conf, Raw) when ?is_rebar_dep(DepSpec)
        andalso erlang:is_tuple(erlang:element(2, DepSpec))
        andalso erlang:tuple_size(erlang:element(2, DepSpec)) > 1
        andalso erlang:element(1, erlang:element(2, DepSpec)) =:= raw ->
    NextDeps = [erlang:element(1, DepSpec) | Deps],
    case brt:dep_list_member(rebar_raw_resource, Plugins) of
        true ->
            dep_plugins(NextDeps, Plugins, Conf, Raw);
        _ ->
            dep_plugins(NextDeps, [rebar_raw_resource | Plugins], Conf, Raw)
    end;

dep_plugins([DepSpec | Deps], Plugins, Conf, Raw) when ?is_rebar_dep(DepSpec) ->
    dep_plugins([erlang:element(1, DepSpec) | Deps], Plugins, Conf, Raw);

dep_plugins([Dep | _], _, _, _) ->
    erlang:error(badarg, [Dep]);

dep_plugins([], Plugins, _, _) ->
    lists:sort(brt:dep_list(Plugins)).

-spec get_pkg_dep(Config :: config(), Package :: brt:app_name())
        -> brt:dep_spec().
get_pkg_dep(Config, Package) ->
    case brt:get_key_tuple(Package, brt:get_key_list(deps, Config)) of
        undefined ->
            Spec = {
                brt:get_key_value(default_repo_type, Config),
                get_pkg_repo(Config, Package),
                get_pkg_version(Config, Package)},
            case lists:member(Package, brt:get_key_list(raw, Config)) of
                true ->
                    {Package, {raw, Spec}};
                _ ->
                    {Package, Spec}
            end;
        Dep ->
            Dep
    end.

-spec get_pkg_repo(Config :: config(), Package :: brt:app_name())
        -> brt:dep_loc().
get_pkg_repo(Config, Package) ->
    case brt:get_key_tuple(Package, brt:get_key_list(repos, Config)) of
        undefined ->
            lists:flatten(io_lib:format(
                brt:get_key_value(default_repo_format, Config), [Package]));
        {_, Target} when erlang:is_atom(Target) ->
            get_pkg_repo(Config, Target);
        {_, Repo} ->
            Repo
    end.

-spec get_pkg_version(Config :: config(), Package :: brt:app_name())
        -> brt:dep_vsn().
get_pkg_version(Config, Package) ->
    case brt:get_key_tuple(Package, brt:get_key_list(versions, Config)) of
        undefined ->
            brt:get_key_value(default_repo_version, Config);
        {_, Vsn} ->
            Vsn
    end.

-spec init(Dirs :: [brt:fs_path()], Defaults :: list())
        -> ok | brt:err_result().

init([Dir | Dirs], Defaults) ->
    case config_file(Dir) of
        false ->
            init(Dirs, Defaults);
        File ->
            case file:consult(File) of
                {ok, Config} ->
                    ?CONFIG_FILE(File),
                    ?CONFIG_TERMS(merge(Defaults, Config)),
                    ok;
                {error, What} ->
                    brt:file_error(File, What)
            end
    end;

init([], Defaults) ->
    ?CONFIG_TERMS(Defaults),
    ok.

-spec merge(Defaults :: config(), Config :: config()) -> config().

merge(Defaults, Config) ->
    Nested  = brt:get_key_list(nested_merge_sects, Defaults),
    merge_terms(Config, Defaults, Nested).

-spec merge_terms(Terms :: config(), Result :: list(), Nested :: [term()])
        -> list().

merge_terms([{Key, AddElems} = Term | Terms], Result, Nested)
        when erlang:is_list(AddElems) ->
    NextResult = case lists:member(Key, Nested) of
        true ->
            case lists:keytake(Key, 1, Result) of
                {value, {_, PrevElems}, NewResult} ->
                    [{Key, merge_list(AddElems, PrevElems)} | NewResult];
                false ->
                    [Term | Result]
            end;
        _ ->
            lists:keystore(Key, 1, Result, Term)
    end,
    merge_terms(Terms, NextResult, Nested);

merge_terms([Term | Terms], Result, Nested)
        when erlang:is_tuple(Term) andalso erlang:tuple_size(Term) >= 1 ->
    Key = erlang:element(1, Term),
    merge_terms(Terms, lists:keystore(Key, 1, Result, Term), Nested);

merge_terms([_|_] = Terms, _, _) ->
    erlang:error(badarg, [Terms]);

merge_terms([], Result, _) ->
    Result.

-spec merge_list(Terms :: list(), Result :: list()) -> list().

merge_list([Term | Terms], Result)
        when erlang:is_tuple(Term) andalso erlang:tuple_size(Term) > 1 ->
    Key = erlang:element(1, Term),
    merge_list(Terms, lists:keystore(Key, 1, Result, Term));

merge_list([Term | Terms], Result) ->
    case lists:member(Term, Result) of
        true ->
            merge_list(Terms, Result);
        _ ->
            merge_list(Terms, [Term | Result])
    end;

merge_list([], Result) ->
    Result.

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

default_config_test() ->

    % this file should always be found
    Terms = brt_defaults:file_terms(?DEFAULT_CONFIG_FILE),
    ?assertMatch([_|_], Terms),

    % default should always be a branch
    ?assertMatch({branch, [_|_]},
        proplists:get_value(default_repo_version, Terms)),

    % default should be empty
    ?assertEqual([], proplists:get_value(deps, Terms, [])),

    ok.

-endif.
