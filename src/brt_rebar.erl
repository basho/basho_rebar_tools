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

-module(brt_rebar).

% API
-export([
    app_deps/1,
    app_info/2,
    apps_deps_dirs/1,
    cfg_file_names/0,
    cfg_file_names/1,
    config_file/2,
    config_format/0,
    config_format/1,
    copyright_info/3,
    dep_app_specs/1,
    dep_apps/1,
    fold/4,
    format_dep/2,
    in_checkouts/2,
    in_prj_or_checkouts/2,
    in_project/2,
    parse_dep/1,
    prj_app_specs/1,
    prj_apps/1
]).

-export_type([
    fold_ctx/0,
    fold_func/0,
    fold_pred/0
]).

% Private
-export([init/0]).

-include("brt.hrl").

-type fold_ctx()    ::  term().
-type fold_func()   ::  fun((brt:app_spec(), fold_ctx(), brt:rebar_state())
                        -> {'ok', fold_ctx()} | brt:err_result()).
-type fold_pred()   ::  fun((brt:app_spec(), brt:rebar_state()) -> boolean()).

-define(REBAR_CFG_NAMES_KEY,    {?MODULE, 'rebar_config_names'}).
-define(REBAR_CFG_NAMES(),      erlang:get(?REBAR_CFG_NAMES_KEY)).
-define(REBAR_CFG_NAMES(New),   erlang:put(?REBAR_CFG_NAMES_KEY, New)).

-define(REBAR_CFG_FORMAT_KEY,   {?MODULE, 'rebar_config_format'}).
-define(REBAR_CFG_FORMAT(),     erlang:get(?REBAR_CFG_FORMAT_KEY)).
-define(REBAR_CFG_FORMAT(New),  erlang:put(?REBAR_CFG_FORMAT_KEY, New)).

%% ===================================================================
%% API
%% ===================================================================

-spec app_deps(State :: brt:rebar_state())
        -> {'ok', [brt:app_name()]} | brt:err_result().
%%
%% @doc Returns the applications called by the current project's application(s).
%%
%% The current project's applications are filtered out of the result list.
%%
%% A new xref server is created, populated, and discarded by this operation -
%% if multiple analyses are to be performed use the discrete functions in the
%% 'brt_xref' module.
%%
app_deps(State) ->
    case brt_xref:new(State) of
        {'ok', XRef} ->
            Apps = prj_apps(State),
            Result = brt_xref:app_deps(XRef, Apps),
            brt_xref:stop(XRef),
            case Result of
                {'ok', Deps} ->
                    {'ok', Deps -- Apps};
                _ ->
                    Result
            end;
        Error ->
            Error
    end.

-spec app_info(
        App :: brt:app_name() | binary(),
        Ref :: brt:rebar_state() | [brt:rebar_app()])
        -> brt:rebar_app() | 'false'.
%%
%% @doc Finds the rebar_app_info:t() for the specified application.
%%
%% 'false' is returned if the application is not found in the provided
%% reference element(s).
%%
app_info(Name, [AppInfo | AppInfos]) when erlang:is_binary(Name) ->
    case rebar_app_info:name(AppInfo) of
        Name ->
            AppInfo;
        _ ->
            app_info(Name, AppInfos)
    end;

app_info(_, []) ->
    'false';

app_info(Name, Ref) when not erlang:is_binary(Name) ->
    app_info(brt:to_binary(Name), Ref);

app_info(Name, State) when ?is_rebar_state(State) ->
    case app_info(Name, rebar_state:project_apps(State)) of
        'false' ->
            app_info(Name, rebar_state:all_deps(State));
        Ret ->
            Ret
    end.

-spec apps_deps_dirs(State :: brt:rebar_state())
        -> {'ok', [brt:app_spec()], [brt:fs_path()]} | brt:err_result().
%%
%% @doc Return the project applications and all dependency lib directories.
%%
%% Results are sketchy if rebar's 'compile' command has not yet been run in the
%% current rebar profile(s).
%%
%% On success, the result is {'ok', AppSpecs, DepsDirs}, where:
%%
%%  AppSpecs
%%      A list of {AppName, AppDir} tuples of all of the current project
%%      applications as determined by rebar.
%%      Normally, there will be one tuple for each *.app file, and if there is
%%      more than one they may or may not have duplicate directories.
%%      Because rebar3 requires at least one application in a project, the
%%      resulting list can never be empty - an error is returned if it would be.
%%
%%  DepsDirs
%%      All of the library directories where dependencies may be built, based
%%      on the effective rebar profiles and local working tree(s).
%%      The directories MAY NOT all exist!
%%      The checkouts directory is always at the head of the result list,
%%      and the 'default' profile directory is always the last.
%%
apps_deps_dirs(State) when ?is_rebar_state(State) ->
    case prj_app_specs(State) of
        [] ->
            % This really should never happen, but maintain sanity if it does.
            {'error', {'brt', 'app_undefined'}};
        Specs ->
            Profs   = order_profiles(rebar_state:current_profiles(State), []),
            % There are a LOT of nuances in how Rebar assembles directory paths
            % and comes up with where any given application might be built.
            % Best not to muck around in here without the rebar source in front
            % of you.
            % Some paths and/or segments may have embedded dots, so make sure
            % they're canonical for later comparisons.
            % Rebar's rebar_dir:make_normalized_path/1 should do the same thing
            % as brt:abspath/1, but the latter is used in non-rebar-specific
            % code so use it here, too ... just in case.
            BaseDir = brt:abspath(rebar_dir:base_dir(State)),
            BldDir  = brt:abspath(rebar_state:get(State, 'base_dir')),
            CoDir   = brt:abspath(rebar_dir:checkouts_dir(State)),
            LibDir  = rebar_state:get(State, 'deps_dir'),
            % Put together all of the library directories where dependencies
            % might be found - these directories MAY NOT all exist!
            % The checkouts directory is always at the head of the result list,
            % and the 'default' profile directory is always the last.
            Cands   = [brt:abspath(filename:join([BldDir, Prof, LibDir]))
                        || Prof <- Profs],
            Tail    = [Cand || Cand <- Cands, Cand /= BaseDir, Cand /= CoDir],
            Libs    = [CoDir, BaseDir | Tail],
            {'ok', Specs, Libs}
    end;
apps_deps_dirs(Term) ->
    erlang:error('badarg', [Term]).

-spec cfg_file_names() -> nonempty_list(nonempty_string()).
%%
%% @doc Returns the list of candidate rebar.config names.
%%
%% When writing, the first name in the list is used.
%% When reading or updating, the first name that exists is used.
%%
cfg_file_names() ->
    ?REBAR_CFG_NAMES().

-spec cfg_file_names(Names :: nonempty_list(nonempty_string()))
        -> nonempty_list(nonempty_string()).
%%
%% @doc Sets the candidate rebar.config names and returns the previous setting.
%%
%% The result is as for cfg_file_names/0.
%%
cfg_file_names([_|_] = Names) ->
    ?REBAR_CFG_NAMES(Names).

-spec config_file(
        App :: brt:app_name() | brt:app_spec() | binary(),
        Ref :: brt:rebar_state() | [brt:rebar_app()])
        -> brt:fs_path() | 'false'.
%%
%% @doc Returns the path of the rebar.config file for the specified application.
%%
%% 'false' is returned if the application is not found in the provided
%% reference element(s).
%%
%% If the application IS found (or a name/path tuple is specified for the
%% application), then a file path is returned regardless of whether the file
%% exists.
%%
%% If one of the files returned by cfg_file_names/0 exists, the first such
%% fully-qualified path is returned. If none of the files exists, the FQ path
%% of the first candidate name is returned.
%%
%% Note that this departs from Rebar3's strategy, in that this function always
%% searches candidate config file names, including names added as command
%% options and $REBAR_CONFIG if present, whereas Rebar3 only uses $REBAR_CONFIG
%% at the top of the project tree and knows nothing of command additions.
%%
%% We use this strategy specifically to allow writing rebar.config files with
%% alternate names for direct comparison with the original.
%%
config_file({Name, Path, _}, Ref) ->
    case config_file(Name, Ref) of
        'false' ->
            config_file(Path);
        File ->
            File
    end;

config_file(Name, Ref) ->
    case app_info(Name, Ref) of
        'false' = Ret ->
            Ret;
        AppInfo ->
            config_file(rebar_app_info:dir(AppInfo))
    end.

-spec copyright_info(
        File    :: brt:fs_path(),
        Loose   :: boolean(),
        Type    :: brt_io:comment_type())
        ->  'current' | brt:basho_year() | iolist() | brt:prv_error().
%%
%% @doc Encapsulates acquisition of copyright information for a rebar provider.
%%
copyright_info(File, Loose, Type) ->
    case brt_io:copyright_info(File, Type) of
        {'basho', Year} ->
            Year;
        {'error', 'enoent'} ->
            'current';
        'none' ->
            rebar_api:warn(
                "~s: file exists but does not contain any copyright "
                "statement(s), generating default using first commit date, "
                "verify before committing changes.",
                [File]),
            brt_repo:added_year(File, 'current');
        {'other', Header} ->
            case Loose of
                'true' ->
                    case Header of
                        'long' ->
                            rebar_api:warn(
                                "~s: unable to re-use ambiguous copyright, "
                                "generating default, verify before "
                                "committing changes.",
                                [File]),
                            'current';
                        _ ->
                            rebar_api:warn(
                                "~s: re-using ambiguous copyright, "
                                "verify before committing changes.",
                                [File]),
                            Header
                    end;
                _ ->
                    {'error', {'brt', {'copyright_dirty', File}}}
            end;
        {'error', What} when erlang:is_atom(What) ->
            brt:file_error(File, What);
        Error ->
            Error
    end.

-spec dep_apps(State :: brt:rebar_state()) -> [brt:app_name()].
%%
%% @doc Returns the names of the current project's applications.
%%
dep_apps(State) ->
    [brt:to_atom(rebar_app_info:name(AI))
        || AI <- rebar_state:all_deps(State)].

-spec dep_app_specs(State :: brt:rebar_state()) -> [brt:app_spec()].
%%
%% @doc Returns the name/path tuples of the current project's applications.
%%
dep_app_specs(State) ->
    [{brt:to_atom(rebar_app_info:name(AI)),
        rebar_app_info:dir(AI), rebar_app_info:out_dir(AI)}
            || AI <- rebar_state:all_deps(State)].

-spec config_format() -> 2 | 3.
%%
%% @doc Returns the dependency format version as an integer.
%%
config_format() ->
    ?REBAR_CFG_FORMAT().

-spec config_format(Version :: 2 | 3 | 'default' | 'legacy' | 'rebar2' | 'rebar3')
        -> 2 | 3.
%%
%% @doc Sets the dependency format version and returns the previous setting.
%%
%% The dependency format version is always stored as an integer; atom variants
%% are aliases for setting only.
%%
%% The result is as for config_format/0.
%%
config_format(Version) when Version =:= 2 orelse Version =:= 3 ->
    ?REBAR_CFG_FORMAT(Version);
config_format('default') ->
    config_format(3);
config_format('legacy') ->
    config_format(2);
config_format('rebar3') ->
    config_format(3);
config_format('rebar2') ->
    config_format(2).

-spec fold(
        Select  ::  [brt:app_spec()] | 'all' | fold_pred(),
        Func    ::  fold_func(),
        Context ::  fold_ctx(),
        State   ::  brt:rebar_state())
        -> {'ok', fold_ctx()} | brt:err_result().
%%
%% @doc Apply a function to each selected application.
%%
%% Applications are selected by either specifying a list of name/path tuples,
%% or a predicate function to apply to all applications (project and
%% dependencies) in the Rebar state, or 'all' to apply to all applications
%% without filtering.
%%
%% The specified Func is invoked for each selected application as
%%
%%  Func({AppName, AppSrcPath, AppOutPath}, Context, State)
%%
%% and returns either {'ok', NewContext} or {'error', Reason}. As long as the
%% 'ok' result is returned, iteration through the selected applications
%% continues, with each receiving the Context returned by the previous
%% invocation (or initially, the Context provided to `fold').
%%
%% The last Context is returned when the iteration is complete.
%%
%% The BRT configuration in each non-project (i.e. dependency) application
%% directory is merged into the effective configuration before the function is
%% invoked and reset after it, so applications inherit the top-level
%% configuration but not each others.
%%
%% The in_checkouts/2 and in_prj_or_checkouts/2 functions in this module are
%% valid predicate functions.
%%
%% This operation is NOT atomic - if an error occurs, changes MAY have been
%% applied to some applications before the error occurred.
%%
fold(Pred, Func, Context, State) when not ?is_rebar_state(State) ->
    erlang:error('badarg', [Pred, Func, Context, State]);
%
% Once it's confirmed that State is the right type, don't include it in raised
% errors, because it's huge.
%
fold(Pred, Func, Context, _) when not erlang:is_function(Func, 3) ->
    erlang:error('badarg', [Pred, Func, Context, '_']);

fold([], _, Context, _) ->
    {'ok', Context};

fold(Apps, Func, Context, State) when erlang:is_list(Apps) ->
    MapFun = fun(App) ->
        case in_project(App, State) of
            'true' ->
                {'prj', App};
            _ ->
                {'dep', App}
        end
    end,
    fold_apps(lists:map(MapFun, Apps), Func, Context, State);

fold('all', Func, Context, State) ->
    PrjApps = [{'prj', PS} || PS <- prj_app_specs(State)],
    DepApps = [{'dep', DS} || DS <- dep_app_specs(State)],
    fold_apps(PrjApps ++ DepApps, Func, Context, State);

fold(Pred, Func, Context, _) when not erlang:is_function(Pred, 2) ->
    erlang:error('badarg', [Pred, Func, Context, '_']);

fold(Pred, Func, Context, State) ->
    PrjApps = [{'prj', PS} || PS <- prj_app_specs(State), Pred(PS, State)],
    DepApps = [{'dep', DS} || DS <- dep_app_specs(State), Pred(DS, State)],
    fold_apps(PrjApps ++ DepApps, Func, Context, State).

-spec in_checkouts(
        App :: brt:app_spec() | brt:fs_path(), State :: brt:rebar_state())
        -> boolean().
%%
%% @doc Reports whether an application is in State's _checkouts directory.
%%
in_checkouts({_, Path, _}, State) ->
    in_checkouts(Path, State);
in_checkouts(Path, State) ->
    brt:abspath(filename:dirname(Path))
        == brt:abspath(rebar_dir:checkouts_dir(State)).

-spec in_prj_or_checkouts(
        App :: brt:app_spec() | brt:fs_path(), State :: brt:rebar_state())
        -> boolean().
%%
%% @doc Equivalent to in_project/2 orelse in_checkouts/2.
%%
in_prj_or_checkouts(App, State) ->
    in_project(App, State) orelse in_checkouts(App, State).

-spec in_project(
        App :: brt:app_spec() | brt:fs_path(), State :: brt:rebar_state())
        -> boolean().
%%
%% @doc Reports whether an application is in State's project.
%%
in_project({Name, _, _}, State) ->
    AppInfName = brt:to_binary(Name),
    lists:any(
        fun(AppInfo) ->
            rebar_app_info:name(AppInfo) == AppInfName
        end, rebar_state:project_apps(State));
in_project(Path, State) ->
    brt:abspath(Path) == brt:abspath(rebar_state:dir(State)).

-spec init() -> 'ok'.
%% @private
%% @doc Initializes the environment for rebar operations.
%%
init() ->
    DefaultFile = "rebar.config",
    FileNames = case os:getenv("REBAR_CONFIG") of
        'false' ->
            [DefaultFile];
        DefaultFile ->
            [DefaultFile];
        Val ->
            [Val, DefaultFile]
    end,
    _ = cfg_file_names(FileNames),
    _ = config_format('default'),
    'ok'.

-spec prj_apps(State :: brt:rebar_state()) -> [brt:app_name()].
%%
%% @doc Returns the names of the current project's applications.
%%
prj_apps(State) ->
    [brt:to_atom(rebar_app_info:name(AI))
        || AI <- rebar_state:project_apps(State)].

-spec prj_app_specs(State :: brt:rebar_state()) -> [brt:app_spec()].
%%
%% @doc Returns the name/path tuples of the current project's applications.
%%
prj_app_specs(State) ->
    [{brt:to_atom(rebar_app_info:name(AI)),
        rebar_app_info:dir(AI), rebar_app_info:out_dir(AI)}
            || AI <- rebar_state:project_apps(State)].

%% ===================================================================
%% Dependency Formats
%% ===================================================================

-spec format_dep(Indent :: iolist(), Dep :: brt:app_name() | brt:dep_spec())
        -> iolist().
%%
%% @doc Returns the specified dependency in the current version format.
%%
%% The result DOES NOT include any leading indent or trailing delimiter and/or
%% newline, but if it contains embedded newlines each is followed by Indent.
%%
%% A dependency spec that cannot be formatted for the current output version
%% causes a 'badarg' error to be raised.
%%
%% Fields:
%%
%%  AppName     ::  atom()
%%  RepoLoc     ::  string()
%%  RepoOpt     ::  list | tuple()
%%  RepoType    ::  atom()
%%  VsnStr      ::  string()
%%  VsnType     ::  atom()
%%  WrapOpt     ::  list | tuple()
%%  WrapType    ::  atom()
%%
%% Patterns:
%%
%%  AppName
%%
%%  {AppName, {WrapType, {RepoType, RepoLoc, {VsnType, VsnStr} }, WrapOpt} }
%%  {AppName, {WrapType, {RepoType, RepoLoc, VsnStr}, WrapOpt} }
%%
%%  {AppName, {WrapType, {RepoType, RepoLoc, {VsnType, VsnStr} } } }
%%  {AppName, {WrapType, {RepoType, RepoLoc, VsnStr} } }
%%
%%  {AppName, {RepoType, RepoLoc, {VsnType, VsnStr} } }
%%  {AppName, {RepoType, RepoLoc, VsnStr} }
%%
%% TODO: Package manager specs
%%
format_dep(Indent, AppName) when ?is_app_name(AppName) ->
    format_dep(Indent, brt_config:pkg_dep(AppName));
format_dep(Indent, Dep) ->
    case config_format() of
        2 ->
            format_dep2(Indent, Dep);
        _ ->
            format_dep3(Indent, Dep)
    end.

-spec format_dep3(Indent :: iolist(), Dep :: brt:app_name() | brt:dep_spec())
        -> iolist().

format_dep3(Indent,
    {AppName, {WrapType, {RepoType, RepoLoc, VsnStr}, WrapOpt}})
        when erlang:is_list(VsnStr) ->
    format_dep3(Indent,
        {AppName, {WrapType, {RepoType, RepoLoc, {'ref', VsnStr}}, WrapOpt}});

format_dep3(Indent,
    {AppName, {WrapType, {RepoType, RepoLoc, VsnStr}}})
        when erlang:is_list(VsnStr) ->
    format_dep3(Indent,
        {AppName, {WrapType, {RepoType, RepoLoc, {'ref', VsnStr}}}});

format_dep3(Indent, {AppName, {RepoType, RepoLoc, VsnStr}})
        when erlang:is_list(VsnStr) ->
    format_dep3(Indent,
        {AppName, {RepoType, RepoLoc, {'ref', VsnStr}}});

format_dep3(Indent,
    {AppName, {WrapType, {RepoType, RepoLoc, {VsnType, VsnStr}}, WrapOpt}})
        when erlang:is_atom(AppName) andalso erlang:is_atom(WrapType)
        andalso erlang:is_atom(RepoType) andalso erlang:is_list(RepoLoc)
        andalso erlang:is_atom(VsnType) andalso erlang:is_list(VsnStr) ->
    io_lib:format(
        "{'~s', {'~s',~n~s{'~s', \"~s\",~n~s{'~s', \"~s\"}},~n~s~s }}",
        [AppName, WrapType,
            Indent, RepoType, RepoLoc,
            Indent, VsnType, VsnStr,
            Indent, brt_io:format_flat(WrapOpt)]);

format_dep3(Indent,
    {AppName, {WrapType, {RepoType, RepoLoc, {VsnType, VsnStr}}}})
        when erlang:is_atom(AppName) andalso erlang:is_atom(WrapType)
        andalso erlang:is_atom(RepoType) andalso erlang:is_list(RepoLoc)
        andalso erlang:is_atom(VsnType) andalso erlang:is_list(VsnStr) ->
    io_lib:format(
        "{'~s', {'~s',~n~s{'~s', \"~s\",~n~s{'~s', \"~s\"} }}}",
        [AppName, WrapType,
            Indent, RepoType, RepoLoc,
            Indent, VsnType, VsnStr]);

format_dep3(Indent,
    {AppName, {RepoType, RepoLoc, {VsnType, VsnStr}}})
        when erlang:is_atom(AppName)
        andalso erlang:is_atom(RepoType) andalso erlang:is_list(RepoLoc)
        andalso erlang:is_atom(VsnType) andalso erlang:is_list(VsnStr) ->
    io_lib:format(
        "{'~s',~n~s{'~s', \"~s\",~n~s{'~s', \"~s\"} }}",
        [AppName, Indent, RepoType, RepoLoc, Indent, VsnType, VsnStr]);

format_dep3(_Indent, Dep) ->
    erlang:error('badarg', ['rebar3', Dep]).

-spec format_dep2(Indent :: iolist(), Dep :: brt:app_name() | brt:dep_spec())
        -> iolist().

format_dep2(Indent,
    {AppName, {WrapType, {RepoType, RepoLoc, {'ref', VsnStr}}, WrapOpt}})
        when erlang:is_list(VsnStr) ->
    format_dep2(Indent,
        {AppName, {WrapType, {RepoType, RepoLoc, VsnStr}, WrapOpt}});

format_dep2(Indent,
    {AppName, {WrapType, {RepoType, RepoLoc, {'ref', VsnStr}}}})
        when erlang:is_list(VsnStr) ->
    format_dep2(Indent,
        {AppName, {WrapType, {RepoType, RepoLoc, VsnStr}}});

format_dep2(Indent,
    {AppName, {RepoType, RepoLoc, {'ref', VsnStr}}})
        when erlang:is_list(VsnStr) ->
    format_dep2(Indent,{AppName, {RepoType, RepoLoc, VsnStr}});

format_dep2(Indent,
    {AppName, {WrapType, {RepoType, RepoLoc, VsnSpec}, _WrapOpt}}) ->
    format_dep2(Indent,
        {AppName, {WrapType, {RepoType, RepoLoc, VsnSpec}}});

format_dep2(Indent,
    {AppName, {WrapType, {RepoType, RepoLoc, {VsnType, VsnStr}}}})
        when erlang:is_atom(AppName) andalso erlang:is_atom(WrapType)
        andalso erlang:is_atom(RepoType) andalso erlang:is_list(RepoLoc)
        andalso erlang:is_atom(VsnType) andalso erlang:is_list(VsnStr) ->
    io_lib:format(
        "{'~s', \".*\",~n~s{'~s', \"~s\",~n~s{'~s', \"~s\"}}, ['~s']}",
        [AppName,
            Indent, RepoType, RepoLoc, Indent, VsnType, VsnStr, WrapType]);

format_dep2(Indent, {AppName, {WrapType, {RepoType, RepoLoc, VsnStr}}})
        when erlang:is_atom(AppName) andalso erlang:is_atom(WrapType)
        andalso erlang:is_atom(RepoType) andalso erlang:is_list(RepoLoc)
        andalso erlang:is_list(VsnStr) ->
    io_lib:format(
        "{'~s', \".*\",~n~s{'~s', \"~s\",~n~s\"~s\" }, ['~s']}",
        [AppName,
            Indent, RepoType, RepoLoc, Indent, VsnStr, WrapType]);

format_dep2(Indent, {AppName, {RepoType, RepoLoc, {VsnType, VsnStr}}})
        when erlang:is_atom(AppName)
        andalso erlang:is_atom(RepoType) andalso erlang:is_list(RepoLoc)
        andalso erlang:is_atom(VsnType) andalso erlang:is_list(VsnStr) ->
    io_lib:format(
        "{'~s', \".*\",~n~s{'~s', \"~s\",~n~s{'~s', \"~s\"} }}",
        [AppName, Indent, RepoType, RepoLoc, Indent, VsnType, VsnStr]);

format_dep2(Indent, {AppName, {RepoType, RepoLoc, VsnStr}})
        when erlang:is_atom(AppName)
        andalso erlang:is_atom(RepoType) andalso erlang:is_list(RepoLoc)
        andalso erlang:is_list(VsnStr) ->
    io_lib:format(
        "{'~s', \".*\",~n~s{'~s', \"~s\",~n~s\"~s\" }}",
        [AppName, Indent, RepoType, RepoLoc, Indent, VsnStr]);

format_dep2(_Indent, Dep) ->
    erlang:error('badarg', ['rebar2', Dep]).

-spec parse_dep(Dep :: brt:dep_spec()) -> brt:dep_spec().
%%
%% @doc Returns the specified dependency as an appropriate internal term.
%%

%
% TODO: Only use Rebar3 to consume rebar.config files, so we don't need this?
%
% At present, "appropriate internal term" means a Rebar3 dependency tuple, so
% just recognize Rebar2 deps where the second element is a RegEx string and
% transform it.
%
% Unfortunately, there are legitimate Rebar3 deps that match a simplistic
% pattern check, so this gets a bit more elaborate. As of this writing,
% rebar_app_utils:parse_deps/5 is the place to look for guidance.
%
% The rebar2 'raw' option is handled, all others are passed through.
%
% Note that rebar2 had a much longer list of supported resource types by
% default, so it's possible the resulting spec may not be valid in the
% current reabr3 configuration without additional resource providers, which
% we don't deal with.
%

% Rebar3 package dependencies can have a (possibly empty) version string as
% the 2nd element, so don't misinterpret it as a RegEx.
parse_dep({Name, Vsn, {'pkg', _}} = Dep)
        when erlang:is_atom(Name) andalso erlang:is_list(Vsn) ->
    Dep;

parse_dep({Name, [_|_], Where, []})
        when erlang:is_atom(Name) andalso erlang:is_tuple(Where) ->
    {Name, Where};

parse_dep({Name, [_|_], Where, Opts})
        when erlang:is_atom(Name) andalso erlang:is_tuple(Where) ->
    case erlang:is_list(Opts) andalso lists:member('raw', Opts) of
        'true' ->
            case Opts -- ['raw'] of
                [] ->
                    {Name, {'raw', Where}};
                Rest ->
                    {Name, {'raw', Where}, Rest}
            end;
        _ ->
            {Name, Where, Opts}
    end;

parse_dep({Name, [_|_], Where})
        when erlang:is_atom(Name) andalso erlang:is_tuple(Where) ->
    {Name, Where};

% Catch any remaining potential Rebar2 patterns.
% Note that a 2-tuple with a string as the 2nd element is a legitimate Rebar3
% package dependency, so keep the size above that.
parse_dep(Dep) when erlang:is_tuple(Dep)
        andalso erlang:tuple_size(Dep) >= 3
        andalso erlang:is_atom(erlang:element(1, Dep))
        andalso erlang:is_list(erlang:element(2, Dep)) ->
    erlang:error('badarg', [Dep]);

parse_dep(Dep) ->
    Dep.

%% ===================================================================
%% Internal
%% ===================================================================

-spec config_file(Path :: brt:fs_path()) -> brt:fs_path().
%
% Returns the path of the rebar.config file for the specified directory.
%
% If one of the files returned by cfg_file_names/0 exists, the first such
% fully-qualified path is returned. If none of the files exists, the FQ path
% of the first candidate name is returned.
%
% Note that this departs from Rebar3's strategy, in that this function always
% searches candidate config file names, including names added as command
% options and $REBAR_CONFIG if present, whereas Rebar3 only uses $REBAR_CONFIG
% at the top of the project tree and knows nothing of command additions.
%
% It remains to be seen whether this is a good idea or not ...
%
config_file(Path) ->
    FileNames = cfg_file_names(),
    case brt:find_first('file', FileNames, [Path]) of
        'false' ->
            filename:absname(erlang:hd(FileNames), Path);
        File ->
            File
    end.

-spec fold_apps(
        Select  ::  [{'prj' | 'dep', brt:app_spec()}],
        Func    ::  fold_func(),
        Context ::  fold_ctx(),
        State   ::  brt:rebar_state())
        -> {'ok', fold_ctx()} | brt:err_result().
%
% Inner behavior for fold/4.
%
fold_apps([{'prj', {_, _, _} = App} | Apps], Func, Context, State) ->
    case Func(App, Context, State) of
        {'ok', NextContext} ->
            fold_apps(Apps, Func, NextContext, State);
        Ret ->
            Ret
    end;
fold_apps([{'dep', {_, Path, _} = App} | Apps], Func, Context, State) ->
    case brt_config:merge(Path) of
        {'ok', Config} ->
            Ret = Func(App, Context, State),
            brt_config:reset(Config),
            case Ret of
                {'ok', NextContext} ->
                    fold_apps(Apps, Func, NextContext, State);
                _ ->
                    Ret
            end;
        Error ->
            Error
    end;
fold_apps([], _, Context, _) ->
    {'ok', Context}.

-spec order_profiles(Profiles :: [atom()], Result :: [atom()]) -> [atom()].
%
% Given any list of profiles, return the list de-duplicated in its original
% order but with with 'default' at the end regardless of whether or where it
% appears in the input.
% Common cases get special handling so most list appends are avoided, since
% multiple non-default profiles is an unusual use case.
%
order_profiles([], []) ->
    ['default'];
order_profiles([], Result) ->
    Result ++ ['default'];
order_profiles(['default'], []) ->
    ['default'];
order_profiles([Profile], []) ->
    [Profile, 'default'];
order_profiles(['default' | Profiles], Result) ->
    order_profiles(Profiles, Result);
order_profiles([Profile | Profiles], []) ->
    order_profiles(Profiles, [Profile]);
order_profiles([Profile | Profiles], Result) ->
    case lists:member(Profile, Result) of
        'true' ->
            order_profiles(Profiles, Result);
        _ ->
            order_profiles(Profiles, Result ++ [Profile])
    end.
