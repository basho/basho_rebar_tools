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

-module(brt_xref).

% API
-export([
    app_deps/1,
    app_deps/3,
    dep_apps/3
]).

-type dep_spec()    ::  {brt:app_name(),  brt:fs_path()}.
-type xref_error()  ::  {'error', module(), term()}.

-define(XREF,   ?MODULE).

%% ===================================================================
%% API
%% ===================================================================

-spec app_deps(State :: brt:rebar_state())
        -> {'ok', [brt:app_name()], [brt:app_name()]} | brt:err_result().
%%
%% @doc Returns the production and test dependencies of an application.
%%
app_deps(State) ->
    case rebar_state:project_apps(State) of
        [] ->
            % this really should never happen, but avoid a bad match
            {'error', {?MODULE, 'app_undefined'}};
        [AppInfo | Tail] ->
            AppName = brt:to_atom(rebar_app_info:name(AppInfo)),
            case Tail of
                [] ->
                    'ok';
                _ ->
                    rebar_api:warn(
                        "Multiple applications defined, analyzing ~s",
                        [AppName])
            end,
            AppDir  = rebar_app_info:out_dir(AppInfo),
            DepsDir = filename:dirname(AppDir),
            DepsAlt = filename:join(rebar_state:dir(State), "_checkouts"),
            DepDirs = case filelib:is_dir(DepsAlt) of
                'true' ->
                    [DepsAlt, DepsDir];
                _ ->
                    [DepsDir]
            end,
            app_deps(AppName, AppDir, DepDirs)
    end.

-spec app_deps(
        AppName     :: brt:app_name(),
        AppDir      :: brt:fs_path(),
        DepsDirs    :: [brt:fs_path()])
        -> {'ok', [brt:app_name()], [brt:app_name()]} | brt:err_result().
%%
%% @doc Returns the production and test dependencies of an application.
%%
%% On success, the result is {'ok', ProdApps, TestApps}, each as a list of
%% application name atoms.
%%
app_deps(AppName, AppDir, DepsDirs) ->
    case analyze_app_to_app(AppName, AppDir, DepsDirs, 'application_call') of
        {'error', _} = ProdErr ->
            ProdErr;
        {'ok', XRefApps} ->
            ProdApps = case brt:get_key_list('force', brt_config:config()) of
                [] ->
                    XRefApps;
                ForcedApps ->
                    lists:usort(ForcedApps ++ XRefApps)
            end,
            %
            % It would be better to use xref on test modules, which would pick
            % up meck and the like (but might also pick up false positives on
            % quickcheck), but for now just look for the pattern we use for
            % cuttlefish schema files and assume we actually test them.
            %
            % Best results will be obtained by compiling with the 'prod' and
            % 'test' profiles, running xref on both, and diff'ing the results
            % to come up the apps that are used only in test mode.
            % That, in turn, will require Makefile support and/or some tricky
            % plugin magic to invoke multiple rebar compilations.
            % Doable, in time.
            %
            TestApps = case lists:member('cuttlefish', [AppName | ProdApps]) of
                'true' ->
                    [];
                _ ->
                    Schema = filename:join(
                        [AppDir, "priv", brt:to_string(AppName) ++ ".schema"]),
                    case filelib:is_regular(Schema) of
                        'true' ->
                            ['cuttlefish'];
                        _ ->
                            []
                    end
            end,
            {'ok', ProdApps, TestApps}
    end.

-spec dep_apps(
        AppName     :: brt:app_name(),
        AppDir      :: brt:fs_path(),
        DepsDirs    :: [brt:fs_path()])
        -> {'ok', [brt:app_name()]} | brt:err_result().
%%
%% @doc Returns the applications that depend on an application.
%%
dep_apps(AppName, AppDir, DepsDirs) ->
    analyze_app_to_app(AppName, AppDir, DepsDirs, 'application_use').

%% ===================================================================
%% Internal
%% ===================================================================

-spec analyze_app_to_app(
        AppName :: brt:app_name(),
        AppDir  :: brt:fs_path(), DepsDir :: brt:fs_path(),
        Analysis :: 'application_call' | 'application_use')
        -> {'ok', [brt:app_name()]} | brt:err_result().
analyze_app_to_app(AppName, AppDir, DepsDir, Analysis) ->
    case load_xref(AppName, AppDir, DepsDir) of
        'ok' ->
            case xref:analyze(?XREF, {Analysis, AppName}) of
                {'ok', Result} ->
                    {'ok', lists:delete(AppName, Result)};
                XRefErr ->
                    xref_error(XRefErr)
            end;
        Error ->
            Error
    end.

-spec load_xref(
        AppName     :: brt:app_name(),
        AppDir      :: brt:fs_path(),
        DepsDirs    :: [brt:fs_path()])
        -> 'ok' | brt:err_result().
load_xref(AppName, AppDir, DepsDirs) ->
    case is_app_dir(AppDir) of
        'true' ->
            case erlang:whereis(?XREF) of
                'undefined' ->
                    'ok';
                _ ->
                    catch xref:stop(?XREF)
            end,
            case xref:start(?XREF, {'xref_mode', 'modules'}) of
                {'ok', _Pid} ->
                    case dep_specs(DepsDirs, [{AppName, AppDir}]) of
                        {'ok', AppSpecs} ->
                            add_apps(AppSpecs);
                        DepErr ->
                            DepErr
                    end;
                {'error', {'already_started', _}} ->
                    {'error', {'xref', 'stop_failed'}};
                {'error', What} ->
                    {'error', {'xref', What}};
                Other ->
                    {'error', {'xref', Other}}
            end;
        _ ->
            erlang:error('badarg', [AppName, AppDir, DepsDirs])
    end.

-spec add_apps(Apps :: [dep_spec()]) -> 'ok' | xref_error().
add_apps([{AppName, AppDir} | Apps]) ->
    case xref:add_application(?XREF, AppDir, [{'name', AppName}]) of
        {'ok', _} ->
            add_apps(Apps);
        XRefErr ->
            xref_error(XRefErr)
    end;
add_apps([]) ->
    'ok'.

-spec dep_specs(DepsDirs :: [brt:fs_path()], Result :: [dep_spec()])
        -> [dep_spec()] | brt:err_result().
dep_specs([DepsDir | DepsDirs], Result) ->
    case file:list_dir(DepsDir) of
        {'ok', DepDirs} ->
            dep_specs(DepsDirs, dep_specs(DepDirs, DepsDir, Result));
        {'error', What} ->
            brt:file_error(DepsDir, What)
    end;
dep_specs([], Result) ->
    {'ok', Result}.

-spec dep_specs(
        Dirs    :: [brt:fs_path()],
        DepsDir :: brt:fs_path(),
        Result  :: [dep_spec()])
        -> [dep_spec()] | brt:err_result().
dep_specs([Dir | Dirs], DepsDir, Result) ->
    DepDir = filename:join(DepsDir, Dir),
    case is_app_dir(DepDir) of
        'true' ->
            DepName = brt:app_dir_to_name(Dir),
            case lists:keymember(DepName, 1, Result) of
                'false' ->
                    DepSpec = {DepName, DepDir},
                    dep_specs(Dirs, DepsDir, [DepSpec | Result]);
                _ ->
                    dep_specs(Dirs, DepsDir, Result)
            end;
        _ ->
            dep_specs(Dirs, DepsDir, Result)
    end;
dep_specs([], _, Result) ->
    Result.

-spec is_app_dir(Dir :: brt:fs_path()) -> boolean().
is_app_dir(Dir) ->
    filelib:is_dir(filename:join(Dir, "ebin")).

-spec xref_error(Error :: xref_error()) -> {'error', string()}.
xref_error(Error) ->
%%    erlang:error(lists:flatten(xref:format_error(Error))).
    {'error', lists:flatten(xref:format_error(Error))}.
