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
    app_deps/3,
    dep_apps/3
]).

-type xref_error()  ::  {'error', module(), term()}.

-define(XREF,   ?MODULE).

%% ===================================================================
%% API
%% ===================================================================

-spec app_deps(
        AppName :: brt:app_name(),
        AppDir  :: brt:fs_path(),
        DepsDir :: brt:fs_path())
        -> {'ok', [brt:app_name()], [brt:app_name()]} | brt:err_result().
%%
%% @doc Returns the production and test dependencies of an application.
%%
%% On success, the result is {ProdApps, TestApps}, each as a list of
%% application name atoms.
%%
app_deps(AppName, AppDir, DepsDir) ->
    case analyze_app_to_app(AppName, AppDir, DepsDir, 'application_call') of
        {'error', _} = ProdErr ->
            ProdErr;
        ProdApps ->
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
            {ProdApps, TestApps}
    end.

-spec dep_apps(
        AppName :: brt:app_name(),
        AppDir  :: brt:fs_path(),
        DepsDir :: brt:fs_path())
        -> {'ok', [brt:app_name()]} | brt:err_result().
%%
%% @doc Returns the applications that depend on an application.
%%
dep_apps(AppName, AppDir, DepsDir) ->
    analyze_app_to_app(AppName, AppDir, DepsDir, 'application_use').

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
                    Result -- [AppName];
                XRefErr ->
                    xref_error(XRefErr)
            end;
        Error ->
            Error
    end.

-spec load_xref(
        AppName :: brt:app_name(),
        AppDir  :: brt:fs_path(),
        DepsDir :: brt:fs_path())
        -> 'ok' | brt:err_result().
load_xref(AppName, AppDir, DepsDir) ->
    case is_app_dir(AppDir) andalso filelib:is_dir(DepsDir) of
        'true' ->
            {'ok', DepDirs} = file:list_dir(DepsDir),
            case erlang:whereis(?XREF) of
                'undefined' ->
                    'ok';
                _ ->
                    catch xref:stop(?XREF)
            end,
            case xref:start(?XREF, {'xref_mode', 'modules'}) of
                {'ok', _Pid} ->
                    AppSpecs = dep_specs(DepDirs, AppName, DepsDir, []),
                    case add_apps([{AppName, AppDir} | AppSpecs]) of
                        'ok' ->
                            'ok';
                        AddErr ->
                            xref_error(AddErr)
                    end;
                {'error', {'already_started', _}} ->
                    {'error', {'xref', 'stop_failed'}};
                {'error', What} ->
                    {'error', {'xref', What}};
                Other ->
                    {'error', {'xref', Other}}
            end;
        _ ->
            erlang:error('badarg', [AppName, AppDir, DepsDir])
    end.

-spec add_apps(Apps :: [{brt:app_name(), brt:fs_path()} | brt:app_name()])
        -> 'ok' | xref_error().
add_apps([{AppName, AppDir} | Apps]) ->
    case xref:add_application(?XREF, AppDir, [{'name', AppName}]) of
        {'ok', _} ->
            add_apps(Apps);
        Error ->
            Error
    end;
add_apps([]) ->
    'ok'.

-spec dep_specs(
        DepDirs :: [brt:fs_path()],
        AppName :: brt:app_name(),
        DepsDir :: brt:fs_path(),
        Result  :: [{brt:app_name(), brt:fs_path()}])
        -> [{brt:app_name(), brt:fs_path()}].

dep_specs([Dir | Dirs], AppName, DepsDir, Result) ->
    DepName = brt:app_dir_to_name(Dir),
    case DepName =/= AppName of
        'true' ->
            DepDir = filename:join(DepsDir, Dir),
            case is_app_dir(DepDir) of
                'true' ->
                    dep_specs(Dirs, AppName, DepsDir,
                        [{DepName, DepDir} | Result]);
                _ ->
                    dep_specs(Dirs, AppName, DepsDir, Result)
            end;
        _ ->
            dep_specs(Dirs, AppName, DepsDir, Result)
    end;
dep_specs([], _, _, Result) ->
    Result.

-spec is_app_dir(Dir :: brt:fs_path()) -> boolean().
is_app_dir(Dir) ->
    filelib:is_dir(filename:join(Dir, "ebin")).

-spec xref_error(Error :: xref_error()) -> {'error', string()}.
xref_error(Error) ->
    {'error', lists:flatten(xref:format_error(Error))}.
