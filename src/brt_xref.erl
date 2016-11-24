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
    add/2,
    app/2,
    app_deps/2,
    dep_apps/2,
    new/1,
    stop/1
]).

-export_type([
    addable/0,
    app_dir/0,
    lib_dir/0,
    xref/0
]).

-include("brt.hrl").

%
% For the time being, maintain the list of added applications and their
% directories outside the xref server for easy access.
% This approach may change if we want to use this module for more extensive
% analysis, which is why it's wrapped in a record in the first place.
%
-record(brt_xref, {
    xref        ::  pid() | atom(),
    apps  = []  ::  [brt:app_spec()]
}).

-type addable() ::  brt:app_spec() | app_dir() | lib_dir().
-type app_dir() ::  brt:fs_path().
-type lib_dir() ::  {brt:fs_path()}.
-type xref()    ::  #brt_xref{}.

-type xref_error()  ::  {'error', module(), term()}.

%% ===================================================================
%% API
%% ===================================================================

-spec add(XRef :: xref(), Adds :: addable() | [addable()])
        -> {'ok', xref()} | brt:err_result().
%%
%% @doc Adds the specified applications to the xref server.
%%
%% Applications are identified by their names, and duplicates are silently
%% ignored, as are directories that do not appear to contain an application.
%%
%% Input list elements are each one of:
%%
%%   {AppName, AppDir}  = The name and output path of an application.
%%   {LibDir}           = A directory containing application directories.
%%   AppDir             = An application directory.
%%
add(#brt_xref{xref = X, apps = Apps} = XRef, [{Name, _, Path} = App | Adds]) ->
    case not lists:keymember(Name, 1, Apps) andalso brt:is_app_dir(Path) of
        'true' ->
            case xref:add_application(X, Path, [{'name', Name}]) of
                {'ok', _} ->
                    add(XRef#brt_xref{apps = [App | Apps]}, Adds);
                XRefErr ->
                    xref_error(XRefErr)
            end;
        _ ->
            add(XRef, Adds)
    end;
add(XRef, [{LibDir} | Adds]) ->
    case file:list_dir_all(LibDir) of
        {'ok', Subs} ->
            Paths = [filename:join(LibDir, Sub) || Sub <- Subs],
            add(XRef, Paths ++ Adds);
        {'error', What} ->
            brt:file_error(LibDir, What)
    end;
add(#brt_xref{xref = X, apps = Apps} = XRef, [Path | Adds]) ->
    case brt:is_app_dir(Path) of
        'true' ->
            Name = brt:app_dir_to_name(Path),
            case lists:keymember(Name, 1, Apps) of
                'false' ->
                    case xref:add_application(X, Path, [{'name', Name}]) of
                        {'ok', _} ->
                            App = {Name, Path, Path},
                            add(XRef#brt_xref{apps = [App | Apps]}, Adds);
                        XRefErr ->
                            xref_error(XRefErr)
                    end;
                _ ->
                    add(XRef, Adds)
            end;
        _ ->
            add(XRef, Adds)
    end;
add(XRef, []) ->
    {'ok', XRef};
add(XRef, Addable) ->
    add(XRef, [Addable]).

-spec app(XRef :: xref(), AppName :: brt:app_name())
        -> brt:app_spec() | 'false'.
%%
%% @doc Returns the name/path tuple for the specified application, if found.
%%
%% Returns 'false' if the application has not been added to the xref server.
%%
app(#brt_xref{apps = Apps}, AppName) ->
    lists:keyfind(brt:to_atom(AppName), 1, Apps).

-spec app_deps(XRef :: xref(), Apps :: brt:app_name() | [brt:app_name()])
        -> {'ok', [brt:app_name()]} | brt:err_result().
%%
%% @doc Returns all of the applications called by any of the input Apps.
%%
%% Note that the result is not filtered, so it normally includes the input Apps.
%%
app_deps(#brt_xref{xref = X}, Apps) when erlang:is_list(Apps) ->
    case xref:analyze(X, {'application_call', Apps}) of
        {'ok', _} = Ret ->
            Ret;
        XRefErr ->
            xref_error(XRefErr)
    end;
app_deps(XRef, App) ->
    app_deps(XRef, [App]).

-spec dep_apps(XRef :: xref(), Apps :: brt:app_name() | [brt:app_name()])
        -> {'ok', [brt:app_name()]} | brt:err_result().
%%
%% @doc Returns all of the applications that call any of the input Apps.
%%
%% Note that the result is not filtered, so it normally includes the input Apps.
%%
dep_apps(#brt_xref{xref = X}, Apps) when erlang:is_list(Apps) ->
    case xref:analyze(X, {'application_use', Apps}) of
        {'ok', _} = Ret ->
            Ret;
        XRefErr ->
            xref_error(XRefErr)
    end;
dep_apps(XRef, App) ->
    dep_apps(XRef, [App]).

-spec new(StateOrApps :: brt:rebar_state() | addable() | [addable()])
        -> {'ok', xref()} | brt:err_result().
%%
%% @doc Starts an XRef server and loads it from the provided input.
%%
%% When the input is a list, it is handled according to the rules for input to
%% the add/2 function.
%%
%% An empty server can be started by providing an empty input list.
%%
new(State) when ?is_rebar_state(State) ->
    case brt_rebar:apps_deps_dirs(State) of
        {'ok', AppSpecs, DepsDirs} ->
            new(lists:append([
                AppSpecs, brt_rebar:dep_app_specs(State), [{D} || D <- DepsDirs]
            ]));
        Error ->
            Error
    end;
new([]) ->
    case xref:start([{'xref_mode', 'modules'}]) of
        {'ok', Pid} ->
            {'ok', #brt_xref{xref = Pid}};
        _ ->
            {'error', {'brt', 'xref_start_failed'}}
    end;
new(Dirs) ->
    case new([]) of
        {'ok', XRef} ->
            add(XRef, Dirs);
        Error ->
            Error
    end.

-spec stop(XRef :: xref()) -> 'ok'.
%%
%% @doc Shuts down the xref server.
%%
%% In this short-running context there's little need for this, as multiple
%% analyses can be done on a single instance.
%%
stop(#brt_xref{xref = X}) ->
    catch xref:stop(X),
    'ok'.

%% ===================================================================
%% Internal
%% ===================================================================

-spec xref_error(Error :: xref_error()) -> {'error', string()}.
xref_error(Error) ->
    {'error', lists:flatten(xref:format_error(Error))}.
