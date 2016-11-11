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

-module(brt_escript).

%% Escript Entry Point
-export([main/1]).

-include("../src/brt.hrl").

%
% Define this to redirect I/O operations into this module for access into the
% escript's embedded zip archive.
% As of this writing, the newly-added consult_app_file/2 function is
% not implemented, and it's non-trivial to do so, so code must be written
% before this can be turned on.
%
% -define(BRT_ESCRIPT_REDIRECT,   true).

-ifdef(BRT_ESCRIPT_REDIRECT).
-ifdef(BRT_ESCRIPT_IO_MOD_KEY).
-export([
    consult_app_file/2,
    list_modules/1,
    read_app_file/2
]).
-include_lib("stdlib/include/zip.hrl").
-else.
-undef(BRT_ESCRIPT_REDIRECT).
-endif.
-endif.

%%====================================================================
%% Escript Entry Point
%%====================================================================

-spec main(Args :: [string()]) -> no_return().
main(Args) ->
    try
        Result = case setup_app_env() of
            'ok' ->
                run(Args);
            Err ->
                Err
        end,
        case Result of
            'ok' ->
                erlang:halt(0);
            RC when erlang:is_integer(RC) ->
                erlang:halt(RC);
            {'error', What} ->
                error_exit(What);
            Error ->
                error_exit(Error)
        end
    catch
        Type:Value ->
            error_exit(Type, Value, erlang:get_stacktrace())
    end.

%% ===================================================================
%% Command Handler
%% ===================================================================

-spec run(Args :: [string()]) -> term() | no_return().

run(["deps", AppDir]) ->
    {ProdApps, TestApps} = xref_app_deps(AppDir),
    io:put_chars("Prod:\n"),
    brt_io:write_deps('standard_io', 1, lists:sort(ProdApps)),
    io:put_chars("Test:\n"),
    brt_io:write_deps('standard_io', 1, lists:sort(TestApps));

run(["info"]) ->
    brt_io:write_info('standard_io');

run(["mk", "help"]) ->
    io:put_chars(
        "Write Makefile to standard output or a file.\n"
        "<app-dir> must be a sibling of its dependencies.\n"
        "mk <app-dir>\n"
        "mk create <app-dir> <output-file> <overwite>\n"
        "mk update <app-dir> <output-file> <must-exist>\n"
    );

run(["mk", AppDir]) ->
    {ProdApps, TestApps} = xref_app_deps(AppDir),
    Make = brt_defaults:makefile(ProdApps, TestApps),
    io:put_chars(Make);

run(["mk", "create", AppDir, Makefile, Overwrite]) ->
    brt_make:create_makefile(brt:app_dir_to_name(AppDir),
        AppDir, filename:dirname(AppDir), Makefile, brt:to_atom(Overwrite));

run(["mk", "update", AppDir, Makefile, MustExist]) ->
    brt_make:update_makefile(brt:app_dir_to_name(AppDir),
        AppDir, filename:dirname(AppDir), Makefile, brt:to_atom(MustExist));

run(["rc", "help"]) ->
    io:put_chars(
        "Write rebar.config to standard output or a file.\n"
        "<app-dir> must be a sibling of its dependencies.\n"
        "rc <app-dir>\n"
        "rc create <app-dir> <output-file> <overwite>\n"
        "rc update <app-dir> <output-file> <must-exist>\n"
    );

run(["rc", AppDir]) ->
    {ProdApps, TestApps} = xref_app_deps(AppDir),
    Conf = brt_defaults:rebar_config(ProdApps, TestApps, []),
    brt_io:write_rebar_config('standard_io', Conf, 'current');

run(["rc", "create", AppDir, CfgFile, Overwrite]) ->
    brt_rebar:create_rebar_config(brt:app_dir_to_name(AppDir),
        AppDir, filename:dirname(AppDir), CfgFile, brt:to_atom(Overwrite));

run(["rc", "update", AppDir, CfgFile, MustExist]) ->
    brt_rebar:update_rebar_config(brt:app_dir_to_name(AppDir),
        AppDir, filename:dirname(AppDir), CfgFile, brt:to_atom(MustExist));

run(Args) ->
    erlang:error('badarg', [Args]).

%% ===================================================================
%% I/O Hooks
%% ===================================================================
-ifdef(BRT_ESCRIPT_REDIRECT).

-spec consult_app_file(Dir :: atom() | string(), FileName :: string())
        -> {'ok', [term()]} | brt:err_result().
%%
%% @doc Return the terms in FileName in the specified application directory.
%%
consult_app_file(_Dir, _FileName) ->
    erlang:error('not_implemented').

-spec list_modules(ModPrefix :: string()) -> [module()] | brt:err_result().
%%
%% @doc Return the list of modules in this application matching ModPrefix.
%%
%% ModPrefix is plain string that is matched against the module name as if by
%% lists:prefix/2.
%%
list_modules(ModPrefix) ->
    Archive = escript_archive(),
    {'ok', ArchDir} = zip:list_dir(Archive),
    ModDir  = filename:basename(filename:dirname(code:which(?MODULE))),
    ModExt  = code:objfile_extension(),
    Prefix  = filename:join([?APP_NAME_STRING, ModDir, ModPrefix]),
    lists:foldl(
        fun(#zip_file{name = File}, Mods) ->
                case lists:prefix(Prefix, File)
                        andalso filename:extension(File) == ModExt of
                    'true' ->
                        [erlang:list_to_atom(
                            filename:basename(File, ModExt)) | Mods];
                    _ ->
                        Mods
                end;
            (_, Mods) ->
                Mods
        end, [], ArchDir).

-spec read_app_file(Dir :: atom() | string(), FileName :: string())
        -> {'ok', binary()} | brt:err_result().
%%
%% @doc Return the contents of FileName in the specified application directory.
%%
read_app_file(Dir, FileName) ->
    Archive = escript_archive(),
    File    = filename:join([?APP_NAME_STRING, Dir, FileName]),
    case zip:extract(Archive, [{'file_list', [File]}, 'memory']) of
        {'ok', [{_, Bin}]} ->
            {'ok', Bin};
        {'ok', _} ->
            % with these options it may just return an empty list,
            % so reproduce the error we'd get from an actual filesystem
            brt:file_error(File, 'enoent');
        Err ->
            Err
    end.

-spec escript_archive() -> binary().
%
% Return the zip archive of the application.
%
escript_archive() ->
    Key = {?MODULE, 'archive'},
    case erlang:get(Key) of
        'undefined' ->
            {'ok', Sections} = escript:extract(
                erlang:get({?MODULE, 'escript'}), []),
            Archive = proplists:get_value('archive', Sections),
            erlang:put(Key, Archive),
            Archive;
        Val ->
            Val
    end.

-endif. % BRT_ESCRIPT_REDIRECT

%% ===================================================================
%% Internal
%% ===================================================================

-spec error_exit(What :: term()) -> no_return().
error_exit(What) ->
    Fmt = if
        erlang:is_list(What) ->
            "Error: ~s\n";
        'true' ->
            "Error: ~p\n"
    end,
    io:format('standard_error', Fmt, [What]),
    erlang:halt(1).

-spec error_exit(Class :: atom(), What :: term(), Stack :: [tuple()])
        -> no_return().
error_exit(Class, What, Stack) ->
    Fmt = if
        erlang:is_list(What) ->
            "~s: ~s\n~p\n";
        'true' ->
            "~s: ~p\n~p\n"
    end,
    io:format('standard_error', Fmt, [Class, What, Stack]),
    erlang:halt(1).

-spec xref_app_deps(AppDir :: brt:fs_path())
        -> {'ok', [brt:app_name()], [brt:app_name()]} | no_return().
xref_app_deps(AppDir) ->
    AppName = brt:app_dir_to_name(AppDir),
    DepsDir = filename:dirname(AppDir),
    case brt_xref:app_deps(AppName, AppDir, DepsDir) of
        {'ok', ProdApps, TestApps} ->
            {ProdApps, TestApps};
        {'error', What} ->
            erlang:error(What)
    end.

-spec setup_app_env() -> 'ok' | brt:err_result() | no_return().
%
% Set up the application environment and initialize configuration.
%
-ifdef(BRT_ESCRIPT_REDIRECT).

setup_app_env() ->
    EScript = filename:absname(escript:script_name()),
    erlang:put({?MODULE, 'escript'}, EScript),
    erlang:put(?BRT_ESCRIPT_IO_MOD_KEY, ?MODULE),
    {'ok', CWD} = file:get_cwd(),
    brt_config:init([CWD, filename:dirname(EScript)]).

-else.  % not BRT_ESCRIPT_REDIRECT

%
% The escript only works if the actual file (after dereferencing symlinks) is
% in rebar's output location in the project tree.
% If the escript has been moved, we'll just blow up with an error, as there's
% no way things are going to work.
%
setup_app_env() ->
    EScript = filename:absname(escript:script_name()),
    EsFile  = deref_symlinks(EScript),
    EsDir   = filename:dirname(EsFile),
    AppDir  = case filename:basename(EsDir) of
        ?APP_NAME_STRING ->
            % rebar2 leaves the escript in the project's base directory
            EsDir;
        _ ->
            % rebar3's structure is a bit more convoluted, in a good way
            EsDirUp = filename:dirname(EsDir),
            case filename:basename(EsDirUp) of
                ?APP_NAME_STRING ->
                    EsDirUp;
                _ ->
                    EsDirSib = filename:join(
                        [EsDirUp, "lib", ?APP_NAME_STRING]),
                    case filelib:is_dir(EsDirSib) of
                        'true' ->
                            EsDirSib;
                        _ ->
                            erlang:error('escript_moved', [EsFile])
                    end
            end
    end,
    ObjDir  = filename:join(AppDir, "ebin"),
    PrivDir = filename:join(AppDir, "priv"),
    case filelib:is_dir(ObjDir) andalso filelib:is_dir(PrivDir) of
        'true' ->
            erlang:put({'brt', 'app'}, AppDir),
            erlang:put({'brt', 'ebin'}, ObjDir),
            erlang:put({'brt', 'priv'}, PrivDir),
            {'ok', CWD} = file:get_cwd(),
            brt_config:init([CWD, filename:dirname(EScript)]);
        _ ->
            erlang:error('bad_structure', [AppDir])
    end.

-spec deref_symlinks(AbsPath :: brt:fs_path()) -> brt:fs_path() | no_return().
%
% Dereference the specified absolute path, possibly recursively, until we get
% to something that exists and is not a symbolic link.
%
deref_symlinks(AbsPath) ->
    case file:read_link(AbsPath) of
        {'ok', Dest} ->
            deref_symlinks(filename:absname(Dest, filename:dirname(AbsPath)));
        {'error', What} ->
            case filelib:is_file(AbsPath) of
                'true' ->
                    AbsPath;
                _ ->
                    erlang:error(What, [AbsPath])
            end
    end.

-endif. % BRT_ESCRIPT_REDIRECT
