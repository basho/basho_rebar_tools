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

-module(brt_make).

% API
-export([
    create_makefile/5,
    update_makefile/5
]).

-include("brt.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec create_makefile(
        AppName     :: brt:app_name(),
        AppDir      :: brt:fs_path(),
        DepsDir     :: brt:fs_path(),
        Makefile    :: brt:fs_path(),
        Overwrite   :: boolean())
        -> 'ok' | brt:err_result() | no_return().
%%
%% @doc Writes the Makefile for the specified Package.
%%
%% Xref or defaults errors are reported before the target Makefile is touched.
%%
create_makefile(AppName, AppDir, DepsDir, Makefile, Overwrite) ->
    case brt_xref:app_deps(AppName, AppDir, DepsDir) of
        {'error', _} = XRefErr ->
            XRefErr;
        {'ok', ProdApps, TestApps} ->
            Content = brt_defaults:makefile(ProdApps, TestApps),
            OpenOpts = case Overwrite of
                'true' ->
                    ['write'];
                'false' ->
                    ['write', 'exclusive']
            end,
            case file:open(Makefile, OpenOpts) of
                {'ok', IoDev} ->
                    try
                        brt_io:write_makefile(IoDev, Content, 'current')
                    after
                        file:close(IoDev)
                    end;
                {'error', What} ->
                    brt:file_error(Makefile, What)
            end
    end.

-spec update_makefile(
        AppName     :: brt:app_name(),
        AppDir      :: brt:fs_path(),
        DepsDir     :: brt:fs_path(),
        Makefile    :: brt:fs_path(),
        MustExist   :: boolean())
        -> 'ok' | brt:err_result() | no_return().
%%
%% @doc Updates the Makefile for the specified Package.
%%
%% Xref or defaults errors are reported before the target Makefile is touched.
%%
update_makefile(AppName, AppDir, DepsDir, Makefile, MustExist) ->
    case brt_xref:app_deps(AppName, AppDir, DepsDir) of
        {'error', _} = XRefErr ->
            XRefErr;
        {'ok', ProdApps, TestApps} ->
            Content = brt_defaults:makefile(ProdApps, TestApps),
            YearOrError = case brt_io:copyright_info(Makefile, 'sh') of
                {'basho', CpyYear} ->
                    CpyYear;
                {'error', 'enoent'} = FileErr ->
                    case MustExist of
                        'true' ->
                            FileErr;
                        _ ->
                            'current'
                    end;
                'none' ->
                    brt_repo:added_year(Makefile, 'current');
                'other' ->
                    {'error', {'brt', 'copyright_dirty'}};
                {'error', _} = FileErr ->
                    FileErr
            end,
            case YearOrError of
                {'error', What} when erlang:is_atom(What) ->
                    brt:file_error(Makefile, What);
                {'error', _} = Error ->
                    Error;
                Year ->
                    case file:open(Makefile, ['write']) of
                        {'ok', IoDev} ->
                            try
                                brt_io:write_makefile(IoDev, Content, Year)
                            after
                                file:close(IoDev)
                            end;
                        {'error', What} ->
                            brt:file_error(Makefile, What)
                    end
            end
    end.
