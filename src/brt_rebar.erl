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
    create_rebar_config/5,
    update_rebar_config/5
]).

-include("brt.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec create_rebar_config(
        AppName     :: brt:app_name(),
        AppDir      :: brt:fs_path(),
        DepsDir     :: brt:fs_path(),
        CfgFile     :: brt:fs_path(),
        Overwrite   :: boolean())
        -> 'ok' | brt:err_result() | no_return().
%%
%% @doc Writes the rebar.config file for the specified Package.
%%
%% Xref or defaults errors are reported before the target file is touched.
%%
create_rebar_config(AppName, AppDir, DepsDir, CfgFile, Overwrite) ->
    case brt_xref:app_deps(AppName, AppDir, DepsDir) of
        {'error', _} = XRefErr ->
            XRefErr;
        {'ok', ProdApps, TestApps} ->
            Config  = brt_defaults:rebar_config(ProdApps, TestApps, []),
            OpenOpts = case Overwrite of
                'true' ->
                    ['write'];
                'false' ->
                    ['write', 'exclusive']
            end,
            case file:open(CfgFile, OpenOpts) of
                {'ok', IoDev} ->
                    try
                        brt_io:write_rebar_config(IoDev, Config, 'current')
                    after
                        file:close(IoDev)
                    end;
                {'error', What} ->
                    brt:file_error(CfgFile, What)
            end
    end.

-spec update_rebar_config(
        AppName     :: brt:app_name(),
        AppDir      :: brt:fs_path(),
        DepsDir     :: brt:fs_path(),
        CfgFile     :: brt:fs_path(),
        MustExist   :: boolean())
        -> 'ok' | brt:err_result() | no_return().
%%
%% @doc Updates the rebar.config file for the specified Package.
%%
%% Xref or defaults errors are reported before the target file is touched.
%%
update_rebar_config(AppName, AppDir, DepsDir, CfgFile, MustExist) ->
    case brt_xref:app_deps(AppName, AppDir, DepsDir) of
        {'error', _} = XRefErr ->
            XRefErr;
        {'ok', ProdApps, TestApps} ->
            Config  = brt_defaults:rebar_config(ProdApps, TestApps, []),
            YearOrError = case brt_io:copyright_info(CfgFile, 'erl') of
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
                    brt_repo:added_year(CfgFile, 'current');
                'other' ->
                    {'error', {'brt', {'copyright_dirty', CfgFile}}};
                {'error', _} = FileErr ->
                    FileErr
            end,
            case YearOrError of
                {'error', What} when erlang:is_atom(What) ->
                    brt:file_error(CfgFile, What);
                {'error', _} = Error ->
                    Error;
                Year ->
                    case file:open(CfgFile, ['write']) of
                        {'ok', IoDev} ->
                            try
                                brt_io:write_rebar_config(IoDev, Config, Year)
                            after
                                file:close(IoDev)
                            end;
                        {'error', What} ->
                            brt:file_error(CfgFile, What)
                    end
            end
    end.
