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
%% @doc This module contains kludges that should go away.
%%
%% @deprecated Use other BRT modules.
%%
-module(brt_fudge).
%-deprecated(module).

-export([
    test_deps/1
]).

-include("brt.hrl").

-spec test_deps(Where :: brt:app_spec() | brt:fs_path() | brt:rebar_state())
        -> [brt:app_name()] | brt:err_result().
%
% It would be better to use xref on test modules, which would pick up meck and
% the like (including quickcheck, which would need to be filtered out), but
% for now just look for the pattern we use for cuttlefish schema files and
% assume we actually test them.
%
% Proper results will be obtained by compiling with the 'prod' and 'test'
% profiles, running xref on both, and diffing the results to come up the apps
% that are used only in test mode.
% That can be handled with some some plugin magic to swap profiles on the fly,
% but I'm not biting it off right now.
%
test_deps({Name, Path, _}) ->
    test_deps([Name], Path);

test_deps(Path) when erlang:is_list(Path) ->
    Func = fun(File) ->
        case file:consult(File) of
            {'ok', [{'application', App, [_|_]}]} ->
                App;
            {'ok', _} ->
                erlang:error('app_malformed', [File]);
            {'error', What} ->
                {'error', Error} = brt:file_error(File, What),
                erlang:error(Error)
        end
    end,
    Apps = lists:map(Func,
        filelib:wildcard(filename:join([Path, "ebin", "*.app"]))),
    Names = case Apps of
        [_] ->
            Apps;
        _ ->
            Name = erlang:list_to_atom(erlang:hd(
                string:tokens(filename:basename(Path), "-"))),
            case lists:member(Name, Apps) of
                'true' ->
                    Apps;
                _ ->
                    [Name | Apps]
            end
    end,
    test_deps(Names, Path);

test_deps(State) when ?is_rebar_state(State) ->
    test_deps(
        [rebar_app_info:name(AI) || AI <- rebar_state:project_apps(State)],
        rebar_state:dir(State)).

test_deps(['cuttlefish' | _], _) ->
    [];
test_deps([Name | Names], Path) ->
    Schema = filename:join([Path, "priv", brt:to_string(Name) ++ ".schema"]),
    case filelib:is_regular(Schema) of
        'true' ->
            ['cuttlefish'];
        _ ->
            test_deps(Names, Path)
    end;
test_deps([], _) ->
    [].
