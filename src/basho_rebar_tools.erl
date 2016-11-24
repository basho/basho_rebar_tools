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

-module(basho_rebar_tools).
%
% For efficiency in production use, we don't have a dependency on rebar
% itself, so the behaviors this module implements aren't always visible.
%
-ifdef(brt_check).
-behaviour(provider).
-endif.

%% provider behavior
-export([do/1, format_error/1, init/1]).

-include("brt.hrl").

%%====================================================================
%% provider API
%%====================================================================

-spec init(State :: brt:rebar_state()) -> {'ok', brt:rebar_state()}.
%%
%% @doc Initializes State with the plugin's command providers.
%%
%% This provider performs no actions itself.
%%
init(State) ->
    case provider_modules(State) of
        [] ->
            {'ok', State};
        Mods ->
            % There's no application at this point, so don't try to get its
            % directory - it will be 'undefined'.
            case brt_config:init([rebar_state:dir(State)]) of
                'ok' ->
                    init_providers(Mods, {'ok', State});
                {'error', What} ->
                    erlang:error(What)
            end
    end.

-spec do(State :: brt:rebar_state()) -> {'ok', brt:rebar_state()}.
%%
%% @doc Placeholder to fill out the 'provider' API, should never be called.
%%
%% This provider performs no actions itself.
%%
do(State) ->
    {'ok', State}.

-spec format_error(Error :: term()) -> iolist().
%%
%% @doc Placeholder to fill out the 'provider' API, should never be called.
%%
%% This provider performs no actions itself.
%%
format_error(Error) ->
    io_lib:format("~p", [Error]).

%%====================================================================
%% Internal
%%====================================================================

-spec init_providers(Mods :: [module()], State :: {'ok', brt:rebar_state()})
        -> {'ok', brt:rebar_state()}.
%
% Initialize the command providers.
% Yes, this could be done with lists:foldl/3, but doing it explicitly here is
% handy for working out the kinks that are a natural part of developing rebar
% providers.
%
init_providers([Mod | Mods], {'ok', State}) ->
    init_providers(Mods, Mod:init(State));
init_providers([], Result) ->
    Result.

-spec provider_modules(State :: brt:rebar_state()) -> [module()].
%
% Return the list of modules in this application implementing command
% providers.
%
provider_modules(_State) ->
    lists:filter(
        fun brt:implements_behaviour/1,
        brt:list_modules(?PRV_MOD_PREFIX)).
