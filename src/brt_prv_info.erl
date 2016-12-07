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
%% @doc BRT provider for the 'brt-info' command.
%%
-module(brt_prv_info).

%% provider behavior
-ifndef(brt_validate).
-behaviour(brt).
-endif.
-export([do/1, format_error/1, init/1, spec/0]).

-include("brt.hrl").

-define(PROVIDER_ATOM,  'brt-info').
-define(PROVIDER_STR,   "brt-info").
-define(PROVIDER_DEPS,  []).
-define(PROVIDER_OPTS,  []).

%% ===================================================================
%% Behavior
%% ===================================================================

-spec init(State :: brt:rebar_state()) -> {'ok', brt:rebar_state()}.
%%
%% @doc Adds the command provider to rebar's state.
%%
init(State) ->
    Provider = providers:create(spec()),
    {'ok', rebar_state:add_provider(State, Provider)}.

-spec do(State :: brt:rebar_state()) -> {'ok', brt:rebar_state()}.
%%
%% @doc Display provider information.
%%
do(State) ->
    {brt_io:write_info('standard_io'), State}.

-spec format_error(Error :: term()) -> iolist().
%%
%% @doc Placeholder to fill out the 'provider' API, should never be called.
%%
format_error(Error) ->
    brt:format_error(Error).

-spec spec() -> [{atom(), term()}].
%%
%% @doc Return the proplist that will be supplied to providers:create/1.
%%
spec() ->
    [
        {'name',        ?PROVIDER_ATOM},
        {'module',      ?MODULE},
        {'bare',        'true'},
        {'deps',        ?PROVIDER_DEPS},
        {'example',     "rebar3 " ?PROVIDER_STR},
        {'short_desc',  short_desc()},
        {'desc',        long_desc()},
        {'opts',        ?PROVIDER_OPTS}
    ].

%%====================================================================
%% Help Text
%%====================================================================

-spec short_desc() -> string().
short_desc() ->
    "Information about " ?APP_NAME_DISPLAY.

-spec long_desc() -> string().
long_desc() ->
    short_desc().

%%====================================================================
%% Internal
%%====================================================================

