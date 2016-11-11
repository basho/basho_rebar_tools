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
%% @doc BRT provider for the 'brt-deps' command.
%%
-module(brt_prv_deps).

%% provider behavior
-ifndef(brt_validate).
-behaviour(brt).
-endif.
-export([do/1, format_error/1, init/1]).

-include("brt.hrl").

-define(PROVIDER_ATOM,  'brt-deps').
-define(PROVIDER_STR,   "brt-deps").
-define(PROVIDER_DEPS,  ['compile']).
-define(PROVIDER_OPTS,  [
    {'check', $c, "check", 'boolean',
        "Verify that the true dependencies match the rebar configuration."},
    {'list', $l, "list", 'boolean',
        "List the full dependency specifications."},
    {'short', $s, "short", 'boolean',
        "List the dependency names [default]."}
]).

%% ===================================================================
%% Behavior
%% ===================================================================

-spec init(State :: brt:rebar_state()) -> {'ok', brt:rebar_state()}.
%%
%% @doc Adds the command provider to rebar's state.
%%
init(State) ->
    Provider = providers:create([
        {'name',        ?PROVIDER_ATOM},
        {'module',      ?MODULE},
        {'bare',        'true'},
        {'deps',        ?PROVIDER_DEPS},
        {'example',     "rebar3 " ?PROVIDER_STR},
        {'short_desc',  short_desc()},
        {'desc',        long_desc()},
        {'opts',        ?PROVIDER_OPTS}
    ]),
    {'ok', rebar_state:add_provider(State, Provider)}.

-spec do(State :: brt:rebar_state())
        -> {'ok', brt:rebar_state()} | brt:prv_error().
%%
%% @doc Execute the provider command logic.
%%
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    ?BRT_VAR(Args),
    handle_command(['check', 'list', 'short'], Args, State).

-spec format_error(Error :: term()) -> iolist().
%%
%% @doc Format errors for display.
%%
format_error(Error) ->
    brt:format_error(Error).

%%====================================================================
%% Internal
%%====================================================================

handle_command(['check' = Opt | Opts], Args, State) ->
    case proplists:get_value(Opt, Args) of
        'true' ->
            case brt_xref:app_deps(State) of
                {'ok', ProdApps, _TestApps} ->
                    RebarDeps = lists:sort(
                        [brt:to_atom(R) || R <- rebar_state:deps_names(State)]),
                    ProdDeps = lists:sort(ProdApps),
                    case RebarDeps =:= ProdDeps of
                        'true' ->
                            {'ok', State};
                        _ ->
                            {'error', {?MODULE, 'deps_mismatch'}}
                    end;
                {'error', _} = XRefErr ->
                    XRefErr
            end;
        _ ->
            handle_command(Opts, Args, State)
    end;

handle_command(['list' = Opt | Opts], Args, State) ->
    case proplists:get_value(Opt, Args) of
        'true' ->
            case brt_xref:app_deps(State) of
                {'ok', ProdApps, TestApps} ->
                    io:put_chars("Prod:\n"),
                    brt_io:write_deps('standard_io', 1, lists:sort(ProdApps)),
                    io:put_chars("Test:\n"),
                    brt_io:write_deps('standard_io', 1, lists:sort(TestApps)),
                    {'ok', State};
                {'error', _} = XRefErr ->
                    XRefErr
            end;
        _ ->
            handle_command(Opts, Args, State)
    end;

% 'short' - this is the default, so no need to check.
handle_command(_, _, State) ->
    case brt_xref:app_deps(State) of
        {'ok', ProdApps, TestApps} ->
            io:put_chars("Prod:\n"),
            [io:format("    ~s~n", [PA]) || PA <- lists:sort(ProdApps)],
            io:put_chars("Test:\n"),
            [io:format("    ~s~n", [TA]) || TA <- lists:sort(TestApps)],
            {'ok', State};
        {'error', _} = XRefErr ->
            XRefErr
    end.

short_desc() ->
    "Lists or verifies the true dependencies of the current project".

long_desc() ->
    short_desc().

