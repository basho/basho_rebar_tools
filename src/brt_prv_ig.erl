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
%% @doc BRT provider for the `brt-ig' command.
%%
-module(brt_prv_ig).

%% provider behavior
-ifndef(BRT_VALIDATE).
-behaviour(brt).
-endif.
-export([do/1, format_error/1, init/1, spec/0]).

-include("brt.hrl").

-define(PROVIDER_ATOM,  'brt-ig').
-define(PROVIDER_STR,   "brt-ig").
-define(PROVIDER_DEPS,  []).
-define(PROVIDER_OPTS,  [
    {native, $n, "nif", boolean,
        "Generate a file with additional exclusions for NIF artifacts."},
    ?BRT_VERBOSITY_OPTS
]).

%% ===================================================================
%% Behavior
%% ===================================================================

-spec init(State :: brt:rebar_state()) -> {ok, brt:rebar_state()}.
%%
%% @doc Adds the command provider to rebar's state.
%%
init(State) ->
    Provider = providers:create(spec()),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(State :: brt:rebar_state())
        -> {ok, brt:rebar_state()} | brt:prv_error().
%%
%% @doc Execute the provider command logic.
%%
do(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    handle_command(Opts, State).

-spec format_error(Error :: term()) -> iolist().
%%
%% @doc Format errors for display.
%%
format_error(Error) ->
    brt:format_error(Error).

-spec spec() -> [{atom(), term()}].
%%
%% @doc Return the proplist that will be supplied to providers:create/1.
%%
spec() ->
    [
        {name,          ?PROVIDER_ATOM},
        {module,        ?MODULE},
        {bare,          true},
        {deps,          ?PROVIDER_DEPS},
        {example,       "rebar3 " ?PROVIDER_STR},
        {short_desc,    short_desc()},
        {desc,          long_desc()},
        {opts,          ?PROVIDER_OPTS}
    ].

%%====================================================================
%% Help Text
%%====================================================================

-spec short_desc() -> nonempty_string().
short_desc() ->
    "Creates or OVERWRITES the .gitignore file in the current directory.".

-spec long_desc() -> nonempty_string().
long_desc() ->
    short_desc() ++
    "\n"
    "This is a convenience operation and has none of the recursion and naming "
    "options found in other providers, as that would require calculating "
    "whether dependencies include NIFs.\n".

%%====================================================================
%% Internal
%%====================================================================

-spec handle_command(
    Opts :: [proplists:property()], State :: brt:rebar_state())
        -> {ok, brt:rebar_state()} | brt:prv_error().
handle_command(Opts, State) ->
    Body = brt_defaults:gitignore(proplists:get_value(native, Opts, false)),
    File = ".gitignore",
    case file:write_file(File, Body) of
        ok ->
            {ok, State};
        {error, What} ->
            brt:file_error(File, What)
    end.
