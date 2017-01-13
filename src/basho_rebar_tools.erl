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
-ifdef(BRT_CHECK).
-behaviour(provider).
-endif.

%% provider behavior
-export([do/1, format_error/1, init/1]).

-include("brt.hrl").

% This is the version that contains the profile precedence fix.
-define(REBAR_MIN_VSN,  [3, 3, 3]).

%%====================================================================
%% provider API
%%====================================================================

-spec init(State :: brt:rebar_state()) -> {ok, brt:rebar_state()}.
%%
%% @doc Initializes State with the plugin's command providers.
%%
%% This provider performs no actions itself.
%%
init(StateIn) ->
    case provider_modules(StateIn) of
        [] ->
            {ok, StateIn};
        Mods ->
            {ok, State} = init_prov_env(Mods, StateIn),
            % There's no application at this point, so don't try to
            % get its directory - it will be `undefined'.
            case brt_config:init([rebar_state:dir(State)]) of
                ok ->
                    init_providers(Mods, {ok, State});
                {error, What} ->
                    erlang:error(What)
            end
    end.

-spec do(State :: brt:rebar_state()) -> {ok, brt:rebar_state()}.
%%
%% @doc Placeholder to fill out the `provider' API, should never be called.
%%
%% This provider performs no actions itself.
%%
do(State) ->
    {ok, State}.

-spec format_error(Error :: term()) -> iolist().
%%
%% @doc Placeholder to fill out the `provider' API, should never be called.
%%
%% This provider performs no actions itself.
%%
format_error(Error) ->
    brt:format_error(Error).

%%====================================================================
%% Internal
%%====================================================================

-spec check_rebar_version() -> ok.
%
% Check the Rebar version and warn or raise an error accordingly.
%
check_rebar_version() ->
    {ok, RebarVsnStr} = application:get_key(rebar, vsn),
    RebarVsn = brt:parse_version(RebarVsnStr),
    case brt:is_min_version(?REBAR_MIN_VSN, RebarVsn) of
        true ->
            ok;
        _ ->
            VsnStr = case RebarVsn of
                [N | _] when erlang:is_integer(N) ->
                    brt:version_string(RebarVsn);
                _ ->
                    RebarVsnStr
            end,
            ?LOG_WARN(
                "Using Rebar ~s may not work properly."
                " Upgrade to ~s or higher.",
                [VsnStr, brt:version_string(?REBAR_MIN_VSN)])
    end.

-spec command_mod_spec(Cmd :: atom() | string(), Mods :: [module()])
        -> {atom(), module(), brt:prv_spec()} | false.
%
% Find the module and provider spec for the specified command.
%
command_mod_spec(Cmd, Mods) when not erlang:is_atom(Cmd) ->
    command_mod_spec(brt:to_atom(Cmd), Mods);
command_mod_spec(Cmd, [Mod | Mods]) ->
    Spec = Mod:spec(),
    case brt:get_key_value(name, Spec) of
        Cmd ->
            {Cmd, Mod, Spec};
        _ ->
            command_mod_spec(Cmd, Mods)
    end;
command_mod_spec(_, []) ->
    false.

-spec init_prov_env(Mods :: [module()], State :: brt:rebar_state())
        -> {ok, brt:rebar_state()}.
%
% If one of our commands has been invoked:
% - Check the Rebar version.
% - See if there are any options requiring early adjustment of the log level.
%
init_prov_env(Mods, State) ->
    %
    % The Rebar state's `command_args' element hasn't been populated at this
    % point, so we have to prowl around a bit depending on how Rebar was
    % invoked.
    %
    Caller = rebar_state:get(State, caller),
    [CmdIn | Args] = case Caller of
        command_line ->
            % first element is the escript path
            erlang:tl(init:get_plain_arguments());
        _ ->
            rebar_state:get(State, task)
    end,
    case command_mod_spec(CmdIn, Mods) of
        {_Cmd, _Mod, Spec} ->
            check_rebar_version(),
            maybe_adjust_log_level(Spec, Args, Caller),
            {ok, State};
        false ->
            {ok, State}
    end.

-spec init_providers(
    Mods :: [module()], Result :: {ok, brt:rebar_state()} | brt:err_result())
        -> {ok, brt:rebar_state()} | brt:err_result().
%
% Initialize the command providers.
% Yes, this could be done with lists:foldl/3, since providers' init/1 function
% is only (currently) allowed to return success, but doing it explicitly here
% allows:
% - Bailing out early if we decide to return errors at some point.
% - Instrumenting the calls to work out the kinks that are a natural part of
%   developing rebar providers.
%
init_providers([Mod | Mods], {ok, State}) ->
    % ?BRT_VAR(Mod),
    init_providers(Mods, Mod:init(State));
init_providers(_, Result) ->
    Result.

-spec maybe_adjust_log_level(
    Spec :: brt:prv_spec(), Args :: [string()], Caller :: atom()) -> ok.
%
% If the `quiet' command line switch was given to a BRT command ensure the
% log level is no higher than `error'.
% If the `warn' command line switch was given to a BRT command ensure the
% log level is no higher than `warn'.
%
maybe_adjust_log_level(Spec, Args, Caller) ->
    case getopt:parse(brt:get_key_list(opts, Spec), Args) of
        {ok, {Opts, _}} ->
            CurLevel = rebar_log:get_level(),
            MaxLevel = case proplists:get_value(quiet, Opts, false) of
                true ->
                    rebar_log:error_level();
                _ ->
                    case proplists:get_value(warn, Opts, false) of
                        true ->
                            (rebar_log:error_level() + 1);
                        _ ->
                            CurLevel
                    end
            end,
            case MaxLevel < CurLevel of
                true ->
                    rebar_log:init(Caller, MaxLevel),
                    ok;
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

-spec provider_modules(State :: brt:rebar_state()) -> [module()].
%
% Return the list of modules in this application implementing command
% providers.
%
provider_modules(_State) ->
    lists:filter(
        fun brt:implements_behaviour/1,
        brt:list_modules(?PRV_MOD_PREFIX)).
