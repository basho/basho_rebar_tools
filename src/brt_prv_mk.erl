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
%% @doc BRT provider for the 'brt-mk' command.
%%
-module(brt_prv_mk).

%% provider behavior
-ifndef(brt_validate).
-behaviour(brt).
-endif.
-export([do/1, format_error/1, init/1, spec/0]).

-include("brt.hrl").

-define(PROVIDER_ATOM,  'brt-mk').
-define(PROVIDER_STR,   "brt-mk").
-define(PROVIDER_DEPS,  ['compile']).
-define(PROVIDER_OPTS,  [
    ?BRT_RECURSIVE_OPT,
    ?BRT_CHECKOUTS_OPT,
    ?BRT_LOOSE_OPT,
    {'name', $n, "name", 'string',
        "Prepend <name> to the candidate makefile names. "
        "By default, the only candidate is \"Makefile\". "
        "When writing, the outpust is always to the first filename in the "
        "candidate list, whether it exists or not. "
        "When reading, the list is traversed until an existing file is "
        "found, and if no such file exists defaults are used. "
        "This behavior allows writing an updated version of a file with a new "
        "name, leaving the original intact."},
    ?BRT_VERBOSITY_OPTS
]).

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

-spec do(State :: brt:rebar_state())
        -> {'ok', brt:rebar_state()} | brt:prv_error().
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

-spec short_desc() -> nonempty_string().
short_desc() ->
    "Create or update project Makefile(s).".

-spec long_desc() -> nonempty_string().
long_desc() ->
    "Creates or OVERWRITES one or more Makefiles.\n"
    "\n"
    "When updating an existing file, especially in a forked repository, "
    "the span of the generated copyright should be sanity checked, as the "
    "earliest commit of the file is used as the start of the copyright year "
    "range when a pre-existing copyright header is not present.\n".

%%====================================================================
%% Internal
%%====================================================================

-type context() ::  {brt_xref:xref(), boolean(), [brt:fs_path()]}.

-spec handle_command(
        Opts :: [proplists:property()], State :: brt:rebar_state())
        -> {'ok', brt:rebar_state()} | brt:prv_error().
handle_command(Opts, State) ->
    case brt_xref:new(State) of
        {'ok', XRef} ->
            FileNames = case proplists:get_value('name', Opts, "Makefile") of
                "Makefile" ->
                    ["Makefile"];
                File ->
                    [File, "Makefile"]
            end,
            Loose   = proplists:get_value('loose', Opts, 'false'),
            Context = {XRef, Loose, FileNames},
            Select  = case proplists:get_value('recurse', Opts) of
                'true' ->
                    case proplists:get_value('checkouts', Opts) of
                        'true' ->
                            fun brt_rebar:in_prj_or_checkouts/2;
                        _ ->
                            'all'
                    end;
                _ ->
                    brt_rebar:prj_app_specs(State)
            end,
            Result = brt_rebar:fold(Select, fun update/3, Context, State),
            brt_xref:stop(XRef),
            case Result of
                {'ok', _} ->
                    {'ok', State};
                Err ->
                    Err
            end;
        Error ->
            Error
    end.

-spec update(
        App     :: brt:app_spec(),
        Context :: context(),
        State   :: brt:rebar_state())
        -> {'ok', context()} | brt:prv_error().

update({Name, Path, _} = App, {XRef, Loose, FileNames} = Context, _State) ->
    rebar_api:debug("~s:update/3: App = ~p", [?MODULE, App]),
    case brt_xref:app_deps(XRef, Name) of
        {'ok', Deps} ->
            Content = brt_defaults:makefile(
                Deps, brt_fudge:test_deps(Path) -- Deps),
            FileOut = filename:absname(erlang:hd(FileNames), Path),
            FileIn  = case brt:find_first('file', FileNames, [Path]) of
                'false' ->
                    FileOut;
                File ->
                    File
            end,
            case brt_rebar:copyright_info(FileIn, Loose, 'sh') of
                {'error', _} = Error ->
                    Error;
                CpyInfo ->
                    case file:open(FileOut, ['write']) of
                        {'ok', IoDev} ->
                            Result = brt_io:write_makefile(
                                IoDev, Content, CpyInfo),
                            _ = file:close(IoDev),
                            case Result of
                                'ok' ->
                                    {'ok', Context};
                                _ ->
                                    Result
                            end;
                        {'error', What} ->
                            brt:file_error(FileOut, What)
                    end
            end;
        Error ->
            Error
    end.
