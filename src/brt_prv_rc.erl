%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016-2017 Basho Technologies, Inc.
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
%% @doc BRT provider for the `brt-rc' command.
%%
-module(brt_prv_rc).

%% provider behavior
-ifndef(BRT_VALIDATE).
-behaviour(brt).
-endif.
-export([do/1, format_error/1, spec/0]).

-include("brt.hrl").

-define(PROVIDER_ATOM,  'brt-rc').
-define(PROVIDER_STR,   "brt-rc").
-define(PROVIDER_DEPS,  [compile]).
-define(PROVIDER_OPTS,  [
    {deps, $d, "deps", boolean,
        "Only update the target file's global 'deps' section. NOTE that even "
        "with a partial update any comments in the file will be lost, as it "
        "is read and written as Erlang terms."},
    ?BRT_RECURSIVE_OPT,
    ?BRT_CHECKOUTS_OPT,
    {skip, $s, "skip", boolean,
        "When operating recursively (-r|--recursive), skip rebar.config "
        "files whose brt.protect flag disallows updates. "
        "A warning, rather than an error, is issued for such files."},
    ?BRT_LOOSE_OPT,
    {name, $n, "name", string,
        "Prepend <name> to the candidate rebar.config file names. "
        "By default, candidates are $REBAR_CONFIG (if it's set) followed by "
        "\"rebar.config\". "
        "When writing, the outpust is always to the first filename in the "
        "candidate list, whether it exists or not. "
        "When reading, the list is traversed until an existing file is "
        "found, and if no such file exists defaults are used. "
        "This behavior allows writing an updated version of a file with a new "
        "name, leaving the original intact."},
    {rebar2, $2, "rebar2", boolean,
        "Write rebar.config elements in Rebar2 format."},
    ?BRT_VERBOSITY_OPTS
]).

%% ===================================================================
%% Behavior
%% ===================================================================

-spec do(State :: brt:rebar_state())
        -> {ok, brt:rebar_state()} | brt:prv_error().
%%
%% @doc Execute the provider command logic.
%%
do(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    ?LOG_DEBUG("~s:do/1: Opts = ~p", [?MODULE, Opts]),
    ResetVersion = case proplists:get_value(rebar2, Opts) of
        true ->
            brt_rebar:config_format(rebar2);
        _ ->
            brt_rebar:config_format(default)
    end,
    FileNames = brt_rebar:cfg_file_names(),
    ResetNames = case proplists:get_value(name, Opts, false) of
        false ->
            FileNames;
        Name ->
            case lists:member(Name, FileNames) of
                true ->
                    case FileNames of
                        [Name | _] ->
                            FileNames;
                        _ ->
                            brt_rebar:cfg_file_names(
                                [Name | lists:delete(Name, FileNames)])
                    end;
                _ ->
                    brt_rebar:cfg_file_names([Name | FileNames])
            end
    end,
    Result = handle_command(Opts, State),
    _ = brt_rebar:config_format(ResetVersion),
    _ = brt_rebar:cfg_file_names(ResetNames),
    Result.

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
    "Create or update project rebar.config file(s).".

-spec long_desc() -> nonempty_string().
long_desc() ->
    "Creates or OVERWRITES one or more rebar.config files.\n"
    "\n"
    "When updating an existing file, especially in a forked repository, "
    "the span of the generated copyright should be sanity checked, as the "
    "earliest commit of the file is used as the start of the copyright year "
    "range when a pre-existing copyright header is not present.\n".

%%====================================================================
%% Internal
%%====================================================================

-record(ctx, {
    xref    :: brt_xref:xref(),
    deps    :: boolean(),
    loose   :: boolean(),
    skip    :: boolean()
}).
-type context() ::  #ctx{}.

-spec all_deps(CalcDeps :: [brt:app_name()]) -> [brt:app_name()].

all_deps(CalcDeps) ->
    Forced  = brt:get_key_list(forced, brt_config:config()),
    lists:usort(lists:append(Forced, CalcDeps)).

-spec create_rebar_config(
    File        :: brt:fs_path(),
    App         :: brt:app_spec(),
    XrefDeps    :: [brt:app_name()],
    CpyInfo     :: current | brt:basho_year() | iolist(),
    WarnBlocked :: boolean())
        -> ok | brt:prv_error().

create_rebar_config(File, {Name, _, _} = App, XrefDeps, CpyInfo, WarnBlocked) ->
    AllDeps = all_deps(XrefDeps),
    PrdDeps = lists:delete(Name, AllDeps),
    TstDeps = lists:subtract(brt_fudge:test_deps(App), AllDeps),
    Config  = brt_defaults:rebar_config(Name, PrdDeps, TstDeps, []),
    write_rebar_config(File, Config, CpyInfo, WarnBlocked).

-spec handle_command(
        Opts :: [proplists:property()], State :: brt:rebar_state())
        -> {ok, brt:rebar_state()} | brt:prv_error().

handle_command(Opts, State) ->
    ?LOG_INFO("Calculating dependencies...", []),
    case brt_xref:new(State) of
        {ok, XRef} ->
            Recurse = proplists:get_value(recurse, Opts),
            Context = #ctx{
                xref    = XRef,
                deps    = proplists:get_value(deps, Opts, false),
                loose   = proplists:get_value(loose, Opts, false),
                skip    = Recurse /= true andalso
                        proplists:get_value(skip, Opts) == true
            },
            Select = case Recurse of
                true ->
                    case proplists:get_value(checkouts, Opts) of
                        true ->
                            fun brt_rebar:in_prj_or_checkouts/2;
                        _ ->
                            all
                    end;
                _ ->
                    brt_rebar:prj_app_specs(State)
            end,
            Result = brt_rebar:fold(Select, fun update/3, Context, State),
            brt_xref:stop(XRef),
            case Result of
                {ok, _} ->
                    {ok, State};
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
        -> {ok, context()} | brt:prv_error().

update({Name, Path, _} = App, #ctx{xref = XRef} = Context, _State) ->
    ?LOG_DEBUG("~s:update/3: App = ~p", [?MODULE, App]),
    case brt_xref:app_deps(XRef, Name) of
        {ok, XrefDeps} ->
            FileNames = brt_rebar:cfg_file_names(),
            case update_rebar_config(
                    brt:find_first(file, FileNames, [Path]),
                    filename:absname(erlang:hd(FileNames), Path),
                    App, XrefDeps, Context) of
                ok ->
                    {ok, Context};
                Err ->
                    Err
            end;
        Error ->
            Error
    end.

-spec update_rebar_config(
    FileIn      :: brt:fs_path() | false,
    FileOut     :: brt:fs_path(),
    App         :: brt:app_spec(),
    XrefDeps    :: [brt:app_name()],
    Context     :: context())
        -> ok | brt:prv_error().

update_rebar_config(false, _, {_, Path, _}, _, #ctx{deps = true}) ->
    {error, {?MODULE, {no_rebar_config, Path}}};

update_rebar_config(
        false, File, {Name, _, _} = App, XrefDeps, #ctx{skip = Skip}) ->
    ?LOG_INFO("Writing ~s:~s", [Name, filename:basename(File)]),
    create_rebar_config(File, App, XrefDeps, current, Skip);

update_rebar_config(
        FileIn, FileOut, {Name, _, _} = App, XrefDeps,
        #ctx{deps = DepsOnly, loose = Loose, skip = Skip}) ->
    if
        FileIn == FileOut ->
            ?LOG_INFO("Updating ~s:~s", [Name, filename:basename(FileOut)]);
        ?else ->
            ?LOG_INFO("Updating ~s:~s from ~s", [Name,
                filename:basename(FileOut), filename:basename(FileIn)])
    end,
    case brt_rebar:copyright_info(FileIn, Loose, erl) of
        {error, _} = CpyErr ->
            CpyErr;
        CpyInfo ->
            case DepsOnly of
                true ->
                    case file:consult(FileIn) of
                        {ok, Terms} ->
                            Deps = [brt_config:pkg_dep(A)
                                || A <- lists:delete(Name, all_deps(XrefDeps))],
                            Conf = lists:keystore(
                                deps, 1, Terms, {deps, Deps}),
                            write_rebar_config(FileOut, Conf, CpyInfo, Skip);
                        {error, What} ->
                            brt:file_error(FileIn, What)
                    end;
                _ ->
                    create_rebar_config(FileOut, App, XrefDeps, CpyInfo, Skip)
            end
    end.

-spec write_rebar_config(
    File        :: brt:fs_path(),
    Config      :: brt:rebar_conf(),
    CpyInfo     :: current | brt:basho_year() | iolist(),
    WarnBlocked :: boolean())
        -> ok | brt:prv_error().

write_rebar_config(File, Config, CpyInfo, WarnBlocked) ->
    case brt_config:overwrite_blocked(File) of
        true ->
            case WarnBlocked of
                true ->
                    ?LOG_WARN("~s: ~s",
                        [format_error(overwrite_blocked), File]);
                _ ->
                    {error, {?MODULE, {overwrite_blocked, File}}}
            end;
        _ ->
            case file:open(File, [write]) of
                {ok, IoDev} ->
                    Result = brt_io:write_rebar_config(IoDev, Config, CpyInfo),
                    _ = file:close(IoDev),
                    Result;
                {error, What} ->
                    brt:file_error(File, What)
            end
    end.
