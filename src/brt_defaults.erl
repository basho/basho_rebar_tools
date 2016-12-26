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

-module(brt_defaults).

% API
-export([
    comment_line/2,
    copyright/1,
    copyright/2,
    current_year/0,
    editor_erlang/0,
    editor_makefile/0,
    file_data/1,
    file_terms/1,
    gitignore/1,
    makefile/2,
    rebar_config/4
]).

-include("brt.hrl").

% Module constant instead or recreating it on the fly each time.
-define(COMMENT_DIVIDER,
    "-------------------------------------------------------------------").

%% ===================================================================
%% API
%% ===================================================================

-spec comment_line(Prefix :: string(), Line :: blank | divider | iolist())
        -> iolist().
%%
%% @doc Returns a line comment with specified text and trailing newline.
%%
comment_line(Prefix, []) ->
    comment_line(Prefix, blank);
comment_line(Prefix, blank) ->
    [Prefix, $\n];
comment_line(Prefix, divider) ->
    comment_line(Prefix, ?COMMENT_DIVIDER);
comment_line(Prefix, Line) ->
    [Prefix, $\s, Line, $\n].

-spec copyright(Prefix :: string()) -> iolist().
%%
%% @doc Returns a Basho copyright header comment for the current year.
%%
copyright(Prefix) ->
    make_copyright(Prefix, copyright_year_string(current)).

-spec copyright(Prefix :: string(), StartYear :: brt:basho_year())
        -> iolist().
%%
%% @doc Returns a Basho copyright header comment for starting-current year.
%%
copyright(Prefix, StartYear) ->
    make_copyright(Prefix, copyright_year_string(StartYear, current)).

-spec current_year() -> brt:year1970().
%%
%% @doc Returns the current year in local time.
%%
current_year() ->
    {{Year, _, _}, _} = calendar:local_time(),
    Year.

-spec file_data(FileName :: brt:fs_path()) -> binary().
%%
%% @doc Returns the default data for the specified FileName.
%%
%% The data is read from the suitable defaults file in the application's
%% private directory.
%%
file_data(FileName) ->
    File = ["default." | FileName],
    case brt:read_app_file(priv, File) of
        {ok, Bin} ->
            Bin;
        {error, What} ->
            erlang:error(What)
    end.

-spec file_terms(FileName :: brt:fs_path()) -> [term()].
%%
%% @doc Returns the default terms for the specified FileName.
%%
%% The terms are read from the suitable defaults file in the application's
%% private directory.
%%
file_terms(FileName) ->
    File = ["default." | FileName],
    case brt:consult_app_file(priv, File) of
        {ok, Terms} ->
            Terms;
        {error, What} ->
            erlang:error(What)
    end.

-spec editor_erlang() -> iolist().
%%
%% @doc Returns appropriate editor formatting comment line(s) for Erlang terms.
%%
%% Returned lines, if any, include their trailing newline.
%%
editor_erlang() ->
    comment_line("%%",
        "-*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-").

-spec editor_makefile() -> iolist().
%%
%% @doc Returns appropriate editor formatting comment line(s) for a Makefile.
%%
%% Returned lines, if any, include their trailing newline.
%%
editor_makefile() ->
    [].

-spec gitignore(Native :: boolean()) -> iolist().
%%
%% @doc Returns a Basho standard .gitignore body.
%%
%% If Native is `true' the file includes exclusions for NIF artifacts.
%%
%% Note that the result is binary data (bytes), NOT Unicode characters.
%%
gitignore(true) ->
    [file_data("gitignore"), file_data("gitignore.nif")];
gitignore(_) ->
    file_data("gitignore").

-spec makefile(
        ProdDeps    :: [brt:app_name() | brt:dep_spec()],
        TestDeps    :: [brt:app_name() | brt:dep_spec()])
        -> iolist().
%%
%% @doc Returns a Basho standard Makefile body.
%%
%% The returned content is structured to work with the standard rebar.config.
%%
%% @see rebar_config/3
%%
makefile(ProdDeps, TestDeps) ->
    brt_config:dep_makefile(ProdDeps ++ TestDeps,
        unicode:characters_to_list(file_data("basho.mk"), utf8)).

-spec rebar_config(
        AppName     :: brt:app_name(),
        ProdDeps    :: [brt:app_name() | brt:dep_spec()],
        TestDeps    :: [brt:app_name() | brt:dep_spec()],
        PluginDeps  :: [brt:app_name() | brt:dep_spec()])
        -> brt:rebar_conf().
%%
%% @doc Returns the Basho-standard rebar.config properties list.
%%
%% These are the standard terms used in Basho projects, including `profiles'
%% `deps', and `plugins'.
%% This plugin doesn't need to be specified, it will always be included.
%%
%% Most non-global settings are in three main profiles: `prod', `check',
%% and `test'.
%% These profiles are structured to work with the standard Makefile targets.
%%
%% @see makefile/2
%%
rebar_config(AppName, ProdDeps, TestDeps, PluginDeps) ->
    DefTerms = file_terms("rebar.config"),
    BrtTerms = case brt:get_key_tuple(AppName,
            brt:get_key_list(upstream, brt_config:config())) of
        {_, URL} ->
            BRT = lists:keystore(upstream, 1,
                brt:get_key_list(brt, DefTerms), {upstream, URL}),
            lists:keystore(brt, 1, DefTerms, {brt, BRT});
        _ ->
            DefTerms
    end,
    %
    % Prior to Rebar3 there are no profiles, so there's no opportunity to
    % override the `no_debug_info' pseudo option.
    % If it can't be overridden, don't let it in in the first place.
    %
    Default = case brt_rebar:config_format() < 3 of
        true ->
            Opts  = brt:get_key_list(erl_opts, BrtTerms),
            case lists:member(no_debug_info, Opts) of
                true ->
                    lists:keyreplace(erl_opts, 1, BrtTerms, {erl_opts,
                        lists:delete(no_debug_info, lists:usort(Opts))});
                _ ->
                    BrtTerms
            end;
        _ ->
            BrtTerms
    end,
    AppDeps = lists:foldl(
        fun(AAppOrDep, AResult) ->
            case brt:dep_list_member(AAppOrDep, AResult) of
                true ->
                    AResult;
                _ ->
                    [brt_config:pkg_dep(AAppOrDep) | AResult]
            end
        end, [], ProdDeps),
    TstDeps = lists:foldl(
        fun(TAppOrDep, TResult) ->
            % nest the tests because dialyzer can't grok them orelse'd together
            case brt:dep_list_member(TAppOrDep, TResult) of
                false ->
                    case brt:dep_list_member(TAppOrDep, AppDeps) of
                        false ->
                            [brt_config:pkg_dep(TAppOrDep) | TResult];
                        _ ->
                            TResult
                    end;
                _ ->
                    TResult
            end
        end, [], TestDeps),
    Plugins = brt_config:dep_plugins(TstDeps ++ AppDeps, PluginDeps),
    ErlOpts = brt_config:dep_erl_opts(AppDeps,
                brt:get_key_list(erl_opts, Default)),
    Config1 = update_profiles(Default, AppDeps, TstDeps, Plugins),
    Config2 = lists:keystore(plugins, 1, Config1, {plugins, Plugins}),
    Config3 = lists:keystore(erl_opts, 1, Config2, {erl_opts, ErlOpts}),
    lists:keystore(deps, 1, Config3, {deps, lists:sort(AppDeps)}).

%% ===================================================================
%% Internal
%% ===================================================================

-spec copyright_year_string(Year :: brt:basho_year()) -> iolist().
copyright_year_string(current) ->
    erlang:integer_to_list(current_year());
copyright_year_string(Year)
        when erlang:is_integer(Year) andalso Year >= ?BASHO_YEAR_MIN ->
    erlang:integer_to_list(Year).

-spec copyright_year_string(
        First :: brt:basho_year(), Last :: brt:basho_year()) -> iolist().
copyright_year_string(current, Year) ->
    copyright_year_string(current_year(), Year);
copyright_year_string(Year, current) ->
    copyright_year_string(Year, current_year());
copyright_year_string(Year, Year) ->
    copyright_year_string(Year);
copyright_year_string(First, Last)
        when    erlang:is_integer(First)
        andalso erlang:is_integer(Last)
        andalso First >= ?BASHO_YEAR_MIN
        andalso First < Last ->
    [erlang:integer_to_list(First), $-, erlang:integer_to_list(Last)].

-spec make_copyright(Prefix :: string(), Years :: iolist()) -> iolist().
make_copyright(Prefix, Years) ->
    Lines = [
        divider,
        blank,
        ["Copyright (c) ", Years, " Basho Technologies, Inc."],
        blank,
        "This file is provided to you under the Apache License,",
        "Version 2.0 (the \"License\"); you may not use this file",
        "except in compliance with the License.  You may obtain",
        "a copy of the License at",
        blank,
        "  http://www.apache.org/licenses/LICENSE-2.0",
        blank,
        "Unless required by applicable law or agreed to in writing,",
        "software distributed under the License is distributed on an",
        "\"AS IS\" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY",
        "KIND, either express or implied.  See the License for the",
        "specific language governing permissions and limitations",
        "under the License.",
        blank,
        divider
    ],
    [comment_line(Prefix, Line) || Line <- Lines].

-spec update_profiles(
        Config      :: brt:rebar_conf(),
        ProdDeps    :: [brt:app_name() | brt:dep_spec()],
        TestDeps    :: [brt:app_name() | brt:dep_spec()],
        PluginDeps  :: [brt:app_name() | brt:dep_spec()])
        -> brt:rebar_conf().

update_profiles(Config, ProdDeps, TestDeps, PluginDeps) ->
    ProfIn  = brt:get_key_list(profiles, Config),
    ProfOut = update_profiles(
        ProfIn, Config, ProdDeps, TestDeps, PluginDeps, []),
    lists:keystore(profiles, 1, Config, {profiles, ProfOut}).

-spec update_profiles(
        Sects       :: [brt:rebar_sect()],
        Config      :: brt:rebar_conf(),
        ProdDeps    :: [brt:app_name() | brt:dep_spec()],
        TestDeps    :: [brt:app_name() | brt:dep_spec()],
        PluginDeps  :: [brt:app_name() | brt:dep_spec()],
        Result      :: [brt:rebar_sect()])
        -> [brt:rebar_sect()].

update_profiles(
        [{check = Sect, Terms0} | Sects],
        Config, ProdDeps, TestDeps, PluginDeps, Result) ->
    Over0   = brt:get_key_list(overrides, Terms0),
    Over1   = [{add, Pkg, [{erl_opts, [debug_info]}]}
        || Pkg <- lists:map(fun brt:dep_name/1, ProdDeps)],
    Over2   = lists:usort(Over0 ++ Over1),
    Terms1  = lists:keystore(overrides, 1, Terms0, {overrides, Over2}),
    Update  = {Sect, lists:keysort(1, Terms1)},
    update_profiles(
        Sects, Config, ProdDeps, TestDeps, PluginDeps, [Update | Result]);

update_profiles(
        [{test = Sect, Terms0} | Sects],
        Config, ProdDeps, TestDeps, PluginDeps, Result) ->
    Deps0   = brt:get_key_list(deps, Terms0),
    Deps1   = lists:keymerge(1, lists:sort(Deps0), lists:sort(TestDeps)),
    Terms1  = lists:keystore(deps, 1, Terms0, {deps, Deps1}),
    Opts0   = brt:get_key_list(erl_opts, Terms1),
    Opts1   = brt_config:dep_erl_opts(TestDeps, Opts0),
    Opts2   = lists:subtract(Opts1, brt:get_key_list(erl_opts, Config)),
    Terms2  = lists:keystore(erl_opts, 1, Terms1, {erl_opts, Opts2}),
    Update  = {Sect, lists:keysort(1, Terms2)},
    update_profiles(
        Sects, Config, ProdDeps, TestDeps, PluginDeps, [Update | Result]);

update_profiles(
        [{Sect, Terms} | Sects],
        Config, ProdDeps, TestDeps, PluginDeps, Result) ->
    Update  = {Sect, lists:keysort(1, Terms)},
    update_profiles(
        Sects, Config, ProdDeps, TestDeps, PluginDeps, [Update | Result]);

update_profiles([], _, _, _, _, Result) ->
    lists:keysort(1, Result).

