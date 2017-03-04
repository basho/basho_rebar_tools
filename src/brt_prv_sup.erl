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
%% @doc BRT provider for the `brt-sup' command.
%%
-module(brt_prv_sup).

%% provider behavior
-ifndef(BRT_VALIDATE).
-behaviour(brt).
-endif.
-export([do/1, format_error/1, spec/0]).

-include("brt.hrl").

-define(PROVIDER_ATOM,  'brt-sup').
-define(PROVIDER_STR,   "brt-sup").
-define(PROVIDER_DEPS,  [lock]).
-define(PROVIDER_OPTS,  [
    {gitignore, $g, "gitignore", boolean,
        "Create or overwrite .gitignore [default]."},
    {thumbs, $t, "thumbs", boolean,
        "Create or overwrite .thumbs.yml [default]."},
    {makefile, $m, "makefile", boolean,
        "Create or overwrite Makefile."},
    {ct, $c, "ct", boolean,
        "Generate file(s) appropriate for applications with Common Tests."},
    {escript, $e, "escript", boolean,
        "Generate file(s) appropriate for applications with an EScript."},
    {native, $n, "nif", boolean,
        "Generate file(s) appropriate for applications with NIF artifacts."},
    ?BRT_VERBOSITY_OPTS
]).

-define(NO_ESCRIPT_WARNING,
    "The --escript option was given, but neither an explicit escript nor a "
    "main application is defined. The option is ignored. Use the rebar3 "
    "template directly, optionally with a 'name' argument, to force use of "
    "escript templates."
).

-define(NO_NIFNAME_WARNING,
    "The --native option was given, but no main application is defined. The "
    "option is ignored. Use the rebar3 template directly, optionally with a "
    "'name' argument, to force use of NIF templates."
).

%% Keep the function around for later use.
-dialyzer({no_match, set_template_var/4}).

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
    Tgts = [T || T <- [gitignore, thumbs, makefile],
        proplists:get_value(T, Opts, false)],
    Targets = case Tgts of
        [] ->
            % defaults
            [gitignore, thumbs];
        _ ->
            Tgts
    end,
    Vars = native_opt_vars(escript_opt_vars([], Opts, State), Opts, State),
    handle_command(Targets, Vars, Opts, State).

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
    "Creates or OVERWRITES supporting files in the current directory.".

-spec long_desc() -> nonempty_string().
long_desc() ->
    short_desc() ++
    "\n\n"
    "This is a convenience operation and has no recursion or naming options. "
    "It operates strictly in the current directory, and overwrites files "
    "without warnings.\n"
    "Operations in this provider can all be accomplished individually using "
    "Rebar's template capabilities - run \"rebar3 new\" for the list of "
    "templates.\n\n"
    "If no target file options are provided, the default is "
    "--thumbs --gitignore.\n\n"
    "Makefiles are discouraged in Rebar3 projects; they are supported for "
    "their utility during development. "
    "If you use them, consider *not* committing them, and adding them to "
    ".gitignore instead.\n".

%%====================================================================
%% Internal
%%====================================================================

-spec handle_command(
    Targets :: [gitignore | thumbs | makefile],
    Vars    :: [{atom(), string()}],
    Opts    :: [proplists:property()],
    State   :: brt:rebar_state())
        -> {ok, brt:rebar_state()} | brt:prv_error().

handle_command([gitignore | Targets], VarsIn, Opts, State) ->
    FileName = ".gitignore",
    TmplBase = "ig",
    case prepare_file_vars(FileName, VarsIn) of
        Vars when erlang:is_list(Vars) ->
            Template = case proplists:get_value(native, Opts) of
                true ->
                    TmplBase ++ "-n";
                _ ->
                    case proplists:get_value(escript, Opts) of
                        true ->
                            TmplBase ++ "-es";
                        _ ->
                            TmplBase
                    end
            end,
            case rebar_templater:new(Template, Vars, true, State) of
                ok ->
                    handle_command(Targets, VarsIn, Opts, State);
                RebarErr ->
                    format_error(RebarErr)
            end;
        Error ->
            Error
    end;

handle_command([thumbs | Targets], VarsIn, Opts, State) ->
    FileName = ".thumbs.yml",
    TmplBase = "th",
    case prepare_file_vars(FileName, VarsIn) of
        Vars when erlang:is_list(Vars) ->
            TmplPart = case proplists:get_value(ct, Opts) of
                true ->
                    TmplBase ++ "-ct";
                _ ->
                    TmplBase
            end,
            Template = case proplists:get_value(escript, Opts) of
                true ->
                    TmplPart ++ "-es";
                _ ->
                    TmplPart
            end,
            case rebar_templater:new(Template, Vars, true, State) of
                ok ->
                    handle_command(Targets, VarsIn, Opts, State);
                RebarErr ->
                    format_error(RebarErr)
            end;
        Error ->
            Error
    end;

handle_command([makefile | Targets], VarsIn, Opts, State) ->
    FileName = "Makefile",
    Template = "mk",
    case prepare_file_vars(FileName, VarsIn) of
        Vars when erlang:is_list(Vars) ->
            case rebar_templater:new(Template, Vars, true, State) of
                ok ->
                    handle_command(Targets, VarsIn, Opts, State);
                RebarErr ->
                    format_error(RebarErr)
            end;
        Error ->
            Error
    end;

handle_command([], _, _, State) ->
    {ok, State}.

-spec prepare_file_vars(File :: string(), Vars :: [{atom(), string()}])
        -> [{atom(), string()}] | brt:prv_error().
%
% Populates template context from the target file.
%
prepare_file_vars(File, Vars) ->
    case brt_io:copyright_info(File, sh) of
        {basho, StartYear} ->
            case brt_defaults:current_year() of
                CurYear when CurYear > StartYear ->
                    Range = lists:flatten([
                        erlang:integer_to_list(StartYear), $-,
                        erlang:integer_to_list(CurYear)]),
                    lists:keystore(
                        copyright_year, 1, Vars, {copyright_year, Range});
                _ ->
                    Vars
            end;
        {other, _} ->
            ?LOG_WARN(
                "~s: file contains ambiguous or non-Basho copyright, "
                "verify changes before committing.", [File]),
            Vars;
        none ->
            Vars;
        {error, enoent} ->
            Vars;
        {error, What} when erlang:is_atom(What) ->
            brt:file_error(File, What);
        {error, What} ->
            {error, ?MODULE, What}
    end.

-spec escript_opt_vars(
    Vars    :: [{atom(), string()}],
    Opts    :: [{atom(), term()}],
    State   :: brt:rebar_state())
        -> [{atom(), string()}] | brt:prv_error().
%%
%% Populates escript template context from options.
%%
escript_opt_vars(Vars, Opts, State) ->
    case proplists:get_value(escript, Opts) of
        true ->
            case rebar_state:get(State, escript_main_app, undefined) of
                undefined ->
                    set_teplate_prj_app(
                        name, State, Vars, false, ?NO_ESCRIPT_WARNING);
                AppName ->
                    set_template_var(name, AppName, Vars, true)
            end;
        _ ->
            Vars
    end.

-spec native_opt_vars(
    Vars    :: [{atom(), string()}],
    Opts    :: [{atom(), term()}],
    State   :: brt:rebar_state())
        -> [{atom(), string()}] | brt:prv_error().
%%
%% Populates NIF/port template context from options.
%% Lower priority than escript name, so it defers to a previously set value.
%%
native_opt_vars(Vars, Opts, State) ->
    case proplists:get_value(native, Opts) of
        true ->
            set_teplate_prj_app(
                name, State, Vars, false, ?NO_NIFNAME_WARNING);
        _ ->
            Vars
    end.

-spec set_teplate_prj_app(
    Name    :: atom(),
    State   :: brt:rebar_state(),
    Vars    :: [{atom(), string()}],
    Force   :: boolean(),
    Warn    :: string())
        -> [{atom(), string()}].

set_teplate_prj_app(Name, State, Vars, true, Warn) ->
    case rebar_state:project_apps(State) of
        [AppInfo | _] ->
            set_template_var(
                Name, rebar_app_info:name(AppInfo), Vars, true);
        _ ->
            Key = {?MODULE, prj_app_warned},
            case erlang:get(Key) of
                true ->
                    Vars;
                _ ->
                    ?LOG_WARN(Warn, []),
                    _ = erlang:put(Key, true),
                    Vars
            end
    end;
set_teplate_prj_app(Name, State, Vars, false, Warn) ->
    case lists:keymember(Name, 1, Vars) of
        true ->
            Vars;
        _ ->
            set_teplate_prj_app(Name, State, Vars, true, Warn)
    end.

-spec set_template_var(
    Name    :: atom(),
    Value   :: atom() | binary()| [char() | [char()]],
    Vars    :: [{atom(), string()}],
    Force   :: boolean())
        -> [{atom(), string()}].

set_template_var(Name, Value, Vars, true) ->
    lists:keystore(Name, 1, Vars, {Name, brt:to_string(Value)});
set_template_var(Name, Value, Vars, false) ->
    case lists:keymember(Name, 1, Vars) of
        true ->
            Vars;
        _ ->
            set_template_var(Name, Value, Vars, true)
    end.
