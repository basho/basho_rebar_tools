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

-module(brt_io).

% API
-export([
    copyright_info/2,
    write_deps/3,
    write_info/1,
    write_makefile/3,
    write_rebar_config/3
]).

-include("brt.hrl").

%
% Implementation Notes:
%
%   Indents are opaque iolists whose basic block is defined only in
%   inc_indent/2.
%   Manipulating indents outside of the explicit indent functions is
%   deliberately not supported.
%

% Maximum number of lines to process looking for a copyright header.
-define(MAX_CPY_LINES,  50).

%% ===================================================================
%% Guards
%% ===================================================================

% Section's value can only be a list of brt:dep_spec().
-define(is_deps_sect(Key),
    Key =:= 'deps' orelse Key =:= 'plugins'
).

% Section is omitted if its value is an empty list.
-define(is_omit_empty_list_sect(Key),
            Key =:= 'overrides'
    orelse  Key =:= 'plugins'
    orelse  Key =:= 'profiles'
).

% Section's value can only be a list of string().
-define(is_string_list_sect(Key),
            Key =:= 'artifacts'
    orelse  Key =:= 'ct_first_files'
    orelse  Key =:= 'erl_first_files'
    orelse  Key =:= 'eunit_first_files'
    orelse  Key =:= 'extra_src_dirs'
    orelse  Key =:= 'project_app_dirs'
    orelse  Key =:= 'src_dirs'
).

% Section's value can only be a single flat string().
-define(is_string_sect(Key),
            Key =:= 'base_dir'
    orelse  Key =:= 'checkouts_dir'
    orelse  Key =:= 'deps_dir'
    orelse  Key =:= 'escript_comment'
    orelse  Key =:= 'escript_emu_args'
    orelse  Key =:= 'escript_name'
    orelse  Key =:= 'escript_shebang'
    orelse  Key =:= 'minimum_otp_vsn'
    orelse  Key =:= 'plugins_dir'
    orelse  Key =:= 'root_dir'
).

%% ===================================================================
%% API
%% ===================================================================

-spec copyright_info(File :: brt:fs_path(), Type :: 'erl' | 'sh')
        ->  {'basho', brt:year1970()}
        |   'none' | 'other' | brt:err_result().
%%
%% @doc Retrieves info from a file's copyright header.
%%
%% Logic is necessarily fuzzy, especially if it's not a Basho copyright block.
%% The file is examined until a non-whitespace, non-comment line is
%% encountered, or until some constant number of lines have been examined (the
%% precise number is an implementation detail, but assume a couple dozen at
%% least).
%%
%% Returns:
%%
%%  {'basho', Year}
%%      The file was unambiguously parsed and a single Basho copyright
%%      statement was found.  Year is the only or first (sequentially, not
%%      numerically) 4-digit number in the year position.
%%
%%  'none'
%%      The file was unambiguously parsed and does not begin with a copyright
%%      block.
%%
%%  'other'
%%      The file may contain one or more copyright blocks but manual evaluation
%%      is necessary.
%%
%%  {'error', What}
%%      An I/O error occurred.  Inability to parse the file results in the
%%      'other' result, not an error.
%%
copyright_info(File, Type) ->
    CommentLineStart = case Type of
        'erl' ->
            "^\\s*%";
        'sh' ->
            "^\\s*#";
        _ ->
            erlang:error('badarg', [File, Type])
    end,
    case file:open(File, ['read']) of
        {'ok', IoDev} ->
            try
                parse_copyright(IoDev, CommentLineStart)
            after
                file:close(IoDev)
            end;
        Error ->
            Error
    end.

-spec write_info(IoDev :: io:device()) -> 'ok'.
%%
%% @doc Writes some basic info about the application's configuration.
%%
write_info(IoDev) ->
    case brt_config:config_file() of
        'undefined' ->
            io:put_chars(IoDev, "No config file loaded.\n");
        File ->
            io:format(IoDev, "Config File: ~s\n", [File])
    end,
    io:format(IoDev, "Config: ~p\n", [brt_config:config()]),
    case lists:filter(
            fun brt:implements_behaviour/1,
            brt:list_modules(?PRV_MOD_PREFIX)) of
        [] ->
            io:put_chars(IoDev, "No providers found.\n");
        Provs ->
            io:put_chars(IoDev, "Providers:\n"),
            lists:foreach(
                fun(Prov) ->
                    io:format(IoDev, "\t~s\n", [Prov])
                end,
                lists:sort(Provs) )
    end,
    'ok'.

-spec write_makefile(
        IoDev   :: io:device(),
        Content :: iolist(),
        StartYear :: brt:basho_year() ) -> 'ok'.
%%
%% @doc Writes the specified Content as a standard Makefile.
%%
write_makefile(IoDev, Content, StartYear) ->
    io:put_chars(IoDev, [
        brt_defaults:editor_makefile(),
        brt_defaults:copyright("#", StartYear),
        $\n, Content ]).

-spec write_rebar_config(
        IoDev :: io:device(),
        Terms :: [{atom(), term()}],
        StartYear :: brt:basho_year() )
        -> 'ok'.
%%
%% @doc Writes the specified Terms as a standard rebar.config file.
%%
write_rebar_config(IoDev, Terms, StartYear) ->
    io:put_chars(IoDev, [
        brt_defaults:editor_erlang(),
        brt_defaults:copyright("%%", StartYear),
        $\n ]),
    write_rebar_config_terms(
        IoDev, indent(0), order_rebar_config_terms(Terms)).

-spec write_deps(
        IoDev :: io:device(),
        Level :: non_neg_integer() | iolist(),
        Deps :: [brt:app_name() | brt:dep_spec()])
        -> 'ok'.
%%
%% @doc Writes a list of dependency specifications in standard format.
%%
write_deps(_IoDev, _Level, []) ->
    'ok';
write_deps(IoDev, Level, Deps) when erlang:is_integer(Level) ->
    write_deps(IoDev, indent(Level), Deps);
write_deps(IoDev, Indent, Deps) ->
    write_deps(IoDev, Indent, inc_indent(Indent), Deps).

%% ===================================================================
%% Internal
%% ===================================================================

-spec close_list_term(
        IoDev :: io:device(), Indent :: iolist(), Remain :: list())
        -> 'ok'.
close_list_term(IoDev, Indent, Remain) ->
    io:format(IoDev, "~s]", [Indent]),
    close_tuple_term(IoDev, Indent, Remain).

-spec close_tuple_term(
        IoDev :: io:device(), Indent :: iolist(), Remain :: list())
        -> 'ok'.
close_tuple_term(IoDev, Indent, Remain) ->
    io:put_chars(IoDev, "}"),
    close_term(IoDev, Indent, Remain).

-spec close_term(
        IoDev :: io:device(), Indent :: iolist(), Remain :: list())
        -> 'ok'.
close_term(IoDev, [], _Remain) ->
    io:put_chars(IoDev, ".\n\n");
close_term(IoDev, _Indent, []) ->
    io:nl(IoDev);
close_term(IoDev, _Indent, _Remain) ->
    io:put_chars(IoDev, ",\n").

%%-spec dec_indent(Indent :: iolist()) -> iolist().
%%dec_indent(Indent) ->
%%    dec_indent(1, Indent).
%%
%%-spec dec_indent(Levels :: non_neg_integer(), Indent :: iolist()) -> iolist().
%%dec_indent(_, []) ->
%%    [];
%%dec_indent(0, Indent) ->
%%    Indent;
%%dec_indent(Levels, [_ | Indent])
%%        when erlang:is_integer(Levels) andalso Levels > 0 ->
%%    dec_indent((Levels - 1), Indent).

-spec inc_indent(Indent :: iolist()) -> iolist().
inc_indent(Indent) ->
    inc_indent(1, Indent).

-spec inc_indent(Levels :: non_neg_integer(), Indent :: iolist()) -> iolist().
inc_indent(0, Indent) ->
    Indent;
inc_indent(Levels, Indent)
        when erlang:is_integer(Levels) andalso Levels > 0 ->
    inc_indent((Levels - 1), [[$\s, $\s, $\s, $\s] | Indent]).

-spec indent(Level :: non_neg_integer()) -> iolist().
indent(0) ->
    [];
indent(Level) ->
    inc_indent(Level, []).

-spec order_rebar_config_terms(Terms :: brt:rebar_conf()) -> brt:rebar_conf().
order_rebar_config_terms(Terms) ->
    %
    % Order of terms (sections) in the rebar.config file.
    % Anything not specified is sorted in the 'everything else' space.
    %
    Order = [
        {'head', 'minimum_otp_vsn'},
        {'head', 'erl_first_files'},
        {'head', 'plugins'},
        % everything else, sorted
        {'tail', 'dialyzer'},
        {'tail', 'xref'},
        {'tail', 'overrides'},
        {'tail', 'pre_hooks'},
        {'tail', 'post_hooks'},
        {'tail', 'provider_hooks'},
        {'tail', 'deps'},
        {'tail', 'profiles'}
    ],
    order_rebar_config_terms(lists:reverse(Order), [], Terms, []).

-spec order_rebar_config_terms(
        Keys    :: [{'head' | 'tail', brt:rebar_key()}],
        Head    :: brt:rebar_conf(),
        Remain  :: brt:rebar_conf(),
        Tail    :: brt:rebar_conf())
        -> brt:rebar_conf().
order_rebar_config_terms([{Where, Key} | Keys], Head, Remain, Tail) ->
    case lists:keytake(Key, 1, Remain) of
        {'value', Term, Rest} ->
            case Where of
                'head' ->
                    order_rebar_config_terms(Keys, [Term | Head], Rest, Tail);
                'tail' ->
                    order_rebar_config_terms(Keys, Head, Rest, [Term | Tail])
            end;
        _ ->
            order_rebar_config_terms(Keys, Head, Remain, Tail)
    end;
order_rebar_config_terms([], Head, Remain, Tail) ->
    lists:append([Head, lists:sort(Remain), Tail]).

-spec parse_copyright(IoDev :: io:device(), CLS :: string())
        ->  'none' | 'other' | {'basho', brt:year1970()} | brt:err_result().

% Appended to Comment Line Start, use 'caseless' compile option.
-define(RE_ANY_COPYRIGHT,   ".*(\\scopyright\\s|\\s\\(c\\)\\s)" ).

% Appended to Comment Line Start, first/only sub-pattern capture is year.
-define(RE_BASHO_CPY_YEAR,
    ".*\\sCopyright\\s+(?:\\([cC]\\)\\s+)?(2\\d\\d\\d)"
    "(?:[-,]2\\d\\d\\d)*\\s+Basho Technologies\\b" ).

parse_copyright(IoDev, CLS) ->
    {'ok', ReSpaceOnly} = re:compile("^\\s*$"),
    {'ok', ReComment}   = re:compile(CLS),
    {'ok', ReCopyright} = re:compile([CLS, ?RE_ANY_COPYRIGHT], ['caseless']),
    {'ok', ReBashoYear} = re:compile([CLS, ?RE_BASHO_CPY_YEAR]),
    RegEx = {ReSpaceOnly, ReComment, ReCopyright, ReBashoYear},
    parse_copyright_line(0, IoDev, 0, RegEx, 'none').

-undef(RE_ANY_COPYRIGHT).
-undef(RE_BASHO_CPY_YEAR).

-spec parse_copyright_line(
        Tries   :: 0..?MAX_CPY_LINES,
        IoDev   :: io:device(),
        First   :: 0 | 1,
        RegEx   :: {brt:regex(), brt:regex(), brt:regex(), brt:regex()},
        Result  :: 'none' | {'basho', brt:year1970()} )
        ->  'none' | 'other' | {'basho', brt:year1970()} | brt:err_result().
%
% First == 0 while initially consuming whitespace lines before reaching the
% first comment line, then switches to 1 for the remainder of the lines.
%
parse_copyright_line(Tries, IoDev, First, RegEx, Result)
        when Tries < ?MAX_CPY_LINES ->
    case file:read_line(IoDev) of
        {'ok', Line} ->
            parse_copyright_line(
                (Tries + 1), IoDev, First, RegEx, Line, Result);
        'eof' ->
            Result;
        Error ->
            Error
    end;
parse_copyright_line(_Tries, _IoDev, _First, _RegEx, Result) ->
    Result.

-spec parse_copyright_line(
        Tries   :: non_neg_integer(),
        IoDev   :: io:device(),
        ReIndex :: 0..3,
        RegEx   :: {brt:regex(), brt:regex(), brt:regex(), brt:regex()},
        Line    :: string(),
        Result  :: 'none' | {'basho', brt:year1970()} )
        ->  'none' | 'other' | {'basho', brt:year1970()}.

parse_copyright_line(Tries, IoDev, 0, {RE, _, _, _} = RegEx, Line, Result) ->
    case re:run(Line, RE, [{'capture', 'none'}]) of
        'match' ->
            parse_copyright_line(Tries, IoDev, 0, RegEx, Result);
        'nomatch' ->
            parse_copyright_line(Tries, IoDev, 1, RegEx, Line, Result)
    end;

parse_copyright_line(Tries, IoDev, 1, {_, RE, _, _} = RegEx, Line, Result) ->
    case re:run(Line, RE, [{'capture', 'none'}]) of
        'match' ->
            parse_copyright_line(Tries, IoDev, 2, RegEx, Line, Result);
        'nomatch' ->
            Result
    end;

parse_copyright_line(Tries, IoDev, 2, {_, _, RE, _} = RegEx, Line, Result) ->
    case re:run(Line, RE, [{'capture', 'none'}]) of
        'match' ->
            parse_copyright_line(Tries, IoDev, 3, RegEx, Line, Result);
        'nomatch' ->
            parse_copyright_line(Tries, IoDev, 1, RegEx, Result)
    end;

parse_copyright_line(Tries, IoDev, 3, {_, _, _, RE} = RegEx, Line, Result) ->
    case re:run(Line, RE, [{'capture', 'all_but_first', 'list'}]) of
        {'match', [[_,_,_,_] = YearStr]} ->
            Year = erlang:list_to_integer(YearStr),
            case Result of
                'none' ->
                    parse_copyright_line(
                        Tries, IoDev, 1, RegEx, {'basho', Year});
                {'basho', Year} ->
                    parse_copyright_line(Tries, IoDev, 1, RegEx, Result);
%%                {'basho', Prev} when Prev =< Year ->
%%                    parse_copyright_line(Tries, IoDev, 1, RegEx, Result);
%%                {'basho', _} ->
%%                    parse_copyright_line(
%%                        Tries, IoDev, 1, RegEx, {'basho', Year});
                _ ->
                    'other'
            end;
        _ ->
            'other'
    end.

-spec write_rebar_config_terms(
        IoDev :: io:device(), Indent :: iolist(), Terms :: brt:rebar_conf())
        -> 'ok'.

% Terms that are omitted if their value is an empty list.
write_rebar_config_terms(IoDev, Indent, [{Section, []} | Terms])
        when ?is_omit_empty_list_sect(Section) ->
    write_rebar_config_terms(IoDev, Indent, Terms);

% Terms whose value can only be a single flat string.
write_rebar_config_terms(IoDev, Indent, [{Section, String} | Terms])
        when ?is_string_sect(Section) ->
    io:format(IoDev, "~s{~s, \"~s\"", [Indent, Section, String]),
    close_tuple_term(IoDev, Indent, Terms),
    write_rebar_config_terms(IoDev, Indent, Terms);

% Anything not specifically matched above is included even if the value is an
% empty list.
write_rebar_config_terms(IoDev, Indent, [{Section, []} | Terms]) ->
    io:format(IoDev, "~s{~s, [\n", [Indent, Section]),
    close_list_term(IoDev, Indent, Terms),
    write_rebar_config_terms(IoDev, Indent, Terms);

% Terms formatted as dependencies.
write_rebar_config_terms(IoDev, Indent, [{Section, Deps} | Terms])
        when ?is_deps_sect(Section) ->
    io:format(IoDev, "~s{~s, [\n", [Indent, Section]),
    write_deps(IoDev, inc_indent(Indent), Deps),
    close_list_term(IoDev, Indent, Terms),
    write_rebar_config_terms(IoDev, Indent, Terms);

% Terms whose value can only be a list of flat strings.
write_rebar_config_terms(IoDev, Indent, [{Section, Strings} | Terms])
        when ?is_string_list_sect(Section) ->
    io:format(IoDev, "~s{~s, [\n", [Indent, Section]),
    write_strings(IoDev, inc_indent(Indent), Strings),
    close_list_term(IoDev, Indent, Terms),
    write_rebar_config_terms(IoDev, Indent, Terms);

write_rebar_config_terms(IoDev, Indent, [{Section, Overrides} | Terms])
        when Section =:= 'overrides' ->
    write_overrides(IoDev, Indent, Overrides),
    close_term(IoDev, Indent, Terms),
    write_rebar_config_terms(IoDev, Indent, Terms);

write_rebar_config_terms(IoDev, Indent, [{Section, Profiles} | Terms])
        when Section =:= 'profiles' ->
    io:format(IoDev, "~s{~s, [\n", [Indent, Section]),
    write_profiles(IoDev, inc_indent(Indent), Profiles),
    close_list_term(IoDev, Indent, Terms),
    write_rebar_config_terms(IoDev, Indent, Terms);

write_rebar_config_terms(IoDev, Indent, [Term | Terms]) ->
    io:put_chars(IoDev, Indent),
    write_value(IoDev, Indent, Term),
    close_term(IoDev, Indent, Terms),
    write_rebar_config_terms(IoDev, Indent, Terms);

write_rebar_config_terms(_IoDev, _Indent, []) ->
    'ok'.

-spec write_deps(
        IoDev   :: io:device(),
        Indent  :: iolist(), SubIndent :: iolist(),
        Deps    :: [brt:app_name() | brt:dep_spec()])
        -> 'ok'.
write_deps(IoDev, Indent, SubIndent, [Dep | Deps]) ->
    io:put_chars(IoDev, Indent),
    write_dep(IoDev, SubIndent, Dep),
    close_term(IoDev, Indent, Deps),
    write_deps(IoDev, Indent, SubIndent, Deps);
write_deps(_IoDev, _Indent, _SubIndent, []) ->
    'ok'.

-spec write_dep(
        IoDev :: io:device(), SubIndent :: iolist(),
        Dep :: brt:app_name() | brt:dep_spec())
        -> 'ok'.
%
% Fields:
%
%   AppName     ::  atom()
%   RepoLoc     ::  string()
%   RepoOpt     ::  list | tuple()
%   RepoType    ::  atom()
%   VsnStr      ::  string()
%   VsnType     ::  atom()
%   WrapOpt     ::  list | tuple()
%   WrapType    ::  atom()
%
% Patterns:
%
%   AppName
%
%   {AppName, {WrapType, {RepoType, RepoLoc, {VsnType, VsnStr} }, WrapOpt} }
%   {AppName, {WrapType, {RepoType, RepoLoc, VsnStr}, WrapOpt} }
%
%   {AppName, {WrapType, {RepoType, RepoLoc, {VsnType, VsnStr} } } }
%   {AppName, {WrapType, {RepoType, RepoLoc, VsnStr} } }
%
%   {AppName, {RepoType, RepoLoc, {VsnType, VsnStr} } }
%   {AppName, {RepoType, RepoLoc, VsnStr} }
%
% TODO: Package manager specs
%

write_dep(IoDev, SubIndent, AppName) when erlang:is_atom(AppName) ->
    write_dep(IoDev, SubIndent, brt_config:pkg_dep(AppName));

write_dep(IoDev, SubIndent,
    {AppName, {WrapType, {RepoType, RepoLoc, VsnStr}, WrapOpt}})
        when erlang:is_list(VsnStr) ->
    write_dep(IoDev, SubIndent,
        {AppName, {WrapType, {RepoType, RepoLoc, {'ref', VsnStr}}, WrapOpt}});

write_dep(IoDev, SubIndent,
    {AppName, {WrapType, {RepoType, RepoLoc, VsnStr}}})
        when erlang:is_list(VsnStr) ->
    write_dep(IoDev, SubIndent,
        {AppName, {WrapType, {RepoType, RepoLoc, {'ref', VsnStr}}}});

write_dep(IoDev, SubIndent, {AppName, {RepoType, RepoLoc, VsnStr}})
        when erlang:is_list(VsnStr) ->
    write_dep(IoDev, SubIndent,
        {AppName, {RepoType, RepoLoc, {'ref', VsnStr}}});

write_dep(IoDev, SubIndent,
        {AppName, {WrapType, {RepoType, RepoLoc, {VsnType, VsnStr}}, WrapOpt}})
        when erlang:is_atom(AppName) andalso erlang:is_atom(WrapType)
        andalso erlang:is_atom(RepoType) andalso erlang:is_list(RepoLoc)
        andalso erlang:is_atom(VsnType) andalso erlang:is_list(VsnStr) ->
    io:format(IoDev,
        "{'~s', {'~s',~n~s{'~s', \"~s\",~n~s{'~s', \"~s\"}},~n~s",
        [AppName, WrapType,
            SubIndent, RepoType, RepoLoc,
            SubIndent, VsnType, VsnStr,
            SubIndent]),
    write_value(IoDev, SubIndent, WrapOpt),
    io:put_chars(IoDev, " }}");

write_dep(IoDev, SubIndent,
        {AppName, {WrapType, {RepoType, RepoLoc, {VsnType, VsnStr}}}})
        when erlang:is_atom(AppName) andalso erlang:is_atom(WrapType)
        andalso erlang:is_atom(RepoType) andalso erlang:is_list(RepoLoc)
        andalso erlang:is_atom(VsnType) andalso erlang:is_list(VsnStr) ->
    io:format(IoDev,
        "{'~s', {'~s',~n~s{'~s', \"~s\",~n~s{'~s', \"~s\"} }}}",
        [AppName, WrapType,
            SubIndent, RepoType, RepoLoc,
            SubIndent, VsnType, VsnStr]);

write_dep(IoDev, SubIndent,
        {AppName, {RepoType, RepoLoc, {VsnType, VsnStr}}})
        when erlang:is_atom(AppName)
        andalso erlang:is_atom(RepoType) andalso erlang:is_list(RepoLoc)
        andalso erlang:is_atom(VsnType) andalso erlang:is_list(VsnStr) ->
    io:format(IoDev,
        "{'~s',~n~s{'~s', \"~s\",~n~s{'~s', \"~s\"} }}",
        [AppName, SubIndent, RepoType, RepoLoc, SubIndent, VsnType, VsnStr]);

write_dep(_IoDev, _SubIndent, Dep) ->
    erlang:error('badarg', [Dep]).

-spec write_overrides(
        IoDev :: io:device(), Indent :: iolist(), Overrides :: [tuple()])
        -> 'ok'.
write_overrides(IoDev, Indent, Overrides) ->
    io:put_chars(IoDev, [Indent, "{overrides, [\n"]),
    write_overrides(IoDev, Indent, inc_indent(Indent), Overrides),
    io:put_chars(IoDev, [Indent, "]}"]).

-spec write_overrides(
        IoDev       :: io:device(),
        Indent      :: iolist(),
        SubIndent   :: iolist(),
        Overrides   :: [tuple()])
        -> 'ok'.
write_overrides(IoDev, Indent, SubIndent, [Override | Overrides]) ->
    write_override(IoDev, SubIndent, Override),
    close_term(IoDev, Indent, Overrides),
    write_overrides(IoDev, Indent, SubIndent, Overrides);
write_overrides(_IoDev, _Indent, _SubIndent, []) ->
    'ok'.

-spec write_override(
        IoDev :: io:device(), Indent :: iolist(), Override :: tuple())
        -> 'ok'.
% Special case because this is a common override.
write_override(IoDev, Indent, {Mode, Pkg, [{'erl_opts', List}]}) ->
    io:format(IoDev, "~s{~s, ~s, [{erl_opts, [", [Indent, Mode, Pkg]),
    write_values(IoDev, Indent, List),
    io:put_chars(IoDev, "]}]}");
write_override(IoDev, Indent, {Mode, Pkg, List}) ->
    io:format(IoDev, "~s{~s, ~s, ", [Indent, Mode, Pkg]),
    write_values(IoDev, Indent, List),
    io:put_chars(IoDev, "]}");
write_override(IoDev, Indent, {Mode, List}) ->
    io:format(IoDev, "~s{~s, ", [Indent, Mode]),
    write_values(IoDev, Indent, List),
    io:put_chars(IoDev, "]}");
write_override(IoDev, Indent, Override) ->
    io:put_chars(IoDev, Indent),
    write_value(IoDev, Indent, Override).

-spec write_profiles(
        IoDev   :: io:device(),
        Indent  :: iolist(),
        Profiles :: [{brt:rebar_key(), brt:rebar_conf()}])
        -> 'ok'.
write_profiles(IoDev, Indent, [{Name, Terms} | Profiles]) ->
    io:format(IoDev, "~s{~s, [\n", [Indent, Name]),
    write_rebar_config_terms(IoDev, inc_indent(Indent), Terms),
    close_list_term(IoDev, Indent, Profiles),
    write_profiles(IoDev, Indent, Profiles);
write_profiles(_IoDev, _Indent, []) ->
    'ok'.

-spec write_strings(
        IoDev :: io:device(), Indent :: iolist(), Strings :: [string()])
        -> 'ok'.
write_strings(IoDev, Indent, [String]) ->
    io:format(IoDev, "~s\"~s\"\n", [Indent, String]);
write_strings(IoDev, Indent, [String | Strings]) ->
    io:format(IoDev, "~s\"~s\",\n", [Indent, String]),
    write_strings(IoDev, Indent, Strings);
write_strings(_IoDev, _Indent, []) ->
    'ok'.

-spec write_values(
        IoDev :: io:device(), Indent :: iolist(), Values :: [term()])
        -> 'ok'.
write_values(IoDev, Indent, [Value]) ->
    write_value(IoDev, Indent, Value);
write_values(IoDev, Indent, [Value | Values]) ->
    write_value(IoDev, Indent, Value),
    io:put_chars(IoDev, [$,, $\s]),
    write_values(IoDev, Indent, Values);
write_values(_IoDev, _Indent, []) ->
    'ok'.

-spec write_value(IoDev :: io:device(), Indent :: iolist(), Value :: term())
        -> 'ok'.

write_value(IoDev, Indent, {'overrides', Overrides}) ->
    write_overrides(IoDev, Indent, inc_indent(Indent), Overrides);

write_value(IoDev, _Indent, []) ->
    io:put_chars(IoDev, "[]");
write_value(IoDev, Indent, [First | Values] = Value) ->
    case io_lib:char_list(Value) of
        'true' ->
            io:put_chars(IoDev, [$\", Value, $\"]);
        _ ->
            ElemIndent = inc_indent(Indent),
            ElemDelim = [$,, $\n | ElemIndent],
            io:put_chars(IoDev, [$[, $\n | ElemIndent]),
            write_value(IoDev, ElemIndent, First),
            lists:foreach(
                fun(Val) ->
                    io:put_chars(IoDev, ElemDelim),
                    write_value(IoDev, ElemIndent, Val)
                end, Values),
            io:put_chars(IoDev, [$\n, Indent, $]])
    end;

write_value(IoDev, _Indent, <<>>) ->
    io:put_chars(IoDev, "<<>>");
write_value(IoDev, _Indent, Value) when erlang:is_binary(Value) ->
    %
    % There's no good way to do this without resorting to ugly hackery, but
    % fortunately we're not likely to run into a binary in a rebar.config in
    % the first place ...
    %
    List = erlang:binary_to_list(Value),
    case io_lib:char_list(List) of
        'true' ->
            io:put_chars(IoDev, ["<<", List, ">>"]);
        _ ->
            io:format(IoDev, "~w", [Value])
    end;

write_value(IoDev, _Indent, {}) ->
    io:put_chars(IoDev, "{}");
write_value(IoDev, Indent, Value) when erlang:is_tuple(Value) ->
    io:put_chars(IoDev, [${]),
    write_values(IoDev, Indent, erlang:tuple_to_list(Value)),
    io:put_chars(IoDev, [$}]);

write_value(IoDev, _Indent, Value) ->
    io:format(IoDev, "~w", [Value]).


