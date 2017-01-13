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

-module(brt_io).

% API
-export([
    copyright_info/2,
    format_flat/1,
    format_wrap/2,
    inc_indent/1,
    inc_indent/2,
    indent/1,
    write_deps/3,
    write_info/1,
    write_makefile/3,
    write_rebar_config/3
]).

-export_type([
    basho_cpy_info/0,
    comment_type/0,
    copyright_head/0,
    copyright_info/0,
    other_cpy_info/0
]).

-include("brt.hrl").

-type basho_cpy_info()  ::  {basho, brt:basho_year()}.
-type comment_type()    ::  erl | sh.
-type copyright_head()  ::  [string()]. % lines
-type copyright_info()  ::  none | basho_cpy_info() | other_cpy_info().
-type other_cpy_info()  ::  {other, long | copyright_head()}.

%
% Implementation Notes:
%
%   Indents are opaque iolists whose basic block is defined only in
%   inc_indent/2.
%   Manipulating indents outside of the explicit indent functions is
%   deliberately not supported.
%

% Maximum number of lines to process looking for a complete copyright header.
% Basho's official header is ~20 lines if it includes leading editor
% instructions, but many older files have other gunk in there as well, so be
% lenient.
% The much higher limit is to be able to grab a non-Basho header intact for
% re-use.
-define(MAX_CPY_LINES,  99).

% Used ONLY by parse_copyright/2 and its processing functions.
-record(cpystate, {
    iodev       ::  io:device(),
    tries = 0   ::  0..?MAX_CPY_LINES,
    lines = []  ::  [string()]
}).

% Created/cached/returned by regexes/1.
-record(regexes, {
    type        ::  comment_type(),
    blank       ::  brt:regex(),    % whitespace-only line
    cmt_any     ::  brt:regex(),    % any comment line
    cmt_empty   ::  brt:regex(),    % whitespace-only comment line
    cpy_any     ::  brt:regex(),    % any copyright comment line
    cpy_basho   ::  brt:regex()     % Basho copyright comment line
}).

%% ===================================================================
%% Guards
%% ===================================================================

% Section's value can only be a list of brt:dep_spec().
-define(is_deps_sect(Key),
    Key =:= deps orelse Key =:= plugins
).

% Section is omitted if its value is an empty list.
-define(is_omit_empty_list_sect(Key),
    ?is_deps_sect(Key)
    orelse  Key =:= overrides
    orelse  Key =:= profiles
).

% Section's value can only be a list of string().
-define(is_string_list_sect(Key),
            Key =:= artifacts
    orelse  Key =:= ct_first_files
    orelse  Key =:= erl_first_files
    orelse  Key =:= eunit_first_files
    orelse  Key =:= extra_src_dirs
    orelse  Key =:= project_app_dirs
    orelse  Key =:= src_dirs
).

% Section's value can only be a single flat string().
-define(is_string_sect(Key),
            Key =:= base_dir
    orelse  Key =:= checkouts_dir
    orelse  Key =:= deps_dir
    orelse  Key =:= escript_comment
    orelse  Key =:= escript_emu_args
    orelse  Key =:= escript_name
    orelse  Key =:= escript_shebang
    orelse  Key =:= minimum_otp_vsn
    orelse  Key =:= plugins_dir
    orelse  Key =:= root_dir
).

%% ===================================================================
%% API
%% ===================================================================

-spec copyright_info(File :: brt:fs_path(), Type :: comment_type())
        ->  copyright_info() | brt:err_result().
%%
%% @doc Retrieves info from a file's copyright header.
%%
%% Logic is necessarily fuzzy, especially if it's not a Basho copyright block.
%% The file is examined until a non-whitespace, non-comment line is encountered,
%% or until some constant number of lines have been examined (the precise
%% number is an implementation detail, but assume at least several dozen).
%%
%% Returns:
%%
%%  {`basho', Year}
%%      The file was unambiguously parsed and a single Basho copyright
%%      statement was found.  Year is the only or first (sequentially, not
%%      numerically) 4-digit number in the year position.
%%
%%  `none'
%%      The file was unambiguously parsed and does not begin with a copyright
%%      block.
%%
%%  {`other', Lines}
%%      The file appears to contain at least one non-Basho copyright statement
%%      and/or multiple copyright statements, Basho or otherwise.
%%      The lines of the initial comment block containing the copyright(s) are
%%      returned in order, with trailing newlines, suitable for writing as an
%%      iolist().
%%
%%  {`other', `long'}
%%      The maximum number of lines was processed without finding a clearly
%%      delimited comment block or non-comment data.
%%
%%  {`error', What}
%%      An I/O error occurred, distinct from parsing ambiguity.
%%      Inability to parse the file with reasonable certainty results in the
%%      `other' result, not an error.
%%
copyright_info(File, Type) ->
    % This will raise a `badarg' if Type isn't recognized, preventing the file
    % from being touched.
    ReInfo = comment_line_start_re(Type),
    % The header will almost certainly be completely in the first 4K, no need
    % to read the whole file here. Keep it to a likely single storage block.
    case file:open(File, [read, raw, {read_ahead, 4096}]) of
        {ok, IoDev} ->
            try
                parse_copyright(IoDev, ReInfo)
            after
                _ = file:close(IoDev)
            end;
        Error ->
            Error
    end.

-spec format_flat(Term :: term()) -> iolist().
%%
%% @doc Formats the specified Term without added newlines or indents.
%%
format_flat(Term) ->
    format_value(flat, Term).

-spec format_wrap(Indent :: iolist(), Term :: term()) -> iolist().
%%
%% @doc Formats the specified Term with added newlines and indents for lists.
%%
format_wrap(Indent, Term) ->
    format_value(Indent, Term).

-spec inc_indent(Indent :: non_neg_integer() | iolist()) -> iolist().
%%
%% @doc Returns an indent one block wider than the input indent.
%%
%% For transparency, the input can be either a level (integer) or indent
%% (iolist) - an iolist() is returned in either case.
%%
inc_indent(Indent) when erlang:is_integer(Indent) andalso Indent >= 0 ->
    inc_indent(Indent + 1, []);
inc_indent(Indent) ->
    inc_indent(1, Indent).

-spec inc_indent(
        Levels :: non_neg_integer(), Indent :: non_neg_integer() | iolist())
        -> iolist().
%%
%% @doc Returns an indent the specified number of blocks wider than the input.
%%
%% For transparency, the input can be either a level (integer) or indent
%% (iolist) - an iolist() is returned in either case.
%%
inc_indent(Levels, Indent)
    %
    % All of the indent-related operations eventually call this function, so
    % the full parameter validation is only done here.
    % Make it past this guard and you're all set.
    %
    when not (erlang:is_integer(Levels) andalso Levels >= 0
            andalso ((erlang:is_integer(Indent) andalso Indent >= 0)
                    orelse  erlang:is_list(Indent))) ->
    erlang:error(badarg, [Levels, Indent]);

inc_indent(Levels, Indent) when erlang:is_integer(Indent) ->
    inc_indent(Levels + Indent, []);
inc_indent(0, Indent) ->
    Indent;
inc_indent(Levels, Indent) ->
    inc_indent((Levels - 1), [$\s, $\s, $\s, $\s | Indent]).

-spec indent(Level :: non_neg_integer() | iolist()) -> iolist().
%%
%% @doc Returns an indent the specified number of block wide.
%%
%% For transparency, the input can be an indent block (iolist), which is
%% returned unchanged.
%%
indent(Indent) when erlang:is_list(Indent) ->
    Indent;
indent(0) ->
    [];
indent(Level) ->
    inc_indent(Level, []).

-spec write_info(IoDev :: io:device()) -> ok.
%%
%% @doc Writes some basic info about the application's configuration.
%%
write_info(IoDev) ->
    case brt_config:config_file() of
        undefined ->
            io:put_chars(IoDev, "No config file loaded.\n");
        File ->
            io:format(IoDev, "Config File: ~s\n", [File])
    end,
    io:format(IoDev, "Config:\n   ~p\n", [brt_config:config()]),
    case lists:filter(
            fun brt:implements_behaviour/1,
            brt:list_modules(?PRV_MOD_PREFIX)) of
        [] ->
            io:put_chars(IoDev, "No providers found.\n");
        Provs ->
            io:put_chars(IoDev, "Providers:\n"),
            lists:foreach(
                fun(Prov) ->
                    io:format(IoDev, "    ~s\n", [Prov])
                end,
                lists:sort(Provs) )
    end,
    ok.

-spec write_makefile(
        IoDev   :: io:device(),
        Content :: iolist(),
        CpyInfo :: brt:basho_year() | iolist() )
        -> ok.
%%
%% @doc Writes the specified Content as a standard Makefile.
%%
write_makefile(IoDev, Content, Header) when erlang:is_list(Header) ->
    io:put_chars(IoDev, [Header, $\n, Content]);

write_makefile(IoDev, Content, StartYear) ->
    write_makefile(IoDev, Content, [
        brt_defaults:editor_makefile(),
        brt_defaults:copyright("#", StartYear) ]).

-spec write_rebar_config(
        IoDev   :: io:device(),
        Terms   :: [{atom(), term()}],
        CpyInfo :: brt:basho_year() | iolist() )
        -> ok.
%%
%% @doc Writes the specified Terms as a standard rebar.config file.
%%
write_rebar_config(IoDev, Terms, Header) when erlang:is_list(Header) ->
    io:put_chars(IoDev, [Header, $\n]),
    write_rebar_config_terms(
        IoDev, indent(0), order_rebar_config_terms(Terms));

write_rebar_config(IoDev, Terms, StartYear) ->
    write_rebar_config(IoDev, Terms, [
        brt_defaults:editor_erlang(),
        brt_defaults:copyright("%%", StartYear) ]).

-spec write_deps(
        IoDev   :: io:device(),
        Level   :: non_neg_integer() | iolist(),
        Deps    :: [brt:app_name() | brt:dep_spec()])
        -> ok.
%%
%% @doc Writes a list of dependency specifications in standard format.
%%
write_deps(_IoDev, _Level, []) ->
    ok;
write_deps(IoDev, Level, Deps) when erlang:is_integer(Level) ->
    write_deps(IoDev, indent(Level), Deps);
write_deps(IoDev, Indent, Deps) ->
    write_deps(IoDev, Indent, inc_indent(Indent), Deps).

%% ===================================================================
%% Internal
%% ===================================================================

-spec close_list_term(
        IoDev :: io:device(), Indent :: iolist(), Remain :: list())
        -> ok.
close_list_term(IoDev, Indent, Remain) ->
    io:format(IoDev, "~s]", [Indent]),
    close_tuple_term(IoDev, Indent, Remain).

-spec close_tuple_term(
        IoDev :: io:device(), Indent :: iolist(), Remain :: list())
        -> ok.
close_tuple_term(IoDev, Indent, Remain) ->
    io:put_chars(IoDev, "}"),
    close_term(IoDev, Indent, Remain).

-spec close_term(
        IoDev :: io:device(), Indent :: iolist(), Remain :: list())
        -> ok.
close_term(IoDev, [], _Remain) ->
    io:put_chars(IoDev, ".\n\n");
close_term(IoDev, _Indent, []) ->
    io:nl(IoDev);
close_term(IoDev, _Indent, _Remain) ->
    io:put_chars(IoDev, ",\n").

-spec format_values(Indent :: flat | iolist(), Values :: [term()])
        -> iolist().
format_values(Indent, [Value]) ->
    format_value(Indent, Value);
format_values(Indent, [Value | Values]) ->
    [format_value(Indent, Value), $,, $\s | format_values(Indent, Values)];
format_values(_Indent, []) ->
    ok.

-spec format_value(Indent :: flat | iolist(), Value :: term()) -> iolist().

format_value(_Indent, []) ->
    "[]";
format_value(Indent, Value) when erlang:is_list(Value) ->
    case io_lib:deep_char_list(Value) of
        true ->
            [$\", Value, $\"];
        _ ->
            case Indent of
                flat ->
                    [$[, format_values(Indent, Value), $]];
                _ ->
                    SubInd = inc_indent(Indent),
                    case Value of
                        [Val] ->
                            [$[, $\n, SubInd, format_value(SubInd, Val),
                                $\n, Indent, $]];
                        [Head | Tail] ->
                            Delim = [$,, $\n | SubInd],
                            Rest = [[Delim | format_value(SubInd, Elem)]
                                || Elem <- Tail],
                            [$[, $\n, SubInd, format_value(SubInd, Head),
                                Rest, $\n, Indent, $]]
                    end
            end
    end;

format_value(_Indent, <<>>) ->
    "<<>>";
format_value(_Indent, Value) when erlang:is_binary(Value) ->
    %
    % There's no good way to do this without resorting to ugly hackery, but
    % fortunately we're not likely to run into many binaries dealing with
    % rebar.config files in the first place ...
    %
    List = erlang:binary_to_list(Value),
    case io_lib:char_list(List) of
        true ->
            [$<, $<, List, $>, $>];
        _ ->
            io_lib:format("~w", [Value])
    end;

format_value(_Indent, {}) ->
    "{}";
format_value(Indent, Value) when erlang:is_tuple(Value) ->
    [${, format_values(Indent, erlang:tuple_to_list(Value)), $}];

format_value(_Indent, Value) when erlang:is_atom(Value) ->
    io_lib:format("~p", [Value]);

format_value(_Indent, Value) ->
    io_lib:format("~w", [Value]).

-spec order_rebar_config_terms(Terms :: brt:rebar_conf()) -> brt:rebar_conf().
order_rebar_config_terms(Terms) ->
    %
    % Order of terms (sections) in the rebar.config file.
    % Anything not specified is sorted in the 'everything else' space.
    %
    Order = [
        {head,  brt_protect},
        {head,  minimum_otp_vsn},
        {head,  erl_first_files},
        % everything else, sorted
        {tail,  dialyzer},
        {tail,  xref},
        {tail,  overrides},
        {tail,  pre_hooks},
        {tail,  post_hooks},
        {tail,  provider_hooks},
        {tail,  deps},
        {tail,  profiles},
        {tail,  brt},
        {tail,  brt_upstream},
        {tail,  plugins}
    ],
    order_rebar_config_terms(lists:reverse(Order), [], Terms, []).

-spec order_rebar_config_terms(
        Keys    :: [{head | tail, brt:rebar_key()}],
        Head    :: brt:rebar_conf(),
        Remain  :: brt:rebar_conf(),
        Tail    :: brt:rebar_conf())
        -> brt:rebar_conf().
order_rebar_config_terms([{Where, Key} | Keys], Head, Remain, Tail) ->
    case lists:keytake(Key, 1, Remain) of
        {value, Term, Rest} ->
            case Where of
                head ->
                    order_rebar_config_terms(Keys, [Term | Head], Rest, Tail);
                tail ->
                    order_rebar_config_terms(Keys, Head, Rest, [Term | Tail])
            end;
        _ ->
            order_rebar_config_terms(Keys, Head, Remain, Tail)
    end;
order_rebar_config_terms([], Head, Remain, Tail) ->
    lists:append([Head, lists:sort(Remain), Tail]).

-spec comment_line_start_re(Type :: atom() | string())
        -> {comment_type(), string()}.
comment_line_start_re(Type)
        when Type == erl orelse Type == hrl orelse Type == config ->
    {erl, "^\\s*%"};
comment_line_start_re(Type)
        when Type == sh orelse Type == mk orelse Type == conf ->
    {sh, "^\\s*#"};
comment_line_start_re([$. | Ext]) ->
    comment_line_start_re(brt:to_atom(Ext));
comment_line_start_re(Arg) ->
    erlang:error(badarg, [Arg]).

-spec parse_copyright(
        IoDev :: io:device(), ReInfo :: {comment_type(), string()})
        ->  copyright_info() | brt:err_result().
%
% Results as specified for copyright_info/2.
% This function is tightly integrated with parse_copyright_line/3.
%
parse_copyright(IoDev, ReInfo) ->
    State = #cpystate{iodev = IoDev},
    case parse_copyright_line(regexes(ReInfo), none, State) of
        %
        % When `other' is returned with a list, it's the lines read in reverse
        % order, so the last line read is at the head of the list.
        % It's possible (though unlikely) that the last line read didn't end in
        % a newline, but with EOF.
        % Our contract says each line ends with a newline, so make sure it does
        % while we've got it handy, then return the list reversed into it's
        % original order in the file.
        %
        {other, [Last | Rest] = Lines} ->
            % There are no blank lines in the list, so we don't have to check.
            Fixed = case lists:last(Last) of
                $\n ->
                    Lines;
                _ ->
                    [Last ++ [$\n] | Rest]
            end,
            {other, lists:reverse(Fixed)};
        Result ->
            Result
    end.

-spec parse_copyright_line(
        RegEx   :: #regexes{},
        Status  :: none | other | basho_cpy_info(),
        State   :: #cpystate{} )
        ->  copyright_info() | brt:err_result().
%
% Results as specified for copyright_info/2 EXCEPT that {`other', Lines}
% requires the fixup performed by parse_copyright/2, which should be the only
% non-recursive caller of this function.
% Mutually recursive with, and tightly coupled to, parse_copyright_line/4.
%
parse_copyright_line(_, _, #cpystate{tries = Tries})
        when Tries > ?MAX_CPY_LINES ->
    {other, long};

parse_copyright_line(RegEx, Status,
        #cpystate{iodev = IoDev, tries = Tries, lines = Lines} = State) ->
    case file:read_line(IoDev) of
        {ok, Line} ->
            parse_copyright_line(#regexes.blank, RegEx, Status,
                State#cpystate{tries = (Tries + 1), lines = [Line | Lines]});
        eof ->
            case Status of
                other ->
                    {other, Lines}
            end;
        Error ->
            Error
    end.

-spec parse_copyright_line(
        ReIndex :: pos_integer(),   % #regexes.blank | #regexes.cmt_any
        RegEx   :: #regexes{},
        Status  :: none | other | basho_cpy_info(),
        State   :: #cpystate{} )
        ->  copyright_info() | brt:err_result().
%
% Basically a mini state machine for handling each line.
% Mutually recursive with, and tightly coupled to, parse_copyright_line/3.
%
parse_copyright_line(#regexes.blank, #regexes{blank = RE} = RegEx,
        Status, #cpystate{lines = [Line | Lines]} = State) ->
    case re:run(Line, RE, [{capture, none}]) of
        match ->
            case Lines of
                [] ->
                    % we're still burning off leading blank lines, keep going
                    parse_copyright_line(
                        RegEx, Status, State#cpystate{lines = Lines});
                _ ->
                    % blank line after we've got a header, we're done
                    case Status of
                        other ->
                            {other, Lines};
                        _ ->
                            Status
                    end
            end;
        nomatch ->
            parse_copyright_line(#regexes.cmt_any, RegEx, Status, State)
    end;

parse_copyright_line(#regexes.cmt_any, #regexes{cmt_any = RE} = RegEx,
        Status, #cpystate{lines = [Line | Lines]} = State) ->
    case re:run(Line, RE, [{capture, none}]) of
        match ->
            parse_copyright_line(#regexes.cpy_any, RegEx, Status, State);
        nomatch ->
            % line is neither blank nor a comment, we're done
            case Status of
                other ->
                    {other, Lines};
                _ ->
                    Status
            end
    end;

parse_copyright_line(#regexes.cpy_any, #regexes{cpy_any = RE} = RegEx,
        Status, #cpystate{lines = [Line | _]} = State) ->
    case re:run(Line, RE, [{capture, none}]) of
        match ->
            case Status of
                none ->
                    parse_copyright_line(
                        #regexes.cpy_basho, RegEx, Status, State);
                _ ->
                    parse_copyright_line(RegEx, other, State)
            end;
        nomatch ->
            parse_copyright_line(RegEx, Status, State)
    end;

% Only called when Status == `none' and Line contains a copyright statement.
parse_copyright_line(#regexes.cpy_basho, #regexes{cpy_basho = RE} = RegEx,
        _Status, #cpystate{lines = [Line | _]} = State) ->
    case re:run(Line, RE, [{capture, all_but_first, list}]) of
        {match, [[_,_,_,_] = YearStr]} ->
            Year = erlang:list_to_integer(YearStr),
            parse_copyright_line(RegEx, {basho, Year}, State);
        _ ->
            parse_copyright_line(RegEx, other, State)
    end.

-spec regexes({CT :: comment_type(), CLS :: string()}) -> #regexes{}.

% Appended to Comment Line Start, use `caseless' compile option.
-define(RE_ANY_COPYRIGHT,   ".*(\\scopyright\\s|\\s\\(c\\)\\s)" ).

% Appended to Comment Line Start, first/only sub-pattern capture is year.
-define(RE_BASHO_CPY_YEAR,
    ".*\\sCopyright\\s+(?:\\([cC]\\)\\s+)?(2\\d\\d\\d)"
    "(?:[-,]2\\d\\d\\d)*\\s+Basho Technologies\\b" ).

regexes({CT, CLS}) ->
    Key = {?MODULE, regex, CT},
    case erlang:get(Key) of
        undefined ->
            {ok, RE_WS} = re:compile("^\\s*$"),
            {ok, RE_CL} = re:compile(CLS),
            {ok, RE_CE} = re:compile([CLS, "\\s*$"]),
            {ok, RE_CP} = re:compile([CLS, ?RE_ANY_COPYRIGHT], [caseless]),
            {ok, RE_BY} = re:compile([CLS, ?RE_BASHO_CPY_YEAR]),
            Val = #regexes{
                type = CT, blank = RE_WS,
                cmt_any = RE_CL, cmt_empty = RE_CE,
                cpy_any = RE_CP, cpy_basho = RE_BY },
            erlang:put(Key, Val),
            Val;
        Rec ->
            Rec
    end.

-undef(RE_ANY_COPYRIGHT).
-undef(RE_BASHO_CPY_YEAR).

-spec write_rebar_config_terms(
        IoDev :: io:device(), Indent :: iolist(), Terms :: brt:rebar_conf())
        -> ok.

% Terms that are omitted if their value is an empty list.
write_rebar_config_terms(IoDev, Indent, [{Section, []} | Terms])
        when ?is_omit_empty_list_sect(Section) ->
    write_rebar_config_terms(IoDev, Indent, Terms);

% Terms whose value can only be a single flat string.
write_rebar_config_terms(IoDev, Indent, [{Section, String} | Terms])
        when ?is_string_sect(Section) ->
    io:format(IoDev, "~s{~p, \"~s\"", [Indent, Section, String]),
    close_tuple_term(IoDev, Indent, Terms),
    write_rebar_config_terms(IoDev, Indent, Terms);

% Anything not specifically matched above is included even if the value is an
% empty list.
write_rebar_config_terms(IoDev, Indent, [{Section, []} | Terms]) ->
    io:format(IoDev, "~s{~p, [\n", [Indent, Section]),
    close_list_term(IoDev, Indent, Terms),
    write_rebar_config_terms(IoDev, Indent, Terms);

% Terms formatted as dependencies.
write_rebar_config_terms(IoDev, Indent, [{Section, Deps} | Terms])
        when ?is_deps_sect(Section) ->
    io:format(IoDev, "~s{~p, [\n", [Indent, Section]),
    write_deps(IoDev, inc_indent(Indent), Deps),
    close_list_term(IoDev, Indent, Terms),
    write_rebar_config_terms(IoDev, Indent, Terms);

% Terms whose value can only be a list of flat strings.
write_rebar_config_terms(IoDev, Indent, [{Section, Strings} | Terms])
        when ?is_string_list_sect(Section) ->
    io:format(IoDev, "~s{~p, [\n", [Indent, Section]),
    write_strings(IoDev, inc_indent(Indent), Strings),
    close_list_term(IoDev, Indent, Terms),
    write_rebar_config_terms(IoDev, Indent, Terms);

write_rebar_config_terms(IoDev, Indent, [{Section, Overrides} | Terms])
        when Section =:= overrides ->
    io:format(IoDev, "~s{~p, [\n", [Indent, Section]),
    write_overrides(IoDev, inc_indent(Indent), Overrides),
    close_list_term(IoDev, Indent, Terms),
    write_rebar_config_terms(IoDev, Indent, Terms);

write_rebar_config_terms(IoDev, Indent, [{Section, Profiles} | Terms])
        when Section =:= profiles ->
    %
    % No profiles support before Rebar3.
    %
    case brt_rebar:config_format() >= 3 of
        true ->
            io:format(IoDev, "~s{~p, [\n", [Indent, Section]),
            write_profiles(IoDev, inc_indent(Indent), Profiles),
            close_list_term(IoDev, Indent, Terms),
            write_rebar_config_terms(IoDev, Indent, Terms);
        _ ->
            ok
    end;

write_rebar_config_terms(IoDev, Indent, [Term | Terms]) ->
    io:put_chars(IoDev, [Indent, format_value(Indent, Term)]),
    close_term(IoDev, Indent, Terms),
    write_rebar_config_terms(IoDev, Indent, Terms);

write_rebar_config_terms(_IoDev, _Indent, []) ->
    ok.

-spec write_deps(
        IoDev   :: io:device(),
        Indent  :: iolist(), SubIndent :: iolist(),
        Deps    :: [brt:app_name() | brt:dep_spec()])
        -> ok.
write_deps(IoDev, Indent, SubIndent, [Dep | Deps]) ->
    io:put_chars(IoDev, [Indent, brt_rebar:format_dep(SubIndent, Dep)]),
    close_term(IoDev, Indent, Deps),
    write_deps(IoDev, Indent, SubIndent, Deps);
write_deps(_IoDev, _Indent, _SubIndent, []) ->
    ok.

-spec write_overrides(
        IoDev :: io:device(), Indent :: iolist(), Overrides :: [tuple()])
        -> ok.
write_overrides(IoDev, Indent, [Override | Overrides]) ->
    io:put_chars(IoDev, [Indent, format_flat(Override)]),
    close_term(IoDev, Indent, Overrides),
    write_overrides(IoDev, Indent, Overrides);
write_overrides(_IoDev, _Indent, []) ->
    ok.

-spec write_profiles(
        IoDev   :: io:device(),
        Indent  :: iolist(),
        Profiles :: [{brt:rebar_key(), brt:rebar_conf()}])
        -> ok.
write_profiles(IoDev, Indent, [{Name, Terms} | Profiles]) ->
    io:format(IoDev, "~s{~p, [\n", [Indent, Name]),
    write_rebar_config_terms(IoDev, inc_indent(Indent), Terms),
    close_list_term(IoDev, Indent, Profiles),
    write_profiles(IoDev, Indent, Profiles);
write_profiles(_IoDev, _Indent, []) ->
    ok.

-spec write_strings(
        IoDev :: io:device(), Indent :: iolist(), Strings :: [string()])
        -> ok.
write_strings(IoDev, Indent, [String]) ->
    io:format(IoDev, "~s\"~s\"\n", [Indent, String]);
write_strings(IoDev, Indent, [String | Strings]) ->
    io:format(IoDev, "~s\"~s\",\n", [Indent, String]),
    write_strings(IoDev, Indent, Strings);
write_strings(_IoDev, _Indent, []) ->
    ok.
