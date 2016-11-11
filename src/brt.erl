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
%% @doc Basho Rebar Tools command provider behavior and common operations.
%%
%% The 'brt' behavior has the same callbacks as rebar's 'provider', but with
%% stricter types.
%%
-module(brt).

% API
-export([
    app_dir_to_name/1,
    consult_app_file/2,
    dep_list/1,
    dep_list_member/2,
    dep_name/1,
    format_error/1,
    get_key_list/2,
    get_key_tuple/2,
    get_key_value/2,
    file_error/2,
    implements_behaviour/1,
    is_list_key/2,
    list_modules/1,
    read_app_file/2,
    to_atom/1,
    to_binary/1,
    to_string/1
]).

% Types
-export_type([
    app_name/0,
    basho_year/0,
    dep_loc/0,
    dep_spec/0,
    dep_type/0,
    dep_vsn/0,
    err_result/0,
    file_mode/0,
    fs_path/0,
    prv_error/0,
    rebar_app/0,
    rebar_conf/0,
    rebar_key/0,
    rebar_prov/0,
    rebar_sect/0,
    rebar_state/0,
    regex/0,
    rsrc_spec/0,
    rsrc_wrap/0,
    year1970/0
]).

-include("brt.hrl").

-ifdef(brt_check).
-type rebar_app()   ::  rebar_app_info:t().
-type rebar_prov()  ::  providers:t().
-type rebar_state() ::  rebar_state:t().
-else.
-type rebar_app()   ::  tuple().
-type rebar_prov()  ::  tuple().
-type rebar_state() ::  tuple().
-endif.

-type app_name()    ::  atom().
-type basho_year()  ::  ?BASHO_YEAR_MIN..9999 | 'current'.
-type dep_loc()     ::  string().   % currently, always a GitHub URL
-type dep_spec()    ::  {app_name(), rsrc_spec() | rsrc_wrap()}.
-type dep_type()    ::  atom().     % currently, 'git' or 'raw'
-type dep_vsn()     ::  {atom(), string()} | string().  % branch/tag/ref
-type err_result()  ::  {'error', term()}.
-type fs_path()     ::  nonempty_string().
-type prv_error()   ::  {'error', string()} | {'error', {module(), term()}}.
-type rebar_conf()  ::  [rebar_sect()].
-type rebar_key()   ::  atom().
-type rebar_sect()  ::  {rebar_key(), term()}.
-type rsrc_spec()   ::  {dep_type(), dep_loc(), dep_vsn()}.
-type rsrc_wrap()   ::  {dep_type(), rsrc_spec()}
                    |   {dep_type(), rsrc_spec(), list() | tuple()}.

%% Despite what the docs imply, these types are NOT exported from their
%% Erlang/OTP modules.
-type year1970()    ::  1970..9999.         %%  calendar:year1970()
-type regex()       ::  tuple() | iodata(). %%  re:mp() | iodata()
-type file_mode()   ::  atom() | tuple().   %%  file:mode()

%% ===================================================================
%% Behavior
%% ===================================================================

-callback do(rebar_state()) -> {ok, rebar_state()} | prv_error().
-callback format_error(term()) -> iolist().
-callback init(rebar_state()) -> {ok, rebar_state()}.

-spec implements_behaviour(Module :: module()) -> boolean().
%%
%% @doc Reports whether the specified module implements this behavior.
%%
implements_behaviour(Module) ->
    case code:ensure_loaded(Module) of
        {'module', Module} ->
            lists:all(
                fun({Function, Arity}) ->
                    erlang:function_exported(Module, Function, Arity)
                end,
                [{'do', 1}, {'format_error', 1}, {'init', 1}]);
        _ ->
            'false'
    end.

%% ===================================================================
%% API
%% ===================================================================

-spec app_dir_to_name(AppDir :: fs_path()) -> app_name() | no_return().
%%
%% @doc Return the name of the app residing in AppDir.
%%
app_dir_to_name(AppDir) ->
    erlang:list_to_atom(erlang:hd(
        string:tokens(filename:basename(AppDir), "-"))).

-spec consult_app_file(Dir :: atom() | string(), FileName :: string())
        -> {'ok', [term()]} | err_result() | no_return().
%%
%% @doc Return the terms in FileName in the specified application directory.
%%
-ifdef(BRT_ESCRIPT_IO_MOD_KEY).
consult_app_file(Dir, FileName) ->
    case erlang:get(?BRT_ESCRIPT_IO_MOD_KEY) of
        'undefined' ->
            file_operation('consult', Dir, FileName);
        Mod ->
            Mod:consult_app_file(Dir, FileName)
    end.
-else.
consult_app_file(Dir, FileName) ->
    file_operation('consult', Dir, FileName).
-endif.

-spec dep_list(AppsOrDeps :: [brt:app_name() | brt:dep_spec()])
        -> [brt:dep_spec()].
%%
%% @doc Returns a list of dependency tuples from a possibly mixed input.
%%
dep_list(AppsOrDeps) ->
    case lists:all(fun erlang:is_tuple/1, AppsOrDeps) of
        'true' ->
            AppsOrDeps;
        _ ->
            [brt_config:pkg_dep(Elem) || Elem <- AppsOrDeps]
    end.

-spec dep_list_member(
        Package :: brt:app_name(),
        AppsOrDeps :: [brt:app_name() | brt:dep_spec()])
        -> boolean().
%%
%% @doc Reports whether an application is contained in a list of dependencies.
%%
dep_list_member(PkgSpec, AppsOrDeps) when ?is_rebar_dep(PkgSpec) ->
    is_list_key(erlang:element(1, PkgSpec), AppsOrDeps);
dep_list_member(PkgName, AppsOrDeps) ->
    is_list_key(to_atom(PkgName), AppsOrDeps).

-spec dep_name(Dep :: brt:app_name() | brt:dep_spec()) -> brt:app_name().
%%
%%
%%
dep_name(PkgName) when ?is_app_name(PkgName) ->
    PkgName;
dep_name(PkgSpec) when ?is_rebar_dep(PkgSpec) ->
    erlang:element(1, PkgSpec);
dep_name(Arg) ->
    erlang:error('badarg', [Arg]).

-spec file_error(File :: fs_path(), What :: atom() | tuple())
        -> {'error', string()}.
%%
%% @doc Returns a human-readable representation of a 'file' module error.
%%
file_error(File, What) ->
    {'error', lists:flatten([File, ": ", file:format_error(What)])}.

-spec format_error(Error :: term()) -> iolist().
%%
%% @doc Map errors to consistent messages.
%%
format_error(Error) when erlang:is_list(Error) ->
    Error;
format_error('app_undefined') ->
    "No top-level application defined.";
format_error({'copyright_dirty', File}) ->
    [File, ": Multiple or non-Basho copyrights, adjust manually"];
format_error('deps_mismatch') ->
    "Static and calculated dependencies differ.";
format_error(Error) ->
    io_lib:format("~p", [Error]).

-spec get_key_list(Key :: term(), Terms :: list()) -> list() | no_return().
%%
%% @doc Returns the list associated with the specified Key in Terms.
%%
%% If the key is not found an empty list is returned.
%% If Key exists as a standalone element in Terms, or is the first element of
%% a tuple whose size is not exactly two or whose second element is not a
%% list, a 'badarg' error is raised.
%%
get_key_list(Key, Terms) ->
    case get_key_tuple(Key, Terms) of
        'undefined' ->
            [];
        {_Key, List} when erlang:is_list(List) ->
            List;
        _ ->
            erlang:error('badarg', [Key, Terms])
    end.

-spec get_key_tuple(Key :: term(), Terms :: list())
        -> 'undefined' | tuple() | no_return().
%%
%% @doc Returns the tuple whose first element is Key in Terms.
%%
%% If the key is not found 'undefined' is returned.
%% This is roughly analogous to lists:keyfind(Key, 1, Terms) except that if Key
%% exists as a standalone element in Terms a 'badarg' error is raised.
%%
get_key_tuple(Key, [Key | _] = Terms) ->
    erlang:error('badarg', [Key, Terms]);
get_key_tuple(Key, [Term | _])
        when erlang:is_tuple(Term)
        andalso erlang:tuple_size(Term) >= 1
        andalso erlang:element(1, Term) =:= Key ->
    Term;
get_key_tuple(Key, [_ | Terms]) ->
    get_key_tuple(Key, Terms);
get_key_tuple(_, []) ->
    'undefined'.

-spec get_key_value(Key :: term(), Terms :: list())
        -> 'undefined' | term() | no_return().
%%
%% @doc Returns the value associated with the specified Key in Terms.
%%
%% If the key is not found 'undefined' is returned.
%% If Key exists as a standalone element in Terms, or is the first element of
%% a tuple whose size is not exactly two, a 'badarg' error is raised.
%%
get_key_value(Key, Terms) ->
    case get_key_tuple(Key, Terms) of
        'undefined' ->
            'undefined';
        {_Key, Value} ->
            Value;
        _ ->
            erlang:error('badarg', [Key, Terms])
    end.

-spec is_list_key(Key :: term(), Terms :: list()) -> boolean().
%%
%% @doc Reports whether Key is, or is the first element of, a member of Terms.
%%
%% Equivalent to:
%%  lists:member(Key, Terms) orelse lists:keymember(Key, 1, Terms).
%%
is_list_key(Key, [Key | _]) ->
    'true';
is_list_key(Key, [Term | _])
        when erlang:is_tuple(Term)
        andalso erlang:tuple_size(Term) >= 1
        andalso erlang:element(1, Term) =:= Key ->
    'true';
is_list_key(Key, [_ | Terms]) ->
    is_list_key(Key, Terms);
is_list_key(_, []) ->
    'false'.

-spec list_modules(ModPrefix :: string()) -> [module()] | err_result().
%%
%% @doc Return the list of modules in this application matching ModPrefix.
%%
%% ModPrefix is plain string that is matched against the module name as if by
%% lists:prefix/2.
%%
-ifdef(BRT_ESCRIPT_IO_MOD_KEY).
list_modules(ModPrefix) ->
    case erlang:get(?BRT_ESCRIPT_IO_MOD_KEY) of
        'undefined' ->
            ObjDir  = brt_dir('obj'),
            ObjExt  = code:objfile_extension(),
            Pattern = lists:flatten([ModPrefix, $*, ObjExt]),
            Objs    = filelib:wildcard(Pattern, ObjDir),
            [erlang:list_to_atom(filename:basename(F, ObjExt)) || F <- Objs];
        Mod ->
            Mod:list_modules(ModPrefix)
    end.
-else.
list_modules(ModPrefix) ->
    ObjDir  = brt_dir('obj'),
    ObjExt  = code:objfile_extension(),
    Pattern = lists:flatten([ModPrefix, $*, ObjExt]),
    Objs    = filelib:wildcard(Pattern, ObjDir),
    [erlang:list_to_atom(filename:basename(F, ObjExt)) || F <- Objs].
-endif.

-spec read_app_file(Dir :: atom() | string(), FileName :: string())
        -> {'ok', binary()} | err_result() | no_return().
%%
%% @doc Return the contents of FileName in the specified application directory.
%%
-ifdef(BRT_ESCRIPT_IO_MOD_KEY).
read_app_file(Dir, FileName) ->
    case erlang:get(?BRT_ESCRIPT_IO_MOD_KEY) of
        'undefined' ->
            file_operation('read_file', Dir, FileName);
        Mod ->
            Mod:consult_app_file(Dir, FileName)
    end.
-else.
read_app_file(Dir, FileName) ->
    file_operation('read_file', Dir, FileName).
-endif.

-spec to_atom(Term :: atom() | binary() | [byte() | [byte()]])
        -> atom() | no_return().
%%
%% @doc Return Term as an atom.
%%
to_atom(Term) when erlang:is_atom(Term) ->
    Term;
to_atom(Term) when erlang:is_binary(Term) ->
    erlang:binary_to_atom(Term, 'latin1');
to_atom(Term) when erlang:is_list(Term) ->
    erlang:list_to_atom(lists:flatten(Term));
to_atom(Term) ->
    erlang:error('badarg', [Term]).

-spec to_binary(Term :: atom() | binary() | [byte() | [byte()]])
        -> binary() | no_return().
%%
%% @doc Return Term as a binary.
%%
to_binary(Term) when erlang:is_atom(Term) ->
    erlang:atom_to_binary(Term, 'latin1');
to_binary(Term) when erlang:is_binary(Term) ->
    Term;
to_binary(Term) when erlang:is_list(Term) ->
    erlang:list_to_binary(Term);
to_binary(Term) ->
    erlang:error('badarg', [Term]).

-spec to_string(Term :: atom() | binary() | [char() | [char()]])
        -> string() | no_return().
%%
%% @doc Return Term as a flat array of characters.
%%
to_string(Term) when erlang:is_atom(Term) ->
    erlang:atom_to_list(Term);
to_string(Term) when erlang:is_binary(Term) ->
    erlang:binary_to_list(Term);
to_string(Term) when erlang:is_list(Term) ->
    case io_lib:deep_char_list(Term) of
        'true' ->
            lists:flatten(Term);
        _ ->
            erlang:error('badarg', [Term])
    end;
to_string(Term) ->
    erlang:error('badarg', [Term]).

%% ===================================================================
%% Internal
%% ===================================================================

-spec file_operation(
    Func :: atom(), Dir :: atom() | string(), FileName :: string())
        -> {'ok', term()} | err_result() | no_return().
%
% Invoke file:Func(FullFilePath) on an application file.
%
file_operation(Func, Dir, FileName) ->
    case brt_dir(Dir) of
        {'error', _} = Err ->
            Err;
        Path ->
            File = filename:join(Path, FileName),
            case file:Func(File) of
                {'error', What} ->
                    file_error(File, What);
                Result ->
                    Result
            end
    end.

%
% VERY tightly coupled code follows - beware infinite recursion!
%

-spec brt_dir(atom() | string()) -> fs_path() | err_result() | no_return().

brt_dir('app' = What) ->
    Key = {?MODULE, What},
    case erlang:get(Key) of
        'undefined' ->
            ADir = case code:lib_dir(?APP_NAME_ATOM) of
                [_|_] = Lib ->
                    case filelib:is_dir(Lib) of
                        'true' ->
                            Lib;
                        _ ->
                            filename:dirname(brt_dir('this'))
                    end;
                _ ->
                    filename:dirname(brt_dir('this'))
            end,
            erlang:put(Key, ADir),
            ADir;
        Val ->
            Val
    end;

brt_dir("ebin") ->
    brt_dir('ebin');

brt_dir('ebin' = What) ->
    Key = {?MODULE, What},
    case erlang:get(Key) of
        'undefined' ->
            EBin = case code:lib_dir(?APP_NAME_ATOM, What) of
                [_|_] = Lib ->
                    case filelib:is_dir(Lib) of
                        'true' ->
                            Lib;
                        _ ->
                            brt_dir('this')
                    end;
                _ ->
                    brt_dir('this')
            end,
            erlang:put(Key, EBin),
            EBin;
        Val ->
            Val
    end;

brt_dir('obj') ->
    brt_dir('ebin');

brt_dir("priv") ->
    brt_dir('priv');

brt_dir('priv' = What) ->
    Key = {?MODULE, What},
    case erlang:get(Key) of
        'undefined' ->
            PDir = case code:priv_dir(?APP_NAME_ATOM) of
                [_|_] = Lib ->
                    case filelib:is_dir(Lib) of
                        'true' ->
                            Lib;
                        _ ->
                            find_priv_dir()
                    end;
                _ ->
                    find_priv_dir()
            end,
            erlang:put(Key, PDir),
            PDir;
        Val ->
            Val
    end;

brt_dir('this' = What) ->
    Key = {?MODULE, What},
    case erlang:get(Key) of
        'undefined' ->
            ODir = filename:dirname(code:which(?MODULE)),
            erlang:put(Key, ODir),
            ODir;
        Val ->
            Val
    end;

brt_dir(Dir) when erlang:is_list(Dir) ->
    Path = filename:join(brt_dir('app'), lists:flatten(Dir)),
    case filelib:is_dir(Path) of
        'true' ->
            Path;
        _ ->
            case filelib:is_file(Path) of
                'true' ->
                    {'error', {'enotdir', Path}};
                _ ->
                    {'error', {'enoent', Path}}
            end
    end;

brt_dir(Dir) when erlang:is_atom(Dir) ->
    brt_dir(erlang:atom_to_list(Dir));

brt_dir(Arg) ->
    erlang:error('badarg', [Arg]).

-spec find_priv_dir() -> fs_path() | err_result() | no_return().

find_priv_dir() ->
    find_priv_dir(['app', 'obj', 'this']).

-spec find_priv_dir([atom()]) -> fs_path() | err_result() | no_return().

find_priv_dir(['app' = Parent | More]) ->
    Dir = filename:join(brt_dir(Parent), "priv"),
    case filelib:is_dir(Dir) of
        'true' ->
            Dir;
        _ ->
            find_priv_dir(More)
    end;

find_priv_dir([Sibling | More]) ->
    Dir = filename:join(filename:dirname(brt_dir(Sibling)), "priv"),
    case filelib:is_dir(Dir) of
        'true' ->
            Dir;
        _ ->
            find_priv_dir(More)
    end;

find_priv_dir([]) ->
    {'error', "Application private directory not found"}.

