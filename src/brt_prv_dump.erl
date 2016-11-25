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
%% @doc BRT provider for the 'brt-dump' command.
%%
-module(brt_prv_dump).

%% provider behavior
-ifndef(brt_validate).
-behaviour(brt).
-endif.
-export([do/1, format_error/1, init/1, spec/0]).

-include("brt.hrl").

-define(PROVIDER_ATOM,  'brt-dump').
-define(PROVIDER_STR,   "brt-dump").
-define(PROVIDER_DEPS,  []).
-define(PROVIDER_OPTS,  []).

% Dialyzer doesn't want us peeking inside opaque types, but we want to give
% dicts special handling.
% In older Erlangs the compiler will complain if it encounters attribute
% declarations once functions have been defined, so this needs to stay up here
% for now.
-dialyzer({no_opaque, dump/3}).

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

-spec do(State :: brt:rebar_state()) -> {'ok', brt:rebar_state()}.
%%
%% @doc Display provider information.
%%
do(State) ->
    {dump(State), State}.

-spec format_error(Error :: term()) -> iolist().
%%
%% @doc Placeholder to fill out the 'provider' API, should never be called.
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
        {'bare',        'false'},
        {'deps',        ?PROVIDER_DEPS},
        {'example',     "rebar3 " ?PROVIDER_STR "[<field> ...]"},
        {'short_desc',  short_desc()},
        {'desc',        long_desc()},
        {'opts',        ?PROVIDER_OPTS}
    ].

%%====================================================================
%% Help Text
%%====================================================================

-spec short_desc() -> string().
short_desc() ->
    "Dumps assorted state, for debugging ONLY!".

-spec long_desc() -> string().
long_desc() ->
    short_desc() ++ "\n"
    "\n"
    "If you want to dump the entire rebar state, try the 'state' command for "
    "prettier formatting.\n"
    "This command allows dumping just a portion of the state, as specified by "
    "a list of record field names on the command line.\n".

%%====================================================================
%% Internal
%%====================================================================

% Oh so tightly coupled to the rebar state record :(
-define(STATE_FIELDS, [
    dir,
    opts,
    code_paths,
    default,
    escript_path,
    lock,
    current_profiles,
    namespace,
    command_args,
    command_parsed_args,
    current_app,
    project_apps,
    deps_to_build,
    all_plugin_deps,
    all_deps,
    resources,
    providers,
    allow_provider_overrides
]).

dump(State) ->
    Fields = case rebar_state:command_args(State) of
        [] ->
            lists:seq(1, erlang:length(?STATE_FIELDS));
        Args ->
            fields(Args, [])
    end,
    ['state_t' | Elems] = erlang:tuple_to_list(State),
    dump(Elems, 1, Fields).

fields([Field | Fields], Result) ->
    case list_pos(brt:to_atom(Field), ?STATE_FIELDS, 1) of
        0 ->
            fields(Fields, Result);
        N ->
            fields(Fields, [N | Result])
    end;
fields([], Result) ->
    Result.

-spec dump(
        Elems   :: [term()],
        Pos     :: non_neg_integer(),
        Fields  :: [non_neg_integer()])
        -> 'ok'.

dump([Elem | Elems], Pos, Fields)
        when erlang:is_tuple(Elem)
        andalso erlang:tuple_size(Elem) > 1
        andalso erlang:element(1, Elem) =:= 'dict' ->
    dump([dict:to_list(Elem) | Elems], Pos, Fields);

dump([Elem | Elems], Pos, Fields) ->
    case lists:member(Pos, Fields) of
        'true' ->
            io:format("~s:~n    ~p~n", [label(Pos), Elem]);
        _ ->
            'ok'
    end,
    dump(Elems, (Pos + 1), Fields);

dump([], _, _) ->
    'ok'.

label(Pos) when Pos =< erlang:length(?STATE_FIELDS) ->
    lists:nth(Pos, ?STATE_FIELDS);
label(Pos) ->
    erlang:integer_to_list(Pos).

list_pos(Elem, [Elem | _], Pos) ->
    Pos;
list_pos(Elem, [_ | Elems], Pos) ->
    list_pos(Elem, Elems, (Pos + 1));
list_pos(_, [], _) ->
    0.

