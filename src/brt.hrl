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

-ifndef(brt_included).
-define(brt_included, true).

-define(APP_NAME_DISPLAY,   "Basho Rebar Tools").

-define(APP_NAME_ATOM,      'basho_rebar_tools').
-define(APP_NAME_STRING,    "basho_rebar_tools").

-define(PRV_MOD_PREFIX,     "brt_prv_").
-define(PRV_CMD_PREFIX,     "brt-").

%
% Keep these usable as guard conditions.
%
-define(is_app_name(Name),  erlang:is_atom(Name)).

-define(is_rebar_dep(Spec), erlang:is_tuple(Spec)
    andalso erlang:tuple_size(Spec) >= 2
    andalso erlang:tuple_size(Spec) =< 4
    andalso erlang:is_atom(erlang:element(1, Spec))).

-define(is_rebar_dep(Name, Spec), erlang:is_tuple(Spec)
    andalso erlang:tuple_size(Spec) >= 2
    andalso erlang:tuple_size(Spec) =< 4
    andalso erlang:element(1, Spec) =:= Name).

%
% The earliest legitimate Basho copyright year.
% Basho was founded in 2008, but it was in January, so there are some 2007
% copyrights that are arguably ok.
%
-define(BASHO_YEAR_MIN, 2007).

%
% For debugging ONLY!
%
-define(BRT_VAR(Var),   io:format(
    'standard_error', "~s:~b: ~s = ~p\n", [?MODULE, ?LINE, ??Var, Var])).

%
% When built as an EScript, what strategy is used for finding files ...
% When the ?BRT_ESCRIPT_IO_MOD_KEY is set in the process environment, I/O on
% files in the application is redirected to operations in the module it points
% to. When it's unset, operations are performed on the filesystem in which the
% plugin was built.
%
-ifdef(brt_escript).
-define(BRT_ESCRIPT_IO_MOD_KEY, 'brt_escript_app_io_mod').
-endif.

-endif. % brt_included
