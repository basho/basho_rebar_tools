%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Basho Technologies, Inc.
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

%% @private
%% @doc Adjust the Rebar state for Basho tools.
%%
%% It would be preferable to do this with hooks, but Rebar hooks can't
%% change the state seen by subsequent tasks, so we do it globally at
%% initialization.
%%
-module(brt_inject_state).

% Private API
-export([
    inject/1
]).

-include("brt.hrl").

-record(tgt, {
    profile             :: atom(),
    app                 :: atom(),
    define  = default   :: atom() | default,
    header  = undefined :: string() | default | undefined
}).

-define(INJECT_TARGETS, [
    #tgt{profile = test,  app = eqc,  header = default}
]).

%% ===================================================================
%% Private API
%% ===================================================================

-spec inject(State :: brt:rebar_state()) -> brt:rebar_state().
%% @private
%% @doc Inject whatever changes we need into the State.
%%
inject(State) ->
    lists:foldl(fun inject/2, State, ?INJECT_TARGETS).

%% ===================================================================
%% Internal
%% ===================================================================

-spec inject(Target :: #tgt{}, State :: brt:rebar_state()) -> brt:rebar_state().
%
% Default is to add `{d, <uppercase-App>}' to `erl_opts' in Profile if the App is
% present. If that element is already present in State, the test is skipped.
% Default is to pass expanded Target to inject_target/2.
% Add discriminating function heads to override default behavior.
%
inject(Target, State) ->
    ?LOG_DEBUG("~s:inject(~p, _State).", [?MODULE, Target]),
    Tgt = expand_defaults(Target),
    Defd = lists:any(
        fun({d, Def}) ->
                Def =:= Tgt#tgt.define;
            ({d, Def, _}) ->
                Def =:= Tgt#tgt.define;
            (_) ->
                false
        end, brt:get_key_list(erl_opts, get_profile(Tgt#tgt.profile, State))),
    if
        Defd ->
            State;
        ?else ->
            inject_target(Tgt, State)
    end.

-spec inject_target(Tgt :: #tgt{}, State :: brt:rebar_state()) -> brt:rebar_state().
%
% Default is to look for a header file with the app's name as part of the test.
% When the test is positive, `{d, <uppercase-Target>}' is added to `erl_opts' in Profile.
% Add discriminating function heads to override default behavior.
%
inject_target(#tgt{header = undefined} = Tgt, State) ->
    ?LOG_DEBUG("~s:inject_target(~p, _State).", [?MODULE, Tgt]),
    case code:lib_dir(Tgt#tgt.app) of
        {error, bad_name} ->
            State;
        _ ->
            set_target_opts(Tgt, State)
    end;

inject_target(Tgt, State) ->
    ?LOG_DEBUG("~s:inject_target(~p, _State).", [?MODULE, Tgt]),
    case code:lib_dir(Tgt#tgt.app, include) of
        {error, bad_name} ->
            State;
        Dir ->
            Incl = filename:join(Dir, Tgt#tgt.header),
            ?LOG_DEBUG("Checking ~s", [Incl]),
            case filelib:is_regular(Incl) of
                true ->
                    set_target_opts(Tgt, State);
                _ ->
                    State
            end
    end.

-spec expand_defaults(Tgt :: #tgt{}) -> #tgt{}.
%
% Fill in the blanks ...
%
expand_defaults(#tgt{app = App, define = default} = Tgt) ->
    expand_defaults(Tgt#tgt{define = brt:to_atom(string:to_upper(brt:to_string(App)))});
expand_defaults(#tgt{app = App, header = default} = Tgt) ->
    expand_defaults(Tgt#tgt{header = lists:flatten(io_lib:format("~s.hrl", [App]))});
% would be nice to make this conditional, if only we had a preprocessor ...
expand_defaults(Tgt) when not (
        erlang:is_record(Tgt, tgt)
        andalso erlang:is_atom(Tgt#tgt.profile)
        andalso erlang:is_atom(Tgt#tgt.app)
        andalso erlang:is_atom(Tgt#tgt.define)
        andalso (Tgt#tgt.header =:= undefined
                orelse erlang:is_list(Tgt#tgt.header)) ) ->
    erlang:error(badarg, [Tgt]);
expand_defaults(Tgt) ->
    Tgt.

-spec get_profile(Name :: atom(), State :: brt:rebar_state()) -> list().
%
% Returns the Named profile from State.
%
% The result is always a list, even if the profile is undefined.
%
get_profile(Name, State) ->
    case rebar_state:get(State, profiles) of
        undefined ->
            [];
        Profs ->
            brt:get_key_list(Name, Profs)
    end.

-spec set_profile(Name :: atom(), Value :: list(), State :: brt:rebar_state())
        -> brt:rebar_state().
%
% Updates the Named profile in State and returns the new State.
%
set_profile(Name, Value, State) ->
    Profiles = case rebar_state:get(State, profiles) of
        undefined ->
            [{Name, Value}];
        Profs ->
            lists:keystore(Name, 1, Profs, {Name, Value})
    end,
    rebar_state:set(State, profiles, Profiles).

-spec set_target_opts(Tgt :: #tgt{}, State :: brt:rebar_state()) -> brt:rebar_state().
%
% Add the appropriate options to the target profile.
%
set_target_opts(Tgt, State) ->
    Orig = get_profile(Tgt#tgt.profile, State),
    % we wouldn't be here if it was already defined
    Opts = [{d, Tgt#tgt.define} | brt:get_key_list(erl_opts, Orig)],
    Prof = lists:keystore(erl_opts, 1, Orig, {erl_opts, Opts}),
    set_profile(Tgt#tgt.profile, Prof, State).
