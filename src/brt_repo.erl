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

-module(brt_repo).

% API
-export([
    added_year/2,
    branch/1,
    commit/1,
    dirty/1,
    pull/1,
    tag/1,
    version/1
]).

-export_type([
    version/0,
    vsn_type/0,
    year/0
]).

-include("brt.hrl").

-type dir()         ::  brt:fs_path().
-type file()        ::  brt:fs_path().
-type repo()        ::  brt:fs_path().
-type version()     ::  {vsn_type(), string()}.
-type vsn_type()    ::  branch | tag | ref.
-type year()        ::  brt:year1970().

-define(GIT_MIN_VSN,    [2, 0]).

%% ===================================================================
%% API
%% ===================================================================

-spec added_year(AppFile :: file(), Default :: term() | year())
        -> year() | term() | brt:err_result().
%%
%% @doc Return the year the specified file was first added to the repo.
%%
%% Probably only useful for generating missing copyright headers.
%%
added_year(AppFile, Default) ->
    case filelib:is_regular(AppFile) of
        true ->
            CmdOut = git_cmd(filename:dirname(AppFile),
                ["log", "--date=format:%Y", "--format=%ad",
                    "--", filename:basename(AppFile)], []),
            case CmdOut of
                {ok, [Year | _]} ->
                    erlang:list_to_integer(Year);
                {ok, []} ->
                    Default;
                Error ->
                    Error
            end;
        _ ->
            case filelib:is_file(AppFile) of
                true ->
                    {error, lists:flatten(
                        [AppFile, " exists but is not a file"])};
                _ ->
                    Default
            end
    end.

-spec branch(Repo :: repo()) -> version() | brt:err_result().
%%
%% @doc Reports the Repo's working branch.
%%
%% Returns:
%%
%%  {`branch', Branch}
%%      Latest commit is the HEAD of Branch.
%%
%%  {`error', noversion}
%%      Latest commit is not the HEAD of a branch.
%%
branch(Repo) ->
    version([branch], Repo).

-spec commit(Repo :: repo()) -> version() | brt:err_result().
%%
%% @doc Reports the Repo's latest commit.
%%
%% Returns:
%%
%%  {`ref', CommitHash}
%%      Latest commit.
%%
%%  {`error', noversion}
%%      The repo has no commits.
%%
commit(Repo) ->
    version([ref], Repo).

-spec dirty(Repo :: repo())
        -> false | {true, [string()]} | brt:err_result().
%%
%% @doc Reports whether there are uncommitted changes in Repo.
%%
%% Returns {`true', [OutLine]} if there are:
%%  - tracked files that have been changed
%%  - untracked files that are not ignored
%%
dirty(Repo) ->
    case filelib:is_dir(filename:join(Repo, ".git")) of
        true ->
            CmdOut = git_cmd(Repo, ["status", "--short"], []),
            case CmdOut of
                {ok, []} ->
                    false;
                {ok, Out} ->
                    {true, lists:reverse(Out)};
                Error ->
                    Error
            end;
        _ ->
            {error, lists:flatten([Repo, ": Not a Git repository"])}
    end.

-spec pull(Repo :: repo()) -> {ok, [string()]} | brt:err_result().
%%
%% @doc Updates Repo from its default remote repository.
%%
%% On success, the command's output is returned as a list of lines, without
%% their terminating newlines.
%%
pull(Repo) ->
    case filelib:is_dir(filename:join(Repo, ".git")) of
        true ->
            CmdOut = git_cmd(Repo, ["pull"], []),
            case CmdOut of
                {ok, Out} ->
                    {ok, lists:reverse(Out)};
                Error ->
                    Error
            end;
        _ ->
            {error, lists:flatten([Repo, ": Not a Git repository"])}
    end.

-spec tag(Repo :: repo()) -> version() | brt:err_result().
%%
%% @doc Reports the Repo's working tag.
%%
%% Returns:
%%
%%  {`tag', Tag}
%%      Latest commit is exactly Tag.
%%
%%  {`error', noversion}
%%      Latest commit is not exactly aligned with a tag.
%%
tag(Repo) ->
    version([tag], Repo).


-spec version(Repo :: repo()) -> version() | brt:err_result().
%%
%% @doc Reports the Repo's working version.
%%
%% Returns:
%%
%%  {`branch', Branch}
%%      Latest commit is the HEAD of Branch.
%%
%%  {`tag', Tag}
%%      Latest commit is exactly Tag.
%%
%%  {`ref', CommitHash}
%%      Latest commit.
%%
%%  {`error', `noversion'}
%%      The repo has no commits.
%%
version(Repo) ->
    version([branch, tag, ref], Repo).

%% ===================================================================
%% Internal
%% ===================================================================

-spec version(Types :: [vsn_type()], Repo :: repo())
        -> version() | brt:err_result().

version(Types, Repo) ->
    case filelib:is_dir(filename:join(Repo, ".git")) of
        true ->
            repo_version(Types, Repo);
        _ ->
            {error, lists:flatten([Repo, ": Not a Git repository"])}
    end.

-spec repo_version(Types :: [vsn_type()], Repo :: repo())
        -> version() | brt:err_result().

repo_version([branch | Types], Repo) ->
    CmdOut = git_cmd(Repo, ["branch", "--list"], []),
    case CmdOut of
        {ok, []} ->
            repo_version(Types, Repo);
        {ok, List} ->
            Filt =
             fun([$*, $\s, Ch | Rest]) when Ch /= $( ->
                    {true, [Ch | Rest]};
                (_) ->
                    false
            end,
            case lists:filtermap(Filt, List) of
                [Branch] ->
                    {branch, Branch};
                _ ->
                    repo_version(Types, Repo)
            end;
        Error ->
            Error
    end;
repo_version([tag | Types], Repo) ->
    CmdOut = git_cmd(Repo, ["describe", "--exact-match"], []),
    case CmdOut of
        % It's possible for more than one tag to point to the latest commit,
        % so just take the last one git printed out, which is (hopefully) the
        % newest.
        {ok, [Tag | _]} ->
            {tag, Tag};
        {ok, _} ->
            repo_version(Types, Repo);
        {error, {git, _, _, [Msg]}} = Error ->
            case re:run(Msg,
                    " no tag exactly matches ", [{capture, none}]) of
                match ->
                    repo_version(Types, Repo);
                _ ->
                    Error
            end;
        Error ->
            Error
    end;
repo_version([ref | Types], Repo) ->
    CmdOut = git_cmd(Repo, ["log", "--max-count=1", "--format=%H"], []),
    case CmdOut of
        {ok, [Hash]} ->
            {ref, Hash};
        {ok, _} ->
            repo_version(Types, Repo);
        Error ->
            Error
    end;
repo_version([], _Repo) ->
    {error, noversion}.

%% ===================================================================
%% Run Git
%% ===================================================================

-spec git_cmd(
        RunDir  :: dir(),
        ExeArgs :: [string()],
        ExeEnv  :: [{string(), string() | false}])
        -> {ok, [string()]} | brt:err_result().
%
% On success, output lines are returned in reverse order for efficiency!
%   There's a method to my madness - on success, there's often only one line
%   (or no lines) of output, so order doesn't matter.
%   The commands that DO output a lot are usually from the log, and their order
%   can be flipped with the --reverse switch. It's often the case that you only
%   want the first or last log message, so with an appropriate command you can
%   end up with what you want at the head of the list without ever having to
%   reorder it. Nevertheless, you can always reverse the list if you have to.
% If an error occurs, the output (presumably from stderr) is returned in its
% original order, since it's probably going to be passed upward to be displayed
% elsewhere.
%
git_cmd(RunDir, ExeArgs, ExeEnv) ->
    ExeKey = {?MODULE, git_exe},
    GitExe = case erlang:get(ExeKey) of
        undefined ->
            case find_git() of
                {ok, Exe} ->
                    erlang:put(ExeKey, Exe),
                    Exe;
                {notgit, Exe} ->
                    erlang:error(lists:flatten(
                        ["Executable '", Exe, "' does not appear to be Git"]));
                {oldgit, Exe} ->
                    erlang:error(lists:flatten(
                        ["Git xecutable '", Exe, "' is too old"]));
                _ ->
                    erlang:error("Git program not found or not executable")
            end;
        Val ->
            Val
    end,
    Port = erlang:open_port({spawn_executable, GitExe}, [
        {cd, RunDir}, {args, ExeArgs}, {env,  ExeEnv},
        {line, 16384}, exit_status, stderr_to_stdout, hide, eof]),
    try handle_port(Port, []) of
        {ok, Output} ->
            {ok, Output};
        {error, Why, Output} ->
            Cmd = lists:flatten(
                [RunDir, ": '", string:join([GitExe | ExeArgs], "' '"), $\']),
            {error, {git, Why, Cmd, lists:reverse(Output)}}
    after
        erlang:port_close(Port)
    end.

-spec find_git()
        -> {ok | notgit | oldgit, brt:fs_path()} | brt:err_result().
find_git() ->
    case check_git(os:getenv("GIT")) of
        {ok, _} = Ret ->
            Ret;
        _ ->
            check_git(os:find_executable("git"))
    end.

-spec check_git(Exe :: term())
        -> {ok | notgit | oldgit, brt:fs_path()} | brt:err_result().
check_git([_|_] = Exe) ->
    try
        Port = erlang:open_port({spawn_executable, Exe}, [
            {args, ["--version"]}, {env, []}, {line, 1024},
            exit_status, stderr_to_stdout, hide, eof]),
        try handle_port(Port, []) of
            {ok, [VsnLine]} ->
                case re:run(VsnLine, "^git\\s+version\\s+(\\S+)\\b",
                        [{capture, all_but_first, list}]) of
                    {match, [VsnStr]} ->
                        case brt:is_min_version(
                                ?GIT_MIN_VSN, brt:parse_version(VsnStr)) of
                            true ->
                                {ok, Exe};
                            _ ->
                                {oldgit, Exe}
                        end;
                    _ ->
                        {notgit, Exe}
                end;
            {ok, _} ->
                {notgit, Exe};
            {error, RC, _} ->
                {error, RC}
        after
            erlang:port_close(Port)
        end
    catch
        Type:What ->
            {Type, What}
    end;
check_git(Error) ->
    Error.

-spec handle_port(Port :: port(), Accum :: term())
        -> {ok, [string()]} | {error, pos_integer() | no_rc, [string()]}.

handle_port(Port, Accum) ->
    receive
        {Port, {data, Data}} ->
            handle_port(Port, Data, Accum);
        {Port, eof} ->
            handle_port(Port, eof, Accum)
    end.

-spec handle_port(Port :: port(), Data :: term(), Accum :: term())
        -> {ok, [string()]} | {error, pos_integer() | no_rc, [string()]}.

handle_port(Port, {eol, Data}, []) ->
    handle_port(Port, [Data]);
handle_port(Port, {eol, Data}, {Cont, Lines}) ->
    handle_port(Port, [lists:flatten([Cont | Data]) | Lines]);
handle_port(Port, {eol, Data}, Lines) ->
    handle_port(Port, [Data | Lines]);

handle_port(Port, {noeol, Data}, {Cont, Lines}) ->
    handle_port(Port, {[Cont | Data], Lines});
handle_port(Port, {noeol, Data}, Lines) ->
    handle_port(Port, {Data, Lines});

handle_port(Port, eof, {Cont, Lines}) ->
    handle_port(Port, eof, [lists:flatten(Cont) | Lines]);
handle_port(Port, eof, Lines) ->
    receive
        {Port, {exit_status, 0}} ->
            {ok, Lines};
        {Port, {exit_status, RC}} ->
            {error, RC, Lines}
    after
        1213 ->
            {error, no_rc, Lines}
    end.



