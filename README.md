# Basho Tools for Rebar3

Basho Rebar Tools (BRT) adds commands to [Rebar3][rebar3] supporting Basho's development of our products.

## Status

***Work In Progress!***

The `master` branch should always be stable.

The `develop` branch, if it differs from `master`, is generally stable but may contain features under development and/or review.

## What It Does

BRT is essentially a toolbox providing easy access to tools that are part of our development process.  There's no single theme, other than using the Rebar3 plugin mechanism to ensure that the tools are available in a known location whenever you're working in our source tree.

A few of the highlights are:

* List true dependencies.

* Check true versus configured dependencies.

* Display source tree version control information, including:
  * Current branch.
  * Current version.
  * Clean/dirty status.

* Synchronize with upstream repository branches.

* Create (and update*) coordinated project-level files.
  * `rebar.config` files are created and updated, with overwrite controls.
  * `.thumbs.yml` and `.gitignore` files are created from templates aligned with project configurations.
  * _Coordinated Makefiles can also be generated, but their use is discouraged._

There are robust `git` and `GitHub API` manipulation capabilities under the hood that can, and will, be leveraged as appropriate use cases arise.

> Just because we _can_ automate a bunch of repository manipulation operations, that doesn't make it a good idea.

## How to Use It

### Install or Update Rebar3

A recent version of Rebar3 is required, and the latest version is ***strongly*** recomended.

Proper handling of profiles requires Rebar3 version `3.3.3` or later.

> The latest stable version of Rebar3 can be downloaded directly from [here][rebar3dl].

### Add The Plugin

The following addition to `rebar.config` makes the plugin available for use:

```erlang
{plugins, [
    {basho_rebar_tools,
        {git, "https://github.com/basho/basho_rebar_tools.git",
        {branch, "master"}}}
]}.
```

#### OTP Support

Rebar3 and this plugin support all versions of Erlang/OTP from R16 on.

#### Where It's Installed

A growing list of Basho repositories build with Rebar3 using BRT.

Look for branch `feature/riak-2903/rebar3`.

### Commands

Once the plugin is included in `rebar.config`, list the available commands with `rebar3 help`.

BRT commands are all named `brt-<something>`, so they'll be grouped together near the top of the list of commands with short descriptions.

The command `rebar3 help brt-<something>` provides a more complete description of the command and its options.

### Configuration

By default, the plugin looks for its configuration in a file named `brt.config` in the current working directory, though it can be made to look elsewhere or for a different file name by setting the `BRT_CONFIG` environment variable.

The command `rebar3 brt-info` displays the effective configuration.

Refer to the extensive documentation in the default [brt.config](priv/defaults/brt.config) to see what you can adjust.

## Contributing

If something you try doesn't work, ***PLEASE*** file an [issue][issues].

Pull requests are welcome, but understand that we're interested in supporting Basho's workflow, which may not be the same as yours.

## License

Everything here is covered by this [license][].


  [license]:    LICENSE
  [issues]:     https://github.com/basho/basho_rebar_tools/issues
  [rebar3]:     https://www.rebar3.org
  [rebar3dl]:   https://s3.amazonaws.com/rebar3/rebar3
  [rebar3cfg]:  https://www.rebar3.org/docs/configuration
  [rebar3src]:  https://github.com/erlang/rebar3

