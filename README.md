# Basho Tools for Rebar3

Basho Rebar Tools (BRT) adds commands to [Rebar3][rebar3] supporting Basho's development of our products.

## WARNING

This is very much a ***WORK IN PROGRESS!***

Whatever you find on the `master` branch should be relatively stable.

Something that kinda sorta works may exist on the `develop` branch, but if you want to play with it you're advised to talk to Ted first.

## What It Does

The current focus is on generating and verifying true dependencies.

There's functionality to:
* Create/update cooperating `Makefile` and `rebar.config` files.
* List true dependencies.
* Check true versus configured dependencies.
* Read and write Rebar2 rebar.config elements.

There's also fairly robust `git` manipulation capability, but it needs a command strategy for exposing it as rebar commands.
Specifically, just because we _can_ automate a bunch of repository manipulation operations, that doesn't make it a good idea.

## How to Use It

### Update Rebar3

A recent version of Rebar3 is required, and the latest version is ***strongly*** recomended.

Proper handling of profiles requires Rebar3 version `3.3.2+build.3638` or later.
As of this writing, you'll have to build it yourself from the `master` branch at [the rebar3 GitHub repository][rebar3src].
The necessary changes will be included in the next release of Rebar3, presumably v3.3.3.

### Add The Plugin

The following addition to `rebar.config` makes the plugin available for use:

```erlang
{plugins, [
    {basho_rebar_tools,
        {git, "git://github.com/tburghart/basho_rebar_tools.git",
        {branch, "develop"}}}
]}.
```

### Commands

Whatever's enabled through the rebar command structure will show up in `rebar3 help` as commands named `brt-<something>`.

The command `rebar3 help brt-<something>` provides a description of the command and its options.

### Configuration

By default, the plugin looks for its configuration in a file named `brt.config` in the current working directory, though it can be made to look elsewhere or for a different file name by setting the `BRT_CONFIG` environment variable.

The command `rebar3 brt-info` displays the effective configuration.

Until I get around to documenting it, read the comments in [default.brt.config](priv/default.brt.config) to see what you can adjust.

## License

Everything here is covered by this [license][].


  [license]:    LICENSE
  [rebar3]:     https://www.rebar3.org
  [rebar3cfg]:  https://www.rebar3.org/docs/configuration
  [rebar3src]:  https://github.com/erlang/rebar3

