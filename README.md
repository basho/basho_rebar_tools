#Basho Tools for Rebar3

Basho Rebar Tools (BRT) adds commands to [Rebar3][rebar3] supporting Basho's development of our products.

##WARNING

This is very much a ***WORK IN PROGRESS*** - docs are missing or wrong, code may or may not do what it's supposed to, and pretty much everything is subject to change.
Something that kinda sorta works may exist on the `develop` branch.
If you want to try some things out, you're advised to talk to Ted first.

##What It Does

The current focus is on generating and verifying true dependencies.

There's functionality to create/update cooperating `Makefile` and `rebar.config` files.

There's also fairly robust `git` manipulation capability, but it needs a command strategy for exposing it as rebar commands.

##How to Use It

###Add The Plugin

The following addition to `rebar.config` makes the plugin available for use:

```erlang
{plugins, [
    {basho_rebar_tools,
        {git, "git://github.com/tburghart/basho_rebar_tools.git",
        {branch, "develop"}}}
]}.
```

###Commands

Whatever's enabled through the rebar command structure will show up in `rebar3 help` as commands named `brt-<something>`. The command `rebar3 help brt-<something>` provides _at least_ a brief description of the command and a listing of its options.

###Configuration

By default, the plugin looks for its configuration in a file named `brt.config` in the current working directory, though it can be made to look elsewhere or for a different file name by setting the `BRT_CONFIG` environment variable.

The command `rebar3 brt-info` displays the effective configuration.
Until I get around to documenting it, read the comments in the `priv/default.brt.config` file to see what you can adjust.

##License

Everything here is covered by this [license][].


  [license]:    LICENSE
  [rebar3]:     https://www.rebar3.org
  [rebar3cfg]:  https://www.rebar3.org/docs/configuration
  [rebar3src]:  https://github.com/erlang/rebar3

