# CLI

## Main script

The main boot script of emqx is a bash script which can also used to
execute some of the administrative commands.

See `emqx help` for more information
Usage: emqx COMMAND [help]

```bash
$ emqx help
Usage: emqx COMMAND [help]

Commonly used COMMANDs:
  start:      Start EMQX in daemon mode
  console:    Start EMQX in an interactive Erlang or Elixir shell
  foreground: Start EMQX in foreground mode without an interactive shell
  stop:       Stop the running EMQX node
  ctl:        Administration commands, execute 'emqx ctl help' for more details

More:
  Shell attach:  remote_console | attach
  Up/Down-grade: upgrade | downgrade | install | uninstall
  Install info:  ertspath | root_dir | versions | root_dir
  Runtime info:  pid | ping | versions
  Advanced:      console_clean | escript | rpc | rpcterms | eval | eval-erl

Execute 'emqx COMMAND help' for more information
```

::: tip Tip
The `emqx_ctl` command actually invokes `emqx ctl` under the hood.
:::

## The ctl command

TODO: add emqx_ctl docs here
