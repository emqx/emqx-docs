# Basic command

EMQX Broker provides the `emqx` command line tool, which is convenient for users to start, stop, and enter the console of EMQX Broker.

+   `emqx start`

    Start EMQX Broker；

+   `emqx stop`

    Stop EMQX Broker；

+   `emqx restart`

    Restart EMQX Broker；

+   `emqx console`

    Start EMQX Broker with console；

+   `emqx foreground`

    Start EMQX Broker with console. Unlike `emqx console` , `emqx foreground` does not support entering Erlang commands;

+   `emqx ping`

    Ping EMQX Broker。

The above commands are commonly used by users. In addition, the `emqx` command has some [other options](../advanced/cli.md) for the convenience of developers, and ordinary users do not need to care about that.