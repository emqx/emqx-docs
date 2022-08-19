# Basic commands

EMQX provides the `emqx` command line tool to start, stop, and enter the console of EMQX.

+ `emqx start`

    Start the EMQX Broker

+ `emqx stop`

    Stop the EMQX Broker

+ `emqx restart`

    Restart the EMQX Broker

+ `emqx console`

    Start the EMQX Broker with console

+ `emqx foreground`

    Start EMQX Broker with console. Unlike `emqx console` , `emqx foreground` does not support entering Erlang commands;

+ `emqx ping`

    Ping EMQX Broker

+ `emqx check_conf`

   Check if the configuration file format is ok.  You may run this command before starting to check if the configuration file format is correct.

The above commands are commonly used by users. In addition, the `emqx` command has some [other options](../advanced/cli.md) for the convenience of developers (ordinary users do not need to care about those).
