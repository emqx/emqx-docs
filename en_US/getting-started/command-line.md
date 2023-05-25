# Basic Commands

EMQX provides the `emqx` command line tool to start, stop, and enter the console of EMQX.

+ `emqx start`

    Start EMQX

+ `emqx stop`

    Stop EMQX

+ `emqx restart`

    Restart EMQX

+ `emqx console`

    Start EMQX with console

+ `emqx foreground`

    Start EMQX with console. Unlike `emqx console` , `emqx foreground` does not support entering Erlang commands;

+ `emqx ping`

    Ping EMQX

+ `emqx check_conf`

   Check if the configuration file format is ok.  You may run this command before starting to check if the configuration file format is correct.

The commands mentioned above are frequently utilized by users. In addition, EMQX provides a range of [advanced commands](https://chat.openai.com/advanced/cli.md) specifically designed to enhance developers' convenience. Feel free to click the link provided to explore these advanced commands further.
