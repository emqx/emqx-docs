# emqx-lua-hook

Before EMQX 4.1. We only provide multi-language support for Lua. Its architecture is different from the above mentioned, which will include the entire language runtime in the Erlang VM:

![Old Multiple Lang Arch](./assets/lua-lang-arch.png)

- Multiple language support appears as a plug-in. For different language environments, different language support plugins are required.
- This supported plugin embeds all the environments of the language runtime.

To maintain compatibility, the plug-in remains in the release version of EMQX.


## Example

In the EMQX Broker distribution, user-defined Lua script files should be placed in `data/script/`.

Take the sending content of the control message as an example, and add the file `data/script/test.lua`:

```lua
function on_message_publish(clientid, username, topic, payload, qos, retain)
    return topic, "hello", qos, retain
end

function register_hook()
    return "on_message_publish"
end
```

The script shows:

- Implemented a callback function `on_message_publish` and changed the ` payload` field of all published messages to `hello`.
- Use `register_hook` to tell ` emqx_lua_hook` the name list of callback function that need to be registered.

It is worth noting that the names, parameters, data types, and number of these callback functions are fixed and must be consistent with the examples provided.

After the script is written, you need to manually load it into the `emqx_lua_hook` plugin:

The `emqx_lua_hook` plugin is enabled at first:

```bash
./bin/emqx_ctl plugins load emqx_lua_hook
```

Load `test.lua` into ` emqx_lua_hook`:

```bash
./bin/emqx_ctl luahook load test.lua
```

When the execution succeeds, it means that the script has been successfully loaded. Otherwise, check whether the syntax of the source file is correct.

After completion, you can start two MQTT clients, one to subscribe to any topic, and the other to publish any message to the topic that you just subscribed to. It can be found that the message content received by the subscriber is `hello` which proves that the `test.lua` script has taken effect.

## Callback function

Supported callback functions and parameter type: [emqx-lua-hook - README.md](https://github.com/emqx/emqx-lua-hook/tree/develop#hook-api)

Example: [examples.lua](https://github.com/emqx/emqx-lua-hook/blob/develop/examples.lua)

## Command

Load the specified Lua script:

```bash
## Script: Script file name
luahook load <Script>
```

Unload the specified Lua script:
```bash
luahook unload <Script>
```

Reload the specified Lua script:
```bash
luahook reload <Script>
```

Load the specified Lua script and set it to start with `emqx_lua_hook`:
```bash
luahook enable <Script>
```

Unload the specified Lua script and cancel it to start with `emqx_lua_hook`:
```bash
luahook disable <Script>
```
