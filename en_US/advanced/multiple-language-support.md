---
# 编写日期
date: 2020-02-21 09:28:26
# 作者 Github 名称
author: hjianbo
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref: undefinedM
---

# Multi-language support

From 4.1, EMQX provides a dedicated multi-language support plug-in of [emqx_extension_hook](https://github.com/emqx/emqx-extension-hook) to optimize multi-language support.

This plugin allows you to use other programming languages to handle hook events in EMQX, for example:

- Authenticate the login authority of a client.
- Check the operation permission of PUB/SUB of a client.
- Handle Session and Message events.

::: tip Tip
Message hooks are only supported in the Enterprise Edition.
:::

## Architecture

The overall event transfer architecture is as follows:

```
                            EMQX
                            +============================+
                            |        Extension           |
 +----------+    CONNECT    | Hooks +----------------+   |
 |  Client  | <===========> - - - ->|    Drivers     |   |
 +----------+    PUB/SUB    |       +----------------+   |
                            |               |            |
                            +===============|============+
                                            |
                                            | Callbacks
             Third-party Runtimes           |
             +=======================+      |
             |  Python Script/ Java  |<-----+
             |  Classes/ Others      |
             +=======================+
```

- `emqx_extension_hook` as a plugin for EMQX:
  * It will receive all the hook events of EMQX and distribute them to the corresponding driver.
  * Provide drive management and statistics of various indicators.

- Support for different languages that requires corresponding driver support.

- The runtime of the three-party language and the runtime of Erlang are independent of each other, which only communicate through the pipeline provided by the operating system.

Theoretically, any other programming language can be expanded by this plug-in if only the corresponding driver is completed.

Currently, only Python and Java support is provided, and corresponding SDKs are provided to facilitate development.

## Quick start

### Python
Python development can refer to: [emqx-extension-python-sdk](https://github.com/emqx/emqx-extension-python-sdk)

### Java
Java development can refer to: [emqx-extension-java-sdk](https://github.com/emqx/emqx-extension-java-sdk)


## Other

Before EMQX 4.1. We only provide multi-language support for Lua. Its architecture is different from the above mentioned, which will include the entire language runtime in the Erlang VM:

![Old Multiple Lang Arch](./assets/lua-lang-arch.png)

- Multiple language support appears as a plug-in. For different language environments, different language support plugins are required.
- This supported plugin embeds all the environments of the language runtime.

To maintain compatibility, the plug-in remains in the release version of EMQX.

### Lua
Support of Lua is achieved by [emqx_lua_hook](https://github.com/emqx/emqx-lua-hook)  which includes:

- A set of Lua runtime environment, implemented by [luerl](https://github.com/rvirding/luerl)
- Some control commands to manage the load and unload of Lua.

#### Example

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

#### Callback function

Supported callback functions and parameter type: [emqx-lua-hook - README.md](https://github.com/emqx/emqx-lua-hook/tree/develop#hook-api)

Example: [examples.lua](https://github.com/emqx/emqx-lua-hook/blob/develop/examples.lua)

#### Command

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
