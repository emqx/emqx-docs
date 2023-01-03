# EMQX Extensions

EMQX extensions are implemented by adding callbacks to various hook points.

See [Hooks](hooks.md) for more information.

## Multi-language support

Since 4.1, EMQX provides **multi-language support**. It allows users to use Python, Java or other programming languages to handle EMQX [hooks](./hooks.md), or parse your private protocol on TCP/UDP transport. In this way, you can customize EMQX with other programming languages.

::: tip Tip

- In 4.1 and 4.2, this functional was implemented using [erlport](https://github.com/emqx/erlport) as the low-level communication support, and it was upgraded to [gRPC](https://grpc.io) in the 4.3. **These two version interface are not compatible**. Please check the corresponding documentation for the older implemented.

- After 4.3, all hooks are publicly supported.
:::


### Extension Hook

The **Extension Hook** is supported by the **emqx-exhook** plugin. It allows users to process EMQX [Hooks](hooks.md) using other programming languages. For example:

- Check the login permission for a client.
- Check the PUB/SUB operation privileges for a client.
- Bridge or Store a message

See: [Extension Hook](./lang-exhook.md).

<!-- ### Extension Protocol -->

<!-- The **Extension Protocol** is supported by the **emqx-exproto** plugin. It allows users to implement their private access protocol using other programming languages. In the `emqx-exproto`, you can: -->

<!-- - Establish a connection based on TCP/SSL/UDP/DTLS -->
<!-- - Subscribe a topic to receive messages and deliver it to your client -->
<!-- - Parse bytes and publish it to EMQX -->


<!-- ### Legacy solutions -->

<!-- Prior to EMQX 4.1, only Lua support was included, and it was implemented by `emqx-lua-hook`. This plugin only supports handling of hooks, not protocol access. -->

<!-- See: [Lua Support](lang-lua.md). -->
