# Plugins

The EMQX Broker distribution contains a large number of official plugins, which provide some basic or various extended functions.

They rely on the code API of  [emqx](https://github.com/emqx/emqx) or [hooks](hooks.md) for their special functions.

You can compile it with the [emqx](https://github.com/emqx/emqx) core project and package it into a working package.

::: tip
EMQX Enteprise does not provide source code for commercial sales, and does not support customers to develop and compile plugins by themselves.
:::

## List of plugins

 The official plug-ins provided by EMQX include:

| Plugin                                                       | Configuration file                    | Description                        |
| ------------------------------------------------------------ | ------------------------------------- | ---------------------------------- |
| [emqx_dashboard](https://github.com/emqx/emqx/tree/main-v4.3/lib-ce/emqx_dashboard)   | etc/plugins/emqx_dashboard.conf       | Web dashboard Plugin (Default)     |
| [emqx_management](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_management)   | etc/plugins/emqx_management.conf      | HTTP API and CLI Management Plugin |
| [emqx_auth_mnesia](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_mnesia) | etc/plugins/emqx_auth_mnesia.conf     | Mnesia Auth/access control         |
| [emqx_auth_jwt](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_jwt)       | etc/plugins/emqx_auth_jwt.conf        | JWT Auth/access control            |
| [emqx_auth_ldap](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_ldap)     | etc/plugins/emqx_auth_ldap.conf       | LDAP Auth/access control           |
| [emqx_auth_http](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_http)     | etc/plugins/emqx_auth_http.conf       | HTTP Auth/access control           |
| [emqx_auth_mongo](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_mongo)   | etc/plugins/emqx_auth_mongo.conf      | MongoDB Auth/access control        |
| [emqx_auth_mysql](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_mysql)   | etc/plugins/emqx_auth_mysql.conf      | MySQL Auth/access control          |
| [emqx_auth_pgsql](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_pgsql)   | etc/plugins/emqx_auth_pgsql.conf      | PostgreSQL Auth/access control     |
| [emqx_auth_redis](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_redis)   | etc/plugins/emqx_auth_redis.conf      | Redis Auth/access control          |
| [emqx_psk_file](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_psk_file)       | etc/plugins/emqx_psk_file.conf        | PSK support                        |
| [emqx_web_hook](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_web_hook)       | etc/plugins/emqx_web_hook.conf        | Web Hook Plugin                    |
| [emqx_lua_hook](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_lua_hook)       | etc/plugins/emqx_lua_hook.conf        | Lua Hook Plugin                    |
| [emqx_retainer](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_retainer)       | etc/plugins/emqx_retainer.conf        | Retain Message storage module      |
| [emqx_rule_engine](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_rule_engine) | etc/plugins/emqx_rule_engine.conf     | Rule engine                        |
| [emqx_bridge_mqtt](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_bridge_mqtt) | etc/plugins/emqx_bridge_mqtt.conf     | MQTT Message Bridge Plugin         |
| [emqx_coap](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_coap)               | etc/plugins/emqx_coap.conf            | CoAP protocol support              |
| [emqx_lwm2m](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_lwm2m)             | etc/plugins/emqx_lwm2m.conf           | LwM2M protocol support             |
| [emqx_sn](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_sn)                   | etc/plugins/emqx_sn.conf              | MQTT-SN protocol support           |
| [emqx_stomp](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_stomp)             | etc/plugins/emqx_stomp.conf           | Stomp protocol support             |
| [emqx_recon](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_recon)             | etc/plugins/emqx_recon.conf           | Recon performance debugging        |
| [emqx_plugin_template](https://github.com/emqx/emqx-plugin-template)               | etc/plugins/emqx_plugin_template.conf | plugin develop template            |

