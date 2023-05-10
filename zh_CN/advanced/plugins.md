---
# 编写日期
date: 2020-02-18 17:15:26
# 作者 Github 名称
author: hjianbo
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref:
---

# 插件

EMQX 发行包中，包含了大量的官方插件，提供了一些基础的、或各类扩展的功能。

它们依赖于 [emqx](https://github.com/emqx/emqx) 的代码 API 或者 [钩子](hooks.md) 进行实现其特殊的功能。

然后通过打包编译工具 [emqx-rel](https://github.com/emqx/emqx-rel) 将其与 [emqx](https://github.com/emqx/emqx) 核心项目一起编译并打包至一个可运行的软件包中。

## 插件列表

目前 EMQX 发行包提供的插件包括：

| 插件                                                                 | 配置文件                              | 说明                      |
| -------------------------------------------------------------------- | ------------------------------------- | ------------------------- |
| [emqx_dashboard](https://github.com/emqx/emqx/tree/main-v4.3/lib-ce/emqx_dashboard)   | etc/plugins/emqx_dashbord.conf        | Web 控制台插件 (默认加载) |
| [emqx_management](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_management)   | etc/plugins/emqx_management.conf      | HTTP API and CLI  管理插件|
| [emqx_auth_mnesia](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_mnesia) | etc/plugins/emqx_auth_mnesia.conf     | Mnesia 认证 / 访问控制    |
| [emqx_auth_jwt](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_jwt)       | etc/plugins/emqx_auth_jwt.conf        | JWT 认证 / 访问控制       |
| [emqx_auth_ldap](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_ldap)     | etc/plugins/emqx_auth_ldap.conf       | LDAP 认证 / 访问控制      |
| [emqx_auth_http](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_http)     | etc/plugins/emqx_auth_http.conf       | HTTP API 与 CLI 管理插件  |
| [emqx_auth_mongo](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_mongo)   | etc/plugins/emqx_auth_mongo.conf      | MongoDB 认证 / 访问控制   |
| [emqx_auth_mysql](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_mysql)   | etc/plugins/emqx_auth_mysql.conf      | MySQL 认证 / 访问控制     |
| [emqx_auth_pgsql](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_pgsql)   | etc/plugins/emqx_auth_pgsql.conf      | PostgreSQL 认证 / 访问控制|
| [emqx_auth_redis](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_redis)   | etc/plugins/emqx_auth_redis.conf      | Redis 认证 / 访问控制     |
| [emqx_psk_file](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_psk_file)       | etc/plugins/emqx_psk_file.conf        | PSK 支持                  |
| [emqx_web_hook](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_web_hook)       | etc/plugins/emqx_web_hook.conf        | Web Hook 插件             |
| [emqx_lua_hook](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_lua_hook)       | etc/plugins/emqx_lua_hook.conf        | Lua Hook 插件             |
| [emqx_retainer](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_retainer)       | etc/plugins/emqx_retainer.conf        | Retain 消息存储模块       |
| [emqx_rule_engine](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_rule_engine) | etc/plugins/emqx_rule_engine.conf     | 规则引擎                  |
| [emqx_bridge_mqtt](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_bridge_mqtt) | etc/plugins/emqx_bridge_mqtt.conf     | MQTT 消息桥接插件         |
| [emqx_coap](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_coap)               | etc/plugins/emqx_coap.conf            | CoAP 协议支持             |
| [emqx_lwm2m](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_lwm2m)             | etc/plugins/emqx_lwm2m.conf           | LwM2M 协议支持            |
| [emqx_sn](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_sn)                   | etc/plugins/emqx_sn.conf              | MQTT-SN 协议支持          |
| [emqx_stomp](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_stomp)             | etc/plugins/emqx_stomp.conf           | Stomp 协议支持            |
| [emqx_recon](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_recon)             | etc/plugins/emqx_recon.conf           | Recon 性能调试            |
| [emqx_plugin_template](https://github.com/emqx/emqx-plugin-template)               | etc/plugins/emqx_plugin_template.conf | 代码热加载插件            |
