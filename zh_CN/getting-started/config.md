---
# 编写日期
date: 2020-02-25 17:15:26
# 作者 Github 名称
author: tigercl
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---

# 配置说明

## 简介

EMQ X 的配置文件通常以 `.conf` 作为后缀名，你可以在 `etc` 目录找到这些配置文件，主要配置文件包括：

{% emqxee %}

| 配置文件           | 说明                      |
| ------------------ | ------------------------- |
| etc/emqx.conf      | EMQ X 基础配置文件  |
| etc/cluster.conf      | EMQ X 集群相关配置文件  |
| etc/rpc.conf      | EMQ X 远程调用配置文件  |
| etc/logger.conf      | EMQ X 日志配置文件  |
| etc/zones.conf      | EMQ X Zone配置文件  |
| etc/listeners.conf      | EMQ X 监听端口配置文件  |
| etc/sys_mon.conf      | EMQ X 告警监控配置文件  |
| etc/acl.conf       | EMQ X 默认 ACL 规则配置文件 |
| etc/plugins/*.conf | EMQ X 扩展插件配置文件      |

{% endemqxee %}

{% emqxce %}

| 配置文件           | 说明                      |
| ------------------ | ------------------------- |
| etc/emqx.conf      | EMQ X 配置文件  |
| etc/acl.conf       | EMQ X 默认 ACL 规则配置文件 |
| etc/plugins/*.conf | EMQ X 扩展插件配置文件      |

{% endemqxce %}


需要注意的是，安装方式不同 `etc` 目录所处的路径可能不同，具体请参见 [目录结构](./directory.md#)。

## 语法规则

- 采用类似 sysctl 的 k = v 通用格式
- 单个配置项的所有信息都在同一行内，换行意味着创建一个新的配置项
- 键可以通过 `.` 进行分层，支持按树形结构管理配置项
- 值的类型可以是 `integer`, `fload`, `percent`, `enum`, `ip`, `string`, `atom`, `flag`, `duration` and `bytesize`
- 任何以＃开头的行均被视为注释

**示例：**

```bash
mqtt.max_packet_size = 1MB
```

## 数据类型

**integer**

整型数据。

**float**

浮点型数据。

**percent**

以 `%` 结尾的百分比数据，最终会被转换为 `float` 类型。

**enum**

通常我们会在类型为 `enum` 的配置项附近列出它的所有可选值。当然，你也可以查找 [配置项](../configuration/configuration.md)。

**ip**

当你看到某个配置项的数据类型为 `ip` 时，意味着你可以使用 `<IP>:<Port>` 的形式来设置该配置项，例如 `0.0.0.0:1883`。

**string**

`*.conf` 文件中除注释以外的所有内容都会先被解析成字符串再转换为其他类型，因此没有必要对 `string` 类型的值额外使用双引号对值进行修饰，并且这种方式也不被支持。

*Yes!*

```bash
dir = tmp
```

*No!!!*

```bash
dir = "tmp"		
```

**atom**

`atom` 类型的值最终会转换成 Erlang 的 `atom`，但它在 `*.conf` 文件中的使用方式与 `string` 完全一致。

**flag**

`flag` 用于那些具有两个可能值的变量，`flag` 默认可用值为 `on` 和 `off`，它们将映射为 `true` 和 `false` 以供应用程序使用。如果我们为某个配置项建立了其他的映射关系，我们会在配置文件中注明，你也可以在 [配置项](../configuration/configuration.md) 中查找这些信息。

**duration**

`duration` 用于指定那些固定的时间间隔，你可以使用以下时间单位：

- f - fortnight
- w - week
- d - day
- h - hour
- m - minute
- s - second
- ms - millisecond

你可以任意组合这些时间单位，例如 `1w13ms`，也可以使用浮点数，例如 `0.5d`，这些时间间隔最终将会被转换成我们指定的基准单位。这里有一点需要注意，如果你以毫秒为单位设置了某个配置项，而它的基准单位为秒，那么它将向上舍入至最接近的描述，例如 `1s50ms` = `2s`。因此，我们会列出这一类配置项的基准单位。

**bytesize**

`bytesize` 支持以更易读的方式来设置报文大小、缓冲区大小等配置，单位可以是 `KB`，`MB` 和 `GB`，你也可以使用小写，例如 `kb`，但不支持大小写混合，例如 `Kb`，它们最终都将被转换为字节数。如果你未指定任何单位，那么它被直接作为字节数使用。

## 默认配置

在 EMQ X 的配置文件中，你会看到很多被注释掉的配置项，这意味着这些配置项会使用他们的默认值，通常我们会列出这些配置的默认值。

## Zone & Listener

EMQ X 提供了非常多的配置项，并支持全局配置和局部配置。例如，EMQ X 提供了匿名访问的功能，即允许客户端不需要用户名与密码就能连接 Broker，通常在用户的生产环境中，此功能被默认关闭，但用户可能又希望在他的内网环境中启用此功能。从 3.0 版本开始，EMQ X 就通过 Zone 与 Listener 为用户提供了这种可能。

### Listener

Listener 主要用于配置不同协议的监听端口和相关参数，EMQ X 支持配置多个 Listener 以同时监听多个协议或端口，以下是支持的 Listener：

| 监听器                     | 说明                                             |
| ------------------------- | ------------------------------------------------------- |
| TCP Listener              | A listener for MQTT which uses TCP                      |
| SSL Listener              | A secure listener for MQTT which uses TLS               |
| Websocket Listener        | A listener for MQTT over WebSockets                     |
| Secure Websocket Listener | A secure listener for MQTT over secure WebSockets (TLS) |

EMQ X 默认提供 5 个 Listener，它们将占用以下端口：

| 端口   | 说明                                       |
| ----- | ------------------------------------------ |
| 1883  | MQTT/TCP 协议端口                           |
| 11883 | MQTT/TCP 协议内部端口，仅用于本机客户端连接 |
| 8883  | MQTT/SSL 协议端口                           |
| 8083  | MQTT/WS 协议端口                            |
| 8084  | MQTT/WSS 协议端口                           |

Listener 配置项的命名规则为 `listener.<Protocol>.<Listener Name>.xxx`，`<Protocol>` 即 Listener 使用的协议，目前支持 `tcp`, `ssl`, `ws`, `wss`。`<Listener Name>` 可以随意命名，但建议是全小写的英文单词，`xxx` 则是具体的配置项。不同协议的 Listener 的 `<Listener Name>` 可以重复，`listener.tcp.external` 与 `listener.ssl.external` 是两个不同的 Listener。

由于默认配置的存在，我们能够非常快速地展示如何添加新的 Listener，以 TCP Listener 为例，我们只需要在 `emqx.conf` 中添加以下一条配置即可：

```bash
listener.tcp.example = 12345
```

当然这种情况我们更建议您复制一份默认 Listener 的配置进行修改。

### Zone

一个 Zone 定义了一组配置项 (比如最大连接数等)，Listener 可以通过配置项 `listener.<Protocol>.<Listener Name>.zone` 指定使用某个 Zone，以使用该 Zone 下的所有配置。多个 Listener 可以共享同一个 Zone。Zone 的命名规则为 `zone.<Zone Name>.xxx`，`Zone Name` 可以随意命名，但同样建议是全小写的英文单词，`xxx` 是具体的配置项，你可以在 [配置项](../configuration/configuration.md) 中查看 Zone 支持的所有配置项。

此时，我们的每个配置项都存在三个可用值，分别是全局的值，Zone 里设置的值以及默认值，它们的优先级顺序为：Zone > Global > Default。

## 配置更新

配置项会在 EMQ X Broker 与扩展插件被启动时读取并载入，EMQ X Broker 目前尚不支持运行时更新配置，但由于扩展插件支持动态加载和卸载，因此可以在修改插件配置后重新加载插件来应用最新的配置项。