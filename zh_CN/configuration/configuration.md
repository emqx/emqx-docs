# 配置文件简介

EMQX 支持通过修改配置文件或使用环境变量来设置 EMQX，本章节将介绍 EMQX 配置文件基本信息，配置项以及详细的介绍请参考 [配置手册](./configuration-manual.md)

## 配置文件介绍

EMQX 主配置文件为 `emqx.conf`，根据安装方式其所在位置有所不同：

| 安装方式     | 配置文件所在位置          |
| ------------ | ------------------------- |
| 二进制包安装 | `/etc/emqx/etc/emqx.conf` |
| 解压缩包安装 | `./etc/emqx.conf`         |
| Docker 容器  | `/opt/emqx/etc/emqx.conf` |

主配置文件包含了大部分常用的配置项，如果您没有在配置文件中明确指定某个配置项，EMQX 将使用默认配置，以简化配置文件的编写和维护。所有可用的配置项及其说明可参考主配置同路径下的 `emqx-example.conf` 文件。

主配置文件隐式地嵌套了 2 个重写配置文件：

**`cluster-override.conf`**

包含集群共有的配置项，通过 REST API、CLI 与 Dashboard 提交的配置将写入其中，并覆盖 `emqx.conf` 中的同名配置项。

当 EMQX 运行在集群中时，重启或后启动的节点会从集群中其他节点复制该文件内容到本地，因此通常不建议手动管理。

**`local-override.conf`**

节点特有的配置，会覆盖 `cluster-override.conf` 中的同名配置项。

该配置文件不会在集群间同步，可以手动管理。

重写配置位于 `$data/configs/` 目录中，根据安装方式 `data` 目录默认所在位置有所不同：

| 安装方式     | data 目录所在位置    |
| ------------ | -------------------- |
| 二进制包安装 | `/var/lib/emqx/data` |
| 解压缩包安装 | `./data`             |
| Docker 容器  | `/opt/emqx/data`     |

您也可以通过 `node.data_dir` 配置或 `EMQX_NODE__DATA_DIR` 环境变量指定 `data` 目录位置。

通常情况下大多数配置项都在主配置文件中定义，需要通过 REST API、CLI 与 Dashboard 配置的内容（热配置）将写入到 `cluster-override.conf` 中，一经配置将覆盖主配置文件的内容；如果需要通过修改配置文件的方式覆盖热配置的内容，可以将其写入 `local-override.conf` 中。覆盖规则参考 [配置覆盖规则](#配置覆盖规则)。

:::tip
有些配置项是不能被覆盖的（例如 `node.name`）。
<!-- TODO 确认规则 配置项如果有 `mapping: path.to.boot.config.key` 这个属性，
则不能被添加到重载文件 `*-override.conf` 中。 -->
:::

## HOCON 配置格式

从 5.0 版本开始，EMQX 采用 [HOCON](https://github.com/emqx/hocon) 作为配置文件格式。

HOCON（Human-Optimized Config Object Notation）是一种可扩展的配置语言，它支持类似 JSON 的语法，易于阅读和编写。同时 HOCON 具有继承、合并、引用等功能，使得配置文件更加灵活可控。

**基本语法：**

HOCON 值可以被记为类似 JSON 的对象，例如：

```hocon
node {
  name = "emqx@127.0.0.1"
  cookie = "mysecret"
  cluster_call {
    retry_interval  =  1m
  }
}
```

也可以使用扁平化的方式：

```bash
node.name = "127.0.0.1"
node.cookie = "mysecret"
node.cluster_call.retry_interval = "1m"
```

这种类似 cuttlefish 的扁平格式一定程度向后兼容了之前版本的 EMQX，但使用时又有所不同：

HOCON 建议字符串两端加上引号，没有特殊字符的字符串也可以不加引号如 `foo`，`foo_bar`，而 cuttlefish 把 `=` 右边的所有字符都视为值。

更多有关 HOCON 的语法请参考 [HOCON 文档](https://github.com/lightbend/config/blob/main/HOCON.md)。

## 环境变量

除了配置文件外，EMQX 还可以通过环境变量设置配置。

比如 `EMQX_NODE__NAME=emqx2@127.0.0.1` 环境变量将覆盖以下配置：

```bash
# emqx.conf

node {
  name = "emqx@127.0.0.1"
}
```

配置项与环境变量之前可以通过以下规则转换：

1. 由于配置文件中的 `.` 分隔符不能使用于环境变量，因此 EMQX 选用双下划线 `__` 作为配置分割；
2. 为了与其他的环境变量有所区分，EMQX 还增加了一个前缀 `EMQX_` 来用作环境变量命名空间;
3. 环境变量的值是按 HOCON 值解析的，这也使得环境变量可以用来传递复杂数据类型的值，但要注意特殊字符如`:` 和 `=` 需要用双引号 `"` 包裹。

转换示例：

```bash
# 环境变量

## localhost:1883 会被解析成一个结构体 `{"localhost": 1883}`，因此需要使用双引号包裹
export EMQX_LISTENERS__SSL__DEFAULT__BIND='"127.0.0.1:8883"'

## 通过字符直接传递 HOCON 数组
export EMQX_LISTENERS__SSL__DEFAULT__AUTHENTICATION__SSL__CIPHERS='["TLS_AES_256_GCM_SHA384"]'


# 配置文件
listeners.ssl.default {
  ...
  authentication {
    bind = "127.0.0.1:8883"
    ssl {
      ciphers = ["TLS_AES_256_GCM_SHA384"]
    }
  }
}
```

::: tip
未定义的根路径会被 EMQX 忽略，例如 `EMQX_UNKNOWN_ROOT__FOOBAR` 这个环境变量会被 EMQX 忽略，因为 `UNKNOWN_ROOT` 不是预先定义好的根路径。

已知的根路径设置了未知的字段名时，将在启动时输出 `warning` 日志，例如将 `enable` 错误的配置为 `enabled` 时将输出：

```bash
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

:::

## 配置覆盖规则

HOCON 的值是分层覆盖的，最简单的规则如下：

- 在同一个文件中，后（在文件底部）定义的值，覆盖前（在文件顶部）到值。
- 当按层级覆盖时，高层级的值覆盖低层级的值。

### 合并覆盖

在如下配置中，最后一行的 `debug` 值会覆盖原先 `level` 字段的 `error` 值，但是 `enable` 字段保持不变：

```bash
log {
  console_handler{
    enable = true
    level = error
  }
}

## 将 console 日志打印级别设置为 debug，其他配置保持不变
log.console_handler.level = debug
```

报文大小限制最先被设置成 1MB，后被覆写为 10MB：

```bash
zone {
  zone1 {
    mqtt.max_packet_size = 1M
  }
}
zone.zone1.mqtt.max_packet_size = 10M
```

### 列表元素覆盖

EMQX 配置中的数组有两种表达方式：

- 列表格式，例如： `[1, 2, 3]`。
- 带下标的 Map 格式，例如： `{"1"=1, "2"=2, "3"=3}`。

以下 3 种格式是等价的：

```bash
authentication.1 = {...}
authentication = {"1": {...}}
authentication = [{...}]
```

基于这个特性，我们就可以轻松覆写数组某个元素的值，例如：

```bash
authentication  = [
  {
    enable = true,
    backend = "built_in_database",
    mechanism = "password_based"
  }
]

# 可以用下面的方式将第一个元素的 `enable` 字段覆写
authentication.1.enable = false
```

::: tip
列表格式是的数组将全量覆写而不是合并覆盖原有值，例如：

```bash
authentication = [
  {
    enable = true
    backend = "built_in_database"
    mechanism="password_based"
  }
]

## 下面这种方式会导致数组第一个元素的除了 `enable` 以外的其他字段全部丢失。
authentication = [{ enable = true }]
```

:::

## Schema 手册

<!-- 这里英文部分原样照搬 -->

为了确保配置正确，EMQX 引入了 schema。schema 定义了数据类型，以及数据字段的名称和元数据，用于配置值的类型检查等。

### 基本数据类型

配置支持的基本数据类型如下：

- 原子：`atom()`。
- 布尔：`boolean()`。
- 字符串：`string()`。
- 整形：`integer()`。
- 浮点数：`float()`。
- 数值：`number()`。
- 二进制编码的字符串 `binary()` 是 `string()` 的另一种格式。
- 时间间隔 `emqx_schema:duration()` 是 `integer()` 的另一种格式。
- ...

### 复杂数据类型

EMQX 配置文件有 4 种复杂数据结构类型：

1. Struct：结构体都是有类型名称的，结构体中可以有任意多个字段。
   结构体和字段的名称由不带特殊字符的全小些字母组成，名称中可以带数字，但不得以数字开头，多个单词可用下划线分隔。
2. Map: Map 与 Struct（结构体）类似，但是内部的字段不是预先定义好的。
3. Union: 联合 `MemberType1 | MemberType2 | ...`，可以理解为：“不是这个，就是那个”
4. Array: 数组 `[ElementType]`

::: tip
如果 Map 的字段名称是纯数字，它会被解析成一个数组。

例如：

```bash
myarray.1 = 74
myarray.2 = 75
```

会被解析成 `myarray = [74, 75]`，这个用法在重载数组元素的值时候非常有用。
:::

### 配置路径

如果我们把 EMQX 的配置值理解成一个类似目录树的结构，如果文件系统中使用斜杠或反斜杠进行层级分割，则 EMQX 的层级分割符是 `.`：

下面有几个例子：

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

### TLS ciphers

从 v5.0.6 开始 EMQX 不在配置文件中详细列出所有默认的密码套件名称。而是在配置文件中使用一个空列表，然后在运行时替换成默认的密码套件。

下面这些密码套件是 EMQX 默认支持的：

tlsv1.3:

```bash
ciphers =
  [ "TLS_AES_256_GCM_SHA384", "TLS_AES_128_GCM_SHA256",
    "TLS_CHACHA20_POLY1305_SHA256", "TLS_AES_128_CCM_SHA256",
    "TLS_AES_128_CCM_8_SHA256"
  ]
```

tlsv1.2 或更早:

```bash
ciphers =
  [ "ECDHE-ECDSA-AES256-GCM-SHA384",
    "ECDHE-RSA-AES256-GCM-SHA384",
    "ECDHE-ECDSA-AES256-SHA384",
    "ECDHE-RSA-AES256-SHA384",
    "ECDH-ECDSA-AES256-GCM-SHA384",
    "ECDH-RSA-AES256-GCM-SHA384",
    "ECDH-ECDSA-AES256-SHA384",
    "ECDH-RSA-AES256-SHA384",
    "DHE-DSS-AES256-GCM-SHA384",
    "DHE-DSS-AES256-SHA256",
    "AES256-GCM-SHA384",
    "AES256-SHA256",
    "ECDHE-ECDSA-AES128-GCM-SHA256",
    "ECDHE-RSA-AES128-GCM-SHA256",
    "ECDHE-ECDSA-AES128-SHA256",
    "ECDHE-RSA-AES128-SHA256",
    "ECDH-ECDSA-AES128-GCM-SHA256",
    "ECDH-RSA-AES128-GCM-SHA256",
    "ECDH-ECDSA-AES128-SHA256",
    "ECDH-RSA-AES128-SHA256",
    "DHE-DSS-AES128-GCM-SHA256",
    "DHE-DSS-AES128-SHA256",
    "AES128-GCM-SHA256",
    "AES128-SHA256",
    "ECDHE-ECDSA-AES256-SHA",
    "ECDHE-RSA-AES256-SHA",
    "DHE-DSS-AES256-SHA",
    "ECDH-ECDSA-AES256-SHA",
    "ECDH-RSA-AES256-SHA",
    "ECDHE-ECDSA-AES128-SHA",
    "ECDHE-RSA-AES128-SHA",
    "DHE-DSS-AES128-SHA",
    "ECDH-ECDSA-AES128-SHA",
    "ECDH-RSA-AES128-SHA"
  ]
```

配置 PSK 认证的监听器

```bash
ciphers = [
  [ "RSA-PSK-AES256-GCM-SHA384",
    "RSA-PSK-AES256-CBC-SHA384",
    "RSA-PSK-AES128-GCM-SHA256",
    "RSA-PSK-AES128-CBC-SHA256",
    "RSA-PSK-AES256-CBC-SHA",
    "RSA-PSK-AES128-CBC-SHA"
  ]
```
