# 从 HiveMQ 迁移到 EMQX

本指南介绍如何将现有的 HiveMQ 部署迁移到 EMQX。本文重点介绍一种常见的企业部署模式：设备通过 TLS（端口 8883）连接，并使用 HiveMQ Enterprise Security Extension（ESE）管理的 X.509 客户端证书或用户名/密码凭据。本文的目标是在 EMQX 中通过 HOCON 配置和规则引擎，实现与 HiveMQ 在连接性、身份验证和数据集成方面等效的行为。

## 迁移概览

整个迁移过程可分为三个阶段：

1. **盘点 HiveMQ 资产**：收集定义监听器、认证、集群和数据管道的 TLS 密钥库、`config.xml`、ESE 文件及扩展属性。
2. **配置 EMQX**：将 HiveMQ 设置转换为 EMQX HOCON 配置，将密钥库转换为 PEM 格式，重建监听器和集群设置，并配置认证链与规则引擎。
3. **更新设备与集成**：将设备连接指向 EMQX 端点，部署 EMQX 服务器 CA 证书，验证客户端身份，并迁移下游集成（如 Kafka 或 Prometheus）。

下表总结了 HiveMQ 与 EMQX 之间关键配置项和工件的映射关系：

| **参数 / 工件** | **HiveMQ（示例）**                                           | **EMQX（示例）**                                             | **说明**                                                 |
| --------------- | ------------------------------------------------------------ | ------------------------------------------------------------ | -------------------------------------------------------- |
| 端点主机名      | `mqtt.internal.example.com`（在负载均衡器 / 控制中心中配置） | `mqtt.example.com`（EMQX 负载均衡 / VIP）                    | 更新设备固件或部署清单。                                 |
| TLS 资产        | `conf/hivemq.jks`                                            | `/etc/emqx/certs/server-cert.pem`, `/etc/emqx/certs/server-key.pem` | 使用 `keytool` + `openssl` 将 JKS/PKCS12 转换为 PEM。    |
| 客户端认证      | ESE 文件领域（`credentials.xml`）                            | `authentication = [{mechanism = password_based, backend = built_in_database}]` | 通过 REST API 或 Dashboard 导入用户列表。                |
| 客户端证书      | 以 PEM 存储在设备端，当启用 mTLS 时由 HiveMQ 验证            | 使用相同设备证书，EMQX 监听器配置 `ssl_options.cacertfile = "device-ca.pem"` | 若使用相同 CA，无需重新签发。                            |
| 集群发现        | DNS 或 `extensions/*-discovery*/*.properties`                | `cluster.discovery_strategy = dns`（或 `static`、`etcd`、`k8s`） | 使用 EMQX 原生发现策略替代扩展。                         |
| Kafka 集成      | `extensions/hivemq-kafka-extension/kafka-configuration.xml`  | EMQX 连接器 + 规则 + 动作（`SELECT ... FROM "device/+/data"`） | 使用 EMQX 数据集成功能替代基于 Java 的扩展进行消息转换。 |
| 限流 / 约束     | `<restrictions>` 块 + 过载保护                               | `listeners.*.max_connections`, `messages_rate`, `bytes_rate`, `limiter.*` | 配置监听器级与全局配额限制。                             |

## 阶段一：盘点 HiveMQ 配置工件

### 收集并转换 TLS 密钥库

1. 找到 `<tls-tcp-listener>` 中引用的密钥库（例如 `/opt/hivemq/conf/hivemq.jks`）。
2. 导出服务器证书与私钥：

```
keytool -importkeystore \
  -srckeystore /opt/hivemq/conf/hivemq.jks \
  -destkeystore /tmp/hivemq.p12 \
  -deststoretype PKCS12

openssl pkcs12 -in /tmp/hivemq.p12 -nodes -nokeys -out /tmp/server-cert.pem
openssl pkcs12 -in /tmp/hivemq.p12 -nodes -nocerts -out /tmp/server-key.pem
```

1. 将生成的 PEM 文件复制到 `/etc/emqx/certs/`（或容器挂载的密钥路径）。保留 HiveMQ 信任的设备 CA（`device-ca.pem`），EMQX 将复用该证书进行 mTLS 验证。

### 导出 HiveMQ 配置文件

将以下文件保存到版本控制系统以便追踪，并标记环境变量占位符（例如 `${ENV:HIVEMQ_PORT}`），便于映射到 EMQX 的双下划线环境变量语法（例如 `EMQX_LISTENERS__TCP__DEFAULT__BIND=0.0.0.0:1883`）。

- `conf/config.xml`：监听器、约束、集群、持久化、控制中心用户
- `conf/logback.xml`：日志目标（对应 EMQX 的 `log` 配置）
- `extensions/<name>/conf/*.xml` 或 `.properties`：发现、Kafka、Prometheus、自定义认证
- `extensions/hivemq-enterprise-security-extension/enterprise-security-extension.xml`：认证领域与管道
- 任意由 ESE 引用的 `credentials.xml` 或自定义用户存储

### 分类认证模式

确定当前使用的认证方式：

- **用户名/密码**（文件领域或 SQL 领域）
- **X.509 客户端证书**（mTLS，使用 CN 作为客户端 ID）
- **混合模式**（例如 TLS + SASL 插件）

每种模式都对应 EMQX 中特定的认证链配置。

## 阶段二：配置 EMQX 以镜像 HiveMQ 基线

### 重建 MQTT 监听器

将 HiveMQ 的 `<tcp-listener>`、`<tls-tcp-listener>`、`<websocket-listener>` 和 `<tls-websocket-listener>` 元素转换为 HOCON 格式。

**HiveMQ 配置示例：**

```xml
<hivemq>
    <listeners>
        <tcp-listener>
            <port>1883</port>
            <bind-address>0.0.0.0</bind-address>
        </tcp-listener>
        <tls-tcp-listener>
            <port>8883</port>
            <bind-address>0.0.0.0</bind-address>
            <tls>
                <keystore>
                    <path>/opt/hivemq/conf/keystore.jks</path>
                    <password>password</password>
                    <private-key-password>pkpassword</private-key-password>
                </keystore>
                <truststore>
                    <path>/opt/hivemq/conf/truststore.jks</path>
                    <password>password</password>
                </truststore>
                <client-authentication-mode>NONE</client-authentication-mode>
            </tls>
        </tls-tcp-listener>
        <tls-websocket-listener>
            <port>8084</port>
            <bind-address>0.0.0.0</bind-address>
            <path>/mqtt</path>
            <subprotocols>
                <subprotocol>mqttv3.1</subprotocol>
                <subprotocol>mqtt</subprotocol>
            </subprotocols>
            <tls>
                <keystore>
                    <path>/opt/hivemq/conf/keystore.jks</path>
                    <password>hivemq</password>
                </keystore>
                <truststore>
                    <path>/opt/hivemq/conf/truststore.jks</path>
                    <password>hivemq</password>
                </truststore>
            </tls>
        </tls-websocket-listener>
    </listeners>
</hivemq>
```

**等效的 EMQX 配置片段：**

```hocon
listeners.tcp.default {
  bind = "0.0.0.0:1883"
}

listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    certfile = "/etc/certs/server-cert.pem"
    keyfile  = "/etc/certs/server-key.pem"
  }
}

listeners.wss.default {
  bind = "0.0.0.0:8083"
  mqtt_path = "/mqtt"
  ssl_options {
    certfile = "/etc/certs/server-cert.pem"
    keyfile  = "/etc/certs/server-key.pem"
  }
}
```

要将 `truststore.jks` 和 `keystore.jks` 转换为 PEM 格式，请参照 收集并转换 TLS 密钥库 一节中的步骤。

### 映射 MQTT 配置选项

HiveMQ 中的设置（如消息队列大小、QoS 和保留消息行为）可直接映射到 EMQX 的 `mqtt` 配置段。

**HiveMQ 配置示例：**

```xml
<queued-messages>
    <max-queue-size>1000</max-queue-size>
    <strategy>discard</strategy>
</queued-messages>

<topic-alias>
    <enabled>true</enabled>
    <max-per-client>5</max-per-client>
</topic-alias>

<message-expiry>
    <max-interval>4294967296</max-interval>
</message-expiry>

<session-expiry>
    <max-interval>4294967295</max-interval>
</session-expiry>

<packets>
    <max-packet-size>268435460</max-packet-size>
</packets>

<receive-maximum>
    <server-receive-maximum>10</server-receive-maximum>
</receive-maximum>

<quality-of-service>
    <max-qos>2</max-qos>
</quality-of-service>

<wildcard-subscriptions>
    <enabled>true</enabled>
</wildcard-subscriptions>

<shared-subscriptions>
    <enabled>true</enabled>
</shared-subscriptions>

<subscription-identifier>
    <enabled>true</enabled>
</subscription-identifier>

<retained-messages>
    <enabled>true</enabled>
</retained-messages>
```

**等效的 EMQX 配置：**

```hocon
mqtt {
  max_mqueue_len          = 1000
  mqueue_priorities       = disabled
  max_topic_alias         = 5
  message_expiry_interval = infinity   # HiveMQ 中为 4294967296
  session_expiry_interval = infinity
  max_packet_size         = "256MB"
  max_inflight            = 10
  max_qos_allowed         = 2
  wildcard_subscription   = true
  shared_subscription     = true
  retain_available        = true
  # subscription_identifier 默认启用
}
```

### 映射 `<restrictions>` 块

HiveMQ 在 `<restrictions>` 中集中定义全局限制。在 EMQX 中，这些限制被拆分为全局 `mqtt` 配置和各监听器配置项。

**HiveMQ 配置示例：**

```xml
<restrictions>
    <max-client-id-length>65535</max-client-id-length>
    <max-connections>-1</max-connections>
    <incoming-bandwidth-throttling>0</incoming-bandwidth-throttling>
    <no-connect-idle-timeout>10000</no-connect-idle-timeout>
</restrictions>
```

**等效的 EMQX 配置片段：**

```hocon
listeners.ssl.default {
  bind             = "0.0.0.0:8883"
  max_connections  = infinity
  bytes_rate       = "0"        # 对应 'incoming-bandwidth-throttling'
  bytes_burst      = "0"
}

mqtt {
  max_clientid_len = 65535
  idle_timeout     = "10s"      # 对应 no-connect-idle-timeout
}
```

### 配置集群

将 HiveMQ 的发现扩展及其他发现方式替换为 EMQX 原生集群发现机制。

**HiveMQ 集群配置示例：**

```xml
<cluster>
    <enabled>true</enabled>
    <transport>
        <tcp>
            <bind-address>127.0.0.1</bind-address>
            <bind-port>7800</bind-port>
        </tcp>
    </transport>
    <discovery>
        <static>
            <node>
                <host>127.0.0.1</host>
                <port>7800</port>
            </node>
            <node>
                <host>127.0.0.1</host>
                <port>7801</port>
            </node>
        </static>
    </discovery>
</cluster>
```

**等效的 EMQX 配置：**

```hocon
cluster {
  discovery_strategy = static
  static {
    seeds = [
      "emqx1@127.0.0.1",
      "emqx2@127.0.0.1"
    ]
  }
}
```

EMQX 在同一主机上运行多个节点时，会自动分配 Erlang 分布端口，无需手动设置 `bind-port`。

若需使用其他发现方式（如 etcd、Kubernetes 或静态文件），请参见[创建与管理集群](../deploy/cluster/create-cluster.md)。

### 翻译认证与授权配置

HiveMQ 通过 **Enterprise Security Extension (ESE)** 管理安全机制，该扩展定义了 **Realms（领域）**（数据源）和 **Pipelines（逻辑流程）**，也可能通过旧版插件进行身份验证。

EMQX 则使用**认证链**（有序的后端模块）和**授权源**（ACL 访问控制列表）来实现类似功能。

| HiveMQ ESE 组件                                | 对应的 EMQX 组件                                             | 迁移策略                                                     |
| ---------------------------------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| **文件领域（File Realm）** (`credentials.xml`) | [**内置数据库（Built-in Database）**](../access-control/authn/mnesia.md) | 从 HiveMQ 导出用户数据，并通过 EMQX REST API 导入。          |
| **SQL 领域（JDBC）**                           | [**MySQL**](../access-control/authn/mysql.md) / [**PostgreSQL**](../access-control/authn/postgresql.md) | 配置基于密码的认证机制，后端为 `mysql` 或 `postgresql`。可复用现有用户表结构。 |
| **LDAP 领域 / Active Directory**               | [**LDAP**](../access-control/authn/ldap.md)                  | 配置基于密码的认证机制，后端为 LDAP。将 HiveMQ 的 DN 模式映射为 EMQX 的筛选模板。 |
| **OAuth / JWT**                                | [**JWT**](../access-control/authn/jwt.md)                    | 配置 JWT 认证机制，指定公钥或 JWKS 端点。                    |
| **HTTP / Webhooks**                            | [**HTTP Server**](../access-control/authn/http.md)           | 配置基于密码的 HTTP 后端认证，将凭证验证委托给外部认证服务。 |
| **X.509 证书**                                 | [**X.509**](../access-control/authn/x509.md) / [**mTLS**](../network/emqx-mqtt-tls.md#enable-ssl-tls-with-two-way-authentication) | 使用 TLS 监听器和双向认证（mTLS），重用现有 CA 与客户端证书。 |

#### 迁移文件领域用户（File Realm Users）

**来源：** HiveMQ 的 `conf/credentials.xml` 文件（通常包含加密或哈希密码）。
**目标：** EMQX 内置数据库（Built-in Database）。

1. **导出用户数据：**
    从 HiveMQ 的文件领域 (`credentials.xml`) 中提取用户信息。该文件通常包含哈希密码与盐值。需要编写脚本解析 XML，生成 EMQX 可导入的 JSON 或 CSV 文件。
2. **导入到 EMQX：**
    使用 EMQX 的 REST API 创建用户。EMQX 支持导入包含哈希密码（如 bcrypt、pbkdf2 等）的用户数据。详情参见[导入认证数据](../access-control/authn/user_management.md#导入认证数据)。

```bash
# 示例：导入一个带明文密码的用户
curl -u admin:public -X POST \
  http://emqx-node:18083/api/v5/authentication/password_based:built_in_database/users \
  -d '{"user_id":"device-001","password":"StrongPass!"}'
```

#### 迁移外部集成认证（SQL、LDAP、HTTP）

将 `enterprise-security-extension.xml` 中定义的认证管道（pipelines）转换为 EMQX 的 HOCON `authentication` 配置块。

**示例：将 SQL 领域迁移至 EMQX MySQL 认证**

HiveMQ 的 SQL 领域使用一个 [固定数据库模式](https://docs.hivemq.com/hivemq-enterprise-security-extension/latest/ese.html#table_users)。而 EMQX 允许您 [自定义数据库模式和查询语句](../access-control/authn/mysql.md)，**因此无需修改现有的 MySQL 或 PostgreSQL 数据库结构。**

下方的 EMQX 配置示例使用查询语句 `SELECT password_hash, salt ...` 以匹配 HiveMQ 默认的 `users` 表结构。

```hocon
authentication = [
  {
    mechanism = "password_based"
    backend = "mysql"
    server = "127.0.0.1:3306"
    database = "mqtt"
    username = "root"
    password = ""
    query = "SELECT password_hash, salt FROM users WHERE username = ${username}"
    password_hash_algorithm {
        name = "sha256"
        salt_position = "suffix"
    }
  }
]
```

**示例：LDAP 领域迁移**

```hocon
authentication = [
  {
    mechanism = "password_based"
    backend = "ldap"
    server = "ldap.example.com:636"
    ssl {
      enable = true
    }
    method {
      type = bind
      bind_password = "${password}"
    }
    username = "root"
    password = "root password"
    base_dn = "uid=${username},ou=testdevice,dc=emqx,dc=io"
    filter = "(objectClass=mqttUser)"
  }
]
```

#### 迁移授权配置（ACLs）

HiveMQ 的访问控制策略（ACL）通常定义在 `enterprise-security-extension.xml`（文件领域）或外部数据库中。EMQX 提供了更灵活的**授权链**，可同时使用多个后端（File、Redis、MySQL、PostgreSQL、MongoDB、HTTP 等）。

**HiveMQ XML 授权策略示例：**

```xml
<permission>
    <topic>device/${clientid}/#</topic>
    <activity>ALL</activity>
</permission>
```

**EMQX 等效配置示例：**

- [**文件 ACL (`acl.conf`)**](../access-control/authz/file.md)：
   `{allow, all, subscribe, ["device/${clientid}/#"]}.`
- [**内置数据库（Mnesia）**](../access-control/authz/mnesia.md)：
   可通过 Dashboard 或 API 基于客户端 ID、用户名或主题配置访问规则。
- [**MySQL**](../access-control/authz/mysql.md)：
   `SELECT action, permission, topic, ipaddress, qos, retain FROM mqtt_acl WHERE clientid = ${clientid} AND ipaddress = ${peerhost};`
- [**PostgreSQL**](../access-control/authz/postgresql.md)：
   `SELECT action, permission, topic, ipaddress, qos, retain FROM mqtt_acl WHERE clientid = ${clientid} AND ipaddress = ${peerhost};`

更多详情请参见[**授权**](../access-control/authz/authz.md)文档。

### 配置数据集成

HiveMQ 依赖独立扩展（例如 Kafka 扩展）实现数据集成功能；而在 EMQX 中，所有数据集成功能都是内置的，并且开箱即用。

在配置具体的数据集成之前，请先熟悉以下核心概念：

- [**数据集成概述**](../data-integration/data-bridges.md)
- [**规则引擎**](../data-integration/rules.md)
- [**可视化 Flow 设计器**](../flow-designer/introduction.md)

#### 示例：迁移 Kafka 扩展

**HiveMQ Kafka 扩展配置示例：**

```xml
<kafka-configuration xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:noNamespaceSchemaLocation="config.xsd">
    <kafka-clusters>
        <kafka-cluster>
            <id>cluster01</id>
            <bootstrap-servers>127.0.0.1:9092</bootstrap-servers>
        </kafka-cluster>
    </kafka-clusters>

    <mqtt-to-kafka-mappings>
        <mqtt-to-kafka-mapping>
            <id>mapping01</id>
            <cluster-id>cluster01</cluster-id>
            <mqtt-topic-filters>
                <mqtt-topic-filter>#</mqtt-topic-filter>
            </mqtt-topic-filters>
            <kafka-topic>emqx</kafka-topic>
        </mqtt-to-kafka-mapping>
    </mqtt-to-kafka-mappings>

    <kafka-to-mqtt-mappings>
        <kafka-to-mqtt-mapping>
            <id>mapping02</id>
            <cluster-id>cluster01</cluster-id>
            <kafka-topics>
                <kafka-topic>topic1</kafka-topic>
                <kafka-topic>topic2</kafka-topic>
            </kafka-topics>
        </kafka-to-mqtt-mapping>
    </kafka-to-mqtt-mappings>
</kafka-configuration>
```

**等效的 EMQX 配置：**

```hocon
connectors {
  kafka_producer {
    cluster01 {
      bootstrap_hosts = "127.0.0.1:9092"
      enable = true
    }
  }
  kafka_consumer {
    cluster01 {
      bootstrap_hosts = "127.0.0.1:9092"
      enable = true
    }
  }
}
actions {
  kafka_producer {
    mapping01 {
      connector = "cluster01"
      enable = true
      parameters {
        message {
          value = "${.}"
        }
        topic = "emqx"
      }
    }
  }
}
rule_engine {
  rules {
    mqtt-to-kafka-mapping-mapping01 {
      sql = "SELECT * FROM '#'"
      actions = [
        "kafka_producer:mapping01"
      ]
      enable = true
    }
    kafka-to-mqtt-mapping-mapping02 {
      actions = [
        {
          args {
            topic = "kafka"
          }
          function = "republish"
        }
      ]
      enable = true
      sql = "SELECT * FROM '$bridges/kafka_consumer:cluster01-topic1','$bridges/kafka_consumer:cluster01-topic2'"
    }
  }
}
sources {
  kafka_consumer {
    cluster01-topic1 {
      connector = "cluster01"
      parameters {
        topic = "topic1"
      }
      enable = true
    }
    cluster01-topic2 {
      connector = "cluster01"
      parameters {
        topic = "topic2"
      }
      enable = true
    }
  }
}
```

### 配置可观测性

#### Prometheus 监控

HiveMQ 通过 “Prometheus Monitoring HiveMQ Extension” 提供监控功能，而 EMQX 则原生支持 Prometheus，无需额外插件。

Prometheus 用于采集指标（即 *scrape*）的端点默认启用：`http://emqx-node:18083/api/v5/prometheus/stats`。

如果您希望使用 **Pushgateway** 模式，可以按如下方式配置：

```hocon
prometheus {
  push_gateway {
    enable = true
    url = "http://127.0.0.1:9091"
  }
}
```

更多配置细节请参考[集成 Prometheus](../observability/prometheus.md)。

#### 日志

HiveMQ 使用 `logback.xml`（Java 标准日志系统），EMQX 则使用基于 HOCON 的内置日志系统。

**HiveMQ (`logback.xml`) 配置示例：**

```xml
<appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
    <encoder>
        <pattern>%-30(%d %level)- %msg%n%ex</pattern>
    </encoder>
</appender>

<appender name="FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
    <file>${hivemq.log.folder}/hivemq.log</file>
    <append>true</append>

    <rollingPolicy class="ch.qos.logback.core.rolling.TimeBasedRollingPolicy">
        <!-- 每日轮转 -->
        <fileNamePattern>${hivemq.log.folder}/hivemq.%d{yyyy-MM-dd}.log</fileNamePattern>

        <!-- 保留 30 天历史日志 -->
        <maxHistory>30</maxHistory>
    </rollingPolicy>
    <encoder>
        <pattern>%-30(%d %level)- %msg%n%ex</pattern>
    </encoder>
</appender>
```

**等效的 EMQX 配置：**

```hocon
log {
  file {
    default {
      enable = true
      level = warning
      path = "/var/log/emqx/emqx.log"
      rotation_count = 30
      rotation_size = "50MB"
    }
  }
  console {
    enable = true
    level = warning
  }
}
```

请参考[日志](../observability/log.md)文档，了解日志级别、轮转策略和格式（text/JSON）的配置方法。

#### 日志追踪

HiveMQ 的 “Trace Recordings” 功能常用于调试特定客户端会话。EMQX 提供了内置的**日志追踪**功能，可通过 Dashboard 或 CLI 实时过滤特定客户端 ID、主题或 IP 的日志。

**示例：启动针对指定客户端的追踪**

```bash
emqx ctl trace start client device-001 trace.log
```

详细功能请参考[日志追踪](../observability/tracer.md)。

## 阶段三：更新设备与集成

### 向设备部署 EMQX 服务器 CA（Deploy EMQX Server CA to Devices）

- 如果 EMQX 使用内部 CA（自签名证书），则需要将 `device-ca.pem` 安装到每个设备的系统信任存储或应用程序包中。
- 如果 EMQX 使用公共 CA（例如 Let’s Encrypt），则无需在设备上执行任何操作。

### 更新设备连接参数

**示例（使用 mqtt-cli 工具）**

```bash
# 迁移前（HiveMQ）
mqtt pub -h mqtt.internal.example.com -p 8883 \
  -u device-001 -pw StrongPass! \
  --cafile device-ca.pem --topic device/001/data --message test

# 迁移后（EMQX）
mqtt pub -h mqtt.example.com -p 8883 \
  -u device-001 -pw StrongPass! \
  --cafile device-ca.pem --topic device/001/data --message test
```

**示例（使用 Python paho-mqtt 客户端并启用 mTLS）**

```hocon
client.tls_set(
    ca_certs="certs/device-ca.pem",
    certfile="certs/device-001.cert.pem",
    keyfile="certs/device-001.key.pem",
    tls_version=ssl.PROTOCOL_TLS_CLIENT
)
client.connect("mqtt.example.com", 8883)
```

迁移后，仅需更改的内容是**端点主机名**和**服务器 CA 文件**。如果设备证书和私钥仍由相同的 CA 签发（即 EMQX 配置中的 `ssl_options.cacertfile` 所引用的 CA），则设备证书和私钥可以继续正常使用，无需重新签发。

### 验证集成配置

- 检查 Kafka 主题是否能接收到消息，可通过查看 EMQX 规则引擎指标实现：

  ```bash
  emqx ctl rule show
  ```

- 更新监控仪表盘，以便 Prometheus 能够采集EMQX 指标。

- 重新配置告警系统（例如 Splunk、ELK），以便正确解析 EMQX 的日志格式。

## 进阶迁移场景

### 保留消息与会话

HiveMQ 的持久化文件无法直接导入至 EMQX。建议通过以下脚本化迁移方式进行处理：

1. 暂时保持 HiveMQ 运行。
2. 运行一个桥接客户端，订阅 HiveMQ 上的 `#` 主题，并将保留消息重新发布到 EMQX。
3. 对于 QoS 1/2 的消息，确保所有在途事务完成后，再切换 DNS 指向。

### 共享订阅

HiveMQ 的 `$share/group/topic` 语法在 EMQX 中完全受支持。如果之前使用 `$queue/topic`，可将其映射为 `$share/queue/topic`。

此外，可以通过调整 `broker.shared_subscription_strategy`（例如 `round_robin` 或 `hash_clientid`）来模拟原有消费者期望的负载均衡行为。

### 基于 HTTP/API 的动态配置

HiveMQ 依赖静态 XML 文件和各扩展模块自身的重新加载机制。而 EMQX 提供了动态配置 API，可直接通过 HTTP 实时修改配置。

**示例：使用 REST API 修改监听器配置**

```bash
curl -s -H "Authorization: Bearer $TOKEN" \
  -H "Content-type: application/json" \
  -X PUT "http://emqx-node:18083/api/v5/listeners/ssl:default" \
  -d '{"type": "ssl", "bind": "0.0.0.0:8883", "id": "ssl:default", "max_connections": 200000}'
```

该命令会将配置写入 `data/configs/cluster.hocon` 文件。在迁移策略上，您可以选择：

- 继续保持配置不可变（仅通过 `emqx.conf` 管理），
- 或采用 EMQX 的“双层配置模型”（即允许环境特定的动态配置覆盖）。

## 验证清单

在将生产流量切换至 EMQX 之前，请确认以下检查项全部通过：

1. 所有 EMQX 监听器均处于 `running` 状态（`emqx ctl listeners list`）。
2. 对于启用 mTLS 的设备，TLS 握手验证应在提供或未提供客户端证书的情况下分别成功与失败。
3. EMQX 会话中的设备 ID 与原 HiveMQ 客户端 ID 一致。
4. ACL 规则与 HiveMQ 中的主题访问策略保持一致。
5. 集群节点在模拟网络分区后能够自动恢复。
6. Kafka 集成功能正常，消息传输无数据转换问题。
7. Prometheus 能正常显示 EMQX 指标数据。

## 总结

从 HiveMQ 迁移至 EMQX 本质上是一个**配置转换过程**：将基于 Java 的配置工件（如 XML 文件、JKS 密钥库、扩展模块）转换为 EMQX 的 HOCON 配置格式、灵活的认证链以及内置的数据集成框架。

通过遵循三个阶段：**盘点**、**配置** 与 **更新**，您可以在保留原有设备凭证、主题结构与集成逻辑的前提下，充分利用 EMQX 的高并发 Erlang 运行时和动态配置能力。

请务必在执行迁移前仔细规划、验证每个监听器与集成配置，并在切换时保持信心与可回退策略。
