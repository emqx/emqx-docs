# 将 MQTT 数据写入到 HStreamDB

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

[HStreamDB](https://hstream.io/) 是一个开源的流数据平台，使您能够在一个统一的平台中高效地摄取、存储、处理和分发所有实时消息、事件和其他数据流。通过 EMQX 与 HStreamDB 的集成，您可以将 MQTT 消息和客户端事件保存到 HStreamDB 中，实现大规模物联网数据的采集、传输与存储，并使用标准 SQL 和物化视图对数据流进行实时处理以及监测和分析。

本页详细介绍了 EMQX 与 HStreamDB 的数据集成并提供了实用的规则和 Sink 创建指导。

{% emqxee %}

::: tip

仅 EMQX 5.2.0 及以上版本支持 HStreamDB Sink 功能。

:::

{% emqxee %}

## 工作原理

HStreamDB 数据集成是 EMQX 的即开即用功能，结合了 EMQX 的设备连接和消息传输能力以及 HStreamDB 强大的数据存储和处理能力。内置的[规则引擎](./rules.md)组件简化了两个平台之间的数据流和处理过程。

下图展示了 EMQX 和 HStreamDB 之间的数据集成的典型架构：

![EMQX-HStreamDB 集成](./assets/emqx-integration-hstreamdb.png)

EMQX 通过规则引擎和配置的 Sink 将 MQTT 数据转发到 Apache HStreamDB，整个过程如下：

1. **消息发布和接收**：物联网设备通过 MQTT 协议建立成功连接，随后发布遥测和状态数据到特定主题。当 EMQX 接收到这些消息时，它将在其规则引擎中启动匹配过程。
2. **规则引擎处理消息**：使用内置的规则引擎，可以根据主题匹配处理来自特定来源的 MQTT 消息。规则引擎匹配相应规则并处理消息，例如数据格式转换、过滤特定信息或用上下文信息丰富消息。
3. **数据流入 HStreamDB**：规则触发将消息转发到 HStreamDB 的动作，可以轻松配置数据到 HStreamDB 流名称、分区键和记录，便于后续的数据处理和分析。

在 MQTT 消息数据写入 HStreamDB 后，您可以进行灵活的应用程序开发，例如：

- 在接收到特定 MQTT 消息时，可以使用 HStreamDB 的规则引擎组件触发相应的动作或事件，实现跨系统和应用的事件驱动功能。
- 在 HStreamDB 中实时分析 MQTT 数据流，检测异常或特定事件模式，并根据这些条件触发警报通知或执行相应动作。
- 将多个 MQTT 主题的数据集中到一个统一的数据流中，并利用 HStreamDB 的计算能力进行实时聚合、计算和分析，以获得更全面的数据洞察。

## 特性与优势

与 HStreamDB 的数据集成为您的业务带来以下特性和优势：

- **可靠的物联网数据消息传递**：EMQX 能够可靠地批量发送 MQTT 消息到 HStreamDB，使物联网设备与 HStreamDB 和应用系统集成。
- **MQTT 消息转换**：使用规则引擎，EMQX 可以过滤和转换 MQTT 消息。在发送到 HStreamDB 之前，消息可以经过数据提取、过滤、丰富和转换。
- **大规模数据流存储**：HStreamDB 支持在专门设计的分布式、容错的日志存储集群中可靠地存储数百万数据流，并在需要的时候重放或推送实时数据流的更新到应用中。能够与 EMQX 消息模型完美结合，实现大规模物联网数据采集传输与存储。
- **集群和可扩展性**：EMQX 和 HStreamDB 采用云原生架构构建，支持集群在线伸缩、动态扩缩容，随着业务增长灵活地水平扩展以满足不断扩大的需求。
- **灵活的处理能力**：在 HStreamDB 可以使用熟悉的 SQL 来过滤、转换、聚合以及连接多个数据流，也支持使用标准 SQL 和物化视图进行数据流实时处理以及监测和分析，获取实时数据洞察。
- **高吞吐量场景中的处理能力**：HStreamDB Sink 支持同步和异步写入模式，允许根据不同场景在延迟和吞吐量之间灵活平衡。

## 准备工作

本节介绍了在 EMQX 中创建 HStreamDB 数据集成之前需要做的准备工作，包括如何设置 HStreamDB 服务器并创建 Stream。

以下小节描述如何使用 Docker 镜像在 Linux/MacOS 安装启动 HStreamDB，因此请确保 Docker 已安装并尽可能使用 Docker Compose v2。关于其他 HStreamDB 的安装方式及 HStreamDB Platform，请参阅[使用 Docker-Compose 快速开始](https://docs.hstream.io/zh/start/quickstart-with-docker.html)以及[开始使用 HStream Platform](https://docs.hstream.io/zh/start/try-out-hstream-platform.html)。

本教程假设您在本地机器上同时运行 EMQX 和容器内的 HStreamDB。 如果您有远程运行的 EMQX 和 HStreamDB，请相应地调整设置。

### 前置准备

- 了解[规则](./rules.md)。
- 了解[数据集成](./data-bridges.md)。

### 启动 HStreamDB 服务并创建 Stream

::::: tabs

:::: tab 启动 HStreamDB TCP 服务并创建 Stream

本节介绍了如何在本地的 Docker 环境中启动一个单节点的 HStreamDB TCP 服务并创建 Stream。

::: tip 注意

HStreamDB 资源已连接状态下，在 HStreamDB 中对 Stream 进行操作，例如删除并重新创建 Stream 后，需要重新连接 HStreamDB，即重启 HStreamDB 资源。

:::

1. 将以下 yaml 文件保存至 `docker-compose-tcp.yaml`。

   ::: details `docker-compose-tcp.yaml`

   ```yaml
   version: "3.9"

   services:
     hserver:
       image: hstreamdb/hstream:v0.17.0
       container_name: quickstart-tcp-hserver
       depends_on:
         - zookeeper
         - hstore
       ports:
         - "127.0.0.1:6570:6570"
       expose:
         - 6570
       networks:
         - quickstart-tcp
       volumes:
         - /var/run/docker.sock:/var/run/docker.sock
         - /tmp:/tmp
         - data_store:/data/store
       command:
         - bash
         - "-c"
         - |
           set -e
           /usr/local/script/wait-for-storage.sh hstore 6440 zookeeper 2181 600 \
           /usr/local/bin/hstream-server \
           --bind-address 0.0.0.0 --port 6570 \
           --internal-port 6571 \
           --server-id 100 \
           --seed-nodes "$$(hostname -I | awk '{print $$1}'):6571" \
           --advertised-address $$(hostname -I | awk '{print $$1}') \
           --metastore-uri zk://zookeeper:2181 \
           --store-config /data/store/logdevice.conf \
           --store-admin-host hstore --store-admin-port 6440 \
           --store-log-level warning \
           --io-tasks-path /tmp/io/tasks \
           --io-tasks-network quickstart-tcp

     hstore:
       image: hstreamdb/hstream:v0.17.0
       container_name: quickstart-tcp-hstore
       networks:
         - quickstart-tcp
       volumes:
         - data_store:/data/store
       command:
         - bash
         - "-c"
         - |
           set -ex
           # N.B. "enable-dscp-reflection=false" is required for linux kernel which
           # doesn't support dscp reflection, e.g. centos7.
           /usr/local/bin/ld-dev-cluster --root /data/store \
           --use-tcp --tcp-host $$(hostname -I | awk '{print $$1}') \
           --user-admin-port 6440 \
           --param enable-dscp-reflection=false \
           --no-interactive

     zookeeper:
       image: zookeeper:3.8.1
       container_name: quickstart-tcp-zk
       expose:
         - 2181
       networks:
         - quickstart-tcp
       volumes:
         - data_zk_data:/data
         - data_zk_datalog:/datalog

   networks:
     quickstart-tcp:
       name: quickstart-tcp

   volumes:
     data_store:
       name: quickstart_tcp_data_store
     data_zk_data:
       name: quickstart_tcp_data_zk_data
     data_zk_datalog:
       name: quickstart_tcp_data_zk_datalog
   ```

   :::

2. 执行以下 shell 命令以启动 HStreamDB TCP 服务。

   ```bash
   docker compose -f docker-compose-tcp.yaml up --build
   ```

3. 进入 HStreamDB 容器并创建名为 `mqtt_connect` 和 `mqtt_message` 的两个 Stream。

   ::: tip

   您也可以使用 HStreamDB 交互式 SQL CLI 来创建 Stream。使用 `hstream --help` 命令获取更多有关 `hstream` 命令的其他用法。

   :::

   ```bash
   $ docker container exec -it quickstart-tcp-hserver bash
   # 创建 Stream `mqtt_connect`
   root@9c7ce2f51860:/# hstream stream create mqtt_connect
   +--------------+---------+----------------+-------------+
   | Stream Name  | Replica | Retention Time | Shard Count |
   +--------------+---------+----------------+-------------+
   | mqtt_connect | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   # 创建 Stream `mqtt_message`
   root@9c7ce2f51860:/# hstream stream create mqtt_message
   +--------------+---------+----------------+-------------+
   | Stream Name  | Replica | Retention Time | Shard Count |
   +--------------+---------+----------------+-------------+
   | mqtt_message | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   # 列出所有 Stream
   root@9c7ce2f51860:/# hstream stream list
   +--------------+---------+----------------+-------------+
   | Stream Name  | Replica | Retention Time | Shard Count |
   +--------------+---------+----------------+-------------+
   | mqtt_message | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   | mqtt_connect | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   ```

::::
:::: tab 启动 HStreamDB TLS 服务并创建 Stream

本节介绍了如何在本地的 Docker 环境中启动一个双节点的 HStreamDB TLS 服务并创建 Stream。

::: tip 关于 Docker 网络环境与证书文件

- 此 docker compose 文件使用了 `172.100.0.0/24` 网段作为 docker network bridge，如有其他网络配置需求，请自行更改 Docker Compose 文件。
- 请注意不要为容器设置默认的 `http_proxy`, `https_proxy`, `all_proxy` 等环境变量，目前版本中这些环境变量会影响 HStream 各个容器间的通讯。参考 [_Docker Network Proxy_](https://docs.docker.com/network/proxy/)。
- 根证书及自签名证书使用了 [_smallstep/step-ca_](https://hub.docker.com/r/smallstep/step-ca) 容器进行自动化生成，并配置了 `172.100.0.10` 及 `172.100.0.11` 两个主题备用名称。
- 如有其他证书需求，请自行挂载证书文件至 HStreamDB 容器或参考 [_Configuring step-ca_](https://smallstep.com/docs/step-ca/configuration/index.html)。
  - step-ca 默认配置下生成的证书仅有一天有效期，若要更改证书有效期配置，请删除 `ca` 目录下的证书，并根据 [_step-ca-configuration-options_](https://smallstep.com/docs/step-ca/configuration/#configuration-options) 更改证书有效期。

:::

1. 新建目录 tls-deploy/ca 作为证书存储目录。

    ```bash
    mkdir tls-deploy/ca
    ```

2. 将以下 yaml 文件保存至 `tls-deploy/docker-compose-tls.yaml`。

   ::: details `docker-compose-tls.yaml`

   ```yaml
   version: "3.9"

   services:
     step-ca:
       image: smallstep/step-ca:0.23.0
       container_name: quickstart-tls-step-ca
       networks:
         - quickstart-tls
       volumes:
         - ${step_ca}:/home/step
       environment:
         - DOCKER_STEPCA_INIT_NAME=HStream
         - DOCKER_STEPCA_INIT_DNS_NAMES=step-ca

     generate-hstream-cert:
       image: smallstep/step-ca:0.23.0
       container_name: quickstart-tls-generate-hstream-cert
       depends_on:
         step-ca:
           condition: service_healthy
       networks:
         - quickstart-tls
       volumes:
         - ${step_ca}:/home/step
       command:
         - bash
         - "-c"
         - |
           sleep 1
           if [ -f hstream.crt ]; then exit 0; fi
           step ca certificate "hstream" hstream.crt hstream.key \
           --provisioner-password-file secrets/password --ca-url https://step-ca:9000 \
           --root certs/root_ca.crt \
           --san localhost \
           --san 127.0.0.1 \
           --san 172.100.0.10 \
           --san 172.100.0.11 \
           --san quickstart-tls-hserver-0 \
           --san quickstart-tls-hserver-1

     hserver0:
       image: hstreamdb/hstream:v0.17.0
       container_name: quickstart-tls-hserver-0
       depends_on:
         - generate-hstream-cert
         - zookeeper
         - hstore
       ports:
         - "127.0.0.1:6570:6570"
       networks:
         quickstart-tls:
           ipv4_address: 172.100.0.10
       volumes:
         - /var/run/docker.sock:/var/run/docker.sock
         - /tmp:/tmp
         - data_store:/data/store
         - ${step_ca}:/data/server
       command:
         - bash
         - "-c"
         - |
           set -e
           /usr/local/script/wait-for-storage.sh hstore 6440 zookeeper 2181 600; \
           timeout=60; \
           until ( \
              [ -f /data/server/hstream.crt ] && [ -f /data/server/hstream.key ] \
           ) >/dev/null 2>&1; do
               >&2 echo 'Waiting for tls files ...'
               sleep 1
               timeout=$$((timeout - 1))
               [ $$timeout -le 0 ] && echo 'Timeout!' && exit 1;
           done; \
           /usr/local/bin/hstream-server \
           --bind-address 0.0.0.0 --port 26570 \
           --internal-port 6571 \
           --server-id 100 \
           --seed-nodes "hserver0:6571,hserver1:6573" \
           --advertised-address $$(hostname -I | awk '{print $$1}') \
           --metastore-uri zk://zookeeper:2181 \
           --store-config /data/store/logdevice.conf \
           --store-admin-host hstore --store-admin-port 6440 \
           --io-tasks-path /tmp/io/tasks \
           --io-tasks-network quickstart-tls \
           --tls-cert-path /data/server/hstream.crt \
           --tls-key-path /data/server/hstream.key \
           --advertised-listeners l1:hstream://172.100.0.10:6570 \
           --listeners-security-protocol-map l1:tls

           # NOTE:
           # advertised-listeners ip addr should same as container addr for tls listener

     hserver1:
       image: hstreamdb/hstream:v0.17.0
       container_name: quickstart-tls-hserver-1
       depends_on:
         - zookeeper
         - hstore
       ports:
         - "127.0.0.1:6572:6572"
       expose:
         - 6572
         - 26572
       networks:
         quickstart-tls:
           ipv4_address: 172.100.0.11
       volumes:
         - /var/run/docker.sock:/var/run/docker.sock
         - /tmp:/tmp
         - data_store:/data/store
         - ${step_ca}:/data/server
       command:
         - bash
         - "-c"
         - |
           set -e
           /usr/local/script/wait-for-storage.sh hstore 6440 zookeeper 2181 600; \
           timeout=60; \
           until ( \
              [ -f /data/server/hstream.crt ] && [ -f /data/server/hstream.key ] \
           ) >/dev/null 2>&1; do
               >&2 echo 'Waiting for tls files ...'
               sleep 1
               timeout=$$((timeout - 1))
               [ $$timeout -le 0 ] && echo 'Timeout!' && exit 1;
           done; \
           /usr/local/bin/hstream-server \
           --bind-address 0.0.0.0 --port 26572 \
           --internal-port 6573 \
           --server-id 101 \
           --seed-nodes "hserver0:6571,hserver1:6573" \
           --advertised-address $$(hostname -I | awk '{print $$1}') \
           --metastore-uri zk://zookeeper:2181 \
           --store-config /data/store/logdevice.conf \
           --store-admin-host hstore --store-admin-port 6440 \
           --io-tasks-path /tmp/io/tasks \
           --io-tasks-network quickstart-tls \
           --tls-cert-path /data/server/hstream.crt \
           --tls-key-path /data/server/hstream.key \
           --advertised-listeners l1:hstream://172.100.0.11:6572 \
           --listeners-security-protocol-map l1:tls

           # NOTE:
           # advertised-listeners ip addr should same as container addr for tls listener

     hserver-init:
       image: hstreamdb/hstream:v0.17.0
       container_name: quickstart-tls-hserver-init
       depends_on:
         - hserver0
         - hserver1
       networks:
         - quickstart-tls
       command:
         - bash
         - "-c"
         - |
           timeout=60
           until ( \
               /usr/local/bin/hadmin server --host 172.100.0.10 --port 26570 status && \
               /usr/local/bin/hadmin server --host 172.100.0.11 --port 26572 status \
           ) >/dev/null 2>&1; do
               >&2 echo 'Waiting for servers ...'
               sleep 1
               timeout=$$((timeout - 1))
               [ $$timeout -le 0 ] && echo 'Timeout!' && exit 1;
           done; \
           /usr/local/bin/hadmin server --host hserver0 --port 26570 init

     hstore:
       image: hstreamdb/hstream:v0.17.0
       container_name: quickstart-tls-hstore
       networks:
         - quickstart-tls
       volumes:
         - data_store:/data/store
       command:
         - bash
         - "-c"
         - |
           set -ex
           /usr/local/bin/ld-dev-cluster --root /data/store \
           --use-tcp --tcp-host $$(hostname -I | awk '{print $$1}') \
           --user-admin-port 6440 \
           --no-interactive

     zookeeper:
       image: zookeeper:3.8.1
       container_name: quickstart-tls-zk
       expose:
         - 2181
       networks:
         - quickstart-tls
       volumes:
         - data_zk_data:/data
         - data_zk_datalog:/datalog

   networks:
     quickstart-tls:
       ipam:
         driver: default
         config:
           - subnet: "172.100.0.0/24"
       name: quickstart-tls

   volumes:
     data_store:
       name: quickstart_tls_data_store
     data_zk_data:
       name: quickstart_tls_data_zk_data
     data_zk_datalog:
       name: quickstart_tls_data_zk_datalog
   ```

   :::

   至此目录结构应为：
   ```bash
   $ tree tls-deploy
   tls-deploy
   ├── ca
   └── docker-compose-tls.yaml

   2 directories, 1 file
   ```

3. 进入 `tls-deploy` 目录执行以下 shell 命令以启动 HStreamDB TLS 服务。

   ```bash
   env step_ca=$PWD/ca docker compose -f docker-compose-tls.yaml up --build
   ```

4. 进入 HStreamDB 容器并创建名为 `mqtt_connect` 和 `mqtt_message` 的两个 Stream。

   ::: tip TLS 连接命令行选项

   类似于 HStreamDB TCP 服务，此处仅需为命令行增加 `--tls-ca [CA_PATH]` 选项。
   需要注意的是，如需在节点 `quickstart-tls-hserver-1` 中执行命令，需要额外指定选项 `--port 6572` 以保证与 docker-compose 文件中指定的端口一致。

   :::

   ```bash
   $ docker container exec -it quickstart-tls-hserver-0 bash
   # Create Stream `mqtt_connect`
   root@75c9351cbb38:/# hstream --tls-ca /data/server/certs/root_ca.crt stream create mqtt_connect
   +--------------+---------+----------------+-------------+
   | Stream Name  | Replica | Retention Time | Shard Count |
   +--------------+---------+----------------+-------------+
   | mqtt_connect | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   # Create Stream `mqtt_message`
   root@75c9351cbb38:/# hstream --tls-ca /data/server/certs/root_ca.crt stream create mqtt_message
   +--------------+---------+----------------+-------------+
   | Stream Name  | Replica | Retention Time | Shard Count |
   +--------------+---------+----------------+-------------+
   | mqtt_message | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   # List all Streams
   root@75c9351cbb38:/# hstream --tls-ca /data/server/certs/root_ca.crt stream list
   +--------------+---------+----------------+-------------+
   | Stream Name  | Replica | Retention Time | Shard Count |
   +--------------+---------+----------------+-------------+
   | mqtt_message | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   | mqtt_connect | 1       | 604800 seconds | 1           |
   +--------------+---------+----------------+-------------+
   ```

::::

:::::

## 创建连接器

在创建 RocketMQ Sink 之前，您需要创建一个 HStreamDB 连接器，以便 EMQX 与 HStreamDB 服务建立连接。以下示例假定您在本地机器上同时运行 EMQX 和 HStreamDB。如果您在远程运行 HStreamDB 和 EMQX，请相应地调整设置。

1. 转到 Dashboard **集成** -> **连接器** 页面。点击页面右上角的**创建**。
2. 在连接器类型中选择 **HStreamDB**，点击**下一步**。
3. 在 **配置** 步骤，配置以下信息（带星号字段为必填字段。）：

   - **连接器名称**：应为大写和小写字母及数字的组合，例如：`my_hstreamdb`。
   - **服务器地址**： `hstream://127.0.0.1:6570`，或使用实际的 HStreamDB 地址和端口。
     - schema 支持 `http`、`https`、`hstream`、`hstreams`。
     - 对与 TLS 连接，scheme 需要使用 `hstreams` 或 `https`，如 `hstreams://127.0.0.1:6570`。
   - **HStreamDB 流名称**： 需要写入的 Stream 名，如  `mqtt_message` （用于消息存储）或 `mqtt_connect` （用于事件记录）。
   - **HStreamDB 分区键**：指定用于确定数据将存储在 HStreamDB 的哪个分区或节点内的分区键。例如，您可以输入 `${topic}` 以确保相同主题的消息被有序写入 HStreamDB。如果未指定，将使用默认键，数据将被映射到某个默认的分片。
   - **HStreamDB gRPC 超时**：指定当发出 gRPC 请求到 HStreamDB 服务器时，系统将等待响应的最长时间。默认值是 `30` 秒。
   - **启用 TLS**： 启用 TLS 连接时，关闭**验证服务器证书**。
     - `tls-deploy/ca` 目录下生成的证书及私钥文件： `ca/certs/root_ca.crt`，`ca/hstream.crt`，`ca/hstream.key` 分别填入 `CA Cert`，`TLS Cert`，`TLS Key`。
4. 高级配置（可选）：详细请参考 [Sink 的特性](./data-bridges.md#sink-的特性)。
5. 点击**创建**按钮完成连接器创建。
6. 在弹出的**创建成功**对话框中您可以点击**创建规则**，继续创建规则以指定需要写入 HStreamDB 的数据和需要记录的客户端事件。您也可以按照[创建消息存储 Sink 规则](#创建消息存储-sink-规则)和[创建事件记录 Sink 规则](#创建事件记录-sink-规则)章节的步骤来创建规则。

## 创建消息存储 Sink 规则

本节演示了如何在 Dashboard 中创建一条规则，用于处理来自源 MQTT 主题 `t/#` 的消息，并通过配置的 Sink 将处理后的数据写入到 HStream 的 Stream `mqtt_message`。

1. 转到 Dashboard **集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 输入规则 ID `my_rule`。在 **SQL 编辑器**中输入规则以实现对指定主题消息的转发。例如将 `t/#` 主题的 MQTT 消息存储至 HStreamDB，输入以下 SQL 语句：
   注意：如果您希望制定自己的 SQL 语句，需要确保规则选出的字段（SELECT 部分）包含之后配置的 HStream Record 模板中用到的所有变量。

   ```sql
    SELECT
      *
    FROM
      "t/#"
   ```

   ::: tip

   如果您初次使用 SQL，可以点击 **SQL 示例** 和**启用调试**来学习和测试规则 SQL 的结果。

   :::

4. 点击右侧的**添加动作**按钮，为规则在被触发的情况下指定一个动作。在**动作类型**下拉框中选择 `HStreamDB`，保持**动作**下拉框为默认的`创建动作`选项，您也可以选择一个之前已经创建好的 HStreamDB Sink。此处我们创建一个全新的 Sink 并添加到规则中。

5. 输入 Sink 名称，名称应为大/小写字母和数字的组合。

6. 从**连接器**下拉框中选择刚刚创建的 `my_hstreamdb`。您也可以通过点击下拉框旁边的按钮创建一个新的连接器。有关配置参数，请参见[创建连接器](#创建连接器)。

7. 配置 **HStream Record 模板**以实现对指定主题消息的转发。使用如下 HRecord 模板完成数据插入：

   ```json
   {"id": ${id}, "topic": "${topic}", "qos": ${qos}, "payload": "${payload}"}
   ```

8. 高级配置（可选），根据情况配置同步/异步模式，队列等参数，详细请参考 [Sink 的特性](./data-bridges.md#sink-的特性)。

9. 点击**添加**按钮完成 Sink 创建，新建的 Sink 将被添加到**动作输出**列表中。

10. 回到创建规则页面，对配置的信息进行确认，点击**创建**。一条规则应该出现在规则列表中。

现在您已成功创建了通过 HStreamDB Sink 将数据转发到 HStreamDB 的规则，同时在**规则**页面的**动作(Sink)** 标签页看到新建的 HStreamDB Sink。

您还可以点击 **集成** -> **Flow 设计器**可以查看拓扑，通过拓扑可以直观的看到，主题 `t/#` 下的消息在经过规则 `my_rule` 解析后被发送到 HStreamDB 中。

## 创建上下线记录 Sink 规则

本节展示如何创建用于记录客户端上/下线状态的规则，并通过配置的 Sink 将记录写入到 HStreamDB 的 Stream `mqtt_connect` 中。

注意：除规则 SQL 和 Sink 的 Stream Record 模板设置不同外，其他操作步骤与[创建消息存储 Sink 规则](#创建消息存储-sink-规则)章节完全相同。

规则 SQL 模版设置如下：

```sql
SELECT
  *
FROM
  "$events/client_connected", "$events/client_disconnected"
```

Sink 的 Stream Record 模板设置如下:

```sql
{"clientid": "${clientid}", "event_type": "${event}", "event_time": ${timestamp}}
```

## 测试规则

使用 MQTTX 向 `t/1` 主题发布消息。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello HStreamDB" }'
```

查看 HStreamDB Sink 运行统计。

- 用于消息存储的 Sink ，命中、发送成功次数均 +1。查看数据是否已经写入流 `mqtt_message` 中：

```bash
# 读取 Stream `mqtt_message` 之后按 `Control-C` 停止
root@9c7ce2f51860:/# hstream stream read-stream mqtt_message
timestamp: "1693903488278", id: 1947758763121538-8589934593-0, key: "", record: {"id": 00060498A3B3C4F8F4400100127E0002, "topic": "t/1", "qos": 0, "payload": { "msg": "Hello HStreamDB" }}
^CRead Done.
```

- 用于存储上下线事件的 HStreamDB Sink ，命中、发送次数均 +2，即一次上线和一次下线。查看设备状态是否已经写入流 `mqtt_connect` 中：

```bash
# 读取 Stream `mqtt_connect` 之后按 `Control-C` 停止
root@9c7ce2f51860:/# hstream stream read-stream mqtt_connect
timestamp: "1693903488274", id: 1947758827604597-8589934593-0, key: "", record: {"clientid": "emqx_c", "event_type": "client.connected", "event_time": 1693903488266}
timestamp: "1693903488294", id: 1947758827604597-8589934594-0, key: "", record: {"clientid": "emqx_c", "event_type": "client.disconnected", "event_time": 1693903488271}
^CRead Done.
```

