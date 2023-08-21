# HStreamDB

通过 HStreamDB 数据桥接可以将 MQTT 消息和客户端事件存储到 HStreamDB 中，也可以通过事件触发对 HStreamDB 中数据的更新或删除操作，从而实现对诸如设备在线状态、上下线历史等的记录。

{% emqxee %}

::: tip

仅 EMQX 5.2.0 及以上版本支持 HStreamDB 数据桥接功能。

:::

{% emqxee %}

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

:::tip 前置准备

- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

:::

## 功能清单

- [连接池](./data-bridges.md#连接池)
- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)

<!-- TODO 配置参数 需要补充链接到配置手册对应配置章节。 -->

## 快速开始教程

本节介绍如何配置 HStreamDB 数据桥接，包括如何设置 HStreamDB 服务器并创建 Stream 、创建数据桥接和转发数据到 HStreamDB 的规则以及测试数据桥接和规则等主题。

本教程假设您在本地机器上同时运行 EMQX 和容器内的 HStreamDB。 如果您有远程运行的 EMQX 和 HStreamDB，请相应地调整设置。

### 安装并连接到 HStreamDB

本节描述如何使用 Docker 镜像在 Linux/MacOS 安装启动 HStreamDB 以及如何使用 `hstream` 命令行程序连接到 HStreamDB。关于其他 HStreamDB 的安装方式及 HStreamDB Platform，请参阅 [使用 Docker-Compose 快速开始](https://docs.hstream.io/zh/start/quickstart-with-docker.html) 及 [开始使用 HStream Platform](https://docs.hstream.io/zh/start/try-out-hstream-platform.html)。

:::tip Docker Compose 版本

请尽可能使用 docker compose v2

:::

---

**启动 HStreamDB TCP 服务并创建 Stream**

按照以下步骤在本地的 Docker 环境中启动一个单节点的 HStreamDB TCP 服务。

- 将以下 yaml 文件保存至 `docker-compose-tcp.yaml`

  <details>
  <summary><code>docker-compose-tcp.yaml</code></summary>

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
        - "127.0.0.1:6670:6570"
      expose:
        - 6670
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

  </details>

- 执行以下 shell 命令以启动 HStreamDB TCP 服务

  ```bash
  docker compose -f docker-compose-tcp.yaml up --build
  ```

- 进入 HStreamDB 容器并创建名为 `mqtt_connect` 和 `mqtt_message` 的两个 Stream
  :::tip `hstream` 命令与交互式 sql
  有关 hstream 命令的其它用法，请参考 help 信息：`hstream --help`
  :::

  <details>
  <summary><b>创建 Stream 命令</b></summary>

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

  </details>

---

**启动 HStreamDB TLS 服务并创建 Stream**

:::tip 关于 Docker 网络环境与证书文件

- 此 docker compose 文件使用了 `172.100.0.0/24` 网段作为 docker network bridge，如有其他网络配置需求，请自行更改 Docker Compose 文件。
- 请注意不要为容器设置默认的 `http_proxy`, `https_proxy`, `all_proxy` 等环境变量，目前版本中这些环境变量会影响 HStream 各个容器间的通讯。参考 [_Docker Network Proxy_](https://docs.docker.com/network/proxy/)
- 根证书及自签名证书使用了 [_smallstep/step-ca_](https://hub.docker.com/r/smallstep/step-ca) 容器进行自动化生成，并配置了 `172.100.0.10` 及 `172.100.0.11` 两个主题备用名。
- 如有其他证书需求，请自行挂载证书文件至 HStreamDB 容器或参考 [_Configuring step-ca_](https://smallstep.com/docs/step-ca/configuration/index.html)。
  - step-ca 默认配置下生成的证书仅有一天有效期，若要更改证书有效期配置，请删除 `ca` 目录下的证书，并根据 [_step-ca-configuration-options_](https://smallstep.com/docs/step-ca/configuration/#configuration-options) 更改证书有效期

:::

按照以下步骤在本地的 Docker 环境中启动一个双节点的 HStreamDB TLS 服务。

- 新建目录 tls-deploy/ca 作为证书存储目录

    ```bash
    mkdir tls-deploy/ca
    ```

- 将以下 yaml 文件保存至 `tls-deploy/docker-compose-tls.yaml`

  <details>
  <summary><code>docker-compose-tls.yaml</code></summary>

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

  </details>

  至此目录结构应为：
  ```bash
  $ tree tls-deploy
  tls-deploy
  ├── ca
  └── docker-compose-tls.yaml

  2 directories, 1 file
  ```

- 进入 `tls-deploy` 目录执行以下 shell 命令以启动 HStreamDB TLS 服务

  ```bash
  env step_ca=$PWD/ca docker compose -f docker-compose-tls.yaml up --build
  ```

- 进入 HStreamDB 容器并创建名为 `mqtt_connect` 和 `mqtt_message` 的两个 Stream
  :::tip TLS 连接命令行选项
  类似与 HStreamDB TCP 服务，此处仅需为命令行增加 `--tls-ca [CA_PATH]` 选项
  需要注意的是，如需在节点 `quickstart-tls-hserver-1` 中执行命令，需要额外指定选项 `--port 6572` 以保证与 docker-compose 文件中指定的端口一致。
  :::

  <details>
  <summary><b>创建 Stream 命令</b></summary>

  ```bash
  $ docker container exec -it quickstart-tls-hserver-0 bash
  # 创建 Stream `mqtt_connect`
  root@75c9351cbb38:/# hstream --tls-ca /data/server/certs/root_ca.crt stream create mqtt_connect
  +--------------+---------+----------------+-------------+
  | Stream Name  | Replica | Retention Time | Shard Count |
  +--------------+---------+----------------+-------------+
  | mqtt_connect | 1       | 604800 seconds | 1           |
  +--------------+---------+----------------+-------------+
  # 创建 Stream `mqtt_message`
  root@75c9351cbb38:/# hstream --tls-ca /data/server/certs/root_ca.crt stream create mqtt_message
  +--------------+---------+----------------+-------------+
  | Stream Name  | Replica | Retention Time | Shard Count |
  +--------------+---------+----------------+-------------+
  | mqtt_message | 1       | 604800 seconds | 1           |
  +--------------+---------+----------------+-------------+
  # 列出所有 Stream
  root@75c9351cbb38:/# hstream --tls-ca /data/server/certs/root_ca.crt stream list
  +--------------+---------+----------------+-------------+
  | Stream Name  | Replica | Retention Time | Shard Count |
  +--------------+---------+----------------+-------------+
  | mqtt_message | 1       | 604800 seconds | 1           |
  +--------------+---------+----------------+-------------+
  | mqtt_connect | 1       | 604800 seconds | 1           |
  +--------------+---------+----------------+-------------+
  ```

  </details>

### 创建 HStreamDB 数据桥接

本节介绍了如何在 EMQX Dashboard 上创建 HStreamDB 数据桥接以实现对客户端发布消息的存储或设备状态的记录。

1. 在 Dashboard 点击 **数据集成** -> **数据桥接**。

2. 点击页面右上角的**创建**。

3. 在数据桥接类型中选择 HStreamDB，点击**下一步**。

4. 输入数据桥接名称，要求是大小写英文字母和数字的组合。

5. 输入 HStreamDB 连接信息。
   - **服务器地址**： `hstream://127.0.0.1:6570`，或使用实际的 HStreamDB 地址和端口
     - 对与 TLS 连接，scheme 需要使用 `hstreams`，如 `hstreams://127.0.0.1:6570`。
   - **HStreamDB 流名称**： 需要写入的 Stream 名，如 `mqtt_connect` 或 `mqtt_message`。
   - **启用 TLS**： 启用 TLS 连接时，关闭 `验证服务器证书`
     - `tls-deploy/ca` 目录下生成的证书及私钥文件： `ca/certs/root_ca.crt`，`ca/hstream.crt`，`ca/hstream.key` 分别填入 `CA Cert`，`TLS Cert`，`TLS Key`。

6. 根据业务实现需要配置 HRecord 模板：

   - 如需实现对指定主题消息的转发，使用如下 HRecord 模板完成数据插入。

     ```json
     {"id": ${id}, "topic": "${topic}", "qos": ${qos}, "payload": "${payload}"}
     ```

   - 如需实现实现设备上下线状态记录，可使用如下 SQL 语句完成数据插入：

     ```json
     {"clientid": "${clientid}", "event_type": "${event}", "event_time": ${timestamp}}
     ```

7. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考[配置参数](./data-bridges.md)。

8. 在点击 **创建** 按钮完成数据桥接创建之前，您可以使用 **测试连接** 来测试当前 EMQX 到 HStreamDB 的连接是否成功。

9. 点击**创建**按钮完成数据桥接创建。

   在弹出的**创建成功**对话框中您可以点击**创建规则**，继续创建规则以指定需要写入 HStreamDB 的数据。您也可以按照[创建 HStreamDB 数据桥接规则](#创建-microsoft-sql-server-数据桥接规则)章节的步骤来创建规则。

至此您已经完成数据桥接创建，HStreamDB 数据桥接应该出现在数据桥接列表（**数据集成** -> **数据桥接**）中，**资源状态**为**已连接**。

### 创建数据转发规则

本节介绍了如何为 HStreamDB 数据桥接创建规则。您需要为实现对客户端发布消息的存储或实现设备上下线状态的记录创建不同的规则。

1. 转到 Dashboard **数据集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 输入规则 ID `my_rule`，在 **SQL 编辑器**中根据业务实现需要输入规则：

   - 如需实现对指定主题消息的转发，例如将 `t/#` 主题的 MQTT 消息存储至 HStreamDB，输入以下 SQL 语句：
     注意：如果您希望制定自己的 SQL 语句，需要确保规则选出的字段（SELECT 部分）包含所有 SQL 模板中用到的变量。

     ```sql
      SELECT
        *
      FROM
        "t/#"
     ```

   - 如需实现设备上下线状态记录，输入以下 SQL 语句：

     ```sql
     SELECT
       *
     FROM
       "$events/client_connected", "$events/client_disconnected"
     ```

4. 点击**添加动作**，在动作下拉框中选择**使用数据桥接转发**选项，选择先前创建好的 HStreamDB 数据桥接。

5. 点击最下方**创建**按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 HStreamDB 存储。

### 测试数据桥接和规则

使用 MQTTX 向 `t/1` 主题发布消息。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello HStreamDB" }'
```

查看 HStreamDB 数据桥接运行统计。

- 用于消息存储的数据桥接，命中、发送成功次数均 +1。查看数据是否已经写入流 `mqtt_message` 中：

```bash
# 读取 Stream `mqtt_message` 之后按 `Control-C` 停止
root@9c7ce2f51860:/# hstream stream read-stream mqtt_message
timestamp: "1693903488278", id: 1947758763121538-8589934593-0, key: "", record: {"id": 00060498A3B3C4F8F4400100127E0002, "topic": "t/1", "qos": 0, "payload": { "msg": "Hello HStreamDB" }}
^CRead Done.
```

- 用于存储上下线事件的 HStreamDB 数据桥接，命中、发送次数均 +2，即一次上线和一次下线。查看设备状态是否已经写入流 `mqtt_connect` 中：

```bash
# 读取 Stream `mqtt_connect` 之后按 `Control-C` 停止
root@9c7ce2f51860:/# hstream stream read-stream mqtt_connect
timestamp: "1693903488274", id: 1947758827604597-8589934593-0, key: "", record: {"clientid": "emqx_c", "event_type": "client.connected", "event_time": 1693903488266}
timestamp: "1693903488294", id: 1947758827604597-8589934594-0, key: "", record: {"clientid": "emqx_c", "event_type": "client.disconnected", "event_time": 1693903488271}
^CRead Done.
```

### 已知问题

HStreamDB 资源已连接状态下，在 HStreamDB 中对 Stream 进行操作，例如删除并重新创建 Stream 后，需要重新连接 HStreamDB，即重启 HStreamDB 资源。
