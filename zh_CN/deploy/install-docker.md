# 通过 Docker 运行 EMQX

本页将指导您使用官方 Docker 镜像快速安装和运行 EMQX，并使用 Docker Compose 实现集群搭建。

## 通过 Docker 运行单个 EMQX 节点

本节主要介绍如何通过 Docker 镜像安装最新版本的 EMQX。有关 EMQX 官方镜像的更多信息，请查看 [Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)。

1. 运行以下命令获取 Docker 镜像：

   ```bash
   docker pull emqx/emqx-enterprise:@EE_VERSION@
   ```

2. 运行以下命令启动 Docker 容器。

   ```bash
   docker run -d --name emqx-enterprise -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx-enterprise:@EE_VERSION@
   ```

### Docker 中的监听地址

从 EMQX 6.3.0 开始，当环境变量 `EMQX_NODE__DEFAULT_LISTENER_ADDRESS` 未设置或为空时，官方镜像的入口脚本将其设置为 `all`。该默认值使仅指定端口的 MQTT 监听器、网关监听器和 Dashboard HTTP 监听器监听所有网络接口，从而在两种安全配置方案下均可通过发布的容器端口访问。监听器绑定中显式指定的 IP 地址保持不变。该配置仅控制绑定地址，不会放宽认证或授权要求。

如需覆盖此默认值，可通过 `docker run -e EMQX_NODE__DEFAULT_LISTENER_ADDRESS=<value>` 传入其他支持的值，或在 Docker Compose 服务的 `environment` 部分设置该变量。环境变量的优先级高于配置文件，因此，仅在挂载的 `emqx.conf` 中设置 `node.default_listener_address` 不会覆盖入口脚本的默认值。支持的取值参见[默认监听地址](../access-control/security-profile.md#默认监听地址)。

使用 Docker 桥接网络时，将该变量设置为 `loopback` 会使受影响的监听器绑定到容器网络命名空间内的回环地址。此时，即使使用 `-p`，也无法通过发布的端口访问这些监听器。如需控制发布端口所使用的宿主机地址，请参见 [Docker 端口发布和映射](https://docs.docker.com/engine/network/port-publishing/)。

### 使用功能门控启动 EMQX

从 EMQX 6.3.0 开始，可以使用 `EMQX_FEATURES` 环境变量控制启动时可用的可选功能。例如，如需仅启动核心应用，运行：

```bash
docker run -d --name emqx-enterprise \
  -e "EMQX_FEATURES=ESSENTIAL" \
  -p 1883:1883 -p 8083:8083 \
  -p 8084:8084 -p 8883:8883 \
  emqx/emqx-enterprise:@EE_VERSION@
```

如需使用自定义功能集启动 EMQX，运行：

```bash
docker run -d --name emqx-enterprise \
  -e "EMQX_FEATURES=dashboard,auth,metrics" \
  -p 1883:1883 -p 18083:18083 \
  emqx/emqx-enterprise:@EE_VERSION@
```

完整功能列表和依赖行为请参见[功能门控](./feature-gates.md)。

### Docker 部署注意事项

1. 如果需要持久化 Docker 容器中生成的数据 ，请将以下目录挂载到容器外部，这样即使容器被删除数据也不会丢失：

   ```bash
   /opt/emqx/data
   /opt/emqx/log
   ```

   关于 EMQX 目录结构的详细信息请参考 [EMQX 文件和目录](./install.md#文件和目录)。

   启动容器并挂载目录：

   ```bash
   docker run -d --name emqx-enterprise \
     --hostname node1.emqx.com \
     -e "EMQX_NODE_NAME=emqx@node1.emqx.com" \
     -p 1883:1883 -p 8083:8083 \
     -p 8084:8084 -p 8883:8883 \
     -p 18083:18083 \
     -v $PWD/data:/opt/emqx/data \
     -v $PWD/log:/opt/emqx/log \
     emqx/emqx-enterprise:@EE_VERSION@
   ```
   
2. Docker 内的 `localhost` 或 `127.0.0.1` 指向的是容器内部地址，如需访问宿主机地址请使用宿主机的真实 IP 或使用 [host 网络模式](https://docs.docker.com/network/host/)。如果您使用的是 Docker for Mac 或 Docker for Windows，可以使用 `host.docker.internal` 作为宿主机地址。

3. 由于 EMQX 使用 `data/mnesia/<节点名>` 作为数据存储目录，请使用 FQDN 等固定的信息作为节点名，避免因为节点名称变动导致数据丢失。

   对于单节点部署，需要使用 `EMQX_NODE_NAME` 环境变量配置节点名，格式为 `emqx@<host>`。您还应该设置容器主机名以保持一致，如上面示例所示。

   **注意：** `<host>` 部分必须是 IP 地址或完全限定域名（FQDN），例如 `node1.emqx.com`。EMQX 的 Erlang 节点以长节点名模式运行，因此不能使用不含点号的短主机名，例如 `node1`。

## 通过 Docker Compose 构建 EMQX 集群

Docker Compose 是一个用于编排和运行多容器的工具，下面将指导您通过 Docker Compose 创建简单的 EMQX 静态集群用于测试。

请注意，本章节中的 Docker Compose 示例文件仅适用于本地测试，如果您需要在生产环境中部署集群请参考 [构建集群](./cluster/introduction.md)。

:::tip

目前 Docker Compose 已经包含在 Docker 安装包中无需单独安装，如果您的 Docker 中没有包含 Compose 请参考 [Install Docker Compose](https://docs.docker.com/compose/install/) 进行安装。

:::

1. 在任意目录创建 `docker-compose.yml` 文件，内容如下：

   ```yml
   version: '3'
   
   services:
     emqx1:
       image: emqx/emqx-enterprise:@EE_VERSION@
       container_name: emqx1
       environment:
       - "EMQX_NODE_NAME=emqx@node1.emqx.com"
       # - "EMQX_FEATURES=dashboard,auth,metrics"
       - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
       - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]"
       healthcheck:
         test: ["CMD", "/opt/emqx/bin/emqx", "ctl", "status"]
         interval: 5s
         timeout: 25s
         retries: 5
       networks:
         emqx-bridge:
           aliases:
           - node1.emqx.com
       ports:
         - 1883:1883
         - 8083:8083
         - 8084:8084
         - 8883:8883
         - 18083:18083
       # volumes:
       #   - $PWD/emqx1_data:/opt/emqx/data
   
     emqx2:
       image: emqx/emqx-enterprise:@EE_VERSION@
       container_name: emqx2
       environment:
       - "EMQX_NODE_NAME=emqx@node2.emqx.com"
       # - "EMQX_FEATURES=dashboard,auth,metrics"
       - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
       - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]"
       healthcheck:
         test: ["CMD", "/opt/emqx/bin/emqx", "ctl", "status"]
         interval: 5s
         timeout: 25s
         retries: 5
       networks:
         emqx-bridge:
           aliases:
           - node2.emqx.com
       # volumes:
       #   - $PWD/emqx2_data:/opt/emqx/data
   
   networks:
     emqx-bridge:
       driver: bridge
   ```

   如果在 Docker Compose 集群中设置 `EMQX_FEATURES`，请为所有 EMQX 服务使用相同的值。

2. 通过命令行切换 `docker-compose.yml` 文件所在目录，然后输入以下命令启动 EMQX 集群：

   ```bash
   docker-compose up -d
   ```

3. 查看集群状态：

   ```bash
   $ docker exec -it emqx1 sh -c "emqx ctl cluster status"
   Cluster status: #{running_nodes => ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                     stopped_nodes => []}
   ```

## 下一步

使用客户端连接到 EMQX，进行消息收发，请参考[发布订阅操作](../messaging/publish-and-subscribe.md)。

配置 EMQX 参数及其他功能，请参考[配置文件](../configuration/configuration.md)。

将多个 EMQX 节点组建为一个集群，请参考[构建集群](./cluster/introduction.md)。
