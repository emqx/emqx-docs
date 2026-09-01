# 通过 Docker 运行 EMQX

本页将指导您使用官方 Docker 镜像快速安装和运行 EMQX，并使用 Docker Compose 实现集群搭建。

## 部署前准备

在 Docker 中启动 EMQX 前，请了解以下部署注意事项。

### 配置稳定的节点名

EMQX 将节点数据存储在 `data/mnesia/<节点名>` 目录中。首次启动容器前，请配置稳定的节点名，避免后续节点名发生变化导致数据丢失。

对于单节点部署，使用 `EMQX_NODE_NAME` 环境变量配置节点名，格式为 `emqx@<host>`。容器主机名应与 `<host>` 的值保持一致。

**注意：** `<host>` 部分必须是 IP 地址或完全限定域名（FQDN），例如 `node1.emqx.com`。EMQX 的 Erlang 节点以长节点名模式运行，因此不能使用不含点号的短主机名，例如 `node1`。

### 准备持久化存储

要在容器被删除后保留 EMQX 数据，请将以下容器目录挂载到宿主机：

- `/opt/emqx/data`：存储 EMQX 数据。
- `/opt/emqx/log`：存储文件日志和崩溃转储文件。

EMQX 容器默认使用控制台日志，但节点异常终止时，Erlang 虚拟机会将崩溃转储文件写入 `/opt/emqx/log`。如果未挂载该目录，删除容器后将无法保留转储文件。宿主机上的日志目录必须对容器内的 `emqx` 用户（UID 1000）可写。详情参见 [Docker 中的崩溃转储文件](../configuration/logs.md#docker-中的崩溃转储文件)。

有关 EMQX 目录结构的更多信息，参见 [EMQX 文件和目录](./install.md#文件和目录)。

### 访问宿主机服务

如果 EMQX 需要访问宿主机上运行的服务，请勿使用 `localhost` 或 `127.0.0.1` 作为服务地址。这些地址指向容器自身的网络接口。请使用宿主机 IP 地址或 [host 网络模式](https://docs.docker.com/network/host/)。在 Docker Desktop for Mac 或 Windows 中，也可以使用 `host.docker.internal`。

## 通过 Docker 运行单个 EMQX 节点

按照以下步骤运行单个 EMQX 节点。有关 EMQX 官方 Docker 镜像的更多信息，参见 [Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)。

1. 拉取 Docker 镜像：

   ```bash
   docker pull emqx/emqx-enterprise:@EE_VERSION@
   ```

2. 创建宿主机目录，并确保容器内的 `emqx` 用户对日志目录具有写权限：

   ```bash
   mkdir -p $PWD/data $PWD/log
   sudo chown 1000:1000 $PWD/log
   ```

3. 使用稳定的节点名和已挂载的目录启动容器：

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
## 通过 Docker Compose 构建 EMQX 集群

Docker Compose 是一个用于编排和运行多容器的工具，下面将指导您通过 Docker Compose 创建简单的 EMQX 静态集群用于测试。

本节中的 Docker Compose 示例仅适用于本地测试，其中的卷挂载配置默认被注释。要保留数据和崩溃转储文件，请按照[部署前准备](#部署前准备)中的说明准备宿主机目录，并取消 `volumes` 配置的注释。有关生产环境中的集群部署，参见[构建集群](./cluster/introduction.md)。

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
       #   - $PWD/emqx1_log:/opt/emqx/log

     emqx2:
       image: emqx/emqx-enterprise:@EE_VERSION@
       container_name: emqx2
       environment:
       - "EMQX_NODE_NAME=emqx@node2.emqx.com"
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
       #   - $PWD/emqx2_log:/opt/emqx/log

   networks:
     emqx-bridge:
       driver: bridge
   ```

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
