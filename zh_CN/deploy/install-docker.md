# Docker 部署指南

本页将指导您使用官方 Docker 镜像快速安装和运行 EMQX，并使用 Docker Compose 实现集群搭建。

## 通过 Docker 运行单个 EMQX 节点

本节主要介绍如何通过 Docker 镜像安装最新版本的 EMQX，如希望体验其他版本，可以前往 [EMQX 下载页面](https://www.emqx.com/zh/try?product=enterprise)。

1. 运行以下命令获取 Docker 镜像：

{% emqxce %}

```bash
docker pull emqx/emqx:@CE_VERSION@
```

{% endemqxce %}

{% emqxee %}

```bash
docker pull emqx/emqx-enterprise:@EE_VERSION@
```

{% endemqxee %}

2. 运行以下命令启动 Docker 容器。

{% emqxce %}

```bash
docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx:@CE_VERSION@
```

有关 EMQX 官方镜像的更多信息，请查看 [Docker Hub - emqx](https://hub.docker.com/_/emqx)。

{% endemqxce %}

{% emqxee %}

```bash
docker run -d --name emqx-enterprise -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx-enterprise:@EE_VERSION@
```

有关 EMQX 官方镜像的更多信息，请查看 [Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)。

{% endemqxee %}

## Docker 部署注意事项

1. 如果需要持久 Docker 容器 ，请将以下目录挂载到容器外部，这样即使容器被删除数据也不会丢失：

  ```bash
  /opt/emqx/data
  /opt/emqx/etc
  /opt/emqx/log
  ```

  注意：挂载 `/opt/etc` 前需要将目录下的文件复制到宿主机下。关于 EMQX 目录结构的详细信息请参考 [EMQX 文件和目录](./install.md#文件和目录)。

  {% emqxce %}

  ```bash

  {% emqxce %}

  将 `/opt/emqx/etc` 目录下的文件复制到宿主机下：

  ```bash
  docker run --rm emqx/emqx:@CE_VERSION@ sh -c 'cd /opt/emqx && tar -c etc' | tar -C $PWD -x
  ```

  启动容器并挂载目录：

  ```bash
  docker run -d --name emqx \
    -p 1883:1883 -p 8083:8083 \
    -p 8084:8084 -p 8883:8883 \
    -p 18083:18083 \
    -v $PWD/etc:/opt/emqx/etc \
    -v $PWD/data:/opt/emqx/data \
    -v $PWD/log:/opt/emqx/log \
    emqx/emqx:@CE_VERSION@
  ```

  ```bash
  docker run -d --name emqx \
    -p 1883:1883 -p 8083:8083 \
    -p 8084:8084 -p 8883:8883 \
    -p 18083:18083 \
    -v $PWD/etc:/opt/emqx/etc \
    -v $PWD/data:/opt/emqx/data \
    -v $PWD/log:/opt/emqx/log \
    emqx/emqx:@CE_VERSION@
  ```

  {% endemqxce %}

  {% emqxee %}

  将 `/opt/emqx/etc` 目录下的文件复制到宿主机下：

  ```bash
  docker run --rm emqx/emqx-enterprise:@EE_VERSION@ sh -c 'cd /opt/emqx && tar -c etc' | tar -C $PWD -x
  ```

  启动容器并挂载目录：

  ```bash
  docker run -d --name emqx-enterprise \
    -p 1883:1883 -p 8083:8083 \
    -p 8084:8084 -p 8883:8883 \
    -p 18083:18083 \
    -v $PWD/etc:/opt/emqx/etc \
    -v $PWD/data:/opt/emqx/data \
    -v $PWD/log:/opt/emqx/log \
    emqx/emqx-enterprise:@EE_VERSION@
  ```

  {% endemqxee %}

2. Docker 内的 `localhost` 或 `127.0.0.1` 指向的是容器内部地址，如需访问宿主机地址请使用宿主机的真实 IP 或使用 [host 网络模式](https://docs.docker.com/network/host/)。如果您使用的是 Docker for Mac 或 Docker for Windows，可以使用 `host.docker.internal` 作为宿主机地址。

3. 由于 EMQX 使用 `data/mnesia/<节点名>` 作为数据存储目录，请使用 hostname 或者 FQDN 等固定的信息作为节点名，避免因为节点名称变动导致数据丢失。

## 通过 Docker Compose 构建 EMQX 集群

Docker Compose 是一个用于编排和运行多容器的工具，下面将指导您通过 Docker Compose 创建简单的 EMQX 静态集群用于测试。

请注意，本章节中的 Docker Compose 示例文件仅适用于本地测试，如果您需要在生产环境中部署集群请参考 [构建集群](./cluster/introduction.md)。

:::tip

目前 Docker Compose 已经包含在 Docker 安装包中无需单独安装，如果您的 Docker 中没有包含 Compose 请参考 [Install Docker Compose](https://docs.docker.com/compose/install/) 进行安装。

:::

1. 在任意目录创建 `docker-compose.yml` 文件，内容如下：

{% emqxce %}

```yml
version: '3'

services:
  emqx1:
    image: emqx:@CE_VERSION@
    container_name: emqx1
    environment:
    - "EMQX_NODE_NAME=emqx@node1.emqx.io"
    - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
    - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.io,emqx@node2.emqx.io]"
    healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx ctl", "status"]
      interval: 5s
      timeout: 25s
      retries: 5
    networks:
      emqx-bridge:
        aliases:
        - node1.emqx.io
    ports:
      - 1883:1883
      - 8083:8083
      - 8084:8084
      - 8883:8883
      - 18083:18083 
    # volumes:
    #   - $PWD/emqx1_data:/opt/emqx/data

  emqx2:
    image: emqx:@CE_VERSION@
    container_name: emqx2
    environment:
    - "EMQX_NODE_NAME=emqx@node2.emqx.io"
    - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
    - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.io,emqx@node2.emqx.io]"
    healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx ctl", "status"]
      interval: 5s
      timeout: 25s
      retries: 5
    networks:
      emqx-bridge:
        aliases:
        - node2.emqx.io
    # volumes:
    #   - $PWD/emqx2_data:/opt/emqx/data

networks:
  emqx-bridge:
    driver: bridge
```

{% endemqxce %}

{% emqxee %}

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
      test: ["CMD", "/opt/emqx/bin/emqx ctl", "status"]
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
    - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
    - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]"
    healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx ctl", "status"]
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

{% endemqxee %}

2. 通过命令行切换 `docker-compose.yml` 文件所在目录，然后输入以下命令启动 EMQX 集群：

```bash
docker-compose up -d
```

3. 查看集群状态

```bash
$ docker exec -it emqx1 sh -c "emqx ctl cluster status"
Cluster status: #{running_nodes => ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                  stopped_nodes => []}
```

## 下一步

使用客户端连接到 EMQX，进行消息收发请参考[发布订阅操作](../messaging/publish-and-subscribe.md)。

配置 EMQX 参数及其他功能请参考 [配置文件手册](../configuration/configuration.md)。

将多个 EMQX 节点组建为一个集群请参考 [构建集群](./cluster/introduction.md)。
