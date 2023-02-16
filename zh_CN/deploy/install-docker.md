# Docker 部署指南

本章节将指导您使用官方 Docker 镜像快速安装和运行 EMQX，并使用 Docker Compose 实现集群搭建。

:::tip Docker 部署注意事项

1. 如需保留数据，请将 EMQX 数据目录 `/opt/emqx/data` 挂载在容器外部，这样即使容器被删除数据也不会丢失。

2. Docker 内的 `localhost` 或 `127.0.0.1` 指向的是容器内部地址，如需访问宿主机地址请使用宿主机的真实 IP 或使用 [host 网络模式](https://docs.docker.com/network/host/)。如果您使用的是 Docker for Mac 或 Docker for Windows，可以使用 `host.docker.internal` 作为宿主机地址。
:::

## 通过 Docker 运行单个 EMQX 节点

本节主要介绍如何通过 Docker 镜像安装最新版本的 EMQX，如希望体验其他版本，可以前往 [EMQX 下载页面](https://www.emqx.com/zh/try?product=enterprise)。

1. 运行以下命令获取 Docker 镜像：

{% emqxce %}

```bash
docker pull emqx:5.0.14
```

{% endemqxce %}

{% emqxee %}

```bash
docker pull emqx/emqx-enterprise:5.0.0
```

{% endemqxee %}

2. 运行以下命令启动 Docker 容器

{% emqxce %}

```bash
docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083  emqx:5.0.14
```

有关 EMQX 官方镜像的更多信息，请查看 [Docker Hub - emqx](https://hub.docker.com/_/emqx)。

{% endemqxce %}

{% emqxee %}

```bash
docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083  emqx/emqx-enterprise:5.0.0
```

有关 EMQX 官方镜像的更多信息，请查看 [Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)。

{% endemqxee %}

## 通过 Docker Compose 构建 EMQX 集群

Docker Compose 是一个用于编排和运行多容器的工具，下面将指导您通过 Docker Compose 创建简单的 EMQX 静态集群。

:::tip 目前 Docker Compose 已经包含在 Docker 安装包中无需单独安装，如果您的 Docker 中没有包含 Compose 请参考 [Install Docker Compos](https://docs.docker.com/compose/install/) 进行安装。
:::

1. 在任意目录创建 `docker-compose.yml` 文件，内容如下：

{% emqxce %}

```yml
version: '3'

services:
  emqx1:
    image: emqx:5.0.14
    container_name: emqx1
    environment:
    - "EMQX_NODE_NAME=emqx@node1.emqx.com"
    - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
    - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]"
    healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
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
    image: emqx:5.0.14
    container_name: emqx2
    environment:
    - "EMQX_NODE_NAME=emqx@node2.emqx.com"
    - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
    - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]"
    healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
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

{% endemqxce %}

{% emqxee %}

```yml
version: '3'

services:
  emqx1:
    image: emqx/emqx-enterprise:5.0.0
    container_name: emqx1
    environment:
    - "EMQX_NODE_NAME=emqx@node1.emqx.com"
    - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
    - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]"
    healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
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
    image: emqx/emqx-enterprise:5.0.0
    container_name: emqx2
    environment:
    - "EMQX_NODE_NAME=emqx@node2.emqx.com"
    - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
    - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]"
    healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
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
$ docker exec -it emqx1 sh -c "emqx_ctl cluster status"
Cluster status: #{running_nodes => ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                  stopped_nodes => []}
```

## 下一步

使用客户端连接到 EMQX，进行消息收发请参考 [发布订阅操作](../messaging/mqtt-publish-and-subscribe.md)。

配置 EMQX 参数及其他功能请参考 [配置文件手册](../configuration/configuration.md)。

将多个 EMQX 节点组建为一个集群请参考 [构建集群](./cluster/introduction.md)。
