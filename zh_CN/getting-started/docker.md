# Docker 部署指南

本章节将指导您使用官方 Docker 镜像快速安装和运行 EMQX，并使用 Docker Compose 实现集群搭建。

:::tip Docker 部署注意事项

由于 emqx 将数据存储在 `data/mnesia/<节点名>` 目录，所以在使用容器启动 emqx 的时候，
必须使用 hostname 或者 FQDN 来组建 emqx 的节点名。否则数据存储目录将发生切换，导致数据丢失。
:::

## 通过 Docker 运行单个 EMQX 节点

本节主要介绍如何通过 Docker 镜像安装最新版本的 EMQX，如希望体验其他版本，可以前往 [EMQX 下载页面](https://www.emqx.com/zh/try?product=enterprise)。

1. 运行以下命令获取 Docker 镜像：

```bash
docker pull emqx/emqx-enterprise:4.3.19
```

2. 运行以下命令启动 Docker 容器

```bash
docker run -d --name emqx-enterprise -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx-enterprise:4.3.19
```

有关 EMQX 官方镜像的更多信息，请查看 [Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise)。

## 通过 Docker Compose 构建 EMQX 集群

Docker Compose 是一个用于编排和运行多容器的工具，下面将指导您通过 Docker Compose 创建简单的 EMQX 静态集群。

:::tip

目前 Docker Compose 已经包含在 Docker 安装包中无需单独安装，如果您的 Docker 中没有包含 Compose 请参考 [Install Docker Compos](https://docs.docker.com/compose/install/) 进行安装。

:::

1. 在任意目录创建 `docker-compose.yml` 文件，内容如下：

```yml
version: '3'

services:
  emqx1:
    image: emqx/emqx-ee:4.3.19
    environment:
    - "EMQX_NAME=emqx"
    - "EMQX_HOST=node1.emqx.io"
    - "EMQX_CLUSTER__DISCOVERY=static"
    - "EMQX_CLUSTER__STATIC__SEEDS=emqx@node1.emqx.io, emqx@node2.emqx.io"
    volumes:
        - ./tmp/emqx.lic:/opt/emqx/etc/emqx.lic
    healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
      interval: 5s
      timeout: 25s
      retries: 5
    networks:
      emqx-bridge:
        aliases:
        - node1.emqx.io

  emqx2:
    image: emqx/emqx-ee:4.3.19
    environment:
    - "EMQX_NAME=emqx"
    - "EMQX_HOST=node2.emqx.io"
    - "EMQX_CLUSTER__DISCOVERY=static"
    - "EMQX_CLUSTER__STATIC__SEEDS=emqx@node1.emqx.io, emqx@node2.emqx.io"
    volumes:
        - ./tmp/emqx.lic:/opt/emqx/etc/emqx.lic
    healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
      interval: 5s
      timeout: 25s
      retries: 5
    networks:
      emqx-bridge:
        aliases:
        - node2.emqx.io

networks:
  emqx-bridge:
    driver: bridge
```

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

使用客户端连接到 EMQX，进行消息收发请参考 [发布订阅](../development/protocol.md)。

配置 EMQX 参数及其他功能请参考 [配置项](../configuration/configuration.md)。

将多个 EMQX 节点组建为一个集群请参考 [构建集群](../advanced/cluster.md)。