---
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref:
---

# 安装

EMQX 目前支持的操作系统:

+ CentOS 7 (EL7)
+ CentOS 8 (EL8)
+ Raspbian 10
+ Debian 9
+ Debian 10
+ Ubuntu 16.04
+ Ubuntu 18.04
+ Ubuntu 20.04
+ macOS 10
+ Windows Server 2019

## RPM/DEB包安装 (Linux)

1. 访问 [emqx.com](https://www.emqx.com/zh/try?product=broker) 或 [Github](https://github.com/emqx/emqx/releases) 下载EMQX 版本的二进制包。

2. 安装 EMQX Broker，将下面的路径更改为您下载 EMQX 软件包的路径。

    + RPM 包:

    	```shell
    	$ sudo yum install emqx-cenots7-v4.0.0.x86_64.rpm
    	```

    + DEB 包:

      ```
      # for ubuntu/debian
      $ sudo apt install ./emqx-ubuntu18.04-v4.0.0_amd64.deb
      $ sudo apt install ./emqx-debian10-v4.0.0_amd64.deb
      ```

3. 启动 EMQX Broker

      - 直接启动

        ```
        $ emqx start
        emqx 4.0.0 is started successfully!
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```

      - systemctl 启动

        ```
        $ sudo systemctl start emqx
        ```

      - service 启动

        ```
        $ sudo service emqx start
        ```

4.  停止 EMQX Broker

    ```
    $ emqx stop
    ok
    ```

5.  卸载 EMQX Broker

    + DEB 包:

      ```
      $ sudo apt remove --purge emqx
      ```

    + RPM 包:

      ```
      $ sudo yum remove emqx
      ```

## ZIP 压缩包安装 (Linux、MacOS、Windows)

::: warning
ZIP包适用于测试和热更，如果不知道如何手动安装所有可能的运行时依赖，请勿在生产环境中使用
:::

1.  访问 [emqx.com](https://www.emqx.com/zh/try?product=broker) 或 [Github](https://github.com/emqx/emqx/releases) 下载要安装的 EMQX 版本的 zip 包。

2.  解压程序包

    ```shell
    $ unzip emqx-ubuntu18.04-v4.0.0.zip
    ```

3.  启动 EMQX Broker

    ```
    $ cd ./emqx
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!

    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

4.  停止 EMQX Broker

    ```
    $ ./bin/emqx stop
    ok
    ```

5.  卸载 EMQX Broker

    直接删除 EMQX 目录即可

## 通过 Docker 运行 (包含简单的 docker-compose 集群)

### 运行单个容器

1.  获取 docker 镜像

      - 通过 [Docker Hub](https://hub.docker.com/r/emqx/emqx) 获取

        ```
        $ docker pull emqx/emqx:v4.0.0
        ```

2.  启动 docker 容器

    ```
    $ docker run -d --name emqx -p 1883:1883 -p 8081:8081 -p 8083:8083 -p 8883:8883 -p 8084:8084 emqx/emqx:v4.0.0
    ```

### 使用 docker-compose 创建简单的 static 集群

1. 创建 `docker-compose.yaml` 文件

   ```
   version: '3'

   services:
     emqx1:
       image: emqx/emqx:v4.0.0
       environment:
       - "EMQX_NAME=emqx"
       - "EMQX_HOST=node1.emqx.io"
       - "EMQX_CLUSTER__DISCOVERY=static"
       - "EMQX_CLUSTER__STATIC__SEEDS=emqx@node1.emqx.io, emqx@node2.emqx.io"
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
       image: emqx/emqx:v4.0.0
       environment:
       - "EMQX_NAME=emqx"
       - "EMQX_HOST=node2.emqx.io"
       - "EMQX_CLUSTER__DISCOVERY=static"
       - "EMQX_CLUSTER__STATIC__SEEDS=emqx@node1.emqx.io, emqx@node2.emqx.io"
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

2. 启动 docker-compose 集群

   ```
   $ docker-compose -p my_emqx up -d
   ```

3. 查看集群

   ```
   $ docker exec -it my_emqx_emqx1_1 sh -c "emqx_ctl cluster status"
   Cluster status: #{running_nodes => ['emqx@node1.emqx.io','emqx@node2.emqx.io'],
                     stopped_nodes => []}
   ```

更多关于 EMQX Docker 的信息请查看 [Docker Hub](https://hub.docker.com/r/emqx/emqx) 或 [Github](https://github.com/emqx/emqx-rel/tree/master/deploy/docker)

## 通过 Helm 安装并集群 (K8S、K3S)

1. 添加 helm 仓库

  ```
  $ helm repo add emqx https://repos.emqx.io/charts
  $ helm repo update
  ```

2. 查询 EMQX Broker

  ```
  helm search repo emqx
  NAME         CHART VERSION APP VERSION DESCRIPTION
  emqx/emqx    v4.0.0        v4.0.0      A Helm chart for EMQX
  emqx/emqx-ee v4.0.0        v4.0.0      A Helm chart for EMQX
  emqx/kuiper  0.1.1         0.1.1       A lightweight IoT edge analytic software
  ```

3. 启动 EMQX 集群

  ```
  $ helm install my-emqx emqx/emqx
  ```

4.  查看 EMQX 集群情况

  ```
  $ kubectl get pods
  NAME       READY  STATUS             RESTARTS  AGE
  my-emqx-0  1/1     Running   0          56s
  my-emqx-1  1/1     Running   0          40s
  my-emqx-2  1/1     Running   0          21s

  $ kubectl exec -it my-emqx-0 -- emqx_ctl cluster status
  Cluster status: #{running_nodes =>
                        ['my-emqx@my-emqx-0.my-emqx-headless.default.svc.cluster.local',
                         'my-emqx@my-emqx-1.my-emqx-headless.default.svc.cluster.local',
                         'my-emqx@my-emqx-2.my-emqx-headless.default.svc.cluster.local'],
                    stopped_nodes => []}
  ```


## 源码编译安装

1. 获取源码

```bash
$ git clone https://github.com/emqx/emqx.git
```

2. 切换到最近的 Tag

```bash
$ cd emqx
$ git checkout $(git describe --abbrev=0 --tags)
```

3. 编译

```bash
$ make
```

4. 启动 EMQX Broker

```bash
$ cd _build/emqx/rel/emqx

$ ./bin/emqx start
EMQX Broker 4.3-beta.1 is started successfully!

$ ./bin/emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.3-beta.1 is running
```
