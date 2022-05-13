# 安装

EMQX 目前支持的操作系统:

+ EL7 (RedHat 7, CentOS 7)
+ EL8 (RedHat 8, RockyLinux 8, AmazonLinux 2022)
+ Raspbian 10
+ Debian 9
+ Debian 10
+ Ubuntu 16.04
+ Ubuntu 18.04
+ Ubuntu 20.04
+ macOS 10
+ Windows Server 2019

## RPM/DEB包安装 (Linux)

:::warning
EMQX 默认以节点名称 `emqx@127.0.0.1` 启动，改名称写入到内置数据库中。
更改节点名称前，请确保 `/var/lib/emqx/data/mnesia` 目录为空。
:::

{% emqxce %}
1. 访问 [emqx.com](https://www.emqx.com/zh/try?product=broker) 或 [Github](https://github.com/emqx/emqx/releases) 下载EMQX 版本的二进制包。
{% endemqxce %}
{% emqxee %}
1. 访问 [emqx.com](https://www.emqx.com/zh/try?product=enterprise) 或 [Github](https://github.com/emqx/emqx/releases) 下载EMQX 版本的二进制包。
{% endemqxee %}

2. 安装 EMQX Broker，将下面的路径更改为您下载 EMQX 软件包的路径。

    + RPM 包 (RedHat/CentOS/RockyLinux/AmazonLinux):

      ```shell
      $ sudo yum install emqx-full-package-name.rpm
      ```

    + DEB 包 (Ubuntu/Debian/Raspbian):

      ```
      $ sudo apt install emqx-full-package-name.deb
      ```

3. 启动 EMQX Broker

      - 直接启动

        ```
        $ sudo su - emqx
        $ emqx start
        $ emqx_ctl status
        ```

      - systemctl 启动

        ```
        $ sudo systemctl start emqx
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

## tgz 压缩包安装

::: warning
tar.gz 包适用于测试和热更，如果不知道如何手动安装所有可能的运行时依赖，请勿在生产环境中使用
:::

{% emqxce %}
1.  访问 [emqx.com](https://www.emqx.com/zh/try?product=broker) 或 [Github](https://github.com/emqx/emqx/releases) 下载要安装的 EMQX 版本的 tar.gz 包。
{% endemqxce %}
{% emqxee %}
1.  访问 [emqx.com](https://www.emqx.com/zh/try?product=enterprise) 或 [Github](https://github.com/emqx/emqx/releases) 下载要安装的 EMQX 版本的 tar.gz 包。
{% endemqxee %}

2.  解压程序包

    ```shell
    $ tar -zxf emqx-full-package-name.tar.gz
    ```

3.  启动 EMQX Broker

    ```
    $ cd ./emqx
    $ ./bin/emqx start
    $ ./bin/emqx_ctl status
    ```

4.  停止 EMQX Broker

    ```
    $ ./bin/emqx stop
    ```

5.  卸载 EMQX Broker

    直接删除 EMQX 目录即可

## 通过 Docker 运行 (包含简单的 docker-compose 集群)

### 运行单个容器

1.  获取 docker 镜像

      - 通过 [Docker Hub](https://hub.docker.com/r/emqx/emqx) 获取

        ```
        $ docker pull emqx/emqx:latest
        ```

2.  启动 docker 容器

    ```
    $ docker run -d --name emqx -p 1883:1883 -p 8081:8081 -p 8083:8083 -p 8883:8883 -p 8084:8084 -p 18083:18083 emqx/emqx:5.0
    ```

### 使用 docker-compose 创建简单的 static 集群

1. 创建 `docker-compose.yaml` 文件

   ```
   version: '3'

   services:
     emqx1:
       image: emqx/emqx:5.0
       environment:
       - "EMQX_NODE_NAME=emqx@node1.emqx.io"
       - "EMQX_CLUSTER__DISCOVERY=static"
       - "EMQX_CLUSTER__STATIC__SEEDS=emqx@node1.emqx.io,emqx@node2.emqx.io"
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
       image: emqx/emqx:5.0
       environment:
       - "EMQX_NODE_NAME=emqx@node2.emqx.io"
       - "EMQX_CLUSTER__DISCOVERY=static"
       - "EMQX_CLUSTER__STATIC__SEEDS=emqx@node1.emqx.io,emqx@node2.emqx.io"
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

3. 编译并运行

{% emqxce %}
```bash
$ make run
```
{% endemqxce %}
{% emqxee %}
```bash
$ env PROFILE=emqx-enterprise make run
```
{% endemqxee %}
