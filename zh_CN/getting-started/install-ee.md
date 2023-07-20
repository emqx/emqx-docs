# EMQX 企业版安装

EMQX 消息服务器可跨平台运行在 Linux 服务器上。

## EMQX License 文件获取

联系商务或登陆 <https://www.emqx.com> 注册账号获取免费的试用 License 文件

## EMQX 程序包下载

EMQX 消息服务器每个版本会发布 CentOS、Ubuntu、Debian 平台程序包与 Docker 镜像。

下载地址: <https://www.emqx.com/zh/downloads>


## CentOS

- CentOS 7 (EL7)
- CentOS 8 (EL8)

### 使用 rpm 包安装 EMQX

1. 访问[emqx.com](https://www.emqx.com/zh/try?product=enterprise) 选择 CentOS
   版本，然后下载要安装的 EMQX 版本的 rpm 包。

2.  安装 EMQX

    ```bash
    $ sudo rpm -ivh emqx-ee-centos7-v4.0.0.x86_64.rpm
    ```

3.  导入License文件:

    ```bash
    $ cp /path/to/emqx.lic /etc/emqx/emqx.lic
    ```

4.  启动 EMQX

      - 直接启动

        ```bash
        $ emqx start
        emqx  is started successfully!

        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx 4.0.0 is running
        ```

      - systemctl 启动

        ```bash
        $ sudo systemctl start emqx
        ```

      - service 启动

        ```bash
        $ sudo service emqx start
        ```

### 使用 zip 包安装 EMQX

:::
ZIP包适用于测试和热更，如果不知道如何手动安装所有可能的运行时依赖，请勿在生产环境中使用
:::

1.  通过 [emqx.com](https://www.emqx.com/en/try?product=enterprise) 选择 Centos
    版本，然后下载要安装的 EMQX 版本的 zip 包。

2.  解压程序包

    ```bash
    $ unzip emqx-ee-centos7-v4.0.0.zip
    ```

3.  导入License文件:

    ```bash
    $ cp /path/to/emqx.lic /path/to/emqx/etc/emqx.lic
    ```

4.  启动 EMQX

    ```bash
    $ ./bin/emqx start
    emqx v4.0.0 is started successfully!

    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx 4.0.0 is running
    ```

## Ubuntu

  - Bionic 18.04 (LTS)
  - Xenial 16.04 (LTS)

### 使用 deb 包安装 EMQX

1.  通过 [emqx.com](https://www.emqx.com/en/try?product=enterprise) 选择 Ubuntu
    版本，然后下载要安装的 EMQX 版本的 deb 包。

2.  安装 EMQX

```bash
# for ubuntu
$ sudo apt install ./emqx-ee-ubuntu18.04-v3.1.0_amd64.deb
# for debian
$ sudo dpkg -i emqx-ee-ubuntu18.04-v3.1.0_amd64.deb
```

3.  导入License文件:

```bash
$ cp /path/to/emqx.lic /etc/emqx/emqx.lic
```

4.  启动 EMQX

- 直接启动

```bash
$ emqx start
emqx  is started successfully!

$ emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

- systemctl 启动

```bash
$ sudo systemctl start emqx
```

- service 启动

```bash
$ sudo service emqx start
```

### 使用 zip 包安装 EMQX

:::
ZIP包适用于测试和热更，如果不知道如何手动安装所有可能的运行时依赖，请勿在生产环境中使用
:::

1.  通过 [emqx.com](https://www.emqx.com/en/try?product=enterprise) 选择 Ubuntu
    版本，然后下载要安装的 EMQX 版本的 zip 包。

2.  解压程序包

```bash
$ unzip emqx-ee-ubuntu18.04-v4.0.0.zip
```

3.  导入License文件:

```bash
$ cp /path/to/emqx.lic /path/to/emqx/etc/emqx.lic
```

4.  启动 EMQX

```bash
$ ./bin/emqx start
emqx v4.0.0 is started successfully!

$ ./bin/emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```


## Debian

- Debian 9
- Debian 10

### 使用 deb 包安装 EMQX

1.  通过 [emqx.com](https://www.emqx.com/en/try?product=enterprise) 选择 Debian
    版本，然后下载要安装的 EMQX 版本的 deb 包。

2.  安装 EMQX

    ```bash
    # for ubuntu
    $ sudo apt install ./emqx-ee-debian9-v3.1.0_amd64.deb

    # for debian
    # 首先确保已安装 libodbc
    $ sudo dpkg -i emqx-ee-debian9-v3.1.0_amd64.deb
    ```

3.  导入License文件:

    ```bash
    $ cp /path/to/emqx.lic /etc/emqx/emqx.lic
    ```

4.  启动 EMQX

      - 直接启动

        ```bash
        $ emqx start
        emqx v4.0.0 is started successfully!

        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx 4.0.0 is running
        ```

      - systemctl 启动

        ```bash
        $ sudo systemctl start emqx
        ```

      - service 启动

        ```bash
        $ sudo service emqx start
        ```

### 使用 zip 包安装 EMQX

:::
ZIP包适用于测试和热更，如果不知道如何手动安装所有可能的运行时依赖，请勿在生产环境中使用
:::

1.  通过 [emqx.com](https://www.emqx.com/en/try?product=enterprise) 选择 Debian
    版本，然后下载要安装的 EMQX 版本的 zip 包。

2.  解压程序包

    ```bash
    $ unzip emqx-ee-debian9-v4.0.0.zip
    ```

3.  导入License文件:

    ```bash
    $ cp /path/to/emqx.lic /path/to/emqx/etc/emqx.lic
    ```

4.  启动 EMQX

    ```bash
    $ ./bin/emqx start
    emqx v4.0.0 is started successfully!

    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx 4.0.0 is running
    ```


## macOS

### 使用 ZIP 包安装 EMQX

1.  通过 [emqx.com](https://www.emqx.com/en/try?product=enterprise) ，选择 EMQX
    版本，然后下载要安装的 zip 包。

2.  解压压缩包

    ```bash
    $ unzip emqx-ee-macos-v4.0.0.zip
    ```

3.  导入License文件:

    ```bash
    $ cp /path/to/emqx.lic /path/to/emqx/etc/emqx.li
    ```

4.  启动 EMQX

    ```bash
    $ ./bin/emqx start
    emqx v4.0.0 is started successfully!

    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx 4.0.0 is running
    ```

## Docker

:::tip Docker 部署注意事项

1. 如果需要持久 Docker 容器 ，请将以下目录挂载到容器外部，这样即使容器被删除数据也不会丢失：

```bash
/opt/emqx/data
/opt/emqx/etc
/opt/emqx/log
```

2. Docker 内的 `localhost` 或 `127.0.0.1` 指向的是容器内部地址，如需访问宿主机地址请使用宿主机的真实 IP 或使用 [host 网络模式](https://docs.docker.com/network/host/)。如果您使用的是 Docker for Mac 或 Docker for Windows，可以使用 `host.docker.internal` 作为宿主机地址。

3. 由于 EMQX 将数据存储在 `data/mnesia/<节点名>` 目录，所以在使用容器启动 EMQX 的时候，
必须使用 hostname 或者 FQDN 作为节点名。否则数据存储目录将发生切换，导致数据丢失。
:::

### 启动单个容器

1.  获取 docker 镜像

- 通过 [Docker Hub](https://hub.docker.com/r/emqx/emqx-ee) 获取

```bash
$ docker pull emqx/emqx-ee:v4.0.0
```

2.  启动 docker 容器

```bash
$ docker run -d \
    --name emqx-ee \
    -p 8081:8081 \
    -p 1883:1883 \
    -p 8083:8083 \
    -p 8883:8883 \
    -p 8084:8084 \
    -p 18083:18083 \
    -v /path/to/emqx.lic:/opt/emqx/etc/emqx.lic \
    emqx/emqx-ee:v4.0.0
```

更多关于 EMQX Docker 的信息请查看 [Docker Hub](https://hub.docker.com/r/emqx/emqx-ee)

### 使用 docker-compose 创建一个简单的静态集群

请注意，本章节中的 Docker Compose 示例文件仅适用于本地测试，如果您需要在生产环境中部署集群请参考 [分布式集群](./cluster.md)。

1. 创建 `docker-compose.yaml` 文件

   ```
   version: '3'

   services:
     emqx1:
       image: emqx/emqx-ee:v4.0.0
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
       image: emqx/emqx-ee:v4.0.0
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

2. 启动 docker-compose 集群

   ```
   $ docker-compose -p my_emqx up -d
   ```

3. 查看 cluster

   ```
   $ docker exec -it my_emqx_emqx1_1 sh -c "emqx_ctl cluster status"
   Cluster status: #{running_nodes => ['emqx@node1.emqx.io','emqx@node2.emqx.io'],
                     stopped_nodes => []}
   ```

关于 EMQX 企业版容器的更多信息，请参看 [Docker Hub](https://hub.docker.com/r/emqx/emqx-ee)
