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
ref: undefined
---

# Installation

EMQX Enterprise binary packages are released on below operating systems:

+ CentOS 7 (EL7)
+ CentOS 8 (EL8)
+ Raspbian 10
+ Debian 9
+ Debian 10
+ Ubuntu 16.04
+ Ubuntu 18.04
+ Ubuntu 20.04

## Package Installation (Linux)

1.  Download EMQX RPM package from [emqx.com](https://www.emqx.com/en/try?product=enterprise).

2. Install EMQX Broker:

    + RPM:
    	```shell
    	$ sudo rpm -ivh emqx-ee-cenots7-v4.0.0.x86_64.rpm
    	```

    + DEB:

      ```
      # for ubuntu
      $ sudo apt install -i ./emqx-ee-ubuntu18.04-v4.0.0_amd64.deb

      # for debian
      # first ensure libodbc is installed, then
      $ sudo dpkg -i emqx-ee-ubuntu18.04-v4.0.0_amd64.deb
      ```

3. Start EMQX Broker

      - quick start

        ```
        $ emqx start
        emqx 4.0.0 is started successfully!
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```

      - systemctl

        ```
        $ sudo systemctl start emqx
        ```

      - start as service

        ```
        $ sudo service emqx start
        ```

4.  Stop EMQX Broker

    ```
    $ emqx stop
    ok
    ```

5.  Uninstall EMQX Broker

    + DEB:

      ```
      $ dpkg -r emqx-ee
      ```

      or

      ```
      $ dpkg -P emqx-ee
      ```

    + RPM:

      ```
      $ rpm -e emqx-ee
      ```


## ZIP (Linux、MacOS)

:::
ZIP packages are released for quick testing and hot-beam upgrade. Do NOT install zip packages for production unless you know how to manually resolve all the runtime dependencies.
:::

1.  Download the zip package from [emqx.com](https://www.emqx.com/en/try?product=enterprise) or [Github](https://github.com/emqx/emqx/releases).

2.   Unzip the installation file:

      ```shell
      $ unzip emqx-ee-ubuntu18.04-v4.0.0.zip
      ```

3.  Start EMQX Broker

    ```
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!

    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

4.  Stop EMQX Broker

    ```
    $ ./bin/emqx stop
    ok
    ```

5.  Remove EMQX Broker

    Simply delete the EMQX Broker directory

## Install EMQX in Docker (Contain a simple docker-compose cluster)

:::tip Note

1. If you want to persist the EMQX Docker container, you need to keep the following directories, so that the data will persist even if the container no longer exists.

```bash
/opt/emqx/data
/opt/emqx/etc
/opt/emqx/log
```

2. In Docker, `localhost` or `127.0.0.1`  points to the internal address of the container. Use the host’s IP or [host networking](https://docs.docker.com/network/host/) to access the host address. If you are using Docker for Mac or Docker for Windows, you can use `host.docker.internal` as the host address.

3. Due to EMQX using `data/mnesia/<node_name>` as the data storage directory, please use fixed information such as hostname or FQDN as the node name to avoid data loss caused by node name changes.

:::

### Run a single container

1.  Get docker image

      - From [Docker Hub](https://hub.docker.com/r/emqx/emqx-ee)

        ```
        $ docker pull emqx/emqx-ee:v4.0.0
        ```
2.  Start docker container

    ```
    $ docker run -d --name emqx -p 1883:1883 -p 8081:8081 -p 8083:8083 -p 8883:8883 -p 8084:8084 -p 18083:18083 emqx/emqx-ee:v4.0.0
    ```

### create a simple static cluster by docker-compose

Please note that the Docker Compose example file in this section is only applicable to local testing. If you need to deploy a cluster in a production environment, please refer to [Create a cluster](./cluster.md).

1. Create `docker-compose.yaml` file

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

2. Start docker-compose cluster

   ```
   $ docker-compose -p my_emqx up -d
   ```

3. View cluster

   ```
   $ docker exec -it my_emqx_emqx1_1 sh -c "emqx_ctl cluster status"
   Cluster status: #{running_nodes => ['emqx@node1.emqx.io','emqx@node2.emqx.io'],
                     stopped_nodes => []}
   ```

For more information about EMQX Broker Docker, please visit [Docker Hub](https://hub.docker.com/r/emqx/emqx-ee)
