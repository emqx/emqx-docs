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

EMQX binary packages are released on below operating systems:

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

## Package Installation (Linux)

1.  Download EMQX package [emqx.com](https://www.emqx.com/en/downloads/try?product=broker) or [Github](https://github.com/emqx/emqx/releases)

2. Install EMQX Broker:

    + RPM package:

    	```shell
    	$ sudo yum install emqx-cenots7-v4.0.0.x86_64.rpm
    	```
    + DEB package:

      ```bash
      # for ubuntu/debian
      $ sudo apt install ./emqx-ubuntu18.04-v4.0.0_amd64.deb
      $ sudo apt install ./emqx-debian10-v4.0.0_amd64.deb
      ```

3. Start EMQX Broker

      - quick start

        ```bash
        $ emqx start
        emqx 4.0.0 is started successfully!
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```

      - systemctl

        ```bash
        $ sudo systemctl start emqx
        ```

      - start as service

        ```bash
        $ sudo service emqx start
        ```

4.  Stop EMQX Broker

    ```bash
    $ emqx stop
    ok
    ```

5.  Uninstall EMQX Broker

    + DEB:

      ```bash
      $ sudo apt remove --purge emqx
      ```

    + RPM:

      ```bash
      $ sudo yum remove emqx
      ```


## ZIP (Linux、MacOS、Windows)

::: warning
ZIP packages are released for quick testing and hot-beam upgrade. Do NOT install zip packages for production unless you know how to manually resolve all the runtime dependencies.
:::

1.  Download EMQX zip package from [emqx.com](https://www.emqx.com/en/downloads/try?product=broker) or [Github](https://github.com/emqx/emqx/releases).

2.   Unzip the installation file:

     ```shell
     $ unzip emqx-ubuntu18.04-v4.0.0.zip
     ```

3.  Start EMQX Broker

    ```bash
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!

    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

4.  Stop EMQX Broker

    ```bash
    $ ./bin/emqx stop
    ok
    ```

5.  Remove EMQX Broker

    Simply delete the EMQX Broker directory

## Running EMQX in Docker (Contain a simple docker-compose cluster)

### Run a single container

1.  Get docker image

      - From [Docker Hub](https://hub.docker.com/r/emqx/emqx)

        ```bash
        $ docker pull emqx/emqx:v4.0.0
        ```

2.  Start docker container

    ```bash
    $ docker run -d --name emqx -p 1883:1883 -p 8081:8081 -p 8083:8083 -p 8883:8883 -p 8084:8084 -p 18083:18083 emqx/emqx:v4.0.0
    ```

### Create a simple static cluster by docker-compose

1. Create `docker-compose.yaml` file

   ```bash
   version: '3'

   services:
     emqx1:
       image: emqx/emqx:v4.0.0
       environment:
       - "EMQX_NAME=emqx"
       - "EMQX_HOST=node1.emqx.io"
       - "EMQX_CLUSTER__DISCOVERY=static"
       - "EMQX_CLUSTER__STATIC__SEEDS=emqx@node1.emqx.io, emqx@node2.emqx.io"
       - "EMQX_ZONE__EXTERNAL__RETRY_INTERVAL=2s"
       - "EMQX_MQTT__MAX_TOPIC_ALIAS=10"
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
       image: emqx/emqx:v4.0.0
       environment:
       - "EMQX_NAME=emqx"
       - "EMQX_HOST=node2.emqx.io"
       - "EMQX_CLUSTER__DISCOVERY=static"
       - "EMQX_CLUSTER__STATIC__SEEDS=emqx@node1.emqx.io, emqx@node2.emqx.io"
       - "EMQX_ZONE__EXTERNAL__RETRY_INTERVAL=2s"
       - "EMQX_MQTT__MAX_TOPIC_ALIAS=10"
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

     client:
       image: python:3.7.2-alpine3.9
       depends_on:
         - emqx1
         - emqx2
       tty: true
       networks:
           emqx-bridge:

   networks:
     emqx-bridge:
       driver: bridge

   ```

2. Start docker-compose cluster

   ```bash
   $ docker-compose -p my_emqx up -d
   ```

3. View cluster

   ```bash
   $ docker exec -it my_emqx_emqx1_1 sh -c "emqx_ctl cluster status"
   Cluster status: #{running_nodes => ['emqx@node1.emqx.io','emqx@node2.emqx.io'],
                     stopped_nodes => []}
   ```

For more information about EMQX Broker Docker, please visit [Docker Hub](https://hub.docker.com/r/emqx/emqx) or [Github](https://github.com/emqx/emqx-rel/tree/master/deploy/docker)

## Install and cluster via Helm (K8S、K3S)

1. Add helm repository

  ```bash
  $ helm repo add emqx https://repos.emqx.io/charts
  $ helm repo update
  ```

2. Query EMQX Broker

  ```bash
  helm search repo emqx
  NAME         CHART VERSION APP VERSION DESCRIPTION
  emqx/emqx    v4.0.0        v4.0.0      A Helm chart for EMQX
  emqx/emqx-ee v4.0.0        v4.0.0      A Helm chart for EMQX
  emqx/kuiper  0.1.1         0.1.1       A lightweight IoT edge analytic software
  ```

3. Start EMQX Broker cluster

  ```bash
  $ helm install my-emqx emqx/emqx
  ```

4.  View EMQX Broker cluster situation

  ```bash
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

## Source code compilation and installation

1. Get the source code

```bash
$ git clone https://github.com/emqx/emqx.git
```

2. Checkout to latest tag

```bash
$ cd emqx
$ git checkout $(git describe --abbrev=0 --tags)
```

3. Compile

```bash
$ make
```

4. Start EMQX Broker

```bash
$ cd _build/emqx/rel/emqx

$ ./bin/emqx start
EMQX Broker 4.3-beta.1 is started successfully!

$ ./bin/emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.3-beta.1 is running
```
