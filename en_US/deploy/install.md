# Installation

EMQX binary packages are released on below operating systems:

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

## Package Installation (Linux)

:::warning
EMQX starts as `emqx@127.0.0.1` by default and the node name will be written
as a part of the builtin database.
Purge database directory `/var/lib/emqx/data/mnesia` before starting it with a different name.
:::

{% emqxce %}

1. Download EMQX package [emqx.com](https://www.emqx.com/en/downloads/try?product=broker) or [Github](https://github.com/emqx/emqx/releases)

{% endemqxce %}

{% emqxee %}

1. Download EMQX package [emqx.com](https://www.emqx.com/en/downloads/try?product=enterprise) or [Github](https://github.com/emqx/emqx/releases)

{% endemqxee %}

2. Install EMQX Broker:

    + RPM package (RedHat/CentOS/RockyLinux/AmazonLinux):

      ```shell
      sudo yum install path/to/emqx-full-package-name.rpm
      ```

    + DEB package (Ubuntu/Debian/Raspbian):

      ```shell
      sudo apt install path/to/emqx-full-package-name.deb
      ```

3. Start EMQX and check status

      - quick start

        ```shell
        sudo su - emqx
        emqx start
        emqx_ctl status
        ```

      - systemctl

        ```shell
        sudo systemctl start emqx
        ```

4.  Stop EMQX Broker

    ```shell
    $ emqx stop
    ```

5.  Uninstall EMQX Broker

    + DEB:

      ```shell
      sudo apt remove --purge emqx
      ```

    + RPM:

      ```shell
      sudo yum remove emqx
      ```

## tgz (Linux, macOS, Windows)

::: warning
tar.gz packages are released for quick testing and hot-beam upgrades.
Do NOT install tar.gz packages for production unless
you know how to manually resolve all the runtime dependencies.
:::

::: warning
The EMQX digital signature work on macOS has not been completed, and the installation and startup of the tgz package may be blocked by [Gatekeeper](https://support.apple.com/zh-cn/guide/security/sec5599b66df/web).

When you encounter a prompt like **“erl” cannot be opened because the developer cannot be verified** or **“erlang_jq_port” cannot be opened because the developer cannot be verified** at startup, solve this as follows:


Remove the extended attribute `com.apple.quarantine` of tgz file or folder:

  ```shell
  xattr -d com.apple.quarantine emqx-full-package-name.tar.gz
  # or
  xattr -r -d com.apple.quarantine ./emqx
  ```

Please verify the SHA256 of the file when using this operation to ensure the integrity of the installation package, so as not to introduce additional security risks.
:::

{% emqxce %}

1. Download EMQX package [emqx.com](https://www.emqx.com/en/downloads/try?product=broker) or [Github](https://github.com/emqx/emqx/releases)

{% endemqxce %}

{% emqxee %}

1. Download EMQX package [emqx.com](https://www.emqx.com/en/downloads/try?product=enterprise)

{% endemqxee %}

2. Extract the package:

     ```shell
     tar -zxf emqx-full-package-name.tar.gz
     ```

3. Start EMQX and check status

    ```shell
    ./bin/emqx start
    ./bin/emqx_ctl status
    ```

4. Stop EMQX Broker

    ```shell
    ./bin/emqx stop
    ```

5. Remove EMQX Broker

    Simply delete the EMQX Broker directory

## Running EMQX in Docker

### Run a single container

1.  Get docker image

    ```shell
    docker pull emqx/emqx:5.0.0
    ```

2.  Start docker container

    ```shell
    docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8883:8883 -p 8084:8084 -p 18083:18083 emqx/emqx:5.0.0
    ```

### Create a simple static cluster by docker-compose

1. Create `docker-compose.yaml` file

   ```yml
   version: '3'

   services:
     emqx1:
       image: emqx/emqx:5.0.0
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
       image: emqx/emqx:5.0.0
       environment:
       - "EMQX_NODE_NAME=emqx@node2.emqx.io"
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

2. Start docker-compose cluster

   ```shell
   docker-compose -p my_emqx up -d
   ```

3. View cluster

   ```shell
   $ docker exec -it my_emqx_emqx1_1 sh -c "emqx_ctl cluster status"
   Cluster status: #{running_nodes => ['emqx@node1.emqx.io','emqx@node2.emqx.io'],
                     stopped_nodes => []}
   ```

For more information about EMQX Broker Docker please visit [Docker Hub](https://hub.docker.com/r/emqx/emqx) or [Github](https://github.com/emqx/emqx-rel/tree/master/deploy/docker)

## Install and cluster via Helm (K8S, K3S)

1. Add helm repository

  ```shell
  helm repo add emqx https://repos.emqx.io/charts
  helm repo update
  ```

2. Query EMQX Broker

  ```shell
  $ helm search repo emqx
  NAME         CHART VERSION APP VERSION DESCRIPTION
  emqx/emqx    v4.0.0        v4.0.0      A Helm chart for EMQX
  emqx/emqx-ee v4.0.0        v4.0.0      A Helm chart for EMQX
  emqx/kuiper  0.1.1         0.1.1       A lightweight IoT edge analytic software
  ```

3. Start EMQX Broker cluster

  ```shell
  helm install my-emqx emqx/emqx
  ```

4. View EMQX Broker cluster situation

  ```shell
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

```shell
git clone https://github.com/emqx/emqx.git
```

2. Checkout to latest tag

```shell
cd emqx
git checkout $(git describe --abbrev=0 --tags)
```

3. Compile and run

{% emqxce %}

```shell
make run
```

{% endemqxce %}
{% emqxee %}

```shell
env PROFILE=emqx-enterprise make run
```

{% endemqxee %}
