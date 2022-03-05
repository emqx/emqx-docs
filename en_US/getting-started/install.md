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

Operating systems currently supported by EMQX Broker:

+ CentOS 6
+ CentOS 7
+ CentOS 8
+ OpenSUSE tumbleweed
+ Debian 8
+ Debian 9
+ Debian 10
+ Ubuntu 14.04
+ Ubuntu 16.04
+ Ubuntu 18.04
+ Ubuntu 20.04
+ macOS 10.13
+ macOS 10.14
+ macOS 10.15
+ Windows Server 2019

## One-click installation of shell script (Linux)

```bash
curl https://repos.emqx.io/install_emqx.sh | bash
```

## Package manager installation (Linux)

### CentOS

1.   Install the required dependencies

     ```bash
     $ sudo yum install -y yum-utils device-mapper-persistent-data lvm2
     ```

2.   Set up a stable repository, taking the CentOS 7 as an example.

     ```bash
     $ sudo yum-config-manager --add-repo https://repos.emqx.io/emqx-ce/redhat/centos/7/emqx-ce.repo
     ```

3.   Install the latest version of EMQX

     ```bash
     $ sudo yum install emqx
     ```

     If prompted to accept the GPG key, confirm that the key complies with fc84 1ba6 3775 5ca8 487b 1e3c c0b4 0946 3e64 0d53 and accept the fingerprint.

4.   Install a specific version of EMQX

     1.   Query available version

          ```bash
          $ yum list emqx --showduplicates | sort -r

          emqx.x86_64                     4.0.0-1.el7                        emqx-stable
          emqx.x86_64                     3.0.1-1.el7                        emqx-stable
          emqx.x86_64                     3.0.0-1.el7                        emqx-stable
          ```

     2.   Install a specific version based on the version string in the second column, such as 4.0.0

          ```bash
          $ sudo yum install emqx-4.0.0
          ```

5.   Start EMQX

      -  Directly start

         ```bash
         $ emqx start
         emqx 4.0.0 is started successfully!

         $ emqx_ctl status
         Node 'emqx@127.0.0.1' is started
         emqx v4.0.0 is running
         ```

      -  systemctl start

         ```bash
         $ sudo systemctl start emqx
         ```

      - service  start

         ```bash
         $ sudo service emqx start
         ```

6.  Stop EMQX Broker

    ```bash
    $ emqx stop
    ok
    ```

7.  Remove EMQX Broker

    ```bash
    $ sudo yum remove emqx
    ```

### Ubuntu | Debian

1.   Install the required dependency

     ```bash
     $ sudo apt update && sudo apt install -y \
         apt-transport-https \
         ca-certificates \
         curl \
         gnupg-agent \
         software-properties-common
     ```

2.   Add the GPG key for EMQX

     ```bash
     $ curl -fsSL https://repos.emqx.io/gpg.pub | sudo apt-key add -
     ```

     Validate key

     ```bash
     $ sudo apt-key fingerprint 3E640D53

     pub   rsa2048 2019-04-10 [SC]
         FC84 1BA6 3775 5CA8 487B  1E3C C0B4 0946 3E64 0D53
     uid           [ unknown] emqx team <support@emqx.io>
     ```

3.  Use the following command to set up the stable repository. If unstable repo is also needed, add the word ‘unstable’ after ‘stable’ in the following command.

     ```bash
     $ sudo add-apt-repository \
         "deb [arch=amd64] https://repos.emqx.io/emqx-ce/deb/ubuntu/ \
         ./$(lsb_release -cs) \
         stable"
     ```

    The lsb_release -cs subcommand returns the name of the Ubuntu distribution, such as xenial. Sometimes, in a distribution like Linux Mint, you might need to change $(lsb_release -cs) to the parent Ubuntu distribution. For example, if you are using Linux Mint Tessa, you can use bionic. EMQX does not provide any guarantees for untested and unsupported Ubuntu distribution.

4.  Update apt package index

    ```bash
    $ sudo apt update
    ```

5.  Install the latest version of EMQX

    ```bash
    $ sudo apt install emqx
    ```

     In the case where multiple EMQX repositories are enabled, and the apt install and apt update commands is not specified with a version number, the latest version of EMQX is installed. This could be a problem for users with stability needs.

6.  Install a specific version of EMQX

    1.   Query available version

         ```bash
         $ sudo apt-cache madison emqx

         emqx |      4.0.0 | https://repos.emqx.io/emqx-ce/deb/ubuntu bionic/stable amd64 Packages
         emqx |      3.0.1 | https://repos.emqx.io/emqx-ce/deb/ubuntu bionic/stable amd64 Packages
         emqx |      3.0.0 | https://repos.emqx.io/emqx-ce/deb/ubuntu bionic/stable amd64 Packages
         ```

    2.   Install a specific version using the version string from the second column, such as 4.0.0

         ```bash
         $ sudo apt install emqx=4.0.0
         ```

7.  Start EMQX Broker

      - Directly start

        ```bash
        $ emqx start
        emqx 4.0.0 is started successfully!

        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```

      - systemctl start

        ```bash
        $ sudo systemctl start emqx
        ```

      - service start

        ```bash
        $ sudo service emqx start
        ```

8.  Stop EMQX Broker

    ```bash
    $ emqx stop
    ok
    ```

9.  Remove EMQX Broker

    ```bash
    $ sudo apt remove emqx
    ```

### OpenSUSE

1.  Download the GPG public key and import it.

    ```bash
    $ curl -L -o /tmp/gpg.pub https://repos.emqx.io/gpg.pub
    $ sudo rpmkeys --import /tmp/gpg.pub
    ```

2.  Add repository address

    ```bash
    $ sudo zypper ar -f -c https://repos.emqx.io/emqx-ce/redhat/opensuse/leap/stable emqx
    ```

3.  Install the latest version of EMQX Broker

    ```bash
    $ sudo zypper in emqx
    ```

4.  Install a specific version of EMQX

    1.  Query available versions

        ```bash
        $ sudo zypper pa emqx

        Loading repository data...
        Reading installed packages...
        S | Repository | Name | Version  | Arch
        --+------------+------+----------+-------
          | emqx       | emqx | 4.0.0-1  | x86_64
          | emqx       | emqx | 3.0.1-1  | x86_64
          | emqx       | emqx | 3.0.0-1  | x86_64
        ```

    2.   Install a specific version, such as 4.0.0

         ```bash
         $ sudo zypper in emqx-4.0.0
         ```

5.  Start EMQX Broker

      - Directly start

        ```bash
        $ emqx start
        emqx 4.0.0 is started successfully!

        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```

      - systemctl start

        ```bash
        $ sudo systemctl start emqx
        ```

      - service start

        ```bash
        $ sudo service emqx start
        ```

6.  Stop EMQX Broker

    ```bash
    $ emqx stop
    ok
    ```

7.  Remove EMQX Broker

    ```bash
    $ sudo zypper rm emqx
    ```

## Binary package installation (Linux)

1.  Use [emqx.io](https://www.emqx.com/en/downloads/broker?osType=Linux) or [Github](https://github.com/emqx/emqx/releases) for download EMQX Broker packages。

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


## ZIP (Linux、MaxOS、Windows)

1.  Download the zip package of the EMQX Broker version to be installed from [emqx.io](https://www.emqx.com/en/downloads/broker?osType=Linux) or [Github](https://github.com/emqx/emqx/releases).

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

## Install via Homebrew(MacOS)

1.  Add tap of EMQX Broker

    ```bash
    $ brew tap emqx/emqx
    ```

2.  Install EMQX Broker

    ```bash
    $ brew install emqx
    ```

3.  Start EMQX Broker

    ```bash
    $ emqx start
    emqx 4.0.0 is started successfully!

    $ emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

4.  Stop EMQX Broker

    ```bash
    $ emqx stop
    ok
    ```

5.  Uninstall EMQX Broker

    ```bash
    $ brew uninstall emqx
    ```

## Install EMQX in Docker (Contain a simple docker-compose cluster)

### Run a single container

1.  Get docker image

      - From [Docker Hub](https://hub.docker.com/r/emqx/emqx)

        ```bash
        $ docker pull emqx/emqx:v4.0.0
        ```

      - Download the Docker image from [emqx.io](https://www.emqx.com/en/downloads/broker?osType=Linux) or [Github](https://github.com/emqx/emqx/releases) and load it manually

        ```bash
        $ wget -O emqx-docker.zip https://www.emqx.com/en/downloads/broker/v4.0.0/emqx-docker-v4.0.0-alpine3.10-amd64.zip
        $ unzip emqx-docker.zip
        $ docker load < emqx-docker-v4.0.0
        ```

2.  Start docker container

    ```bash
    $ docker run -d --name emqx -p 1883:1883 -p 8081:8081 -p 8083:8083 -p 8883:8883 -p 8084:8084 -p 18083:18083 emqx/emqx:v4.0.0
    ```

### create a simple static cluster by docker-compose

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
