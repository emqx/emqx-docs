---
# 标题
title: Install
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

Operating systems currently supported by EMQ X Broker:

+ Centos6
+ Centos7
+ Centos8
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
wget https://repos.emqx.io/install_emqx.sh
chmod +x install_emqx.sh
sudo ./install_emqx.sh emqx-ee
```

## Package manager installation (Linux)

#### Centos

1.   Install the required dependencies 
  
      ```
      $ sudo yum install -y yum-utils device-mapper-persistent-data lvm2
      ```

2.   Set up a stable repository, taking the CentOS7 as an example. 
    
      ```
      $ sudo yum-config-manager --add-repo https://repos.emqx.io/emqx-ee/redhat/centos/7/emqx-ee.repo
      ```
    
3.   Install the latest version of EMQ X 
  
      ```
      $ sudo yum install emqx-ee
      ```
    
      If prompted to accept the GPG key, confirm that the key complies with fc84 1ba6 3775 5ca8 487b 1e3c c0b4 0946 3e64 0d53 and accept the fingerprint.
    
4.   Install a specific version of EMQ X 

       1.   Query available version 
      
            ```
            $ yum list emqx --showduplicates | sort -r
            
            emqx-ee.x86_64                     4.0.0-1.el7                        emqx-stable
            emqx-ee.x86_64                     3.0.1-1.el7                        emqx-stable
            emqx-ee.x86_64                     3.0.0-1.el7                        emqx-stable
            ```

     1.   Install a specific version based on the version string in the second column, such as 4.0.0 
      
            ```
            $ sudo yum install emqx-ee-4.0.0
            ```

5.   Start EMQ X 
  
      -  Directly start 
        
          ```
          $ emqx start
          emqx 4.0.0 is started successfully!
          
          $ emqx_ctl status
          Node 'emqx@127.0.0.1' is started
          emqx v4.0.0 is running
          ```
      
      -  systemctl start 
        
          ```
          $ sudo systemctl start emqx
          ```
      
      - service  start 
        
        ```
        $ sudo service emqx start
        ```

6.  Stop EMQ X Broker

    ```
    $ emqx stop
    ok
    ```

7.  Remove EMQ X Broker

    ```
    $ sudo yum remove emqx-ee
    ```

#### Ubuntu、Debian

1.   Install the required dependency 
  
      ```
      $ sudo apt update && sudo apt install -y \
          apt-transport-https \
          ca-certificates \
          curl \
          gnupg-agent \
          software-properties-common
      ```

2.   Add the GPG key for EMQ X 
  
      ```
      $ curl -fsSL https://repos.emqx.io/gpg.pub | sudo apt-key add -
      ```
      
      Validate key 
      
      ```
      $ sudo apt-key fingerprint 3E640D53
      
      pub   rsa2048 2019-04-10 [SC]
          FC84 1BA6 3775 5CA8 487B  1E3C C0B4 0946 3E64 0D53
      uid           [ unknown] emqx team <support@emqx.io>
      ```

3.  Use the following command to set up the stable repository. If unstable repo is also needed, add the word ‘unstable’ after ‘stable’ in the following command. 
    
    ```
    $ sudo add-apt-repository \
        "deb [arch=amd64] https://repos.emqx.io/emqx-ee/deb/ubuntu/ \
        ./$(lsb_release -cs) \
        stable"
    ```
    
    The lsb_release -cs subcommand returns the name of the Ubuntu distribution, such as xenial. Sometimes, in a distribution like Linux Mint, you might need to change $(lsb_release -cs) to the parent Ubuntu distribution. For example, if you are using Linux Mint Tessa, you can use bionic. EMQ X does not provide any guarantees for untested and unsupported Ubuntu distribution. 
    
4.  Update apt package index 
  
    ```
    $ sudo apt update
    ```

5.  Install the latest version of EMQ X 
  
    ```
    $ sudo apt install emqx-ee
    ```
    
     In the case where multiple EMQ X repositories are enabled, and the apt install and apt update commands is not specified with a version number, the latest version of EMQ X is installed. This could be a problem for users with stability needs. 
    
6.  Install a specific version of EMQ X 
  
    1.   Query available version 
      
          ```
          $ sudo apt-cache madison emqx-ee
          
          emqx-ee |      4.0.0 | https://repos.emqx.io/emqx-ee/deb/ubuntu bionic/stable amd64 Packages
          emqx-ee |      3.0.1 | https://repos.emqx.io/emqx-ee/deb/ubuntu bionic/stable amd64 Packages
          emqx-ee |      3.0.0 | https://repos.emqx.io/emqx-ee/deb/ubuntu bionic/stable amd64 Packages
          ```
    
    2.   Install a specific version using the version string from the second column, such as 4.0.0 
      
          ```
          $ sudo apt install emqx-ee=4.0.0
          ```

7.  Start EMQ X Broker
  
      - Directly start 
        
        ```
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
      
      - systemctl start 
        
        ```
        $ sudo systemctl start emqx
        ```
      
      - service start 
        
        ```
        $ sudo service emqx start
        ```

8.  Stop EMQ X Broker

    ```
    $ emqx stop
    ok
    ```

9.  Remove EMQ X Broker

    ```
    $ sudo apt remove emqx-ee
    ```

#### OpenSUSE

1.  Download the GPG public key and import it.
  
    ```
    $ curl -L -o /tmp/gpg.pub https://repos.emqx.io/gpg.pub
    $ sudo rpmkeys --import /tmp/gpg.pub
    ```

2.  Add repository address
  
    ```
    $ sudo zypper ar -f -c https://repos.emqx.io/emqx-ee/redhat/opensuse/leap/stable emqx
    ```

3.  Install the latest version of EMQ X Broker
  
    ```
    $ sudo zypper in emqx-ee
    ```

4.  Install a specific version of EMQ X 
  
    1.  Query available versions
      
        ```
        $ sudo zypper pa emqx-ee
        
        Loading repository data...
        Reading installed packages...
        S | Repository | Name | Version  | Arch
        --+------------+------+----------+-------
          | emqx       | emqx-ee | 4.0.0-1  | x86_64
          | emqx       | emqx-ee | 3.0.1-1  | x86_64
          | emqx       | emqx-ee | 3.0.0-1  | x86_64
        ```
    
    2.   Install a specific version, such as 4.0.0 
      
          ```
          $ sudo zypper in emqx-ee-4.0.0
          ```

5.  Start EMQ X Broker
  
      - Directly start 
        
        ```
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
      
      - systemctl start 
        
        ```
        $ sudo systemctl start emqx
        ```
      
      - service start 
        
        ```
        $ sudo service emqx start
        ```

6.  Stop EMQ X Broker

    ```
    $ emqx stop
    ok
    ```

7.  Remove EMQ X Broker

    ```
    $ sudo zypper rm emqx-ee
    ```

## Binary package installation (Linux)

1.  Use [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) or [Github](https://github.com/emqx/emqx/releases) for download EMQ X Broker packages。
  
2. Install EMQ X Broker:

    + RPM 包:

    	```shell
    	$ sudo rpm -ivh emqx-ee-cenots7-v4.0.0.x86_64.rpm
    	```
    	
    + DEB 包:

      ```
      $ sudo dpkg -i emqx-ee-ubuntu18.04-v4.0.0_amd64.deb
      ```

3. Start EMQ X Broker

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

4.  Stop EMQ X Broker

    ```
    $ emqx stop
    ok
    ```

5.  Uninstall EMQ X Broker

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


## ZIP (Linux、MaxOS、Windows)

1.  Download the zip package of the EMQ X Broker version to be installed from [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) or [Github](https://github.com/emqx/emqx/releases).
  
2.   Unzip the installation file: 
  
      ```shell
      $ unzip emqx-ee-ubuntu18.04-v4.0.0.zip
      ```

3.  Start EMQ X Broker
  
    ```
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

4.  Stop EMQ X Broker

    ```
    $ ./bin/emqx stop
    ok
    ```

5.  Remove EMQ X Broker

    Simply delete the EMQ X Broker directory

## Install EMQ X in Docker (Contain a simple docker-compose cluster)

#### Run a single container

1.  Get docker image
  
      - From [Docker Hub](https://hub.docker.com/r/emqx/emqx-ee)
        
        ```
        $ docker pull emqx/emqx-ee:v4.0.0
        ```
      
      - Download the Docker image from [emqx.io](https://www.emqx.io/downloads/enterprise?osType=Linux) and load it manually
        
        ```
        $ wget -O emqx-docker.zip https://www.emqx.io/downloads/enterprise/v4.0.0/emqx-docker-v4.0.0-alpine3.10-amd64.zip
        $ unzip emqx-docker.zip
        $ docker load < emqx-docker-v4.0.0
        ```
    
2.  Start docker container
  
    ```
    $ docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8883:8883 -p 8084:8084 -p 18083:18083 emqx/emqx-ee:v4.0.0
    ```

#### create a simple static cluster by docker-compose

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
       image: emqx/emqx-ee:v4.0.0
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

   ```
   $ docker-compose -p my_emqx up -d
   ```

3. View cluster

   ```
   $ docker exec -it my_emqx_emqx1_1 sh -c "emqx_ctl cluster status"
   Cluster status: #{running_nodes => ['emqx@node1.emqx.io','emqx@node2.emqx.io'],
                     stopped_nodes => []}
   ```

For more information about EMQ X Broker Docker, please visit [Docker Hub](https://hub.docker.com/r/emqx/emqx-ee)

## Install and cluster via Helm (K8S、K3S)

1. Add helm repository

  ```
  $ helm repo add emqx https://repos.emqx.io/charts
  $ helm repo update
  ```

2. Query EMQ X Broker

  ```
  helm search repo emqx
  NAME         CHART VERSION APP VERSION DESCRIPTION
  emqx/emqx    v4.0.0        v4.0.0      A Helm chart for EMQ X
  emqx/emqx-ee v4.0.0        v4.0.0      A Helm chart for EMQ X
  emqx/kuiper  0.1.1         0.1.1       A lightweight IoT edge analytic software
  ```

3. Start EMQ X Broker cluster

  ```
  $ helm install my-emqx emqx/emqx-ee
  ```

4.  View EMQ X Broker cluster situation

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
