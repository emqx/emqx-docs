---
# 标题
title: 安装
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

# 安装

EMQ X 目前支持的操作系统:

+ Centos6
+ Centos7
+ OpenSUSE tumbleweed
+ Debian 8
+ Debian 9
+ Debian 10
+ Ubuntu 14.04
+ Ubuntu 16.04
+ Ubuntu 18.04
+ macOS 10.13
+ macOS 10.14
+ macOS 10.15
+ Windows Server 2019

{% hint style="danger" %}
产品部署建议 Linux 服务器，不推荐 Windows 服务器。
{% endhint %}

{% tabs shell="Shell 脚本安装", packages="包管理器安装", binary="二进制包安装", zip="ZIP 压缩包安装", homebrew="Homebrew 安装", docker="Docker 运行", helm="Helm 安装并集群", build="源码编译安装" %}

{% content "shell" %}
## Shell 脚本一键安装 (Linux)

```bash
curl https://repos.emqx.io/install_emqx.sh | bash
```

{% content "packages" %}
## 包管理器安装 (Linux)

#### Centos

1.  安装所需要的依赖包
  
    ```
    $ sudo yum install -y yum-utils device-mapper-persistent-data lvm2
    ```

2.  使用以下命令设置稳定存储库，以 CentOS7
    为例
    
    ```
    $ sudo yum-config-manager --add-repo https://repos.emqx.io/emqx-ce/redhat/centos/7/emqx-ce.repo
    ```

3.  安装最新版本的 EMQ X Broker
  
    ```
    $ sudo yum install emqx
    ```
    
    如果提示接受 GPG 密钥，请确认密钥符合 fc84 1ba6 3775 5ca8 487b 1e3c c0b4 0946 3e64 0d53，并接受该指纹。
    
4.  安装特定版本的 EMQ X Broker

    1.  查询可用版本
      
        ```
        $ yum list emqx --showduplicates | sort -r
        
        emqx.x86_64                     4.0.0-1.el7                        emqx-stable
        emqx.x86_64                     3.0.1-1.el7                        emqx-stable
        emqx.x86_64                     3.0.0-1.el7                        emqx-stable
        ```

    2.  根据第二列中的版本字符串安装特定版本，例如 4.0.0
      
        ```
        $ sudo yum install emqx-4.0.0
        ```

5.  启动 EMQ X Broker
  
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

6.  停止 EMQ X Broker

    ```
    $ emqx stop
    ok
    ```

7.  卸载 EMQ X Broker

    ```
    $ sudo yum remove emqx
    ```

#### Ubuntu、Debian

1.  安装所需要的依赖包
  
    ```
    $ sudo apt update && sudo apt install -y \
        apt-transport-https \
        ca-certificates \
        curl \
        gnupg-agent \
        software-properties-common
    ```

2.  添加 EMQ X 的官方 GPG 密钥
  
    ```
    $ curl -fsSL https://repos.emqx.io/gpg.pub | sudo apt-key add -
    ```
    
    验证密钥
    
    ```
    $ sudo apt-key fingerprint 3E640D53
    
    pub   rsa2048 2019-04-10 [SC]
        FC84 1BA6 3775 5CA8 487B  1E3C C0B4 0946 3E64 0D53
    uid           [ unknown] emqx team <support@emqx.io>
    ```

3.  使用以下命令设置 stable 存储库。 如果要添加 unstable 存储库，请在以下命令中的单词 stable 之后添加单词
    unstable。
    
    ```
    $ sudo add-apt-repository \
        "deb [arch=amd64] https://repos.emqx.io/emqx-ce/deb/ubuntu/ \
        ./$(lsb_release -cs) \
        stable"
    ```
    
    lsb\_release -cs 子命令返回发行版的名称，例如 xenial。 有时，在像 Linux Mint
    这样的发行版中，您可能需要将 $(lsb\_release -cs) 更改为您的父发行版。
    例如，如果您使用的是 Linux Mint Tessa，则可以使用 bionic。 EMQ X Broker
    不对未经测试和不受支持的发行版提供任何保证。
    
4.  更新 apt 包索引
  
    ```
    $ sudo apt update
    ```

5.  安装最新版本的 EMQ X Broker
  
    ```
    $ sudo apt install emqx
    ```
    
    在启用了多个 EMQ X 仓库的情况下，如果 apt install 和 apt update
    命令没有指定版本号，那么会自动安装最新版的 EMQ
    X Broker。这对于有稳定性需求的用户来说是一个问题。
    
6.  安装特定版本的 EMQ X Broker
  
    1.  查询可用版本
      
        ```
        $ sudo apt-cache madison emqx
        
        emqx |      4.0.0 | https://repos.emqx.io/emqx-ce/deb/ubuntu bionic/stable amd64 Packages
        emqx |      3.0.1 | https://repos.emqx.io/emqx-ce/deb/ubuntu bionic/stable amd64 Packages
        emqx |      3.0.0 | https://repos.emqx.io/emqx-ce/deb/ubuntu bionic/stable amd64 Packages
        ```
    
    2.  使用第二列中的版本字符串安装特定版本，例如 4.0.0
      
        ```
        $ sudo apt install emqx=4.0.0
        ```

7.  启动 EMQ X Broker
  
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

8.  停止 EMQ X Broker

    ```
    $ emqx stop
    ok
    ```

9.  卸载 EMQ X Broker

    ```
    $ sudo apt remove emqx
    ```

#### OpenSUSE

1.  下载 GPG 公钥并导入。
  
    ```
    $ curl -L -o /tmp/gpg.pub https://repos.emqx.io/gpg.pub
    $ sudo rpmkeys --import /tmp/gpg.pub
    ```

2.  添加储存库地址
  
    ```
    $ sudo zypper ar -f -c https://repos.emqx.io/emqx-ce/redhat/opensuse/leap/stable emqx
    ```

3.  安装最新版本的 EMQ X Broker
  
    ```
    $ sudo zypper in emqx
    ```

4.  安装特定版本的 EMQ X Broker
  
    1.  查询可用版本
      
        ```
        $ sudo zypper pa emqx
        
        Loading repository data...
        Reading installed packages...
        S | Repository | Name | Version  | Arch
        --+------------+------+----------+-------
          | emqx       | emqx | 4.0.0-1  | x86_64
          | emqx       | emqx | 3.0.1-1  | x86_64
          | emqx       | emqx | 3.0.0-1  | x86_64
        ```
    
    2.  使用 Version 安装特定版本，例如 4.0.0
      
        ```
        $ sudo zypper in emqx-4.0.0
        ```

5.  启动 EMQ X Broker
  
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

6.  停止 EMQ X Broker

    ```
    $ emqx stop
    ok
    ```

7.  卸载 EMQ X Broker

    ```
    $ sudo zypper rm emqx
    ```

{% content "binary" %}
## 二进制包安装 (Linux)

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或 [Github](https://github.com/emqx/emqx/releases) 选择系统发行版，然后下载要安装的 EMQ X 版本的二进制包。
  
2. 安装 EMQ X Broker，将下面的路径更改为您下载 EMQ X 软件包的路径。

    + RPM 包:

    	```shell
    	$ sudo rpm -ivh emqx-cenots7-v4.0.0.x86_64.rpm
    	```
    	
    + DEB 包:

      ```
      $ sudo dpkg -i emqx-ubuntu18.04-v4.0.0_amd64.deb
      ```

3. 启动 EMQ X Broker

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

4.  停止 EMQ X Broker

    ```
    $ emqx stop
    ok
    ```

5.  卸载 EMQ X Broker

    + DEB 包:

      ```
      $ dpkg -r emqx
      ```

      或

      ```
      $ dpkg -P emqx
      ```

    + RPM 包:

      ```
      $ rpm -e emqx
      ```

{% content "zip" %}
## ZIP 压缩包安装 (Linux、MaxOS、Windows)

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或 [Github](https://github.com/emqx/emqx/releases) 下载要安装的 EMQ X 版本的 zip 包。
  
2.  解压程序包
  
    ```shell
    $ unzip emqx-ubuntu18.04-v4.0.0.zip
    ```

3.  启动 EMQ X Broker
  
    ```
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

4.  停止 EMQ X Broker

    ```
    $ ./bin/emqx stop
    ok
    ```

5.  卸载 EMQ X Broker

    直接删除 EMQ X 目录即可

{% content "homebrew" %}
## 通过 Homebrew 安装 (MacOS)

1.  添加 EMQ X 的 tap
  
    ```
    $ brew tap emqx/emqx
    ```

2.  安装 EMQ X Broker
  
    ```
    $ brew install emqx
    ```

3.  启动 EMQ X Broker
  
    ```
    $ emqx start
    emqx 4.0.0 is started successfully!
    
    $ emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

4.  停止 EMQ X Broker

    ```
    $ emqx stop
    ok
    ```

5.  卸载 EMQ X Broker

    ```
    $ brew uninstall emqx
    ```

{% content "docker" %}
## 通过 Docker 运行 (包含简单的 docker-compose 集群)

#### 运行单个容器

1.  获取 docker 镜像
  
      - 通过 [Docker Hub](https://hub.docker.com/r/emqx/emqx) 获取
        
        ```
        $ docker pull emqx/emqx:v4.0.0
        ```
      
      - 通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或 [Github](https://github.com/emqx/emqx/releases) 下载 Docker 镜像，并手动加载
        
        ```
        $ wget -O emqx-docker.zip https://www.emqx.io/downloads/broker/v4.0.0/emqx-docker-v4.0.0-alpine3.10-amd64.zip
        $ unzip emqx-docker.zip
        $ docker load < emqx-docker-v4.0.0
        ```
      
2.  启动 docker 容器
  
    ```
    $ docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8883:8883 -p 8084:8084 -p 18083:18083 emqx/emqx:v4.0.0
    ```

#### 使用 docker-compose 创建简单的 static 集群

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
       image: ${IMAGE}
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

更多关于 EMQ X Docker 的信息请查看 [Docker Hub](https://hub.docker.com/r/emqx/emqx) 或 [Github](https://github.com/emqx/emqx-rel/tree/master/deploy/docker)

{% content "helm" %}
## 通过 Helkm 安装并集群 (K8S、K3S)

1. 添加 helm 仓库

  ```
  $ helm repo add emqx https://repos.emqx.io/charts
  $ helm repo update
  ```

2. 查询 EMQ X Broker

  ```
  helm search repo emqx
  NAME         CHART VERSION APP VERSION DESCRIPTION
  emqx/emqx    v4.0.0        v4.0.0      A Helm chart for EMQ X
  emqx/emqx-ee v4.0.0        v4.0.0      A Helm chart for EMQ X
  emqx/kuiper  0.1.1         0.1.1       A lightweight IoT edge analytic software
  ```

3. 启动 EMQ X 集群

  ```
  $ helm install my-emqx emqx/emqx
  ```

4.  查看 EMQ X 集群情况

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

{% content "build" %}

## 源码编译安装

1. 获取源码

```bash
$ git clone -b v4.0.0 https://github.com/emqx/emqx-rel.git
```

2. 设置环境变量

```bash
$ export EMQX_DEPS_DEFAULT_VSN=v4.0.0
```

3. 编译

```bash
$ cd emqx-rel && make
```

4. 启动 EMQ X Broker
  
```bash
$ cd _build/emqx-rel/_rel/emqx

$ ./bin/emqx start
emqx 4.0.0 is started successfully!

$ ./bin/emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx v4.0.0 is running
```

{% endtabs %}
