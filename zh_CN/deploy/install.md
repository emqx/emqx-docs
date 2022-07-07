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
EMQX 默认以节点名称 `emqx@127.0.0.1` 启动，该名称是内置数据库的一部分。
如需更改名称，请先清除数据库目录 `/var/lib/emqx/data/mnesia`，然后再更改为其他名称启动。
:::

{% emqxce %}

1. 访问 [emqx.io](https://www.emqx.io/zh/downloads) 或 [Github](https://github.com/emqx/emqx/releases) 下载 EMQX 的二进制包。

{% endemqxce %}

{% emqxee %}

1. 访问 [emqx.com](https://www.emqx.com/zh/try?product=enterprise) 下载 EMQX 的二进制包。

{% endemqxee %}

2. 安装 EMQX Broker，将下面的路径更改为您下载 EMQX 软件包的路径。

    + RPM 包 (RedHat/CentOS/RockyLinux/AmazonLinux):

      ```shell
      sudo yum install emqx-full-package-name.rpm
      ```

    + DEB 包 (Ubuntu/Debian/Raspbian):

      ```shell
      sudo apt install ./emqx-full-package-name.deb
      ```

3. 启动 EMQX Broker

      - 直接启动

        ```shell
        sudo su - emqx
        emqx start
        emqx_ctl status
        ```

      - systemctl 启动

        ```shell
        sudo systemctl start emqx
        ```

4.  停止 EMQX Broker

    ```shell
    $ emqx stop
    ok
    ```

5.  卸载 EMQX Broker

    + DEB 包:

      ```shell
      sudo apt remove --purge emqx
      ```

    + RPM 包:

      ```shell
      sudo yum remove emqx
      ```

## tgz 压缩包安装（Linux、macOS、Windows）

::: warning
tar.gz 包适用于测试和热更，如果不知道如何手动安装所有可能的运行时依赖，请勿在生产环境中使用。
:::

::: warning
macOS 上 EMQX 数字签名工作还未完成，使用 tgz 压缩包安装启动时可能会被 [Gatekeeper 保护机制](https://support.apple.com/zh-cn/guide/security/sec5599b66df/web) 拦截。

当你在启动时遇到 「**无法打开“erl”，因为无法验证开发者**」  或 「**无法打开“erlang_jq_port”，因为无法验证开发者**」 类似的弹框提示时，请按以下方式处理：

- 方法一：到 macOS **安全性与隐私** 设置中勾选允许从 **任何来源** 下载的 App，重新启动 EMQX
- 方法二：手动移除 tgz 压缩包或文件夹 Gatekeeper 相关 `com.apple.quarantine` 附加属性：

  ```shell
  xattr -d com.apple.quarantine emqx-full-package-name.tar.gz
  # or
  xattr -r -d com.apple.quarantine ./emqx
  ```

使用此操作时请校验文件 SHA256 确保安装包完整性，以免带来额外的安全风险。
:::

{% emqxce %}

1. 访问 [emqx.io](https://www.emqx.io/zh/downloads) 或 [Github](https://github.com/emqx/emqx/releases) 下载要安装的 EMQX 的 tar.gz 包。

{% endemqxce %}

{% emqxee %}

1. 访问 [emqx.com](https://www.emqx.com/zh/try?product=enterprise) 或 [Github](https://github.com/emqx/emqx/releases) 下载要安装的 EMQX 版本的 tar.gz 包。
{% endemqxee %}

2. 解压程序包

    ```shell
    tar -zxf emqx-full-package-name.tar.gz
    ```

3. 启动 EMQX Broker

    ```shell
    cd ./emqx
    ./bin/emqx start
    ./bin/emqx_ctl status
    ```

4. 停止 EMQX Broker

    ```shell
    ./bin/emqx stop
    ```

5. 卸载 EMQX Broker

    直接删除 EMQX 目录即可

## 通过 Docker 运行 (包含简单的 docker-compose 集群)

### 运行单个容器

1.  获取 docker 镜像

    ```shell
    docker pull emqx/emqx:5.0.0
    ```

2.  启动 docker 容器

    ```shell
    docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8883:8883 -p 8084:8084 -p 18083:18083 emqx/emqx:5.0.0
    ```

### 使用 docker-compose 创建简单的 static 集群

1. 创建 `docker-compose.yaml` 文件

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

   ```shell
   docker-compose -p my_emqx up -d
   ```

3. 查看集群

   ```shell
   $ docker exec -it my_emqx_emqx1_1 sh -c "emqx_ctl cluster status"
   Cluster status: #{running_nodes => ['emqx@node1.emqx.io','emqx@node2.emqx.io'],
                     stopped_nodes => []}
   ```

更多关于 EMQX Docker 的信息请查看 [Docker Hub](https://hub.docker.com/_/emqx) 或 [Github](https://github.com/emqx/emqx-rel/tree/master/deploy/docker)

## 通过 Helm 安装并集群 (K8S、K3S)

1. 添加 helm 仓库

  ```shell
  helm repo add emqx https://repos.emqx.io/charts
  helm repo update
  ```

2. 查询 EMQX Broker

  ```shell
  $ helm search repo emqx
  NAME         CHART VERSION APP VERSION DESCRIPTION
  emqx/emqx    v4.0.0        v4.0.0      A Helm chart for EMQX
  emqx/emqx-ee v4.0.0        v4.0.0      A Helm chart for EMQX
  emqx/kuiper  0.1.1         0.1.1       A lightweight IoT edge analytic software
  ```

3. 启动 EMQX 集群

  ```shell
  helm install my-emqx emqx/emqx
  ```

4. 查看 EMQX 集群情况

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

## 源码编译安装

1. 获取源码

```shell
git clone https://github.com/emqx/emqx.git
```

2. 切换到最近的 Tag

```shell
cd emqx
git checkout $(git describe --abbrev=0 --tags)
```

3. 编译并运行

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
