# 常见安装部署问题解答

## 推荐在哪些操作系统上部署 EMQX？

EMQX 支持在多种操作系统和硬件平台上运行，考虑到企业级部署的稳定性和可靠性，一般来说，我们推荐在 CentOS、Ubuntu、Debian 等 Linux 发行版上部署。

## EMQX 支持 Windows 操作系统吗？

不支持。Windows 下建议使用 Docker 来部署 EMQX。

## EMQX 推荐的部署方案是什么？

我们推荐以集群方式部署 EMQX，并在集群前端部署负载均衡（Nginx、HAProxy 等）使连接均衡地落到集群的每个节点上。

对于通信安全有较高要求的用户，我们建议为客户端启用 TLS 连接，并在 LB 侧终结 TLS 连接，即客户端与 LB 之间采用 TLS 加密通信，LB 与 EMQX 节点之间则仍然采用 TCP 通信。

由于 EMQX 节点并不对公网暴露端口，因此并不会降低整体的安全性，但通过 TLS 卸载，可以有效节省 EMQX 的资源消耗。

## 我的设备数量并不多，消息吞吐也不高，还需要部署集群吗？

即使设备数量不多、消息吞吐也不高，在生产环境下部署集群仍然是很有意义的。集群能提高系统的可用性，降低单点故障的可能性。即便有节点宕机，集群内其他健康节点也可以继续提供服务，保证业务不受影响。

## 部署 EMQX 开源版 Core + Repl 集群失败

从 EMQX v5.8 开始，仅 EMQX 企业版支持 Core + Replicant 节点集群，因此在使用 [EMQX Operator](https://github.com/emqx/emqx-operator) 部署 EMQX 开源版 Core + Repl 集群时，可能会遇到 EMQX 自定义资源一直处于 `replicantNodesProgressing` 状态，且 EMQX Repl Pod 反复崩溃（Crash）的情况。

建议部署纯 Core 节点的开源版集群，或者部署 Core + Repl 的企业版集群。

## EMQX 启动失败时该如何排查？

EMQX 启动失败时，可以查看 [日志目录](../deploy/install.md#文件与目录) 下的 `emqx.log.N` 或 `erlang.log.N` 获取详细的错误提示。或者执行 `emqx console` 从控制台启动 EMQX，这样错误日志将直接输出到控制台。然后根据提日志内容，在本页面中查找对应的解决办法，或在我们的 askemq 论坛发帖寻求支持。

## EMQX 启动失败，日志提示 “logger: command not found”

安装以下依赖即可：

- `CentOS/Redhat`

  ```
  $ yum install rsyslog
  ```

- `Ubuntu/Debian`

  ```
  $ apt-get install bsdutils
  ```

## EMQX 启动失败，日志提示 “...{on_load_function_failed,crypto}...”

该日志表示 EMQX 依赖的 Erlang/OTP 中的 `crypto` 应用启动失败，这通常是当前系统安装的 OpenSSL 版本与 EMQX 依赖的 OpenSSL 版本不同，或者当前系统并未安装 OpenSSL 导致的。

::: warning 重要提示

以下解决方案仅供参考示例。

所列源版本是根据当前已知信息选择的，但可能已过时或存在漏洞。

建议直接从操作系统的包管理器中安装 `libcrypto` 以获取最新的安全更新。

:::

:::: tabs

::: tab Linux

进入到 EMQX 的安装目录（如果使用包管理工具安装 EMQX，则应该进入与 EMQX 的 `lib` 目录同级的位置）：

```bash
## 安装包安装
$ cd emqx

## 包管理器安装，例如 yum。则它的 lib 目录应该在 /lib/emqx
$ cd /lib/emqx
```

查询 `crypto` 依赖的 `.so` 动态库列表及其在内存中的地址：

```bash
$ ldd lib/crypto-*/priv/lib/crypto.so

lib/crypto-4.6/priv/lib/crypto.so: /lib64/libcrypto.so.10: version `OPENSSL_1.1.1' not found (required by lib/crypto-4.6/priv/lib/crypto.so)
        linux-vdso.so.1 =>  (0x00007fff67bfc000)
        libcrypto.so.10 => /lib64/libcrypto.so.10 (0x00007fee749ca000)
        libc.so.6 => /lib64/libc.so.6 (0x00007fee74609000)
        libdl.so.2 => /lib64/libdl.so.2 (0x00007fee74404000)
        libz.so.1 => /lib64/libz.so.1 (0x00007fee741ee000)
        /lib64/ld-linux-x86-64.so.2 (0x00007fee74fe5000)
```

其中 `OPENSSL_1.1.1' not found` 表明相应版本的 OpenSSL `.so` 库未正确安装。

源码编译安装 OPENSSL 1.1.1，并将其 `.so` 文件放置到可以被系统识别的路径：

```bash
## 下载最新版本 1.1.1
$ wget https://www.openssl.org/source/openssl-1.1.1c.tar.gz

## 上传至 ct-test-ha
$ scp openssl-1.1.1c.tar.gz ct-test-ha:~/

## 解压并编译安装
$ tar zxf openssl-1.1.1c.tar.gz
$ cd openssl-1.1.1c
$ ./config
$ make test  # 执行测试；如果输出 PASS 则继续
$ make install

## 确保库的引用
$ ln -s /usr/local/lib64/libssl.so.1.1 /usr/lib64/libssl.so.1.1
$ ln -s /usr/local/lib64/libcrypto.so.1.1 /usr/lib64/libcrypto.so.1.1
```

完成后，在 EMQX 的 lib 同级目录下执行 `ldd lib/crypto-*/priv/lib/crypto.so` ，检查是否已能正确识别。如果不再有 `not found` 的 `.so` 库，即可正常启动 EMQX。

:::

::: tab macOS

进入到 EMQX 的安装目录：

```bash
## 安装包安装
$ cd emqx

## brew 安装
$ cd /usr/local/Cellar/emqx/<version>/
```

查询 `crypto` 依赖的 `.so` 动态库列表：

```bash
$ otool -L lib/crypto-*/priv/lib/crypto.so

lib/crypto-4.4.2.1/priv/lib/crypto.so:
	/usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib (compatibility version 1.1.0, current version 1.1.0)
	/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1252.200.5)
```

检查列出的动态库是否存在：

```bash
$ ls /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib
ls: /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib: No such file or directory
```

若不存在该动态库，则需安装与 `otool` 打印出来的对应的 OpenSSL 版本，例如此处显示的为 `openssl@1.1`：

```bash
brew install openssl@1.1
```

安装完成后，即可正常启动 EMQX。

:::

::::

## EMQX 启动失败，日志提示 “libatomic.so.1: cannot open shared object file: No such file or directory”

这个错误的原因是当前系统缺少了 libatomic 这个依赖项，解决办法是安装这个依赖项：

```
# Rocky Linux, CentOS, ...
yum install -y libatomic
# Debian, Ubuntu, ...
apt install -y libatomic
```

如果你手动安装 RPM 或 DEB 包，可能会在安装时就遇到以下提示：

```
$ rpm -ivh emqx-5.7.0-el8-amd64.rpm
error: Failed dependencies:
libatomic is needed by emqx-5.7.0-el8-amd64.rpm
```

解决办法仍然是先手动安装 libatomic 这个依赖项。

当然，我们最推荐的安装方式是使用包管理器（yum、apt 等）安装，它们会自动安装所需依赖，不用我们额外操心。

## 使用 Docker 启动 EMQX 失败，日志提示 “Permission denied”

当你打算以目录挂载的方式持久化 EMQX 数据时：

```
sudo docker run -d --name emqx -p 18083:18083 -p 1883:1883 -v /emqx/data:/opt/emqx/data -v /emqx/log:/opt/emqx/log emqx:latest
```

你可能会遇到容器启动失败，并提示以下错误：

```
mkdir: cannot create directory '/opt/emqx/data/configs': Permission denied
```

这是因为 EMQX 在容器中以 Linux 用户 `emqx` 运行，而你宿主机中的目录却可能是在 `root` 用户下创建的，因此 EMQX 自然无法在这些目录下创建目录或文件。

想要解决这个问题，你可以在宿主机中创建一个 `emqx` 用户，然后由该用户来创建需要挂载的目录，或者直接将已经创建好的 data、log 目录的权限修改为 777。

当然，最推荐的方式还是以命名数据卷的方式实现 EMQX 数据持久化，这样就不必再关心权限问题：

```
sudo docker volume create --name emqx-data
sudo docker volume create --name emqx-log
sudo docker run -d --name emqx -p 18083:18083 -p 1883:1883 -v emqx-data:/opt/emqx/data -v emqx-log:/opt/emqx/log emqx:latest
```

## EMQX 启动时日志提示端口被占用（eaddrinuse）应该怎么办？

默认情况下，EMQX 启动时会占用 7 个端口，它们分别是：

1. 1883，用于 MQTT over TCP 监听器，可通过配置修改。
2. 8883，用于 MQTT over SSL/TLS 监听器，可通过配置修改。
3. 8083，用于 MQTT over WebSocket 监听器，可通过配置修改。
4. 8084，用于 MQTT over WSS (WebSocket over SSL) 监听器，可通过配置修改。
5. 18083，HTTP API 服务的默认监听端口，Dashboard 功能也依赖于这个端口，可通过配置修改。
6. 4370，用于 EMQX 分布式集群远程函数调用、Mnesia 数据同步等。即便没有组成集群，这个端口也会被默认占用。这个监听端口实际上应该是 `BasePort (4370) + Offset`，4370 固定无法修改，Offset 则由节点名称（`Name@Host`）中 Name 部分的数字后缀决定，没有数字后缀则默认为 0。例如 `emqx@127.0.0.1` 的 Offset 为 0，`emqx1@127.0.0.1` 的 Offset 为 1。
7. 5370，用于分担上一端口压力的集群 RPC 端口，主要用于节点间转发 MQTT 消息。与 4370 端口类似，即便没有组成集群，这个端口也会被默认占用，并且它实际上应该是 `BasePort (5370) + Offset`，5370 固定无法修改，Offset 则由节点名称（`Name@Host`）中 Name 部分的数字后缀决定，没有数字后缀则默认为 0。

## EMQX 启动时日志提示 “WARNING: Default (insecure) Erlang cookie is in use.” 应该怎么办？

完整的 WARNING 日志如下：

```
WARNING: Default (insecure) Erlang cookie is in use.
WARNING: Configure node.cookie in /usr/lib/emqx/etc/emqx.conf or override from environment variable EMQX_NODE__COOKIE
WARNING: NOTE: Use the same cookie for all nodes in the cluster.
```

只有使用相同 Cookie 的 EMQX 节点才能组成一个集群。虽然 Cookie 并不能保证集群的通信安全，但它可以避免节点连接到它不打算与之通信的集群。EMQX 节点默认统一将 `emqxsecretcookie` 作为 Cookie，所以我们会推荐用户在搭建集群时更改 Cookie 的值。

第二条 WARNING 日志则提示了修改 Cookie 的两种方式，分别为 `emqx.conf` 配置文件中的 `node.cookie`，和环境变量 `EMQX_NODE__COOKIE`。

## 使用 Docker 部署 EMQX 时，为什么容器重启会丢失已经配置的规则、资源等数据？

EMQX 的运行时数据，譬如规则和资源的配置、保留消息等等，它们都存储在 `/opt/emqx/data` 目录下，所以为了保证这部分数据不会因容器重启而丢失，我们需要将 `/opt/emqx/data` 目录挂载到本地主机目录或者数据卷中去。

但我们可能会发现，即便已经挂载了 `/opt/emqx/data` 目录，数据仍然可能会在容器重启后丢失。这是因为 EMQX 的运行时数据实际上存储在 `/opt/emqx/data/mnesia/${Node Name}` 目录。所以数据丢失，实际上是容器重启后 EMQX 的节点名发生了变化，进而导致 EMQX 创建了一个全新的存储目录。

EMQX 节点名由 Name 和 Host 两部分组成，其中 Host 默认来自容器的 IP 地址。在默认的网络配置下容器一旦重启它的 IP 就可能发生变化，所以我们要做的就是让容器的 IP 保持固定。

EMQX 提供了一个环境变量 `EMQX_HOST`，允许我们自行设置节点名的 Host 部分。当然前提是这个 Host 必须是其他节点可以连接的，所以我们还需要配合网络别名使用：

```
docker run -d --name emqx -p 18083:18083 -p 1883:1883 -e EMQX_HOST=alias-for-emqx --network example --network-alias alias-for-emqx --mount type=bind,source=/tmp/emqx,target=/opt/emqx/data emqx:5.8.3
```
## 为什么使用 docker-compose 正常启动容器，且打开 Dashboard 管理界面也正常，但是显示 unhealthy 状态？

```bash
docker-compose ps
NAME      IMAGE                         COMMAND                  SERVICE   CREATED          STATUS                    PORTS
emqx1     emqx/emqx:latest   "/usr/bin/docker-ent…"   emqx     120 seconds ago   Up 110 seconds (unhealthy)   0.0.0.0:1883->1883/tcp, :::1883->1883/tcp, 0.0.0.0:18083->18083/tcp, :::18083->18083/tcp
```

EMQX 的健康检查依赖于 `./bin/emqx_ctl status` 命令，如果这个命令执行失败，则容器会显示为 unhealthy 状态。
```yaml
healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
      interval: 60s
      timeout: 15s
      retries: 3
```
手动执行 `./bin/emqx_ctl status` 命令:
```
emqx@docker:/opt/emqx$ emqx_ctl status
Node emqx@docker not responding to pings.
```
表明命令连不上节点，这是由于启动容器时 network 没有使用 alias，且不是 FQDN 格式，无法找到节点。

解决办法：
1. 修改 Docker 的 hostname 和 emqx 的节点名一致。
2. 修改 `docker-compose.yml` 文件，添加 hostname 配置。
```yaml
# xxx.yyy.zzz(docker.emqx.com) 符合 FQDN 格式。
hostname: docker.emqx.com
 environment:
      - EMQX_HOST=docker.emqx.com
```

由于 EMQX 使用 `data/mnesia/<节点名>` 作为数据存储目录，使用 hostname 或者 FQDN 等固定的信息作为节点名（不推荐使用 IP），还可以避免因为节点名称变动导致数据丢失。 

推荐使用 [EMQX Docker Compose 一键生成器](https://docker.emqx.dev/) 一键生成生产就绪的 `docker-compose.yml` 文件。
