# 使用 eMQTT-Bench 进行性能测试

当你部署了单个 EMQX 服务器或者建立了 EMQX 集群，你可以进行对你的部署进行性能测试以了解系统的能力。这个章节介绍了如何安装和使用 [eMQTT-Bench](https://www.emqx.com/zh/try?product=emqtt-bench) 来进行性能测试。eMQTT-Bench是基于 Erlang 编写的，一个简洁强大的 MQTT 协议基准测试工具。如需大规模场景、深度定制化的测试服务推荐使用 [XMeter](https://www.emqx.com/zh/products/xmeter) 进行测试。

## 安装eMQTT-Bench

你可以通过 3 种方式来安装 eMQTT-Bench：

- 运行 Docker 镜像
- 下载并安装二进制包
- 从源代码构建

### 运行 Docker 镜像

你可以通过运行`emqtt-bench`镜像来安装测试工具。`emqtt-bench` docker 镜像已推送到 [hub.docker.com](https://hub.docker.com/r/emqx/emqtt-bench/tags), 且每个新版本都会更新`:latest`标签：

```bash
docker run -it emqx/emqtt-bench:latest
Usage: emqtt_bench pub | sub | conn [--help]
```

注意，Docker 镜像名称使用连字符“-”，而二进制脚本名称使用下划线“_”。

### 用二进制包安装

你可以下载`emqtt-bench`的二进制包并在以下平台上安装测试工具：

- Amazon Linux 2
- Amazon Linux 2023
- CentOS 7
- Rocky Linux 8
- Rocky Linux 9
- Debian 9
- Debian 10
- Debian 11
- Debian 12
- Ubuntu 16.04
- Ubuntu 18.04
- Ubuntu 20.04
- Ubuntu 22.04
- MacOS 11 (Intel)
- MacOS 12 (Intel)
- MacOS 12 (Apple Silicon)

前往 [Releases](https://github.com/emqx/emqtt-bench/releases) 页面查看具体的`emqtt-bench`发布版本信息。

例如，以下是如何在 Ubuntu 20.04 上安装 `emqtt-bench`:

```bash
mkdir emqtt-bench && cd emqtt-bench
wget https://github.com/emqx/emqtt-bench/releases/download/0.4.12/emqtt-bench-0.4.12-ubuntu20.04-amd64.tar.gz
tar xfz emqtt-bench-0.4.12-ubuntu20.04-amd64.tar.gz
rm emqtt-bench-0.4.12-ubuntu20.04-amd64.tar.gz

./emqtt_bench
Usage: emqtt_bench pub | sub | conn [--help]
```

### 源代码构建

`emqtt-bench` 的运行依赖于 Erlang/OTP 21.2 以上版本运行环境，安装过程略过，详情请参考网上各个安装教程。

Erlang 环境安装完成后，下载 `emqtt-bench` 最新代码，并编译：

```bash
git clone https://github.com/emqx/emqtt-bench
cd emqtt-bench

make
```

编译完成后，当前目录下会生成 一个名为`emqtt_bench` 的可执行脚本。执行以下命令，确认其能正常使用：

```bash
./emqtt_bench
Usage: emqtt_bench pub | sub | conn [--help]
```

输出以上内容，则证明 `emqtt-bench` 已正确安装到主机。

## 用 eMQTT-Bench 进行性能测试

`emqtt_bench` 共三个子命令：

1. `pub`：用于创建大量客户端执行发布消息的操作。
2. `sub`：用于创建大量客户端执行订阅主题，并接受消息的操作。
3. `conn`：用于创建大量的连接。

### 发布

执行 `./emqtt_bench pub --help` 会得到可用的参数输出，此处整理：

| 参数              | 简写 | 可选值          | 默认值     | 说明                                                                                                     |
| ----------------- | ---- | --------------- | ---------- | -------------------------------------------------------------------------------------------------------- |
| --host            | -h   | -               | localhost  | 要连接的 MQTT 服务器地址                                                                                 |
| --port            | -p   | -               | 1883       | MQTT 服务端口                                                                                            |
| --version         | -V   | 3<br/>4<br/>5   | 5          | 使用的 MQTT 协议版本                                                                                     |
| --count           | -c   | -               | 200        | 客户端总数                                                                                               |
| --startnumber     | -n   | -               | 0          | 客户端数量起始值                                                                                         |
| --interval        | -i   | -               | 10         | 每间隔多少时间创建一个客户端；单位：毫秒                                                                 |
| --interval_of_msg | -I   | -               | 1000       | 每间隔多少时间发送一个消息                                                                               |
| --username        | -u   | -               | 无；非必选 | 客户端用户名                                                                                             |
| --password        | -P   | -               | 无；非必选 | 客户端密码                                                                                               |
| --topic           | -t   | -               | 无；必选   | 发布的主题；支持站位符：<br />`%c`：表示 ClientId<br />`%u`：表示 Username<br />`%i`：表示客户端的序列数 |
| --size            | -s   | -               | 256        | 消息 Payload 的大小；单位：字节                                                                          |
| --qos             | -q   | -               | 0          | QoS 等级                                                                                                 |
| --retain          | -r   | true<br />false | false      | 消息是否设置 Retain 标志                                                                                 |
| --keepalive       | -k   | -               | 300        | 客户端心跳时间                                                                                           |
| --clean           | -C   | true<br />false | true       | 是否以清除会话的方式建立连接                                                                             |
| --ssl             | -S   | true<br />false | false      | 是否启用 SSL                                                                                             |
| --certfile        | -    | -               | 无         | 客户端 SSL 证书                                                                                          |
| --keyfile         | -    | -               | 无         | 客户端 SSL 秘钥文件                                                                                      |
| --ws              | -    | true<br />false | false      | 是否以 Websocket 的方式建立连接                                                                          |
| --ifaddr          | -    | -               | 无         | 指定客户端连接使用的本地网卡                                                                             |

例如，启动 10 个连接，分别每秒向主题 `t` 发送 100 条 Qos0 消息，其中每个消息体的大小为 `16` 字节大小：

```bash
./emqtt_bench pub -t t -h emqx-server -s 16 -q 0 -c 10 -I 10
```

### 订阅

执行 `./emqtt_bench sub --help`可得到该子命令的所有的可用参数。它们的解释已包含在上表中，此处略过。

例如，启动 500 个连接，每个都以 Qos0 订阅 `t` 主题：

```bash
./emqtt_bench sub -t t -h emqx-server -c 500
```

### 连接

执行 `./emqtt_bench conn --help` 可得到该子命令所有可用的参数。它们的解释已包含在上表中，此处略过。

例如，启动 1000 个连接：

```bash
./emqtt_bench conn -h emqx-server -c 1000
```

### SSL 连接

`emqtt-bench` 支持建立 SSL 的安全连接，并执行测试。

单向证书，例如：

```bash
./emqtt_bench sub -c 100 -i 10 -t bench/%i -p 8883 -S
./emqtt_bench pub -c 100 -I 10 -t bench/%i -p 8883 -s 256 -S
```

双向证书，例如：

```bash
./emqtt_bench sub -c 100 -i 10 -t bench/%i -p 8883 --certfile path/to/client-cert.pem --keyfile path/to/client-key.pem
./emqtt_bench pub -c 100 -i 10 -t bench/%i -s 256 -p 8883 --certfile path/to/client-cert.pem --keyfile path/to/client-key.pem
```

## 典型压测场景

### 场景说明

此处以 2 类最典型的场景来验证工具的使用：

1. 连接量：使用 `emqtt-bench` 创建百万连接到 EMQX Broker。
2. 吞吐量：使用 `emqtt-bench` 在 EMQX 中创建出 `10W/s 的 Qos0` 消息吞吐量。

### 机器及部署拓扑图

共需准备三台 8C16G 服务器，一台为 EMQX Broker，两台为 客户端压力机。其中：

- **系统：**`CentOS Linux release 7.7.1908 (Core)`

- **CPU：**`Intel Xeon Processor (Skylake)` 主频: `2693.670 MHZ`

- **服务端：**`emqx-centos7-v4.0.2.zip`

- **压力机：**`emqtt-bench v0.3.1`
  - 每台压力机分别配置 10 张网卡，用于连接测试中建立大量的 MQTT 客户端连接。

拓扑结构如下：

![benchmark_topology](./assets/benchmark_topology.png)

### 调优

客户端的压力机和服务端的机器都需要执行系统参数的调优，参见：[系统调优](../performance/tune.md)

### 连接量测试

在执行完系统调优后，首先启动 EMQX。

然后在 `bench1` 上的每张网卡上启动 5w 的连接数，共计 50w 的连接：

```bash

./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.100
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.101
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.102
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.103
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.104
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.105
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.106
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.107
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.108
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.109
```

在 `bench2`上也执行同样的操作。

在所有连接建立完成后，执行 `./bin/emqx ctl listeners`，并找到以下的内容， 查看 EMQX 中连接数的信息：

```bash
listener on mqtt:tcp:0.0.0.0:1883
  acceptors       : 8
  max_conns       : 1024000
  current_conn    : 1000000
  shutdown_count  : []
```

### 吞吐测试

同样的，首先启动服 EMQX，然后在 `bench1` 启动 500 个订阅客户端：

```bash
./emqtt_bench sub -t t -h 192.168.0.99 -c 500
```

然后再 `bench2` 上启动 20 个发布端，并且每秒发布 10 条消息：

```bash
./emqtt_bench pub -t t -h 192.168.0.99 -c 20 -I 100
```

然后，回到 `bench1`上的订阅客户端，可看到当前接收消息的速率，类似于：

```bash
recv(28006): total=2102563, rate=99725(msg/sec)
```
