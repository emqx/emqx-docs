# 安装部署(Installation)

emqttd 消息服务器可跨平台运行在 Linux、FreeBSD、Mac OS X 或 Windows 服务器上。

::: tip
产品部署建议 Linux、FreeBSD 服务器，不推荐 Windows 服务器。
:::

## emqttd 程序包下载

emqttd 消息服务器每个版本会发布 Ubuntu、CentOS、FreeBSD、Mac OS X、Windows 平台的程序包。

下载地址: [ https://emqx.io/downloads ](https://emqx.io/downloads)

| Debian   | [ https://www.emqx.io/downloads/v1/latest/emqttd-debian.zip](https://www.emqx.io/downloads/v1/latest/emqttd-debian.zip)   |
| -------- | ------------------------------------------------------------------------------------------------------------------------- |
| Ubuntu   | [ https://www.emqx.io/downloads/v1/latest/emqttd-ubuntu.zip](https://www.emqx.io/downloads/v1/latest/emqttd-ubuntu.zip)   |
| CentOS   | [ https://www.emqx.io/downloads/v1/latest/emqttd-centos.zip](https://www.emqx.io/downloads/v1/latest/emqttd-centos.zip)   |
| FreeBSD  | [ https://www.emqx.io/downloads/v1/latest/emqttd-freebsd.zip](https://www.emqx.io/downloads/v1/latest/emqttd-freebsd.zip) |
| Mac OS X | [ https://www.emqx.io/downloads/v1/latest/emqttd-macosx.zip](https://www.emqx.io/downloads/v1/latest/emqttd-macosx.zip)   |
| Windows  | [ https://www.emqx.io/downloads/v1/latest/emqttd-windows.zip](https://www.emqx.io/downloads/v1/latest/emqttd-windows.zip) |

安装包命名由平台、版本、时间组成，例如: emqttd-centos64-0.16.0-beta-20160216.zip

## Linux 服务器安装

CentOS 平台为例，下载安装包解压: [ https://www.emqx.io/downloads/v1/latest/emqttd-
centos.zip ](https://www.emqx.io/downloads/v1/latest/emqttd-centos.zip)

    unzip emqttd-centos64-1.1-beta-20160601.zip

控制台调试模式启动，检查 emqttd 是否可正常启动:

    cd emqttd && ./bin/emqttd console

emqttd 消息服务器如启动正常，控制台输出:

    starting emqttd on node 'emqttd@127.0.0.1'
    emqttd ctl is starting...[done]
    emqttd trace is starting...[done]
    emqttd pubsub is starting...[done]
    emqttd stats is starting...[done]
    emqttd metrics is starting...[done]
    emqttd retainer is starting...[done]
    emqttd pooler is starting...[done]
    emqttd client manager is starting...[done]
    emqttd session manager is starting...[done]
    emqttd session supervisor is starting...[done]
    emqttd broker is starting...[done]
    emqttd alarm is starting...[done]
    emqttd mod supervisor is starting...[done]
    emqttd bridge supervisor is starting...[done]
    emqttd access control is starting...[done]
    emqttd system monitor is starting...[done]
    http listen on 0.0.0.0:18083 with 4 acceptors.
    mqtt listen on 0.0.0.0:1883 with 16 acceptors.
    mqtts listen on 0.0.0.0:8883 with 4 acceptors.
    http listen on 0.0.0.0:8083 with 4 acceptors.
    Erlang MQTT Broker 1.1 is running now
    Eshell V6.4  (abort with ^G)
    (emqttd@127.0.0.1)1>

CTRL+c 关闭控制台。守护进程模式启动:

    ./bin/emqttd start

启动日志输出在 log/emqttd_sasl.log 文件。

emqttd 消息服务器进程状态查询:

    ./bin/emqttd_ctl status

正常运行状态，查询命令返回:

    $ ./bin/emqttd_ctl status
    Node 'emqttd@127.0.0.1' is started
    emqttd 1.1 is running

emqttd 消息服务器提供了状态监控 URL:

    http://localhost:8083/status

停止服务器:

    ./bin/emqttd stop

## FreeBSD 服务器安装

FreeBSD 程序包下载: [ https://www.emqx.io/downloads/v1/latest/emqttd-freebsd.zip
](https://www.emqx.io/downloads/v1/latest/emqttd-freebsd.zip)

FreeBSD 平台安装过程与 Linux 相同。

## Mac OS X 系统安装

Mac 下开发调试 MQTT 应用，可直接下载安装: [ https://www.emqx.io/downloads/v1/latest/emqttd-
macosx.zip ](https://www.emqx.io/downloads/v1/latest/emqttd-macosx.zip)

配置文件'etc/emqttd.config' lager 段落打开 info 日志，控制台可以查看收发 MQTT 报文详细:

    {lager, [
        ...
        {handlers, [
            {lager_console_backend, info},
            ...
        ]}
    ]},

emqttd 在 Mac 平台下安装启动过程与 Linux 相同。

## Windows 服务器安装

Windows 平台程序包下载: [ https://www.emqx.io/downloads/v1/latest/emqttd-windows.zip
](https://www.emqx.io/downloads/v1/latest/emqttd-windows.zip)

程序包下载解压后，打开 Windows 命令行窗口，cd 到程序目录。

控制台模式启动:

    .\bin\emqttd console

如启动成功，会弹出控制台窗口。

关闭控制台窗口，停止 emqttd 进程，准备注册 Windows 服务。

emqttd 注册为 Windows 服务:

    .\bin\emqttd install

emqttd 服务启动:

    .\bin\emqttd start

emqttd 服务停止:

    .\bin\emqttd stop

emqttd 服务卸载:

    .\bin\emqttd uninstall

::: tip
可通过 Windows 服务管理控制台进行启停。
:::

::: warning
Windows 上管理命令行'./bin/emqttd_ctl'无法使用。日志文件或状态 URL: [ http://localhost:8083/status
](http://localhost:8083/status) 查询当前状态。
:::

## 源码编译安装

emqttd 消息服务器基于 Erlang/OTP 平台开发，项目托管的 GitHub 管理维护，源码编译依赖 Erlang 环境和 git 客户端。

Erlang 安装: [ http://www.erlang.org/ ](http://www.erlang.org/)

Git 客户端: [ http://www.git-scm.com/ ](http://www.git-scm.com/)

Ubuntu 平台可通过 apt-
get 命令安装，CentOS/RedHat 平台可通过 yum 命令安装，Mac 下可通过 brew 包管理命令安装，Windows 下... \:\(

编译环境准备好之后，clone 代码开始编译:

    git clone https://github.com/emqtt/emqttd.git

    cd emqttd

    make && make dist

编译成功后，可执行程序包在目录:

    rel/emqttd

控制台启动编译的 emqttd 程序包:

    cd rel/emqttd && ./bin/emqttd console

## TCP 服务端口占用

emqttd 消息服务器默认占用的 TCP 端口包括:

| 1883  | MQTT 协议端口                  |
| ----- | ------------------------------ |
| 8883  | MQTT(SSL)端口                  |
| 8083  | MQTT(WebSocket), HTTP API 端口 |
| 18083 | Dashboard 管理控制台端口       |

emqttd 占用的上述端口，可通过 etc/emqttd.config 配置文件的 listeners 段落设置:

    {listeners, [
        {mqtt, 1883, [
            ...
        ]},

        {mqtts, 8883, [
            ...
        ]},
        %% HTTP and WebSocket Listener
        {http, 8083, [
            ...
        ]}
    ]},

通过注释或删除相关段落，可禁用相关 TCP 服务启动。

18083 端口是 Web 管理控制占用，该端口由 emqttd_dashboard 插件启用。

控制台 URL: [http://localhost:18083/](http://localhost:18083/) ，默认登录用户名:
admin, 密码: public。

## 快速设置

emqttd 消息服务器主要配置文件:

| etc/vm.args       | Erlang VM 的启动参数设置  |
| ----------------- | ------------------------- |
| etc/emqttd.config | emqttd 消息服务器参数设置 |

etc/vm.args 中两个重要的启动参数:

| +P  | Erlang 虚拟机允许的最大进程数，emqttd 一个连接会消耗 2 个 Erlang 进程 |
| --- | --------------------------------------------------------------------- |
| +Q  | Erlang 虚拟机允许的最大 Port 数量，emqttd 一个连接消耗 1 个 Port      |

::: tip
Erlang 的 Port 非 TCP 端口，可以理解为文件句柄。
:::

+P 参数值 > 最大允许连接数 \* 2

+Q 参数值 > 最大允许连接数


::: warning
实际连接数量超过 Erlang 虚拟机参数设置，会引起 emqttd 消息服务器宕机!
:::

etc/emqttd.config 文件 listeners 段落设置最大允许连接数:

    {listeners, [
        {mqtt, 1883, [
            %% TCP Acceptor池设置
            {acceptors, 16},

            %% 最大允许连接数设置
            {max_clients, 8192},

            ...

        ]},

emqttd 消息服务器详细设置，请参见文档: `config`

## /etc/init.d/emqttd

    #!/bin/sh
    #
    # emqttd       Startup script for emqttd.
    #
    # chkconfig: 2345 90 10
    # description: emqttd is mqtt broker.

    # source function library
    . /etc/rc.d/init.d/functions

    # export HOME=/root

    start() {
        echo "starting emqttd..."
        cd /opt/emqttd && ./bin/emqttd start
    }

    stop() {
        echo "stopping emqttd..."
        cd /opt/emqttd && ./bin/emqttd stop
    }

    restart() {
        stop
        start
    }

    case "$1" in
        start)
            start
            ;;
        stop)
            stop
            ;;
        restart)
            restart
            ;;
        *)
            echo $"Usage: $0 {start|stop}"
            RETVAL=2
    esac

chkconfig:

    chmod +x /etc/init.d/emqttd
    chkconfig --add emqttd
    chkconfig --list

boot test:

    service emqttd start

::: tip Tip
erlexec: HOME must be set uncomment '# export HOME=/root' if "HOME must be set" error.
:::
