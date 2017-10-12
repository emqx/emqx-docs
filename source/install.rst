
.. _install:

=======================
程序安装 (Installation)
=======================

*EMQ* 2.0 消息服务器可跨平台运行在 Linux、FreeBSD、Mac OS X 或 Windows 服务器上。

.. NOTE:: 产品部署建议 Linux、FreeBSD 服务器，不推荐 Windows 服务器。

--------------------
*EMQ* 2.0 程序包下载
--------------------

*EMQ* 2.0 消息服务器每个版本会发布 Ubuntu、CentOS、FreeBSD、Mac OS X、Windows 平台程序包与 Docker 镜像。

下载地址: http://emqtt.com/downloads

.. _install_rpm:

----------
RPM 包安装
----------

EMQ Linux RPM 程序包:

+-------------+---------------------------------------------------+
| CentOS6.8   | http://emqtt.com/downloads/latest/centos6-rpm     |
+-------------+---------------------------------------------------+
| CentOS7     | http://emqtt.com/downloads/latest/centos7-rpm     |
+-------------+---------------------------------------------------+

安装包命名由平台、版本、操纵系统位数组成，例如: emqttd-centos7-v2.0_x86_64.rpm

CentOS、RedHat 操作系统下，推荐 RPM 包安装。RPM 包安装后可通过操作系统，直接管理启停 EMQ 服务。

RPM 安装
--------

.. code-block:: console

    rpm -ivh --force emqttd-centos7-v2.1.2-1.el7.centos.x86_64.rpm

.. NOTE:: Erlang/OTP R19 依赖 lksctp-tools 库

.. code-block:: console

    yum install lksctp-tools

配置文件
--------

EMQ 配置文件: /etc/emqttd/emq.conf，插件配置文件: /etc/emqttd/plugins/\*.conf。

日志文件
--------

日志文件目录: /var/log/emqttd

数据文件
--------

数据文件目录：/var/lib/emqttd/

启动停止
--------

.. code-block:: console

    systemctl start|stop|restart emqttd.service

.. _install_deb:

----------
DEB 包安装
----------

EMQ Linux DEB 程序包:

+-------------+---------------------------------------------------+
| Ubuntu12.04 | http://emqtt.com/downloads/latest/ubuntu12_04-deb |
+-------------+---------------------------------------------------+
| Ubuntu14.04 | http://emqtt.com/downloads/latest/ubuntu14_04-deb |
+-------------+---------------------------------------------------+
| Ubuntu16.04 | http://emqtt.com/downloads/latest/ubuntu16_04-deb |
+-------------+---------------------------------------------------+
| Debian7     | http://emqtt.com/downloads/latest/debian7-deb     |
+-------------+---------------------------------------------------+
| Debian8     | http://emqtt.com/downloads/latest/debian7-deb     |
+-------------+---------------------------------------------------+

安装包命名由平台、版本、操纵系统位数组成，例如: emqttd-debian7-v2.0_amd64.deb

Debian、Ubuntu 操作系统下，推荐 DEB 包安装。DEB 包安装后可通过操作系统，直接管理启停 EMQ 服务。

.. code-block:: console

    sudo dpkg -i emqttd-ubuntu16.04_v2.0_amd64.deb

.. NOTE:: Erlang/OTP R19依赖lksctp-tools库

.. code-block:: console

    apt-get install lksctp-tools

配置文件
--------

EMQ 配置文件: /etc/emqttd/emq.conf，插件配置文件: /etc/emqttd/plugins/\*.conf。

日志文件
--------

日志文件目录: /var/log/emqttd

数据文件
--------

数据文件目录：/var/lib/emqttd/

启动停止
--------

.. code-block:: console

    service emqttd start|stop|restart

.. _install_on_linux:

----------------
Linux 通用包安装
----------------

EMQ Linux 通用程序包:

+-------------+-----------------------------------------------+
| Ubuntu12.04 | http://emqtt.com/downloads/latest/ubuntu12_04 |
+-------------+-----------------------------------------------+
| Ubuntu14.04 | http://emqtt.com/downloads/latest/ubuntu14_04 |
+-------------+-----------------------------------------------+
| Ubuntu16.04 | http://emqtt.com/downloads/latest/ubuntu16_04 |
+-------------+-----------------------------------------------+
| CentOS6.8   | http://emqtt.com/downloads/latest/centos6     |
+-------------+-----------------------------------------------+
| CentOS7     | http://emqtt.com/downloads/latest/centos7     |
+-------------+-----------------------------------------------+
| Debian7     | http://emqtt.com/downloads/latest/debian7     |
+-------------+-----------------------------------------------+
| Debian8     | http://emqtt.com/downloads/latest/debian7     |
+-------------+-----------------------------------------------+
| FreeBSD     | http://emqtt.com/downloads/latest/freebsd     |
+-------------+-----------------------------------------------+

安装包命名由平台、版本组成，例如: emqttd-macosx-v2.0.zip

CentOS 平台为例，下载安装过程:

.. code-block:: bash

    unzip emqttd-centos7-v2.0.zip

控制台调试模式启动，检查 *EMQ* 是否可正常启动:

.. code-block:: bash

    cd emqttd && ./bin/emqttd console

*EMQ* 消息服务器如启动正常，控制台输出:

.. code-block:: bash

    starting emqttd on node 'emqttd@127.0.0.1'
    emqttd ctl is starting...[ok]
    emqttd hook is starting...[ok]
    emqttd router is starting...[ok]
    emqttd pubsub is starting...[ok]
    emqttd stats is starting...[ok]
    emqttd metrics is starting...[ok]
    emqttd pooler is starting...[ok]
    emqttd trace is starting...[ok]
    emqttd client manager is starting...[ok]
    emqttd session manager is starting...[ok]
    emqttd session supervisor is starting...[ok]
    emqttd wsclient supervisor is starting...[ok]
    emqttd broker is starting...[ok]
    emqttd alarm is starting...[ok]
    emqttd mod supervisor is starting...[ok]
    emqttd bridge supervisor is starting...[ok]
    emqttd access control is starting...[ok]
    emqttd system monitor is starting...[ok]
    dashboard:http listen on 0.0.0.0:18083 with 2 acceptors.
    mqtt:tcp listen on 0.0.0.0:1883 with 8 acceptors.
    mqtt:ssl listen on 0.0.0.0:8883 with 4 acceptors.
    mqtt:ws listen on 0.0.0.0:8083 with 4 acceptors.
    Erlang MQTT Broker 2.0 is running now

CTRL+c 关闭控制台。守护进程模式启动:

.. code-block:: bash

    ./bin/emqttd start

启动错误日志将输出在 log/ 目录。

*EMQ* 消息服务器进程状态查询:

.. code-block:: bash

    ./bin/emqttd_ctl status

正常运行状态，查询命令返回:

.. code-block:: bash

    $ ./bin/emqttd_ctl status
    Node 'emqttd@127.0.0.1' is started
    emqttd 2.0 is running

*EMQ* 消息服务器提供了状态监控 URL ::

    http://localhost:8080/status

停止服务器::

    ./bin/emqttd stop

.. _install_on_freebsd:

------------------
FreeBSD 服务器安装
------------------

EMQ FreeBSD 程序包下载: http://emqtt.com/downloads/latest/freebsd

FreeBSD 平台安装过程与Linux相同。

.. _install_on_mac:

-----------------
Mac OS X 系统安装
-----------------

Mac 下开发调试 MQTT 应用，可直接下载安装: http://emqtt.com/downloads/latest/macosx

配置文件 'etc/emq.conf' log 段落打开 debug 日志，控制台可以查看收发 MQTT 报文详细:

.. code-block::

    ## Console log. Enum: off, file, console, both
    log.console = both

    ## Console log level. Enum: debug, info, notice, warning, error, critical, alert, emergency
    log.console.level = debug

    ## Console log file
    log.console.file = log/console.log

*EMQ* 在 Mac 平台下安装启动过程与 Linux 相同。

.. _install_on_windows:

------------------
Windows 服务器安装
------------------

Windows 平台程序包下载: http://emqtt.com/downloads/latest/windows10

程序包下载解压后，打开 Windows 命令行窗口，cd 到程序目录。

控制台模式启动::

    bin\emqttd console

如启动成功，会弹出控制台窗口。

关闭控制台窗口，停止emqttd进程，准备注册 Windows 服务。

.. WARNING:: EMQ-2.0 暂不支持服务注册

*EMQ* 注册为 Windows 服务::

    bin\emqttd install

*EMQ* 服务启动::

    bin\emqttd start

*EMQ* 服务停止::

    bin\emqttd stop

*EMQ* 服务卸载::

    bin\emqttd uninstall

.. _install_docker:

---------------
Docker 镜像安装
---------------

*EMQ* 2.0 Docker 镜像下载: http://emqtt.com/downloads/latest/docker

解压 emqttd-docker 镜像包::

    unzip emqttd-docker-v2.0.zip

加载镜像::

    docker load < emqttd-docker-v2.0

启动容器::

    docker run -tid --name emq20 -p 1883:1883 -p 8083:8083 -p 8883:8883 -p 8084:8084 -p 18083:18083 emqttd-docker-v2.0

停止容器::

    docker stop emq20

开启容器::

    docker start emq20

进入 Docker 控制台::

    docker exec -it emq20 /bin/sh

.. _build_from_source:

------------
源码编译安装
------------

*EMQ* 消息服务器基于 Erlang/OTP 平台开发，项目托管的 GitHub 管理维护，源码编译依赖 Erlang 环境和 git 客户端。

Erlang 安装: http://www.erlang.org/

Git 客户端: http://www.git-scm.com/

Ubuntu 平台可通过 apt-get 命令安装，CentOS/RedHat 平台可通过 yum 命令安装，Mac 下可通过 brew 包管理命令安装，Windows 下... :(

编译环境准备好之后，clone 代码开始编译:

.. code-block:: bash

    git clone https://github.com/emqtt/emq-relx.git

    cd emq-relx && make

    cd _rel/emqttd && ./bin/emqttd console

编译成功后，可执行程序包在目录::

    _rel/emqttd

控制台启动编译的 EMQ 程序包::

    cd _rel/emqttd && ./bin/emqttd console

--------------------
Windows 源码编译安装
--------------------

Erlang 安装: http://www.erlang.org/

MSYS2 安装: http://www.msys2.org/

MSYS2 安装完成后，根据 MSYS2 中的 pacman 包管理工具安装 Git、 Make 工具软件:

.. code-block:: bash

    pacman -S git make

编译环境准备之后，clone 代码开始编译:

.. code-block:: bash

    git clone -b windows https://github.com/emqtt/emqttd-relx.git

    cd emqttd-relx && make

    cd _rel/emqttd && ./bin/emqttd console

编译成功后，可执行程序包在目录::

    _rel/emqttd

控制台启动编译的 EMQ 程序包::

    cd _rel/emqttd && ./bin/emqttd console

.. _tcp_ports:

----------------
TCP 服务端口占用
----------------

*EMQ* 2.0 消息服务器默认占用的 TCP 端口包括:

+-----------+-----------------------------------+
| 1883      | MQTT 协议端口                     |
+-----------+-----------------------------------+
| 8883      | MQTT/SSL 端口                     |
+-----------+-----------------------------------+
| 8083      | MQTT/WebSocket 端口               |
+-----------+-----------------------------------+
| 8080      | HTTP API 端口                     |
+-----------+-----------------------------------+
| 18083     | Dashboard 管理控制台端口          |
+-----------+-----------------------------------+

*EMQ* 2.0 占用的上述端口，可通过 etc/emq.conf 配置文件的 'listener' 段落设置:

.. code-block:: properties

    ## TCP Listener: 1883, 127.0.0.1:1883, ::1:1883
    listener.tcp.external = 0.0.0.0:1883

    ## SSL Listener: 8883, 127.0.0.1:8883, ::1:8883
    listener.ssl.external = 8883
    
    ## External MQTT/WebSocket Listener
    listener.ws.external = 8083
    
    ## HTTP Management API Listener
    listener.api.mgmt = 127.0.0.1:8080

通过注释或删除相关段落，可禁用相关 TCP 服务启动。

18083端口是 Web 管理控制占用，该端口由 `emq_dashboard`_ 插件启用。

控制台 URL: http:://localhost:18083/ ，默认登录用户名: admin, 密码: public。

.. _quick_setup:

--------
快速设置
--------

*EMQ* 消息服务器主要配置文件:

+----------------------+-----------------------------------+
| etc/emq.conf         | EMQ 消息服务器参数设置            |
+----------------------+-----------------------------------+
| etc/plugins/\*.conf  | EMQ 插件配置文件                  |
+----------------------+-----------------------------------+

etc/emq.conf 中两个重要的虚拟机启动参数:

+-----------------------+------------------------------------------------------------------+
| node.process_limit    | Erlang 虚拟机允许的最大进程数，EMQ 一个连接会消耗2个Erlang进程   |
+-----------------------+------------------------------------------------------------------+
| node.max_ports        | Erlang 虚拟机允许的最大 Port 数量，EMQ 一个连接消耗1个 Port      |
+-----------------------+------------------------------------------------------------------+

.. NOTE:: Erlang 的 Port 非 TCP 端口，可以理解为文件句柄。

node.process_limit = 参数值 > 最大允许连接数 * 2

node.max_ports = 参数值 > 最大允许连接数

.. WARNING:: 实际连接数量超过 Erlang 虚拟机参数设置，会引起 EMQ 消息服务器宕机!

etc/emq.conf 配置文件的 `listener` 段落设置最大允许连接数:

.. code-block:: properties

    listener.tcp.external = 0.0.0.0:1883
    
    listener.tcp.external.acceptors = 8

    listener.tcp.external.max_clients = 1024

*EMQ* 2.0 消息服务器详细设置，请参见文档: :ref:`config`

.. NOTE::

    ## erlexec: HOME must be set
    uncomment '# export HOME=/root' if "HOME must be set" error.

.. _emq_dashboard: https://github.com/emqtt/emqttd_dashboard

