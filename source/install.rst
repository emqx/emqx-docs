
.. _install:

=======================
程序安装 (Installation)
=======================

*EMQ X* R3.0 消息服务器可跨平台运行在 Linux、FreeBSD、Mac OS X 或 Windows 服务器上。

.. NOTE:: 产品部署建议 Linux、FreeBSD 服务器，不推荐 Windows 服务器。

-----------------------
*EMQ X* R3.0 程序包下载
-----------------------

*EMQ X* R3.0 消息服务器每个版本会发布 Ubuntu、CentOS、FreeBSD、Mac OS X、Windows 平台程序包与 Docker 镜像。

下载地址: http://emqtt.com/downloads

.. _install_rpm:

----------
RPM 包安装
----------

EMQ X Linux RPM 程序包:

+-------------+---------------------------------------------------+
| CentOS6.8   | https://www.emqx.io/downloads/v3/latest/emqx-centos6.rpm     |
+-------------+---------------------------------------------------+
| CentOS7     | https://www.emqx.io/downloads/v3/latest/emqx-centos7.rpm     |
+-------------+---------------------------------------------------+

安装包命名由平台、版本、操纵系统位数组成，例如: emqx-centos7-v3.0_x86_64.rpm

CentOS、RedHat 操作系统下，推荐 RPM 包安装。RPM 包安装后可通过操作系统，直接管理启停 EMQ X 服务。

RPM 安装
--------

.. code-block:: console

    rpm -ivh emqx-centos7-v3.0-beta.2.x86_64.rpm

配置文件
--------

EMQ X 配置文件: /etc/emqx/emqx.conf，插件配置文件: /etc/emqx/plugins/\*.conf。

日志文件
--------

日志文件目录: /var/log/emqx

数据文件
--------

数据文件目录：/var/lib/emqx/

启动停止
--------

.. code-block:: console

    systemctl start|stop|restart emqx.service

.. _install_deb:

----------
DEB 包安装
----------

EMQ X Linux DEB 程序包:

+-------------+---------------------------------------------------+
| Ubuntu12.04 | https://www.emqx.io/downloads/v3/latest/emqx-ubuntu12.04.deb |
+-------------+---------------------------------------------------+
| Ubuntu14.04 | https://www.emqx.io/downloads/v3/latest/emqx-ubuntu14.04.deb |
+-------------+---------------------------------------------------+
| Ubuntu16.04 | https://www.emqx.io/downloads/v3/latest/emqx-ubuntu16.04.deb |
+-------------+---------------------------------------------------+
| Ubuntu18.04 | https://www.emqx.io/downloads/v3/latest/emqx-ubuntu18.04.deb |
+-------------+---------------------------------------------------+
| Debian7     | https://www.emqx.io/downloads/v3/latest/emqx-debian7.deb     |
+-------------+---------------------------------------------------+
| Debian8     | https://www.emqx.io/downloads/v3/latest/emqx-debian7.deb     |
+-------------+---------------------------------------------------+

安装包命名由平台、版本、操纵系统位数组成，例如: emqx-debian7-v3.0_amd64.deb

Debian、Ubuntu 操作系统下，推荐 DEB 包安装。DEB 包安装后可通过操作系统，直接管理启停 EMQ X 服务。

.. code-block:: console

    sudo dpkg -i emqx-ubuntu16.04-v3.0-beta.2_amd64.deb 

配置文件
--------

EMQ X 配置文件: /etc/emqx/emqx.conf，插件配置文件: /etc/emqx/plugins/\*.conf。

日志文件
--------

日志文件目录: /var/log/emqx

数据文件
--------

数据文件目录：/var/lib/emqx/

启动停止
--------

.. code-block:: console

    service emqx start|stop|restart

.. _install_on_linux:

----------------
Linux 通用包安装
----------------

*EMQ X* Linux 通用程序包:

+-------------+-----------------------------------------------+
| Ubuntu12.04 | https://www.emqx.io/downloads/v3/latest/emqx-ubuntu12_04.zip |
+-------------+-----------------------------------------------+
| Ubuntu14.04 | https://www.emqx.io/downloads/v3/latest/emqx-ubuntu14_04.zip |
+-------------+-----------------------------------------------+
| Ubuntu16.04 | https://www.emqx.io/downloads/v3/latest/emqx-ubuntu16_04.zip |
+-------------+-----------------------------------------------+
| Ubuntu18.04 | https://www.emqx.io/downloads/v3/latest/emqx-ubuntu18_04.zip |
+-------------+-----------------------------------------------+
| CentOS6.8   | https://www.emqx.io/downloads/v3/latest/emqx-centos6.zip     |
+-------------+-----------------------------------------------+
| CentOS7     | https://www.emqx.io/downloads/v3/latest/emqx-centos7.zip     |
+-------------+-----------------------------------------------+
| Debian7     | https://www.emqx.io/downloads/v3/latest/emqx-debian7.zip     |
+-------------+-----------------------------------------------+
| Debian8     | https://www.emqx.io/downloads/v3/latest/emqx-debian7.zip     |
+-------------+-----------------------------------------------+
| FreeBSD     | https://www.emqx.io/downloads/v3/latest/emqx-freebsd.zip     |
+-------------+-----------------------------------------------+

安装包命名由平台、版本组成，例如: emqx-macosx-v3.0.zip

CentOS 平台为例，下载安装过程:

.. code-block:: bash

    unzip emqx-centos7-v3.0.zip

控制台调试模式启动，检查 *EMQ X* 是否可正常启动:

.. code-block:: bash

    cd emqx && ./bin/emqx console

*EMQ X* 消息服务器如启动正常，控制台输出:

.. code-block:: bash

    starting emqx on node 'emqx@127.0.0.1'
    emqx ctl is starting...[ok]
    emqx hook is starting...[ok]
    emqx router is starting...[ok]
    emqx pubsub is starting...[ok]
    emqx stats is starting...[ok]
    emqx metrics is starting...[ok]
    emqx pooler is starting...[ok]
    emqx trace is starting...[ok]
    emqx client manager is starting...[ok]
    emqx session manager is starting...[ok]
    emqx session supervisor is starting...[ok]
    emqx wsclient supervisor is starting...[ok]
    emqx broker is starting...[ok]
    emqx alarm is starting...[ok]
    emqx mod supervisor is starting...[ok]
    emqx bridge supervisor is starting...[ok]
    emqx access control is starting...[ok]
    emqx system monitor is starting...[ok]
    dashboard:http listen on 0.0.0.0:18083 with 2 acceptors.
    mqtt:tcp listen on 0.0.0.0:1883 with 8 acceptors.
    mqtt:ssl listen on 0.0.0.0:8883 with 4 acceptors.
    mqtt:ws listen on 0.0.0.0:8083 with 4 acceptors.
    Erlang MQTT Broker 3.0 is running now

CTRL+C 关闭控制台。守护进程模式启动:

.. code-block:: bash

    ./bin/emqx start

启动错误日志将输出在 log/ 目录。

*EMQ X* 消息服务器进程状态查询:

.. code-block:: bash

    ./bin/emqx_ctl status

正常运行状态，查询命令返回:

.. code-block:: bash

    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx 3.0 is running

*EMQ X* 消息服务器提供了状态监控 URL::

    http://localhost:8080/status

停止服务器::

    ./bin/emqx stop

.. _install_on_freebsd:

------------------
FreeBSD 服务器安装
------------------

*EMQ X* FreeBSD 程序包下载: https://www.emqx.io/downloads/v3/latest/emqx-freebsd.zip

FreeBSD 平台安装过程与Linux相同。

.. _install_on_mac:

-----------------
Mac OS X 系统安装
-----------------

Mac 下开发调试 MQTT 应用，可直接下载安装: https://www.emqx.io/downloads/v3/latest/emqx-macosx.zip

配置文件 'etc/emqx.conf' log 段落打开 debug 日志，控制台可以查看收发 MQTT 报文详细:

.. code-block::

    ## Console log. Enum: off, file, console, both
    log.console = both

    ## Console log level. Enum: debug, info, notice, warning, error, critical, alert, emergency
    log.console.level = debug

    ## Console log file
    log.console.file = log/console.log

*EMQ X* 在 Mac 平台下安装启动过程与 Linux 相同。

.. _install_on_windows:

------------------
Windows 服务器安装
------------------

Windows 平台程序包下载: https://www.emqx.io/downloads/v3/latest/emqx-windows10.zip

程序包下载解压后，打开 Windows 命令行窗口，cd 到程序目录。

控制台模式启动::

    bin\emqx console

如启动成功，会弹出控制台窗口。

关闭控制台窗口，停止emqx进程，准备注册 Windows 服务。

.. WARNING:: EMQ X R3.0 暂不支持服务注册

*EMQ X* 注册为 Windows 服务::

    bin\emqx install

*EMQ X* 服务启动::

    bin\emqx start

*EMQ X* 服务停止::

    bin\emqx stop

*EMQ X* 服务卸载::

    bin\emqx uninstall

.. _install_docker:

---------------
Docker 镜像安装
---------------

*EMQ X* 3.0 Docker 镜像下载: https://www.emqx.io/downloads/v3/latest/emqx-docker.zip

解压 emqx-docker 镜像包::

    unzip emqx-docker-v3.0.zip

加载镜像::

    docker load < emqx-docker-v3.0

启动容器::

    docker run -tid --name emq30 -p 1883:1883 -p 8083:8083 -p 8883:8883 -p 8084:8084 -p 18083:18083 emqx-docker-v3.0

停止容器::

    docker stop emq30

开启容器::

    docker start emq30

进入 Docker 控制台::

    docker exec -it emq30 /bin/sh

.. _build_from_source:

------------
源码编译安装
------------

*EMQ X* 消息服务器基于 Erlang/OTP 平台开发，项目托管的 GitHub 管理维护，源码编译依赖 Erlang 环境和 git 客户端。

.. NOTE:: EMQ X R3.0 依赖 Erlang R21+ 版本

Erlang 安装: http://www.erlang.org/

Git 客户端: http://www.git-scm.com/

Ubuntu 平台可通过 apt-get 命令安装，CentOS/RedHat 平台可通过 yum 命令安装，Mac 下可通过 brew 包管理命令安装，Windows 下... :(

编译环境准备好之后，clone 代码开始编译:

.. code-block:: bash

    git clone -b emqx30 https://github.com/emqx/emqx-rel.git

    cd emq-relx && make

    cd _rel/emqx && ./bin/emqx console

编译成功后，可执行程序包在目录::

    _rel/emqx

控制台启动编译的 EMQ 程序包::

    cd _rel/emqx && ./bin/emqx console

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

    git clone -b win30 https://github.com/emqx/emqx-rel.git

    cd emqx-relx && make

    cd _rel/emqx && ./bin/emqx console

编译成功后，可执行程序包在目录::

    _rel/emqx

控制台启动编译的 EMQ 程序包::

    cd _rel/emqx && ./bin/emqx console

.. _tcp_ports:

----------------
TCP 服务端口占用
----------------

*EMQ X* R3.0 消息服务器默认占用的 TCP 端口包括:

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

*EMQ X* R3.0 占用的上述端口，可通过 etc/emqx.conf 配置文件的 'listener' 段落设置:

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

18083 端口是 Web 管理控制占用，该端口由 `emq_dashboard`_ 插件启用。

控制台 URL: http:://localhost:18083/ ，默认登录用户名: admin, 密码: public。

.. _quick_setup:

--------
快速设置
--------

*EMQ X* 消息服务器主要配置文件:

+----------------------+-----------------------------------+
| etc/emqx.conf        | EMQ 消息服务器参数设置            |
+----------------------+-----------------------------------+
| etc/plugins/\*.conf  | EMQ 插件配置文件                  |
+----------------------+-----------------------------------+

etc/emqx.conf 中两个重要的虚拟机启动参数:

+-----------------------+------------------------------------------------------------------+
| node.process_limit    | Erlang 虚拟机允许的最大进程数，EMQ 一个连接会消耗2个Erlang进程   |
+-----------------------+------------------------------------------------------------------+
| node.max_ports        | Erlang 虚拟机允许的最大 Port 数量，EMQ 一个连接消耗1个 Port      |
+-----------------------+------------------------------------------------------------------+

.. NOTE:: Erlang 的 Port 非 TCP 端口，可以理解为文件句柄。

node.process_limit = 参数值 > 最大允许连接数 * 2

node.max_ports = 参数值 > 最大允许连接数

.. WARNING:: 实际连接数量超过 Erlang 虚拟机参数设置，会引起 EMQ 消息服务器宕机!

etc/emqx.conf 配置文件的 `listener` 段落设置最大允许连接数:

.. code-block:: properties

    listener.tcp.external = 0.0.0.0:1883

    listener.tcp.external.acceptors = 8

    listener.tcp.external.max_clients = 1024

*EMQ X* R3.0 消息服务器详细设置，请参见文档: :ref:`config`

.. NOTE::

    ## erlexec: HOME must be set
    uncomment '# export HOME=/root' if "HOME must be set" error.

.. _emqx_dashboard: https://github.com/emqx/emqx-dashboard.git

