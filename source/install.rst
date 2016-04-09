
.. _install:

========
安装部署
========

emqttd消息服务器可跨平台运行在Linux、FreeBSD、Mac OS X或Windows服务器上。

.. NOTE::

    产品部署建议Linux、FreeBSD服务器，不推荐Windows服务器。

.. _install_download:

----------------
emqttd程序包下载
----------------

emqttd消息服务器每个版本会发布Ubuntu、CentOS、FreeBSD、Mac OS X、Windows平台的程序包。

下载地址: http://emqtt.com/downloads

+-----------+-----------------------------------+
| Debian    | http://emqtt.com/downloads/debian  |
+-----------+-----------------------------------+
| Ubuntu    | http://emqtt.com/downloads/ubuntu  |
+-----------+-----------------------------------+
| CentOS    | http://emqtt.com/downloads/centos  |
+-----------+-----------------------------------+
| FreeBSD   | http://emqtt.com/downloads/freebsd |
+-----------+-----------------------------------+
| Mac OS X  | http://emqtt.com/downloads/macosx  |
+-----------+-----------------------------------+
| Windows   | http://emqtt.com/downloads/windows |
+-----------+-----------------------------------+

安装包命名由平台、版本、时间组成，例如: emqttd-centos64-0.16.0-beta-20160216.zip

.. _install_on_linux:

---------------
Linux服务器安装
---------------

CentOS平台为例，下载安装包解压: http://emqtt.com/downloads/centos

.. code:: console

    unzip emqttd-centos64-0.16.0-beta-20160216.zip

控制台调试模式启动，检查emqttd是否可正常启动:

.. code:: console

    cd emqttd && ./bin/emqttd console

emqttd消息服务器如启动正常，控制台输出:

.. code:: console

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
    Erlang MQTT Broker 0.16.0 is running now
    Eshell V6.4  (abort with ^G)
    (emqttd@127.0.0.1)1>

CTRL+c关闭控制台。守护进程模式启动:

.. code:: console

    ./bin/emqttd start

启动日志输出在log/emqttd_sasl.log文件。

emqttd消息服务器进程状态查询:

.. code:: console

    ./bin/emqttd_ctl status

正常运行状态，查询命令返回:

.. code:: console

    $ ./bin/emqttd_ctl status
    Node 'emqttd@127.0.0.1' is started
    emqttd 0.16.0 is running

emqttd消息服务器提供了状态监控URL:: 

    http://localhost:8083/status

停止服务器::

    ./bin/emqttd stop

.. _install_on_freebsd:

-----------------
FreeBSD服务器安装
-----------------

FreeBSD程序包下载: http://emqtt.com/downloads/freebsd

FreeBSD平台安装过程与Linux相同。

.. _install_on_mac:

----------------
Mac OS X系统安装
----------------

Mac下开发调试MQTT应用，可直接下载安装:  http://emqtt.com/downloads/macosx

配置文件'etc/emqttd.config' lager段落打开info日志，控制台可以查看收发MQTT报文详细:

.. code:: erlang

    {lager, [
        ...
        {handlers, [
            {lager_console_backend, info},
            ...
        ]}
    ]},

emqttd在Mac平台下安装启动过程与Linux相同。

.. _install_on_windows:

-----------------
Windows服务器安装
-----------------

Windows平台程序包下载: http://emqtt.com/downloads/windows

程序包下载解压后，打开Windows命令行窗口，cd到程序目录。

控制台模式启动::
 
    .\bin\emqttd console

如启动成功，会弹出控制台窗口。

关闭控制台窗口，停止emqttd进程，准备注册Windows服务。

emqttd注册为Windows服务::
    
    .\bin\emqttd install

emqttd服务启动::

    .\bin\emqttd start

emqttd服务停止::

    .\bin\emqttd stop

emqttd服务卸载::

    .\bin\emqttd uninstall

.. NOTE:: 可通过Windows服务管理控制台进行启停。

.. WARNING:: Windows上管理命令行'./bin/emqttd_ctl'无法使用。日志文件或状态URL: http://localhost:8083/status 查询当前状态。
 
.. _build_from_source:

------------
源码编译安装
------------

emqttd消息服务器基于Erlang/OTP平台开发，项目托管的GitHub管理维护，源码编译依赖Erlang环境和git客户端。

Erlang安装: http://www.erlang.org/

Git客户端: http://www.git-scm.com/

Ubuntu平台可通过apt-get命令安装，CentOS/RedHat平台可通过yum命令安装，Mac下可通过brew包管理命令安装，Windows下... :(

编译环境准备好之后，clone代码开始编译:

.. code:: console

    git clone https://github.com/emqtt/emqttd.git

    cd emqttd

    make && make dist

编译成功后，可执行程序包在目录::

    rel/emqttd

控制台启动编译的emqttd程序包::

    cd rel/emqttd && ./bin/emqttd console

.. _tcp_ports:

---------------
TCP服务端口占用
---------------

emqttd消息服务器默认占用的TCP端口包括:

+-----------+-----------------------------------+
| 1883      | MQTT协议端口                      |
+-----------+-----------------------------------+
| 8883      | MQTT(SSL)端口                     |
+-----------+-----------------------------------+
| 8083      | MQTT(WebSocket), HTTP API端口     |
+-----------+-----------------------------------+
| 18083     | Dashboard管理控制台端口           |
+-----------+-----------------------------------+

emqttd占用的上述端口，可通过etc/emqttd.config配置文件的listeners段落设置:

.. code:: erlang

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

通过注释或删除相关段落，可禁用相关TCP服务启动。

18083端口是Web管理控制占用，该端口由emqttd_dashboard插件启用。

控制台URL: http:://localhost:18083/ ，默认登录用户名: admin, 密码: public。

.. _quick_setup:

--------
快速设置
--------

emqttd消息服务器主要配置文件:

+-------------------+-----------------------------------+
| etc/vm.args       | Erlang VM的启动参数设置           |
+-------------------+-----------------------------------+
| etc/emqttd.config | emqttd消息服务器参数设置          |
+-------------------+-----------------------------------+

etc/vm.args中两个重要的启动参数:

+-------+------------------------------------------------------------------+
| +P    | Erlang虚拟机允许的最大进程数，emqttd一个连接会消耗2个Erlang进程  |
+-------+------------------------------------------------------------------+
| +Q    | Erlang虚拟机允许的最大Port数量，emqttd一个连接消耗1个Port        |
+-------+------------------------------------------------------------------+

+P 参数值 > 最大允许连接数 * 2

+Q 参数值 > 最大允许连接数

.. WARNING:: 实际连接数量超过Erlang虚拟机参数设置，会引起emqttd消息服务器宕机!

etc/emqttd.config文件listeners段落设置最大允许连接数:

.. code:: erlang

    {listeners, [
        {mqtt, 1883, [
            %% TCP Acceptor池设置
            {acceptors, 16},

            %% 最大允许连接数设置
            {max_clients, 8192},

            ...

        ]},

emqttd消息服务器详细设置，请参见文档: :ref:`config`

.. _init_d_emqttd:

-------------------
/etc/init.d/emqttd
-------------------

.. code:: shell

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


chkconfig::

    chmod +x /etc/init.d/emqttd
    chkconfig --add emqttd
    chkconfig --list

boot test::

    service emqttd start

.. NOTE::

    ## erlexec: HOME must be set
    uncomment '# export HOME=/root' if "HOME must be set" error.

