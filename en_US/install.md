# Installation

The EMQ broker is cross-platform, which could be deployed on Linux, FreeBSD, Mac, Windows and even Raspberry Pi.

::: tip Tip
Linux, FreeBSD Recommended.
:::

## Download Packages

Download binary packages from: [ https://www.emqx.io/downloads ](https://www.emqx.io/downloads)

| Debian      | [ https://www.emqx.io/downloads/v2/latest/emqttd-debian.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-debian.zip)           |
| ----------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| Ubuntu12.04 | [ https://www.emqx.io/downloads/v2/latest/emqttd-ubuntu12.04.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-ubuntu12.04.zip) |
| Ubuntu14.04 | [ https://www.emqx.io/downloads/v2/latest/emqttd-ubuntu14.04.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-ubuntu14.04.zip) |
| Ubuntu16.04 | [ https://www.emqx.io/downloads/v2/latest/emqttd-ubuntu16.04.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-ubuntu16.04.zip) |
| CentOS7     | [ https://www.emqx.io/downloads/v2/latest/emqttd-centos7.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-centos7.zip)         |
| Debian7     | [ https://www.emqx.io/downloads/v2/latest/emqttd-debian7.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-debian7.zip)         |
| Debian8     | [ https://www.emqx.io/downloads/v2/latest/emqttd-debian7.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-debian7.zip)         |
| FreeBSD     | [ https://www.emqx.io/downloads/v2/latest/emqttd-freebsd.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-freebsd.zip)         |
| Windows7    | [ https://www.emqx.io/downloads/v2/latest/emqttd-windows7.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-windows7.zip)       |
| Windows10   | [ https://www.emqx.io/downloads/v2/latest/emqttd-windows10.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-windows10.zip)     |
| Mac OS X    | [ https://www.emqx.io/downloads/v2/latest/emqttd-macosx.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-macosx.zip)           |
| Docker      | [ https://www.emqx.io/downloads/v2/latest/emqttd-docker.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-docker.zip)           |

The package name consists of platform, version and release time.

For example: emqttd-centos64-v2.0.zip

## Installing on Linux

Download CentOS Package from: [ https://www.emqx.io/downloads/v2/latest/emqttd-centos7 ](https://www.emqx.io/downloads/v2/latest/emqttd-centos7) , and then unzip.zip:

    unzip emqttd-centos64-v2.0.zip

Start the broker in console mode:

    cd emqttd && ./bin/emqttd console

If the broker is started successfully, console will print:

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
    Erlang MQTT Broker 2.0 is running now
    Eshell V6.4  (abort with ^G)
    (emqttd@127.0.0.1)1>

CTRL+C to close the console and stop the broker.

Start the broker in daemon mode:

    ./bin/emqttd start

Check the running status of the broker:

    $ ./bin/emqttd_ctl status
    Node 'emqttd@127.0.0.1' is started
    emqttd 2.0 is running

Or check the status by URL:

    http://localhost:8080/status

Stop the broker:

    ./bin/emqttd stop

## Install via RPM

Download the RPM packages:

| CentOS6.8 | [ https://www.emqx.io/downloads/v2/latest/emqttd-centos6.rpm ](https://www.emqx.io/downloads/v2/latest/emqttd-centos6.rpm) |
| --------- | -------------------------------------------------------------------------------------------------------------------------- |
| CentOS7   | [ https://www.emqx.io/downloads/v2/latest/emqttd-centos7.rpm ](https://www.emqx.io/downloads/v2/latest/emqttd-centos7.rpm) |

Install the package:

    rpm -ivh emqttd-centos7-v2.1.2-1.el7.centos.x86_64.rpm

::: tip Tip
Erlang/OTP R19 depends on lksctp-tools library
:::

    yum install lksctp-tools

Configuration, Data and Log Files:

| /etc/emqttd/emq.conf        | Configuration file for the EMQ Broker   |
| --------------------------- | --------------------------------------- |
| /etc/emqttd/plugins/\*.conf | Configuration files for the EMQ Plugins |
| /var/lib/emqttd/            | Data files                              |
| /var/log/emqttd             | Log files                               |

Start/Stop the broker:

    systemctl start|stop|restart emqttd.service

## Install via DEB

Download the DEB packages:

| Ubuntu12.04 | [ https://www.emqx.io/downloads/v2/latest/emqttd-ubuntu12.04.deb ](https://www.emqx.io/downloads/v2/latest/emqttd-ubuntu12.04.deb) |
| ----------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| Ubuntu14.04 | [ https://www.emqx.io/downloads/v2/latest/emqttd-ubuntu14.04.deb ](https://www.emqx.io/downloads/v2/latest/emqttd-ubuntu14.04.deb) |
| Ubuntu16.04 | [ https://www.emqx.io/downloads/v2/latest/emqttd-ubuntu16.04.deb ](https://www.emqx.io/downloads/v2/latest/emqttd-ubuntu16.04.deb) |
| Debian7     | [ https://www.emqx.io/downloads/v2/latest/emqttd-debian7.deb ](https://www.emqx.io/downloads/v2/latest/emqttd-debian7.deb)         |
| Debian8     | [ https://www.emqx.io/downloads/v2/latest/emqttd-debian7.deb ](https://www.emqx.io/downloads/v2/latest/emqttd-debian7.deb)         |

Install the package:

    sudo dpkg -i emqttd-ubuntu16.04_v2.0_amd64.deb

::: tip Tip
Erlang/OTP R19 depends on lksctp-tools library
:::

    apt-get install lksctp-tools

Configuration, Data and Log Files:

| /etc/emqttd/emq.conf        | Configuration file for the EMQ Broker   |
| --------------------------- | --------------------------------------- |
| /etc/emqttd/plugins/\*.conf | Configuration files for the EMQ Plugins |
| /var/lib/emqttd/            | Data files                              |
| /var/log/emqttd             | Log files                               |

Start/Stop the broker:

    service emqttd start|stop|restart

## Installing on FreeBSD

Download FreeBSD Package from: [ https://www.emqx.io/downloads/v2/latest/emqttd-freebsd.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-freebsd.zip)

The installing process is same to Linux.

## Installing on Mac OS X

We could install the broker on Mac OS X to develop and debug MQTT applications.

Download Mac Package from: [ https://www.emqx.io/downloads/v2/latest/emqttd-macosx.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-macosx.zip)

Configure log level in etc/emq.conf , all MQTT messages recevied/sent will be printed on console:

    ## Console log. Enum: off, file, console, both
    log.console = both

    ## Console log level. Enum: debug, info, notice, warning, error, critical, alert, emergency
    log.console.level = debug

    ## Console log file
    log.console.file = log/console.log

The install and boot process on Mac are same to Linux.

## Installing on Windows

Download Package from: [ https://www.emqx.io/downloads/v2/latest/emqttd-windows.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-windows.zip) .

Unzip the package to install folder. Open the command line window and 'cd' to the folder.

Start the broker in console mode:

    bin\emqttd console

If the broker started successfully, a Erlang console window will popup.

Close the console window and stop the emqttd broker. Prepare to register emqttd as window service.

::: warning Warning
Cannot register EMQ-2.0 as a windows service.
:::

Install emqttd serivce:

    bin\emqttd install

Start emqttd serivce:

    bin\emqttd start

Stop emqttd serivce:

    bin\emqttd stop

Uninstall emqttd service:

    bin\emqttd uninstall

## Install via Docker Image

Download EMQ 2.0 Docker Image:

> [ https://www.emqx.io/downloads/v2/latest/emqttd-docker.zip ](https://www.emqx.io/downloads/v2/latest/emqttd-docker.zip)

unzip emqttd-docker image:

    unzip emqttd-docker-v2.0.zip

Load Docker Image:

    docker load < emqttd-docker-v2.0

Run the Container:

    docker run -tid --name emq20 -p 1883:1883 -p 8083:8083 -p 8883:8883 -p 8084:8084 -p 18083:18083 emqttd-docker-v2.0

Stop the broker:

    docker stop emq20

Start the broker:

    docker start emq20

Enter the running container:

    docker exec -it emq20 /bin/sh

## Installing From Source

The EMQ broker requires Erlang/OTP R20+ and git client to build:

Install Erlang: [ http://www.erlang.org/ ](http://www.erlang.org/)

Install Git Client: [ http://www.git-scm.com/ ](http://www.git-scm.com/)

Could use apt-get on Ubuntu, yum on CentOS/RedHat and brew on Mac to install Erlang and Git.

When all dependencies are ready, clone the emqttd project from github.com and build:

    git clone https://github.com/emqtt/emq-relx.git

    cd emq-relx && make

    cd _rel/emqttd && ./bin/emqttd console

The binary package output in folder:

    _rel/emqttd

## Build on Windows

Install Erlang: [ http://www.erlang.org/ ](http://www.erlang.org/)

Install MSYS2: [ http://www.msys2.org/ ](http://www.msys2.org/)

Use pacman of MSYS2 to install git and make:

    pacman -S git make

Clone and build the [ emq-relx ](https://github.com/emqtt/emq-relx) project:

    git clone -b windows https://github.com/emqtt/emqttd-relx.git

    cd emqttd-relx && make

Start the EMQ in console mode:

    cd _rel/emqttd && ./bin/emqttd console

## TCP Ports Used

| 1883  | MQTT Port                |
| ----- | ------------------------ |
| 8883  | MQTT/SSL Port            |
| 8083  | MQTT/WebSocket Port      |
| 8084  | MQTT/WebSocket/SSL Port  |
| 8080  | HTTP Management API Port |
| 18083 | Web Dashboard Port       |

The TCP ports used can be configured in etc/emqttd.config:

    ## TCP Listener: 1883, 127.0.0.1:1883, ::1:1883
    listener.tcp.external = 0.0.0.0:1883

    ## SSL Listener: 8883, 127.0.0.1:8883, ::1:8883
    listener.ssl.external = 8883

    ## External MQTT/WebSocket Listener
    listener.ws.external = 8083

    ## HTTP Management API Listener
    listener.api.mgmt = 127.0.0.1:8080

The 18083 port is used by Web Dashboard of the broker. Default login: admin, Password: public

## Quick Setup

Two main configuration files of the EMQ broker:

| etc/emq.conf        | EMQ Broker Config   |
| ------------------- | ------------------- |
| etc/plugins/\*.conf | EMQ Plugins' Config |

Two important parameters in etc/emq.conf:

| node.process_limit | Max number of Erlang proccesses. A MQTT client consumes two proccesses. The value should be larger than max_clients \* 2 |
| ------------------ | ------------------------------------------------------------------------------------------------------------------------ |
| node.max_ports     | Max number of Erlang Ports. A MQTT client consumes one port. The value should be larger than max_clients.                |

::: tip Tip
node.process_limit > maximum number of allowed concurrent clients \* 2 node.max_ports > maximum number of allowed concurrent clients
:::

The maximum number of allowed MQTT clients:

    listener.tcp.external = 0.0.0.0:1883

    listener.tcp.external.acceptors = 8

    listener.tcp.external.max_clients = 1024

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
