# 分布桥接(Bridge)

## emqttd 节点间桥接

emqttd 消息服务器支持多节点桥接模式互联:

                  ---------                     ---------                     ---------
    Publisher --> | node1 | --Bridge Forward--> | node2 | --Bridge Forward--> | node3 | --> Subscriber
                  ---------                     ---------                     ---------

节点间桥接与集群不同，不复制主题树与路由表，只按桥接规则转发 MQTT 消息。

### emqttd 节点桥接配置

假设在本机创建两个 emqttd 节点，并创建一条桥接转发全部传感器(sensor)主题消息:

| 目录    | 节点              | MQTT 端口 |
| ------- | ----------------- | --------- |
| emqttd1 | emqttd1@127.0.0.1 | 1883      |
| emqttd2 | emqttd2@127.0.0.1 | 2883      |

启动 emqttd1, emqttd2 节点:

    cd emqttd1/ && ./bin/emqttd start
    cd emqttd2/ && ./bin/emqttd start

emqttd1 节点上创建到 emqttd2 桥接:

    $ ./bin/emqttd_ctl bridges start emqttd2@127.0.0.1 sensor/#

    bridge is started.

    $ ./bin/emqttd_ctl bridges list

    bridge: emqttd1@127.0.0.1--sensor/#-->emqttd2@127.0.0.1

测试 emqttd1--sensor/#-->emqttd2 的桥接:

    #emqttd2节点上

    mosquitto_sub -t sensor/# -p 2883 -d

    #emqttd1节点上

    mosquitto_pub -t sensor/1/temperature -m "37.5" -d

删除桥接:

    ./bin/emqttd_ctl bridges stop emqttd2@127.0.0.1 sensor/#

## mosquitto 桥接

mosquitto 可以普通 MQTT 连接方式，桥接到 emqttd 消息服务器:

                 -------------             -----------------
    Sensor ----> | mosquitto | --Bridge--> |               |
                 -------------             |    emqttd     |
                 -------------             |    Cluster    |
    Sensor ----> | mosquitto | --Bridge--> |               |
                 -------------             -----------------

### mosquitto.conf

本机 2883 端口启动 emqttd 消息服务器，1883 端口启动 mosquitto 并创建桥接。

mosquitto.conf 配置:

    connection emqttd
    address 127.0.0.1:2883
    topic sensor/# out 2

    # Set the version of the MQTT protocol to use with for this bridge. Can be one
    # of mqttv31 or mqttv311. Defaults to mqttv31.
    bridge_protocol_version mqttv311

## rsmb 桥接

本机 2883 端口启动 emqttd 消息服务器，1883 端口启动 rsmb 并创建桥接。

broker.cfg 桥接配置:

    connection emqttd
    addresses 127.0.0.1:2883
    topic sensor/#
