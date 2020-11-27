# RPC 桥接

EMQ X 桥接转发 MQTT 消息到远程 EMQ X:

![image](./assets/bridge_rpc.png)

rpc bridge 桥接插件配置文件: etc/plugins/emqx_bridge_mqtt.conf

## 配置 RPC 桥接的 Broker 地址

```bash
bridge.mqtt.emqx.address = emqx2@192.168.1.2
```

## 配置 MQTT 桥接转发和订阅主题

```bash
## 桥接的 mountpoint(挂载点)
bridge.mqtt.emqx.mountpoint = bridge/emqx1/${node}/

## 转发消息的主题
bridge.mqtt.emqx.forwards = topic1/#,topic2/#
```

## MQTT 桥接转发和订阅主题说明

挂载点 Mountpoint: mountpoint 用于在转发消息时加上主题前缀，该配置选项须配合 forwards 使用，转发主题为
sensor1/hello 的消息, 到达远程节点时主题为 bridge/aws/emqx1@192.168.1.1/sensor1/hello。

转发主题 Forwards: 转发到本地 EMQX 指定 forwards 主题上的消息都会被转发到远程 MQTT Broker 上。

## 桥接 CLI 命令

桥接 CLI 的使用方式与 mqtt bridge 相同。