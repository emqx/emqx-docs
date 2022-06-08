# 使用配置文件配置规则

通过修改配置文件 `emqx.conf`，可以在 EMQX 启动之前配置规则。

## 规则配置文件的结构

在 `rule_engine` 命名空间下，使用 `rules` 配置项可以创建一到多个规则:

```js
rule_engine {
    rules.my_rule_id {
        sql = "SELECT * FROM \"t/#\""
        actions = []
    }
}
```

其中 `my_rule_id` 是规则的 ID，必须是字母数字下划线的组合。
在每个规则的定义里面，`sql` 字段用于定义规则的 SQL 语句。`actions` 字段是一个数组，可定义一个或多个动作。

### 配置内置动作

对于内置动作，须要在 `actions` 字段里配置一个包含 `function` 字段的 Object：

```js{1-4,15-17}
rule_engine {
    rules.my_rule_id {
        sql = "SELECT * FROM \"t/#\""
        actions = [
            {
                function = console
            },
            {
                function = republish
                args = {
                    topic = "a/1"
                    payload = "${payload}"
                }
            }
        ]
    }
}
```

其中 `function` 字段为动作的名字。`args` 字段为动作的参数列表。
`republish` 动作须要指定消息主题和消息内容等参数，而 `console` 动作不需要任何参数。

在动作里，可以使用 `${some_key}` 的形式来引用 SQL 结果中的变量。因为这里 SQL 语句使用 `SELECT *`
输出了所有可用的字段，所以我们可以使用 `${payload}` 来获取原来消息里的消息内容，
或者使用 `${clientid}` 来获取发送消息的客户端 ID 等等。
规则引擎支持的事件类型以及可用字段请参见：[事件主题和可用字段](./rule-engine_field.md)

关于内置动作的细节，详见 [内置动作](./rule-engine_builtin_actions.md)

### 引用数据桥接作为动作

若把数据桥接作为动作，须要在 `actions` 字段里配置数据桥接的 ID：

```js{1-3,5-6}
rule_engine {
    rules.my_rule_id {
        sql = "SELECT * FROM \"t/#\""
        actions = ["mqtt:my_mqtt_bridge"]
    }
}
```

其中 `mqtt:my_mqtt_bridge` 是一个数据桥接的 ID，其类型为 `mqtt`、名字为 `my_mqtt_bridge`。

使用数据桥接之前，须要预先创建：

```js
bridges.mqtt.my_egress_mqtt_bridge {
    connector = "mqtt:my_mqtt_connector"
    direction = egress

    remote_topic = "from_emqx/${topic}"
    remote_qos = "${qos}"
    payload = "${payload}"
    retain = false
}

connectors.mqtt.my_mqtt_connector {
    server = "192.168.2.100:1883"
    username = "username1"
    password = ""
    ssl.enable = false
}
```

上面的配置比之前规则的配置稍微复杂了一些，但其实他只是创建了一个 MQTT 桥接。

我们使用 `bridges` 命名空间用来创建数据桥接，
`bridges.mqtt.my_egress_mqtt_bridge` 则创建了一个类型为 `mqtt`、名字为 `my_mqtt_bridge` 的数据桥接，
其 ID 为 `<type>:<name>`，即 `mqtt:my_mqtt_bridge`。这个 ID 正是被前面的规则当做动作引用的。

同时这个 MQTT 桥接引用了一个 connector: `mqtt:my_mqtt_connector`，里面配置了 mqtt 连接相关的配置。
这个 connector 配置可以被多个 MQTT 桥接引用。

关于数据桥接的细节，详见 [数据桥接](./data-bridge.md)

## 配置处理消息的规则

我们可以使用规则引擎处理特定主题的消息。下面给出了一个处理 MQTT 消息的规则的示例。

在 `emqx.conf` 配置文件的最后，添加如下配置：

```js
rule_engine {
    rules.my_republish_rule {
        sql = "SELECT qos, payload.x as y FROM \"t/a\""
        actions = [
            {
                function = republish
                args = {
                    topic = "t/b"
                    qos = "${qos}"
                    payload = "y: ${y}"
                }
            }
        ]
    }
}
```

该配置创建了一个名为 "my_republish_rule" 的规则。这个规则从主题为 `t/a` 的消息内容中筛选出 `x` 字段，并赋值给 `y`。

其中 `actions` 参数定义了规则执行成功时会触发的动作: 重新发布（republish）。

现在如果发送一条内容为 `{"x": 1}`，QoS 为 0 的消息（JSON 格式）到主题 `t/a`, 就会匹配到该规则。
规则首先执行 SQL 语句，从消息上下文中选取 `qos` 字段，并提取 `payload` 里的 `x` 字段赋值给变量 `y`，最后触发 `republish` 动作发送一条消息到主题 `t/b`，消息内容为字符串 `y: 1`（因为 `payload.x = 1`），QoS 为 0。

## 配置处理事件的规则

规则引擎使用 **$events/** 开头的**事件主题**处理 EMQX 事件。下面给出了一个处理“客户端上线”事件的规则的示例。

在 `emqx.conf` 配置文件的最后，添加如下配置：

```js
rule_engine {
    rules.client_connected_debug {
        sql = "SELECT clientid, connected_at FROM \"$events/client_connected\" WHERE username = 'emqx'"
        actions = [{function = console}]
    }
}
```

该配置定义了一个 ID 为 "client_connected_debug" 的规则。
这个规则从事件上下文中选取出 "clientid" 和 "connected_at" 两个字段，然后调用 `console` 动作把 SQL 的输出打印到 emqx 控制台。

除了客户端上下线事件以外，规则引擎还支持订阅和取消订阅、消息投递等事件。
规则引擎支持的事件类型以及可用字段请参见：[事件主题和可用字段](./rule-engine_field.md)

## 配置使用数据桥接作为数据源的规则

有些数据桥接（如 MQTT 桥接）也可以作为规则引擎的数据源，规则引擎使用 **$bridges/** 开头的**数据桥接主题**处理由数据桥接触发的事件。下面给出了一个把 “MQTT 桥接” 作为数据源的规则的示例。

在 `emqx.conf` 配置文件的最后，添加如下配置：

```js
rule_engine {
    rules.receive_msgs_from_remote_mqtt_broker {
        sql = "SELECT * FROM \"$bridges/mqtt:my_mqtt_source\" WHERE username = 'emqx'"
        actions = [{function = console}]
    }
}
```

这个示例创建了一个 ID 为 `receive_msgs_from_remote_mqtt_broker` 的规则，
SQL 语句里指定了一个数据桥接主题：`$bridges/mqtt:my_mqtt_source`，
其中 `mqtt:my_mqtt_source` 是一个 MQTT 桥接的 ID，其类型为 `mqtt`、名字为 `my_mqtt_source`。

这个 MQTT 桥接需要预先创建：

```js
bridges.mqtt.my_mqtt_source {
    connector = "mqtt:my_mqtt_connector"
    direction = ingress
    remote_topic = "aws/#"
    remote_qos = 1
}

connectors.mqtt.my_mqtt_connector {
    server = "192.168.2.100:1883"
    username = "username1"
    password = ""
    ssl.enable = false
}
```

我们使用 `bridges` 命名空间用来创建数据桥接，
`bridges.mqtt.my_mqtt_source` 则创建了一个类型为 `mqtt`、名字为 `my_mqtt_source` 的数据桥接，
其 ID 为 `<type>:<name>`，即 `mqtt:my_mqtt_source`。

这个 MQTT 桥接引用了一个 connector: `mqtt:my_mqtt_connector`，里面配置了 mqtt 连接相关的配置。
关于数据桥接的细节，详见 [数据桥接](./data-bridge.md)。

该规则在最后调用了 `console` 动作，这是一个调试动作，会把 SQL 语句筛选出的所有字段打印到 emqx 控制台里。
因为这里 SQL 语句使用 `SELECT *` 输出了所有可用的字段，所以他会打印从远程 MQTT Broker 收到的消息相关的
所有字段都打印出来，比如 payload, qos, topic 等等。
