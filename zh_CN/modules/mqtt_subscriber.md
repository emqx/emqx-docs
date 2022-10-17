# MQTT 订阅者

## 创建模块

打开 [EMQX Dashboard](http://127.0.0.1:18083/#/modules)，点击左侧的 “模块” 选项卡，选择添加：

![image-20200927213049265](./assets/modules.png)

选择 MQTT 订阅者模块:

![](./assets/mqtt_subscriber1.png)

填写相关参数:

![](./assets/mqtt_subscriber2.png)

点击添加后，模块添加完成:

![](./assets/mqtt_subscriber3.png)

::: warning
如果服务端支持共享订阅，那么应该使用共享订阅 ($shared/<群组名>/<主题>)。因为集群里的所有
emqx 节点都会订阅同一个主题，如果不使用共享订阅，每个节点都会收到同样的消息，导致消息重复。
:::
