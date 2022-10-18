# MQTT subscriber

## Create module

Open [EMQX Dashboard](http://127.0.0.1:18083/#/modules), click the "Modules" tab on the left, and choose to add:

![image-20200927213049265](./assets/modules.png)

Select the MQTT subscriber module:

![](./assets/mqtt_subscriber1.png)

Fill in the relevant parameters:

![](./assets/mqtt_subscriber2.png)

After clicking Add, the module is added:

![](./assets/mqtt_subscriber3.png)

::: warning
If the MQTT server supports shared subscription, then we should use it. This is
because all the emqx nodes in the cluster subscribe to the same topic, if the
shared subscription is not used, each node will receive the same message,
resulting in duplicate messages.
:::
