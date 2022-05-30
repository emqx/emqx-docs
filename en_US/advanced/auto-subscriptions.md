# Auto subscriptions

The auto subscription feature of EMQX Broker allows the client to automatically establish the user's preset subscription relationship without sending additional SUBSCRIBE packets when the connection is established.

::: tip
<!-- Update links to include a link to {{ your-emqx-dashboard-endpoint }}  -->
You can use the Http API to subscribe and unsubscribe to connected devices, see topic subscription and topic unscription
:::


## Built-in auto subscription

Through the built-in auto subscription module you can specify auto subscription rules through the configuration file to achieve auto subscription, which is suitable for regular and static auto subscription requirements.

### Enable auto subscription feature

The auto subscription feature is disabled by default. To enable this feature, you need to modify the `module.subscription` configuration item in the `etc/emqx.conf` file. The default `off` means disabled. If you want to enable it, please change it to ` on`.

```bash
module.subscription = off
```

### Configure auto subscription rules

Of course, just enabling does not mean that the auto subscription has already worked. You need to configure the corresponding rules. The user can add multiple auto subscription rules and each rule should specify the Topic and QoS. There is no limit for the number of rules. The format of auto subscription rules is as follows:

```bash
module.subscription.<number>.topic = <topic>
module.subscription.<number>.qos = <qos>
```

When configuring the topic of auto subscription, EMQX Broker provides two placeholders of  `%c` and `%u` for users to use, and EMQX Broker will replace the `%c` and `%u` in the configuration with the client's `Client ID` and `Username` respectively when performing auto subscription. It should be noted that `%c` and `%u` must occupy an entire topic level.

For example, the following auto subscription rules are added in the `etc/emqx.conf` file:

```bash
module.subscription.1.topic = client/%c
module.subscription.1.qos = 1

module.subscription.2.topic = user/%u
module.subscription.2.qos = 2
```

When a client connects to EMQX Broker, if the client's `Client ID` is `testclient` and `Username` is `tester`, according to the configuration rules above, the auto subscription function will actively help the client subscribe to the topics of `client/testclient`(QoS is 1) and `user/tester`(QoS is 2)


## Dynamic auto subscription

The EMQX Enterprise version supports dynamic auto subscription. An external database is used to set the topic list and the list is read when the device is connected to establish the auto subscription.

