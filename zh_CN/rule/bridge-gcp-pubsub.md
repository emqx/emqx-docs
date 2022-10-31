# 桥接数据到 GCP PubSub

## 设置

1. 创建一个 [服务账户](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount)
   在你的GCP账户中.  确保服务账户有 权限，
   至少可以向感兴趣的主题发布消息。
2. 为该账户创建一个服务账户密钥，并下载其为 JSON格式下载
3. 创建一个PubSub主题（记住，服务账户必须有 权限来发布到该主题）。

## 创建PubSub资源

转到 [EMQX仪表板](http://127.0.0.1:18083/#/resources), 选择左边菜单上的
在左边的菜单上选择_"规则引擎"_项目，然后选择_"资源"_。
然后，点击_"创建"_。

在对话框中，选择GCP PubSub类型，并点击_"选择文件"_。
来选择和上传你要使用的服务账户的服务账户JSON文件。
帐户的JSON文件。 点击_"确认"_。

![创建一个GCP PubSub资源](./assets/gcp_pubsub_1.png)

## 创建规则和行动

转到 [EMQX仪表板](http://127.0.0.1:18083/#/resources), 选择左边菜单上的
在左边的菜单上选择_"规则引擎"_项目，然后选择_"规则"_。 然后。
点击_"创建"_。

键入以下SQL。

```sql
SELECT
    *
FROM
    "t/gcp"
```

![创建一个规则来转发数据到GCP PubSub](./assets/gcp_pubsub_2.png)

然后，点击_"添加动作"_。 选择行动类型_"数据
转发"_，_"数据到GCP PubSub"_，在_"资源的使用"_中选择 之前创建的GCP PubSub资源。
填入行动的参数 动作的参数。 这里唯一需要定义的参数是 PubSub Topic，
信息应该被转发到那里。 点击_"确认"_。"确认"_。

![绑定一个动作来转发数据到GCP PubSub](./assets/gcp_pubsub_3.png)

最后，点击页面底部的_"创建"_。

## 测试行动

你可以通过向EMQX发送一个MQTT消息来测试新的Action。

```bash
Topic: "t/gcp"

QoS: 0

Retained: false

Payload: "hello"
```

通过从PubSub主题中提取消息，你可以验证 数据已被转发到GCP。

```bash
# 为你的主题创建一个订阅
gcloud pubsub subscriptions create my_subscription_id --topic=mytopic

# 拉动数据
gcloud pubsub subscriptions pull my_subscription_id --auto-ack
```

另外，你可以检查该规则的度量是否随着 成功发布。

![GCP PubSub指标](./assets/gcp_pubsub_4.png)
