# GCP PubSub桥

EMQX GCP PubSub 集成可以发送 MQTT 消息和事件到
[Google Cloud PubSub](https://cloud.google.com/pubsub?hl=en-us)，这
可以帮助你灵活地选择谷歌云上的服务并更容易地建立物联网
应用。

下面的步骤将指导你使用这个配置。

## 设置

在配置GCP PubSub之前，必须先在GCP方面进行一些配置步骤
在EMQX上配置GCP PubSub桥。

1.  创建一个[服务
   账户](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount)
   在你的GCP账户中。 确保该服务账户有
   权限，至少可以向感兴趣的主题发布消息。
2. 为该账户创建一个服务账户密钥，并以JSON格式下载它。
   JSON格式。
3. 创建一个PubSub主题（记住，服务帐户必须有
   权限来发布到该主题）。

## GCP PubSub桥通过仪表盘使用的例子

进入EMQX仪表板，选择左边菜单上的_"数据集成"_项目，然后选择_"数据桥"_。
在左边的菜单上选择_"数据集成"_，然后选择_"数据桥"_。 然后，点击_"+创建"_。

在对话框中，选择_Google PubSub_桥类型，点击下一步。
填写该桥的必要字段（这些字段标有
星号）。 _payload模板_字段，如果留空，将编码
所有来自MQTT消息的可见输入都使用JSON格式，例如
clientid, topic, payload, 等等。 否则，它将使用用户定义的
模板，用MQTT上下文中的相应值来填充`${变量_名称}`形式的占位符。
对应的MQTT上下文中的值。 例如，`${topic}`
将被替换为`my/topic`，如果这是MQTT消息的主题。

在_GCP服务账户凭证_字段中，上传你的JSON格式的服务账户凭证。
帐户凭证的JSON格式，你在设置步骤中导出的。

![创建一个GCP PubSub桥](./assets/gcp_pubsub/setup1.png)

![创建GCP公共子网桥](./assets/gcp_pubsub/setup2.png)

![创建一个GCP公共子桥](./assets/gcp_pubsub/setup3.png)

最后，在点击_"Create"_后，你会得到一个选项，即创建一个相关规则。创建一
个相关的规则。 这将允许符合规则的MQTT消息被转发到GCP PubSub。 关于创建
规则的更多信息，请参考 [_规则_](./rules.md)。

<center>
<img src="./assets/gcp_pubsub/setup4.png" alt="为GCP PubSub桥创建一个规则"></img>
</center>

## 通过配置文件的GCP PubSub桥使用实例

在`emqx.conf`文件的末尾添加以下配置。
将`service_account_json`的内容替换为
你的服务帐户JSON文件的相应内容。

```js
bridges.gcp_pubsub.my_gcp_pubsub_bridge {
  connect_timeout = "15s"
  enable = true
  max_retries = 2
  pipelining = 100
  pool_size = 8
  pubsub_topic = "my-topic"
  request_timeout = "15s"
  resource_opts {
    async_inflight_window = 100
    auto_restart_interval = "60s"
    batch_size = 1
    batch_time = "20ms"
    health_check_interval = "15s"
    max_queue_bytes = "100MB"
    query_mode = "async"
    worker_pool_size = 16
  }
  service_account_json {
    "auth_provider_x509_cert_url" = "https://www.googleapis.com/oauth2/v1/certs"
    auth_uri = "https://accounts.google.com/o/oauth2/auth"
    client_email = "test-516@emqx-cloud-pubsub.iam.gserviceaccount.com"
    client_id = "0000000000000000000"
    "client_x509_cert_url" = "https://www.googleapis.com/robot/v1/metadata/x509/test-pubsub.iam.gserviceaccount.com"
    private_key = "-----BEGIN PRIVATE KEY-----nMIIEvAIBA...\n-----END PRIVATE KEY-----\n"
    private_key_id = "000000000000000000000000000000000000"
    project_id = "my-project-id"
    token_uri = "https://oauth2.googleapis.com/token"
    type = "service_account"
  }
}
```
