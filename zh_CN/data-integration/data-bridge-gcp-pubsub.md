# GCP PubSub

[Google Cloud PubSub](https://cloud.google.com/pubsub?hl=en-us) 是一种异步消息传递服务，旨在实现极高的可靠性和可扩缩性。

借助 EMQX GCP PubSub 集成，你可以将 MQTT 消息和客户端事件发送到 GCP PubSub 中，这能够帮助您更快的基于 GCP 构建物联网应用，助力你从 GCP IoT Core 迁移到 EMQX 中。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

## 功能清单

- [连接池](./data-bridges.md#连接池)
- [异步请求模式](./data-bridges.md#异步请求模式)
- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)

## 快速开始

本节介绍如何配置 GCP PubSub 数据桥接，包括如何设置 GCP 的服务、创建数据桥接和转发数据到 GCP PubSub 的规则以及测试数据桥接和规则等主题。

在配置 GCP PubSub 之前，必须先在 GCP 上创建好对应的服务账户凭证以及 PubSub 主题。

### 创建服务账户凭证

服务账户凭证是用于身份验证和授权的 JSON 文件，EMQX 需要通过它访问 PubSub 资源。

1. 进入 GCP 控制台，在搜索框中输入 **IAM** 并进入 **IAM & Admin** 页面。
2. 在 IAM & Admin 页面中点击 **Service Accounts** -> **Email** 中对应的邮箱，选择 **KEYS** 标签页，点击 **ADD KEY** 添加以生成用于身份认证 JSON 格式的 key，请妥善保管该文件。

![GCP 服务账户凭证](./assets/gcp_pubsub/gcp-service-account.png)

### 在 GCP PubSub 中创建主题

1. 打开 [Pub/Sub 控制台](https://console.cloud.google.com/cloudpubsub)，点击 **CREATE TOPIC，**输入自定义的 **Topic ID，**点击 **CREATE** 即可完成创建。

![GCP PubSub 创建主题](./assets/gcp_pubsub/gcp-pubsub-topic-create.png)

2. 点击列表页对应的 **Topic ID** 即可进入 Topic 详情页面，您需要创建一个 **subscription** 来保存消息，有关 subscription 详细介绍请参考 [CCP Pub/Sub subscription](https://cloud.google.com/pubsub/docs/subscriber)，此处选择 Pull 类型，保留 7 天历史消息。

![GCP PubSub 创建订阅](./assets/gcp_pubsub/gcp-pubsub-subscription-create.png)

3. 点击 **Subscription ID** → **MESSAGES** → **PULL** 可以在线查看发送到主题中的消息。

### 创建 GCP PubSub 数据桥接

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。

2. 点击页面右上角的**创建**。

3. 在**数据桥接类型**中选择 GCP PubSub，点击**下一步**。

4. 输入数据桥接名称，要求是大小写英文字母和数字的组合。

5. 输入 **GCP PubSub 主题**，此处填写上述步骤中创建好的 **my-iot-core** 主题。

6. 填写 **HTTP 请求消息体模板**，此处留空，使用 JSON 格式发送所有可用的上下文。你也可以使用 `${field}` 形式的占位符来构造消息。

7. 在 **GCP 服务账户凭证** 字段中，上传你的 JSON 格式的服务账户凭证。

8. 高级配置（可选），根据情况配置同步/异步模式，队列等参数。

9.  设置完成后，您可点击**测试连接**按钮进行验证。

10. 点击**创建**按钮完成数据桥接创建。

至此您已经完成数据桥接创建流程，接下来将继续创建一条规则来指定需要写入的数据：

### 创建数据转发规则

1. 转到 Dashboard **数据集成** -> **规则页面**。
2. 点击页面右上角的**创建**。
3. 输入规则 ID，例如： `my_rule`。
3. 在 SQL 编辑器中输入规则，请确保规则选择出来的字段（SELECT 部分）包含 HTTP 请求消息体模版中用到的变量。例如将 `/devices/+/events` 主题的 MQTT 消息集成到 GCP PubSub，此处规则 SQL 如下：


  ```sql
  SELECT
    *
  FROM
    "/devices/+/events"
  ```

5. 添加动作，在动作下拉框中选择 使用数据桥接转发 选项，选择先前创建好的 GCP PubSub 数据桥接。

6. 点击最下方创建按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `/devices/+/events` 主题的消息经过名为 `my_rule` 的规则处理，处理结果写入到 GCP PubSub 中。

### 测试数据桥接与规则

1. 使用 MQTTX 向 `/devices/+/events` 主题发布消息：

```bash
mqttx pub -i emqx_c -t /devices/+/events -m '{ "msg": "hello GCP PubSub" }'
```

2. 查看数据桥接运行统计，命中、发送成功次数均 +1。

3. 前往 GCP PubSub 控制台查看数据是否已经发送成功。
