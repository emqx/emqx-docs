# 桥接数据到 SAP Event Mesh

EMQ X 规则引擎支持通过 HTTP 请求方式（`httprest`）将消息发送到 SAP Event Mesh。

准备 SAP Event Mesh 环境，获取 Service Keys，例如:

```bash
{
    "xsappname": "some-app-name",
    "management": [
        {
            "oa2": {
                ...
            },
            "uri": "..."
        }
    ],
    "messaging": [
        {
            "oa2": {
                "clientid": "my_clientid",
                "clientsecret": "my_clientsecret",
                "tokenendpoint": "https://123trial.authentication.demo.com/oauth/token",
                "granttype": "client_credentials"
            },
            "protocol": [
                "amqp10ws"
            ]
        },
        {
            "oa2": {
                "clientid": "my_clientid",
                "clientsecret": "my_clientidsecret",
                "tokenendpoint": "https://123trial.authentication.demo.com/oauth/token",
                "granttype": "client_credentials"
            },
            "protocol": [
                "httprest"
            ],
            "broker": {
                "type": "saprestmgw"
            },
            "uri": "https://sap-messaging-pubsub.fooapps.demo.com"
        }
    ],
    "serviceinstanceid": "188783-7893-8765-8872-77866"
}
```

我们只关心 Service Keys 里面的 `"messaging"`字段，我们可以获取到以下与 `protocol: ["httprest"]` 相关的信息：

| Key                | Value|
|--------------------|------------------------------|
| Token Endpoint URI | https://123trial.authentication.demo.com/oauth/token |
| Send Message URI | https://sap-messaging-pubsub.fooapps.demo.com |
| ClientId | my_clientid |
| ClientSecret | my_clientsecret |

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的 “规则” 选项卡。

填写规则 SQL:

```sql
SELECT
    *
FROM
    "#"
```

![image](./assets/rule-engine/zh_sap_rule_sql.png)

关联动作:

在 “响应动作” 界面选择 “添加”，然后在 “动作” 下拉框里选择 “发送数据到 SAP Event Mesh”。

![image](./assets/rule-engine/zh_sap_action.png)

填写动作参数:

“发送数据到 SAP Event Mesh” 动作需要填写以下几个参数：

1). 消息内容模板。这个例子里我们向 SAP Event Mesh 发送一条数据，消息
​    模板为:

```
${payload}
```

2). 队列名。这里填写您在 SAP Event Mesh 平台创建的消息队列名字。比如，
"my_queue_name"。

3). QoS。选择 SAP Event Mesh 的 QoS 级别。

4). 关联资源。现在资源下拉框为空，可以点击右上角的 “新建资源” 来创建一个 SAP Event Mesh 资源:

选择 “SAP Event Mesh 资源”。

填写资源配置:

这里需要填写我们通过 Service Keys 获取到的信息，包括 `Token Endpoint URI`，`ClientId`，`ClientSecret`，`Send Message URI` 等。

其他参数保持默认，然后点击 “测试连接” 按钮，确保连接测试成功。最后点击 “确定” 按钮。

![image](./assets/rule-engine/zh_sap_resource.png)

返回响应动作界面，点击 “确定”。

![image](./assets/rule-engine/zh_sap_action_1.png)

返回规则创建界面，点击 “创建”。

![image](./assets/rule-engine/zh_sap_rule_sql_1.png)

规则已经创建完成，现在使用 MQTT 客户端向 emqx 发送一条数据:

```bash
Topic: "t/1"

QoS: 0

Payload: "abc"
```

现在您可以到 SAP Event Mesh 平台，检查是否可以从 "my_queue_name" 这个队列消费到我们刚才发送的数据： "abc"。
