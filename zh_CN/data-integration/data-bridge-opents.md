# 将 MQTT 数据写入到 OpenTSDB

EMQX 支持与 OpenTSDB 集成，因此可以将 MQTT 消息保存到 OpenTSDB 以便后续进行分析和检索。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

::: tip 前置准备

- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

:::

## 特性

- [连接池](./data-bridges.md#连接池)
- [异步请求模式](./data-bridges.md#异步请求模式)
- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)
- [SQL 预处理](./data-bridges.md#SQL-预处理)

## 快速开始教程

本节介绍如何配置 OpenTSDB 数据桥接，包括如何设置 OpenTSDB 服务器、创建数据桥接和规则以将数据转发到 OpenTSDB、以及如何测试数据桥接和规则。

本教程假定您在本地机器上同时运行 EMQX 和 OpenTSDB。如果您在远程运行 OpenTSDB 和 EMQX，请相应地调整设置。

本教程将使用如下数据进行演示
- 主题: `t/opents`
- 载荷:
```json
{
  "metric": "cpu",
  "tags": {
    "host": "serverA"
  },
  "value":12
}
```

### 安装 OpenTSDB

通过 Docker 安装并启动 OpenTSDB：

```bash
docker pull petergrace/opentsdb-docker

docker run -d --name opentsdb -p 4242:4242 petergrace/opentsdb-docker

```

### 创建 OpenTSDB 桥接

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。

2. 点击页面右上角的**创建**。

3. 在数据桥接类型中选择 **OpenTSDB**，点击下一步。

4. 输入数据桥接名称，要求是大小写英文字母或数字组合。

5. 输入 OpenTSDB 连接信息：

   - 服务器地址填写 `http://127.0.0.1:4242`，如果您在远程运行 OpenTSDB 服务器，需填写实际地址。
   - 其他选项使用默认值即可。

6. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考[数据桥接简介](./data-bridges.md)中的配置参数。

7. 在完成创建之前，您可以点击**测试连接**来测试桥接可以连接到 OpenTSDB 服务器。

8. 点击**创建**按钮完成数据桥接创建。

   在弹出的**创建成功**对话框中您可以点击**创建规则**，继续创建规则以指定需要写入 OpenTSDB 的数据。您也可以按照[创建 OpenTSDB 数据桥接规则](#创建-opentsdb-数据桥接规则)章节的步骤来创建规则。

### 创建 OpenTSDB 数据桥接规则

至此您已经完成数据桥接创建，接下来将继续创建一条规则来指定需要写入的数据。

1. 转到 Dashboard **数据集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 输入规则 ID `my_rule`，在 **SQL 编辑器**中输入规则。例如将 `t/#` 主题的 MQTT 消息存储至 OpenTSDB，需输入以下 SQL 语法：

   注意：如果您希望制定自己的 SQL 语法，需要确保规则选出的字段（SELECT 部分）包含所有 SQL 模板中用到的变量。

   ```sql
   	SELECT
     		payload.metric as metric, payload.tags as tags, payload.value as value
   	FROM
     		"t/#"
   ```

4. 点击**添加动作**，在动作下拉框中选择**使用数据桥接转发**选项，选择先前创建好的 OpenTSDB 数据桥接。点击**添加**。
5. 点击最下方**创建**按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 OpenTSDB 存储。

### 测试桥接和规则

使用 MQTTX 向 `t/opents` 主题发布一条消息:
```bash
mqttx pub -i emqx_c -t t/opents -m '{"metric":"cpu","tags":{"host":"serverA"},"value":12}'
```

查看 OpenTSDB 的数据桥接中的运行统计，命中、发送成功次数均 +1。

查看数据是否已经写入 OpenTSDB 中:

```bash
curl -X POST -H "Accept: Application/json" -H "Content-Type: application/json" http://localhost:4242/api/query -d '{
    "start": "1h-ago",
    "queries": [
        {
            "aggregator": "last",
            "metric": "cpu",
            "tags": {
                "host": "*"
            }
        }
    ],
    "showTSUIDs": "true",
    "showQuery": "true",
    "delete": "false"
}'
```

查询结果经格式化输出后如下：
```json
[
  {
    "metric": "cpu",
    "tags": {
      "host": "serverA"
    },
    "aggregateTags": [],
    "query": {
      "aggregator": "last",
      "metric": "cpu",
      "tsuids": null,
      "downsample": null,
      "rate": false,
      "filters": [
        {
          "tagk": "host",
          "filter": "*",
          "group_by": true,
          "type": "wildcard"
        }
      ],
      "percentiles": null,
      "index": 0,
      "rateOptions": null,
      "filterTagKs": [
        "AAAB"
      ],
      "explicitTags": false,
      "useFuzzyFilter": true,
      "preAggregate": false,
      "rollupUsage": null,
      "rollupTable": "raw",
      "showHistogramBuckets": false,
      "useMultiGets": true,
      "tags": {
        "host": "wildcard(*)"
      },
      "histogramQuery": false
    },
    "tsuids": [
      "000001000001000001"
    ],
    "dps": {
      "1683532519": 12
    }
  }
]% 
```