# 将 MQTT 数据写入到 OpenTSDB

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

[OpenTSDB](http://opentsdb.net/) 是一个可扩展的分布式时间序列数据库。EMQX 支持与 OpenTSDB 集成，因此可以将 MQTT 消息保存到 OpenTSDB 以便后续进行分析和检索。

本页详细介绍了 EMQX 与 OpenTSDB 的数据集成并提供了实用的规则和数据桥接创建指导。

## 工作原理

OpenTSDB 数据集成是 EMQX 的开箱即用功能，结合了 EMQX 的实时数据捕获和传输能力以及 OpenTSDB 的数据存储和分析功能。通过内置的[规则引擎](./rules.md)组件，集成简化了从 EMQX 到 OpenTSDB 的数据摄取过程，无需复杂编码。

EMQX 通过规则引擎和数据桥接将设备数据插入到 OpenTSDB。OpenTSDB 提供丰富的查询功能，支持生成报告、图表和其他数据分析结果。以工业能耗管理场景为例，工作流程如下：

1. **消息发布和接收**：工业设备通过 MQTT 协议成功连接到 EMQX，并定期使用 MQTT 协议发布能耗数据。这些数据包括生产线标识符和能耗值。当 EMQX 接收到这些消息时，它将在其规则引擎中启动匹配过程。
2. **规则引擎处理消息**：内置的规则引擎根据主题匹配处理来自特定来源的消息。当消息到达时，它通过规则引擎进行匹配，规则引擎将处理消息数据。这可能包括转换数据格式、过滤特定信息或用上下文信息丰富消息。
3. **数据写入到 OpenTSDB**：规则引擎中定义的规则触发操作将消息写入 OpenTSDB。

在数据写入 OpenTSDB 后，你可以灵活地使用数据，例如：

- 连接到如 Grafana 等可视化工具生成基于数据的图表，显示能源存储数据。
- 连接到业务系统以监控和警报能源存储设备的状态。

## 特性与优势

在 EMQX 中使用 OpenTSDB 数据桥接能够为您的业务带来以下特性与优势：

- **高效数据处理**：EMQX 能够处理大量物联网设备连接和消息吞吐量，而 OpenTSDB 在数据写入、存储和查询方面表现出色，提供出色的性能以满足物联网场景的数据处理需求，不会给系统带来过重负担。
- **消息转换**：消息可以在写入 OpenTSDB 之前通过 EMQX 规则进行广泛的处理和转换。
- **大规模数据存储**: 通过将 EMQX 与 OpenTSDB 集成，可以将海量设备数据直接存储到 OpenTSDB 中。OpenTSDB 是为存储和查询大规模时间序列数据而设计的数据库，能够高效地处理物联网设备产生的海量时间序列数据。
- **丰富的查询能力**: OpenTSDB 优化过存储结构和索引能够实现数十亿个数据点快速写入和查询，这对于需要对物联网设备数据进行实时监控、分析和可视化的应用场景非常有益。
- **可扩展性**：EMQX 和 OpenTSDB 均能够实现集群扩展，随着业务需求的增长允许灵活的水平扩展集群。

## 桥接准备

本节介绍了在 EMQX 中创建 MySQL 数据桥接之前需要做的准备工作，包括如何设置 OpenTSDB 服务器。

### 前置准备

- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

### 安装 OpenTSDB

通过 Docker 安装并启动 OpenTSDB：

```bash
docker pull petergrace/opentsdb-docker

docker run -d --name opentsdb -p 4242:4242 petergrace/opentsdb-docker

```

## 创建 OpenTSDB 桥接

本节演示了如何在 Dashboard 中创建 OpenTSDB 数据桥接。以下示例假定您在本地机器上同时运行 EMQX 和 OpenTSDB。如果您在远程运行 OpenTSDB 和 EMQX，请相应地调整设置。

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

## 创建 OpenTSDB 数据桥接规则

至此您已经完成数据桥接创建，接下来将继续创建一条规则来指定需要写入的数据。以下数据将用于演示：

- 主题: `t/opents`
- payload:

```json
{
  "metric": "cpu",
  "tags": {
    "host": "serverA"
  },
  "value":12
}
```

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

## 测试桥接和规则

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