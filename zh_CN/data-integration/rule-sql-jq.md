# JQ 函数

[JQ](https://stedolan.github.io/jq/) 是一款功能强大的命令行工具和编程语言，主要用于转换和查询 [JSON](https://www.json.org/json-en.html) 编码的消息。

对于规则 SQL 及其内置函数很难或无法实现的 JSON 消息处理，使用 JQ 函数可以很方便地实现。

## 如何使用

EMQX 规则 SQL 通过以下函数集成了 JQ：

| 函数 | 参数                                                                                                                  | 返回值                                                                                                                                           |
| ------ | --------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| `jq`   | 1. 有效的 jq 程序字符串 <br/>2. JSON 编码的字符串或对象 <br/>3.执行超时（可选，单位为毫秒，默认为 10 秒）<br/> | 返回值为执行生成生成的 JSON 对象列表。如果执行超时或者 JQ 程序抛出异常，该函数将抛出异常。 |

> 默认执行超时可通过 `rule_engine.jq_function_default_timeout` 配置。

JQ 同时也是一个图灵完备的编程语言，JQ 官方文档](https://stedolan.github.io/jq/manual/) 是学习如何编写 JQ 的最佳指南，您也可以[在 JQ Playground](https://jqplay.org/) 或本地安装后进行学习与测试。

以下是一些简单的 `jq` 函数调用及其结果的示例。

### 示例 1

简单处理示例：

```SQL
jq('.', '{"temprature": 10}') = [json_decode('{"temprature": 10}')]

jq('.', json_decode('{"temprature": 10}')) = [json_decode('{"temprature": 10}')]

jq('.temprature', '{"temprature": 10}') = [10]

jq('{temprature_C:.temprature,temprature_F: (.temprature * 1.8 + 32)}', '{"temprature": 10}') = [json_decode('{"temprature_C": 10, "temprature_F": 50}')]

jq('.temprature,(.temprature * 1.8 + 32)', '{"temprature": 10}') = [10, 50]
```

### 示例 2

上面的例子只是浅尝辄止，展示了 JQ 能做什么。

下面提供了一个更复杂的 JQ 程序的示例，展示了如何将 `jq` 函数与 `FOREACH` 语句结合起来，将 JQ 的输出分成多个消息：

```SQL
FOREACH   jq('def rem_first: ' +
             '    if length > 2 then del(.[0]) else . end;' +
             'def rem_last:' +
             '    if length > 1 then del(.[-1]) else . end;' +
             '.date as $date |' +
             '.sensors[] |' +
             '  (.data | sort | rem_first | rem_last | add / length) as $average |' +
             '  {$average, $date}',
             payload)
FROM    "jq_demo/complex_rule/jq/#"
```

对应的消息 `payload` 的示例：

```json
{
  "date": "2020-04-24",
  "sensors": [
    {
      "name": "a",
      "data": [3,1,2,4,5,5]
    },
    {
      "name": "b",
      "data": [1,-100,2,3,4,5,2000]
    },
    {
      "name": "c",
      "data": [3, 7, 9]
    }
  ]
}

```

上面的规则 SQL 片段将为输入数据中的每个传感器创建一条输出消息。每个消息都是一个 JSON 对象，其中包含一个日期字段，以及一个去掉最大值和最小值之后的传感器数据的平均值的字段（假定它们可能是异常值）。

因此，三条输出消息的 payload 分别为：

消息 1:

```json
{
  "average": 3.5,
  "date": "2020-04-24"
}
```

消息 2:

```json
{
  "average": 3,
  "date": "2020-04-24"
}
```

消息 3:

```json
{
  "average": 7,
  "date": "2020-04-24"
}
```



## 注意事项

一般情况下 JQ 程序仅用来做 JSON 数据的简单转换或者过滤，如果有必要，也可以使用 JQ 执行复杂的计算。
但是，不建议在规则中执行长时间运行的计算，因为这会大大降低 EMQX 处理新消息的速度。

EMQX 提供了 JQ 超时配置，防止由于错误的 JQ 程序（如陷入死循环）占用 EMQX 运行资源，进而影响您的业务。

