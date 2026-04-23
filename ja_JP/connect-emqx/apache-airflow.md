# 使用 Apache Airflow 与 EMQX 集成

[Apache Airflow](https://airflow.apache.org/) 是一款开源的数据管道编排平台，支持以代码方式编写、调度和监控工作流。它将工作流定义为 DAG（有向无环图），即描述任务及其执行顺序的 Python 脚本。

EMQX 与 Airflow 分别服务于物联网架构的不同层次：EMQX 负责实时设备连接与消息路由，Airflow 则编排对 EMQX 所产生数据进行处理的批处理与分析管道。两者不直接集成，连接始终通过中间存储系统完成。

本页面介绍以下两种主要集成模式：

- [通过 Amazon S3 或兼容对象存储](#通过-amazon-s3-或兼容对象存储)
- [通过关系型或分析型数据库](#通过关系型或分析型数据库)

## 前提条件

开始配置集成前，请确保满足以下条件：

- 已部署并启用规则引擎的 EMQX 实例（EMQX Enterprise 或 [EMQX Cloud](https://docs.emqx.com/zh/cloud/latest/)）
- 已部署的 Apache Airflow 环境（推荐 2.x 版本）
- 已安装对应的 Airflow Provider 包：
  - S3 模式：`apache-airflow-providers-amazon`
  - PostgreSQL 模式：`apache-airflow-providers-postgres`
  - ClickHouse 模式：`apache-airflow-providers-common-sql` 及 ClickHouse 驱动
- Airflow Worker 可访问中间存储系统（S3 存储桶或数据库）

## 工作原理

EMQX 接收设备发布的 MQTT 消息，并通过规则引擎对数据进行路由和持久化存储。Airflow 随后按计划从存储层获取数据，执行转换、聚合和加载操作。

```
物联网设备
    │ MQTT
    ▼
EMQX Broker
    │ 规则引擎
    ├─────────────────────────┐
    ▼                         ▼
Amazon S3              PostgreSQL /
（Parquet / JSON）      ClickHouse
    │                         │
    └────────────┬────────────┘
                 ▼
         Apache Airflow
        （定时调度 DAG）
                 │
    ┌────────────┼────────────┐
    ▼            ▼            ▼
数据仓库       模型训练      报表/告警
```

EMQX 负责实时层，Airflow 负责批处理与分析层。数据交接始终发生在持久化存储系统，而非两者之间的直接连接。

## 通过 Amazon S3 或兼容对象存储

该模式适用于设备遥测数据的批量归档、机器学习训练管道，以及按计划而非实时运行的分析工作流。

### EMQX 如何写入 S3

在 EMQX 规则引擎中配置 S3 Sink，将接收到的 MQTT 消息写入 S3 存储桶。EMQX 支持 Amazon S3、MinIO 及任何兼容 S3 协议的对象存储。

详细配置步骤，请参阅[将 MQTT 数据写入 Amazon S3](../data-integration/s3.md)。

典型的 Sink 配置会将消息以 JSON Lines 或 Parquet 格式写入按时间分区的路径下，例如：

```
s3://your-bucket/telemetry/year=2024/month=04/day=23/data.parquet
```

### Airflow 如何从 S3 读取数据

在 Airflow 侧，使用 `S3KeySensor` 等待文件到达，再通过 `PythonOperator` 进行处理。

```python
from airflow import DAG
from airflow.providers.amazon.aws.sensors.s3 import S3KeySensor
from airflow.operators.python import PythonOperator
from datetime import datetime
import boto3
import pandas as pd

def process_telemetry(**context):
    s3 = boto3.client("s3")
    obj = s3.get_object(
        Bucket="your-bucket",
        Key=f"telemetry/year={context['ds'][:4]}/month={context['ds'][5:7]}/day={context['ds'][8:10]}/data.parquet"
    )
    df = pd.read_parquet(obj["Body"])
    # 在此对 df 进行转换、聚合或加载操作

with DAG(
    dag_id="emqx_s3_pipeline",
    start_date=datetime(2024, 1, 1),
    schedule_interval="@daily",
    catchup=False,
) as dag:

    wait_for_file = S3KeySensor(
        task_id="wait_for_telemetry_file",
        bucket_name="your-bucket",
        bucket_key="telemetry/year={{ ds[:4] }}/month={{ ds[5:7] }}/day={{ ds[8:10] }}/data.parquet",
        aws_conn_id="aws_default",
        timeout=3600,
        poke_interval=60,
    )

    process = PythonOperator(
        task_id="process_telemetry",
        python_callable=process_telemetry,
    )

    wait_for_file >> process
```

### 适用场景

- 需要以低成本对设备遥测数据进行长期归档。
- 分析工作负载以批处理为主，对延迟要求不高。
- 下游目标为数据湖或 数据仓库 （如 Snowflake、Redshift、BigQuery）。

## 通过关系型或分析型数据库

该模式配置最为简单，适用于 EMQX 已将数据写入数据库用于实时查询的场景。Airflow 作为该数据库的另一个消费方接入即可。

### EMQX 如何写入数据库

在 EMQX 规则引擎中配置数据库 Sink。EMQX 支持 PostgreSQL、MySQL、ClickHouse、TimescaleDB 等多种数据库。

请参阅对应的 Sink 文档：

- [将 MQTT 数据写入 PostgreSQL](../data-integration/data-bridge-pgsql.md)
- [将 MQTT 数据写入 ClickHouse](../data-integration/data-bridge-clickhouse.md)
- [将 MQTT 数据写入 TimescaleDB](../data-integration/data-bridge-timescale.md)

### Airflow 如何从数据库读取数据

使用 `SQLExecuteQueryOperator`（或特定数据库的 Operator）对 EMQX 写入的表执行定时查询。

以下示例对 ClickHouse 表执行每日聚合，并将结果写入汇总表：

```python
from airflow import DAG
from airflow.providers.common.sql.operators.sql import SQLExecuteQueryOperator
from datetime import datetime

AGGREGATE_SQL = """
INSERT INTO device_daily_summary
SELECT
    device_id,
    toDate(timestamp)       AS report_date,
    avg(temperature)        AS avg_temp,
    max(temperature)        AS max_temp,
    count()                 AS reading_count
FROM device_telemetry
WHERE toDate(timestamp) = yesterday()
GROUP BY device_id, report_date
"""

with DAG(
    dag_id="emqx_clickhouse_pipeline",
    start_date=datetime(2024, 1, 1),
    schedule_interval="@daily",
    catchup=False,
) as dag:

    aggregate = SQLExecuteQueryOperator(
        task_id="aggregate_daily_telemetry",
        conn_id="clickhouse_default",
        sql=AGGREGATE_SQL,
    )
```

### 适用场景

- EMQX 已将数据写入数据库用于实时查询，希望复用该数据进行定时分析。
- 需要 Airflow 执行基于 SQL 的聚合、数据质量检查或报表任务。
- 希望减少系统组件，无需管理文件存储层。

## 其他集成方式

### 通过 Apache Kafka

若架构中包含 Kafka，可使用 [Kafka Sink](../data-integration/data-bridge-kafka.md) 将 EMQX 消息桥接至 Kafka Topic，Airflow 再通过带有 Kafka 客户端库的 `PythonOperator` 进行消费。

该方式为微批处理模式：Airflow 在每次 DAG 运行中读取固定窗口内的消息。相较于 S3 和数据库模式，此方式因需要管理消费偏移量而更加复杂，适合已有 Kafka 基础设施的团队。

### 通过 EMQX REST API

Airflow 可使用 `HttpOperator` 或 `PythonOperator` 定时调用 [EMQX REST API](https://docs.emqx.com/zh/emqx/latest/admin/api.html)，例如轮询 `/api/v5/stats` 或 `/api/v5/clients` 以采集 Broker 指标和连接数，用于生成定时运营报表。

该方式属于运维监控模式，而非数据集成模式，不能替代完整的可观测性方案（如 Prometheus + Grafana）。
