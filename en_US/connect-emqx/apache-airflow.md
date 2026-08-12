# Use Apache Airflow with EMQX

[Apache Airflow](https://airflow.apache.org/) is an open-source platform for authoring, scheduling, and monitoring data pipelines. It represents workflows as DAGs (Directed Acyclic Graphs), which are Python scripts that define tasks and their execution order.

EMQX and Airflow serve different layers of an IoT architecture. EMQX handles real-time device connectivity and message routing; Airflow orchestrates the batch and analytical pipelines that process the data EMQX produces. They do not integrate directly. The connection always flows through an intermediate storage system.

This page covers two primary integration patterns:

- [Via Amazon S3 or compatible object storage](#via-amazon-s3-or-compatible-object-storage)
- [Via a relational or analytical database](#via-a-relational-or-analytical-database)

## Prerequisites

Before setting up either integration pattern:

- A running EMQX deployment with the Rule Engine enabled (EMQX Enterprise or [EMQX Cloud](https://docs.emqx.com/en/cloud/latest/))
- An Apache Airflow environment (version 2.x recommended)
- The relevant Airflow provider package is installed:
  - S3 pattern: `apache-airflow-providers-amazon`
  - PostgreSQL pattern: `apache-airflow-providers-postgres`
  - ClickHouse pattern: `apache-airflow-providers-common-sql` and a ClickHouse driver
- Network access from Airflow workers to the intermediate storage system (S3 bucket or database)

## How It Works

EMQX receives MQTT messages from devices and uses the Rule Engine to route and persist data. Airflow then picks up that data on a schedule from the storage layer to run transformations, aggregations, and loads.

```
IoT Devices
    │ MQTT
    ▼
EMQX Broker
    │ Rule Engine
    ├─────────────────────────┐
    ▼                         ▼
Amazon S3              PostgreSQL /
(Parquet / JSON)       ClickHouse
    │                         │
    └────────────┬────────────┘
                 ▼
         Apache Airflow
        (Scheduled DAGs)
                 │
    ┌────────────┼────────────┐
    ▼            ▼            ▼
Data         ML Model      Reports /
Warehouse    Training      Alerts
```

EMQX owns the real-time layer. Airflow owns the batch and analytical layer. The handoff always happens at a persistent storage system, never through a direct connection between the two.

## Via Amazon S3 or Compatible Object Storage

This pattern works well for bulk telemetry archiving, ML training pipelines, and analytics workflows that run on a schedule rather than in real time.

### How EMQX Writes to S3

Configure an S3 Sink in the EMQX Rule Engine to write incoming MQTT messages to an S3 bucket. EMQX supports Amazon S3, MinIO, and any S3-compatible object storage.

Refer to [Ingest MQTT Data into Amazon S3](../data-integration/s3.md) for setup instructions.

A typical sink configuration writes messages as JSON Lines or Parquet files under a time-partitioned prefix, for example:

```
s3://your-bucket/telemetry/year=2024/month=04/day=23/data.parquet
```

### How Airflow Reads from S3

On the Airflow side, use an `S3KeySensor` to wait for files to land, then process them with a `PythonOperator`.

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
    # transform, aggregate, or load df as needed

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

### When to Use This Pattern

- You need long-term archiving of device telemetry at low cost.
- Your analytics workload is batch-oriented and does not require sub-minute latency.
- You use a data lake or warehouse (Snowflake, Redshift, BigQuery) as the downstream target.

## Via a Relational or Analytical Database

This pattern is the simplest to set up and works well when EMQX already sinks data into a database for operational use. Airflow becomes one more consumer of that database.

### How EMQX Writes to a Database

Configure a database Sink in the EMQX Rule Engine. EMQX supports PostgreSQL, MySQL, ClickHouse, TimescaleDB, and others.

Refer to the relevant sink documentation:

- [Ingest MQTT Data into PostgreSQL](../data-integration/data-bridge-pgsql.md)
- [Ingest MQTT Data into ClickHouse](../data-integration/data-bridge-clickhouse.md)
- [Ingest MQTT Data into TimescaleDB](../data-integration/data-bridge-timescale.md)

### How Airflow Reads from a Database

Use `SQLExecuteQueryOperator` (or a database-specific operator) to run scheduled queries against the EMQX-populated table.

The following example runs a nightly aggregation on a ClickHouse table and writes the results to a summary table:

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

### When to Use This Pattern

- EMQX already writes to a database for real-time queries, and you want to reuse that data for scheduled analytics.
- You need Airflow to run SQL-based aggregations, data quality checks, or reporting jobs.
- You prefer minimal moving parts with no file storage layer to manage.

## Other Integration Options

### Via Apache Kafka

If your architecture includes a Kafka broker, EMQX can bridge messages to a Kafka topic using the [Kafka Sink](../data-integration/data-bridge-kafka.md). Airflow can then consume from that topic using a `PythonOperator` with a Kafka client library.

This approach is a micro-batch pattern: Airflow reads a bounded window of messages per DAG run. It is more complex to operate than the S3 or database patterns due to offset management, and is best suited to teams that already have Kafka in their stack.

### Via EMQX REST API

Airflow can call the [EMQX REST API](https://docs.emqx.com/en/emqx/latest/admin/api.html) on a schedule using an `HttpOperator` or `PythonOperator`. This is useful for generating scheduled operational reports, for example, polling `/api/v5/stats` or `/api/v5/clients` to collect broker metrics and connection counts.

This is not a data integration pattern; it is an operational monitoring pattern. It does not replace a proper observability setup (Prometheus + Grafana).
