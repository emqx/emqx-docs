# Write MQTT Data into Dameng

[Dameng Database (DM8)](https://www.dameng.com/) is a large-scale general-purpose relational database widely used in government, finance, energy and telecommunication industries. EMQX supports integration with the Dameng database so that you can store MQTT messages and client events into a Dameng database for data pipelines, analytics, device connection management and system integration.

This page describes how to integrate EMQX with the Dameng database and provides practical guidance for creating rules and Sinks (actions).

::: tip

The Dameng Sink is supported starting from EMQX 6.x. The integration uses an ODBC driver, so unixODBC and the Dameng ODBC driver must be configured on the machine running EMQX (see below).

:::

## How it works

The Dameng data integration is an out-of-the-box EMQX feature that combines EMQX's device access and message transport capabilities with Dameng's data storage. Through the built-in [rule engine](./rules.md) and Sink, you can store MQTT messages and client events into Dameng, or update/delete data on events.

The workflow for ingesting MQTT data into Dameng:

1. **Publish & receive**: industrial IoT devices connect to EMQX over MQTT and publish real-time data. When EMQX receives a message, the rules engine starts the matching process.
2. **Message processing**: the message is processed by the rules; rules decide which messages are routed to Dameng, and apply any payload transformations.
3. **Write into Dameng**: the rule triggers the action that writes the message into Dameng. Using an SQL template, data from the rule result is extracted to build SQL, which is sent to Dameng via ODBC and written into the corresponding table and columns.
4. **Storage & usage**: data is now stored in Dameng and can be used by the enterprise.

## Features and benefits

- **Real-time data stream**: EMQX is built for real-time data streams, ensuring efficient and reliable transport from source to Dameng.
- **High performance & scalability**: both EMQX and Dameng are scalable and reliable for large-scale IoT data.
- **Flexible data transformation**: EMQX's SQL-based rules engine allows preprocessing before storing into Dameng.
- **Batch write**: the integration uses `odbc:param_query` for batched parameterized writes, greatly reducing network round-trips and database overhead.

## Pre-requisites

This section describes what is needed before creating a Dameng integration in EMQX, including installing and configuring the ODBC driver, setting up the Dameng server, and creating the database/table.

### Prerequisites

- Understand [rules](./rules.md).
- Understand [data integration](./data-bridges.md).

### Install and configure the ODBC driver

To access a Dameng database you need to install and configure unixODBC and the Dameng ODBC driver on the machine that runs EMQX.

::: tip Important

EMQX uses the Erlang/OTP `odbc` application to connect to Dameng. The `odbcserver` port program reads ODBC configuration from **`/etc/`** (not `/usr/local/etc/`). Therefore **`odbcinst.ini` and `odbc.ini` must be placed in `/etc/`** (or symlinked there), otherwise `DSN=`/driver name resolution fails.

:::

1. Install unixODBC and the Dameng ODBC driver (the Dameng install path is `/opt/dmdbms`; the driver is `/opt/dmdbms/bin/libdodbc.so`).
2. Edit `/etc/odbcinst.ini` and add the Dameng driver:
   ```
   [DM8 ODBC DRIVER]
   Description = ODBC DRIVER FOR DM8
   Driver = /opt/dmdbms/bin/libdodbc.so
   ```
3. Edit `/etc/odbc.ini` to configure the data source (DSN):
   ```
   [dm8]
   Description = DM ODBC DSN
   Driver = DM8 ODBC DRIVER
   SERVER = 192.168.1.10
   UID = SYSDBA
   PWD = your-password
   TCP_PORT = 5237
   ```
   ::: tip
   Alternatively, skip the DSN and set `driver` (driver name or absolute `.so` path) together with `server/port/username/password` in the connector (field connection-string form).
   :::
4. Verify connectivity: `odbcinst -j` to confirm unixODBC and the driver; run `isql dm8 SYSDBA your-password` and execute `select 1`.

## Create the connector

Create a Dameng connector in EMQX:

```bash
curl -XPOST http://localhost:18083/api/v5/connectors \
  -d '{
    "type": "dameng",
    "name": "dameng",
    "enable": true,
    "server": "192.168.1.10",
    "port": 5237,
    "username": "SYSDBA",
    "password": "your-password",
    "driver": "DM8 ODBC DRIVER",
    "database": "DM8",
    "charset": "utf8",
    "pool_size": 8,
    "resource_opts": {"health_check_interval": "20s"}
  }'
```

Main connection parameters:

| Parameter | Description |
| --- | --- |
| `server` | Dameng host (`host` or `host:port`). |
| `port` | Dameng port, default `5236`. |
| `username` / `password` | Login user (default `SYSDBA`) and password. |
| `driver` | ODBC driver name (e.g. `DM8 ODBC DRIVER`, requiring `/etc/odbcinst.ini`) or absolute driver `.so` path (e.g. `/opt/dmdbms/bin/libdodbc.so`). |
| `dsn` | Optional; use `DSN=<name>` direct connection when `/etc/odbc.ini` defines a DSN. |
| `database` | Database name (passed to the ODBC `Database=` attribute, e.g. `DM8`). |
| `charset` | Character set (passed to `Charset=`, e.g. `utf8`). |
| `pool_size` | Connection pool size, default `8`. |

## Create the action (rule Sink)

Create a Dameng action in EMQX:

```bash
curl -XPOST http://localhost:18083/api/v5/actions \
  -d '{
    "type": "dameng",
    "name": "dameng",
    "parameters": {
      "connector": "dameng",
      "sql": "insert into SYSDBA.t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )",
      "undefined_vars_as_null": false
    },
    "resource_opts": {"batch_size": 100, "batch_time": "100ms", "query_mode": "sync"}
  }'
```

Action parameters:

| Parameter | Description |
| --- | --- |
| `sql` | Insert SQL template (placeholder `${...}` from the rule result). **The INSERT must explicitly list the columns**; the connector probes the table with `describe_table` at action-creation time. |
| `undefined_vars_as_null` | When `true`, unmatched variables are written as `null`; when `false` (default), they are treated as an error. |
| `resource_opts.batch_size` | Batch size, default `100`. |
| `resource_opts.batch_time` | Batch aggregation time, default `100ms`. |

### SQL template example

```sql
insert into SYSDBA.t_mqtt_msg(msgid, topic, qos, payload)
values ( ${id}, ${topic}, ${qos}, ${payload} )
```

- `${id}`, `${topic}`, `${qos}`, `${payload}` are placeholders taken from the rule output fields.
- Batch writes use parameterized queries (`odbc:param_query`); no manual string escaping required.

## Create a rule

Create a rule in EMQX and select the `dameng` action to write matched messages into Dameng:

```bash
curl -XPOST http://localhost:18083/api/v5/rules \
  -d '{
    "name": "write to dameng",
    "sql": "SELECT * FROM \"t/#\"",
    "actions": [{"function": "dameng:dameng"}]
  }'
```

## Example

Publish a message to the matched topic and data is written into Dameng:

```bash
mosquitto_pub -t 't/1' -m 'hello dameng'
```

Then query the Dameng table:

```sql
SELECT * FROM SYSDBA.t_mqtt_msg;
```

## Notes

- The Dameng connection string supports both `DSN=` and field (connection-string) forms. In the field form, `Driver` can be a driver name or an absolute `.so` path.
- When using DSN/driver name, make sure `/etc/odbcinst.ini` and `/etc/odbc.ini` are correctly configured (the Erlang `odbcserver` reads `/etc/`).
- Batch writes rely on `odbc:param_query`; all rows must have the same number of columns.
- If the rule output fields do not match the target columns, set `undefined_vars_as_null: true` to fill missing fields with `null`.
