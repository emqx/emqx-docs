# Write MQTT Data into Dameng

[Dameng Database (DM8)](https://www.dameng.com/) is a large-scale general-purpose relational database widely used in government, finance, energy and telecommunication industries. EMQX supports integration with the Dameng database so that you can store MQTT messages and client events into a Dameng database for data pipelines, analytics, device connection management and system integration.

This page describes how to integrate EMQX with the Dameng database and provides practical guidance for creating rules and Sinks (actions).

::: tip

The Dameng Sink requires EMQX Enterprise 7.0 or later. The integration uses an ODBC driver, so unixODBC and the Dameng ODBC driver must be configured on the machine running EMQX (see below).

:::

## How it works

The Dameng data integration is an out-of-the-box EMQX feature that combines EMQX's device access and message transport capabilities with Dameng's data storage. Through the built-in [rule engine](./rules.md) and Sink, you can store MQTT messages and client events into Dameng.

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

### Create the target table

Run the following SQL in a DM8 database client before creating the action. Use a schema and user with permission to insert into the table.

```sql
CREATE TABLE SYSDBA.t_mqtt_msg (
    msgid VARCHAR(64),
    topic VARCHAR(255),
    qos INTEGER,
    payload VARCHAR(1024)
);
```

## Create the connector

For the REST API examples below, set `EMQX_API_KEY` and `EMQX_API_SECRET` to your EMQX API credentials.

Create a Dameng connector in EMQX:

```bash
curl -XPOST http://localhost:18083/api/v5/connectors \
  -u "$EMQX_API_KEY:$EMQX_API_SECRET" \
  -H "Content-Type: application/json" \
  -d '{
    "type": "dameng",
    "name": "dameng",
    "enable": true,
    "server": "192.168.1.10",
    "port": 5237,
    "username": "SYSDBA",
    "password": "your-password",
    "driver": "DM8 ODBC DRIVER",
    "charset": "utf8",
    "pool_size": 8,
    "resource_opts": {"health_check_interval": "20s"}
  }'
```

Main connection parameters:

| Parameter | Description |
| --- | --- |
| `server` | DM8 host (`host` or `host:port`). Required unless `dsn` is set. |
| `port` | DM8 port, default `5236`; used if `server` has no port. Ignored when `dsn` is set. |
| `username` / `password` | Login credentials. Without `dsn`, the username defaults to `SYSDBA`. With `dsn`, omit these fields to use the DSN credentials. |
| `driver` | ODBC driver name (e.g. `DM8 ODBC DRIVER`, requiring `/etc/odbcinst.ini`) or absolute driver `.so` path (e.g. `/opt/dmdbms/bin/libdodbc.so`). |
| `dsn` | Optional DSN in `odbc.ini`. Takes precedence over `server`, `port`, `driver`, and `charset`. |
| `charset` | Character set (passed to `Charset=`, e.g. `utf8`). |
| `pool_size` | Connection pool size, default `8`. |

There is no `database` connector parameter. Select the DM8 instance with `server`/`port` or `dsn`, and use a schema-qualified table name in the INSERT template.

## Create the action (rule Sink)

Create a Dameng action in EMQX:

```bash
curl -XPOST http://localhost:18083/api/v5/actions \
  -u "$EMQX_API_KEY:$EMQX_API_SECRET" \
  -H "Content-Type: application/json" \
  -d '{
    "type": "dameng",
    "name": "dameng",
    "connector": "dameng",
    "parameters": {
      "sql": "insert into SYSDBA.t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )",
      "undefined_vars_as_null": false
    },
    "resource_opts": {"batch_size": 100, "batch_time": "100ms", "query_mode": "sync"}
  }'
```

Action parameters:

| Parameter | Description |
| --- | --- |
| `sql` | Only an `INSERT` SQL template is accepted. Explicitly list the target columns and use `${...}` placeholders in `VALUES`. Column types are checked when the action is created. |
| `undefined_vars_as_null` | When `true`, unmatched variables are written as `null`; when `false` (default), they are treated as an error. |
| `resource_opts.batch_size` | Batch size, default `100`. |
| `resource_opts.batch_time` | Batch aggregation time, default `100ms`. |

### Supported SQL statements

Only `INSERT INTO ... (columns) VALUES (...)` action templates are supported in this release. The template must explicitly list the target columns, with one `${...}` placeholder per column in `VALUES`. Table and column names must be static. SQL keywords are case-insensitive.

`SELECT`, `UPDATE`, `DELETE`, `MERGE`, and other statement types are rejected when creating or updating an action, with the error `Only INSERT statements are supported`. `INSERT ... SELECT`, multiple statements, and `ON` clauses are also unsupported.

For both single-message and batched writes, EMQX binds message values separately from SQL. Quotes, backslashes, and SQL-looking payload text remain data, preventing SQL injection through message values.

The restriction applies to the Dameng action template. The rule engine still uses `SELECT` to select MQTT messages; the connector uses a fixed `SELECT 1` for health checks. You can run SQL directly in a database client to administer or inspect the database.

### SQL template example

```sql
insert into SYSDBA.t_mqtt_msg(msgid, topic, qos, payload)
values ( ${id}, ${topic}, ${qos}, ${payload} )
```

- `${id}`, `${topic}`, `${qos}`, `${payload}` are placeholders taken from the rule output fields.
- Both single-message and batched writes use parameterized queries (`odbc:param_query`). Do not manually escape message values or add quotes around placeholders.

## Create a rule

Create a rule in EMQX and select the `dameng` action to write matched messages into Dameng:

```bash
curl -XPOST http://localhost:18083/api/v5/rules \
  -u "$EMQX_API_KEY:$EMQX_API_SECRET" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "write to dameng",
    "sql": "SELECT * FROM \"t/#\"",
    "actions": ["dameng:dameng"]
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

- Character, numeric, boolean, and timestamp columns are supported. Binary, CLOB/NCLOB, and interval columns are rejected when creating the action. Values that exceed the declared column size or contain a NUL byte in character data are rejected. Encode binary data as text, such as Base64, and use a supported character column.
- The Dameng connection string supports both `DSN=` and field (connection-string) forms. In the field form, `Driver` can be a driver name or an absolute `.so` path.
- When using DSN/driver name, make sure `/etc/odbcinst.ini` and `/etc/odbc.ini` are correctly configured (the Erlang `odbcserver` reads `/etc/`).
- Batch writes rely on `odbc:param_query`; all rows must have the same number of columns.
- If the rule output fields do not match the target columns, set `undefined_vars_as_null: true` to fill missing fields with `null`.
