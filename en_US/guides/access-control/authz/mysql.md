# Integrate with MySQL

This authorizer implements authorization checks by matching publish/subscription requests against lists of rules stored in the MySQL database.

::: tip Prerequisite

Knowledge about [basic EMQX authorization concepts](./authz.md)


:::

## Data Schema and Query Statement

MySQL authorizer supports almost any storage schema. You can determine how to store credentials and access them as your business needs, for example, using one or multiple tables, views, etc.

Users need to provide a query statement template and ensure the following fields are included:
* `permission` value specifies the applied action if the rule matches. It should be one of `deny` or `allow`.
* `action` value specifies the request for which the rule is relevant. Should be one of `publish`, `subscribe`, or `all`.
* `topic` value specifies the topic filter for topics relevant to the rule. Should be a string that supports wildcards and [topic placeholders](./authz.md#topic-placeholders).
* `qos` (Optional) value specifies the QoS levels that the rule applies to. Value options are `0`, `1`, `2`. It can also be a string separated by `,` to specify multiple QoS levels, e.g. `0,1`. The default is all QoS levels.
* `retain` (Optional) value specifies whether the current rule supports retained messages. Value options are `0` and `1`. The default is to allow retained messages.

Example table structure for storing credentials:

```sql
CREATE TABLE `mqtt_acl` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `ipaddress` VARCHAR(60) NOT NULL DEFAULT '',
  `username` VARCHAR(255) NOT NULL DEFAULT '',
  `clientid` VARCHAR(255) NOT NULL DEFAULT '',
  `action` ENUM('publish', 'subscribe', 'all') NOT NULL,
  `permission` ENUM('allow', 'deny') NOT NULL,
  `topic` VARCHAR(255) NOT NULL DEFAULT '',
  `qos` tinyint(1),
  `retain` tinyint(1),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
```

::: tip
When there is a significant number of users in the system, optimize and index the tables to be queried beforehand to shorten the query response time and reduce the load for EMQX.
:::

In this table, MQTT users are identified by `username`.

For example, if you want to add an authorization rule for a user `user123` who is allowed to publish topics `data/user123/#`, the query statement should be:

```bash
mysql> INSERT INTO mqtt_acl(username, permission, action, topic, ipaddress) VALUES ('user123', 'allow', 'publish', 'data/user123/#', '127.0.0.1');
Query OK, 1 row affected (0,01 sec)
```

The corresponding configuration parameters are:
```bash
query = "SELECT action, permission, topic, ipaddress, qos, retain FROM mqtt_acl where username = ${username} and ipaddress = ${peerhost}"
```

## Configure with Dashboard

You can use EMQX Dashboard to configure how to use MySQL for user authorization.

1. On [EMQX Dashboard](http://127.0.0.1:18083/#/authentication), click **Access Control** -> **Authorization** on the left navigation tree to enter the **Authorization** page. 

2. Click **Create** at the top right corner, then click to select **MySQL** as **Backend**. Click **Next**. The **Configuration** tab is shown below.

   <img src="./assets/authz-MySQL_ee.png" alt="authz-MySQL_ee" style="zoom:67%;" />

3. Follow the instructions below to configure the authorization backend:

   - Enter the information for connecting to MySQL.

     - **Server**: Specify the server address that EMQX is to connect (`host:port`).
     - **Database**: MySQL database name.
     - **Username**: Specify user name.
     - **Password**: Specify user password.

   - **Precondition**: Enter an optional Variform expression. EMQX invokes this authorizer only when the expression evaluates to `true`. For details, see [Authorizer Preconditions](./authz.md#authorizer-preconditions).

   - **Enable TLS**: Turn on the toggle switch if you want to enable TLS. For more information on enabling TLS, see [Network and TLS](../../network/overview.md#tls-for-external-resource-access).

   - **SQL**: Fill in the query statement according to the data schema. For more information, see [Data Schema and Query Statement](#data-schema-and-query-statement).

   - **Advanced Settings**: Configure connection pool, timeout, and prepared statement behavior.
     - **Connection Pool Size** (optional): Input an integer value to define the number of concurrent connections from an EMQX node to MySQL. Default: `8`.
     - **Connect Timeout** (optional): Specify the waiting period before EMQX assumes the connection attempt has timed out. Units supported include milliseconds, second, minute, and hour. Default: `15` seconds.
     - **Disable Prepared Statements** (optional): Disable the use of prepared statements for database queries. Enable this option if your MySQL proxy or middleware (for example, PGBouncer or Supabase in Transaction mode) does not support session-level features such as prepared statements. Default: disabled.

4. Click **Create** to finish the settings.

## Configure with Configuration Items

You can configure the EMQX MySQL authorizer with EMQX configuration items.

The MySQL authorizer is identified by type `mysql`. For a full list of configuration parameters, see the [EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

The optional `precondition` configuration item accepts a Variform expression. EMQX invokes this authorizer only when the expression evaluates to `true`. If `precondition` is omitted or empty, no precondition is applied. For details, see [Authorizer Preconditions](./authz.md#authorizer-preconditions).

Sample configuration:

```bash
{
  type = mysql

  database = "mqtt"
  username = "root"
  password = "public"
  server = "127.0.0.1:3306"
  query = "SELECT permission, action, topic FROM mqtt_acl WHERE username = ${username}"
  connect_timeout = "15s"
  disable_prepared_statements = false
}
```
