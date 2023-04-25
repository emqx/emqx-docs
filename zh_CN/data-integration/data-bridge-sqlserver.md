# SQL Server

通过 SQL Server 数据桥接可以将客户端消息和事件存储到 SQL Server 中，也可以通过事件触发对 SQL Server 中数据的更新或删除操作，从而实现对诸如设备在线状态、上下线历史等的记录。

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

:::tip 前置准备

- 了解 [规则](./rules.md)。
- 了解 [数据桥接](./data-bridges.md)。

:::

## 功能清单

- [连接池](./data-bridges.md#连接池)
- [批量模式](./data-bridges.md#批量模式)
- [缓存队列](./data-bridges.md#缓存队列)

<!-- TODO 配置参数 需要补充链接到配置手册对应配置章节。 -->

## 快速开始

本节介绍如何配置 SQL Server 数据桥接，包括如何设置 SQL Server 服务器、创建数据桥接和转发数据到 SQL Server 的规则以及测试数据桥接和规则等主题。

本教程假设您在本地机器上同时运行 EMQX 和 SQL Server。 如果您有远程运行的 EMQX 和 SQL Server ，请相应地调整设置。

### 安装并连接到 SQL Server

本节描述如何使用 Docker 镜像在 Linux/MacOS 安装启动 SQL Server 2019 以及使用 `sqlcmd` 连接到 SQL Server 并创建数据库与数据表。关于其他 SQL Server 的安装方式，请参阅微软提供的 [SQL Server 安装指南](https://learn.microsoft.com/zh-cn/sql/database-engine/install-windows/install-sql-server?view=sql-server-ver16)。

1. 通过 Docker 安装并启动 SQL Server。

   SQL Server 要求使用复杂密码，请参阅 [使用复杂密码](https://learn.microsoft.com/zh-cn/sql/relational-databases/security/password-policy?view=sql-server-ver16#password-complexity)。
   使用环境变量 `ACCEPT_EULA=Y` 启动 Docker 容器代表您同意 Microsoft 的 EULA 条款，详情请参阅 [MICROSOFT 软件许可条款 MICROSOFT SQL SERVER 2019 STANDARD(ZH_CN)](https://www.microsoft.com/en-us/Useterms/Retail/SQLServerStandard/2019/Useterms_Retail_SQLServerStandard_2019_ChineseSimplified.htm)。

```bash
# 启动一个 SQL Server 容器并设置密码为 `mqtt_public1`
$ docker run --name sqlserver -p 1433:1433 -e ACCEPT_EULA=Y -e SQLSERVER_ROOT_PASSWORD=mqtt_public1 -d mcr.microsoft.com/mssql/server:2019-CU19-ubuntu-20.04
```

2. 进入容器。

```bash
$ docker exec -it sqlserver bash
```

3. 在容器中连接到 SQL Server 服务器，需要输入预设的密码。

   - Microsoft 提供的 SQL Server 容器内已安装 `mssql-tools`，但可执行文件并不在 `$PATH` 中，因此您需要指定可执行文件路径。关于更多 `mssql-tools` 的使用，请参与 Microsoft 提供的相关文档： [sqlcmd 实用工具](https://learn.microsoft.com/zh-cn/sql/tools/sqlcmd/sqlcmd-utility?view=sql-server-ver16)。

   - 因为安全原因，输入密码时字符不会回显。请输入密码后直接键入 `Enter` 。

```bash
$ /opt/mssql-tools/bin/sqlcmd -S 127.0.0.1 -U sa
$ Password:
1>
```

至此 SQL Server 2019 实例已经完成部署并可以连接。

### 创建数据库和数据表

1. 使用已创建的连接在 SQL Server 中创建数据库 `mqtt`。

   ```bash
   ...
   Password:
   1> USE master
   2> GO
   Changed database context to 'master'.
   1> IF NOT EXISTS(SELECT name FROM sys.databases WHERE name = 'mqtt') BEGIN CREATE DATABASE mqtt END
   2> GO
   ```


2. 使用下面的 SQL 语句在此数据库中创建一个数据表，以用于接下来的 MQTT 消息存储。该表存储每条消息的 MsgID、主题、QoS、Payload 以及发布时间。

   ```sql
   CREATE TABLE mqtt.dbo.t_mqtt_msg (id int PRIMARY KEY IDENTITY(1000000001,1) NOT NULL,
                                     msgid   VARCHAR(64) NULL,
                                     topic   VARCHAR(100) NULL,
                                     qos     tinyint NOT NULL DEFAULT 0,
                                     payload VARCHAR(100) NULL,
                                     arrived DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP);
   GO
   ```


### 创建 SQL Server 数据桥接

本节我们将创建一个 SQL Server 数据桥接来实现对客户端发布消息的存储。

1. 在 Dashboard 点击 **数据集成** -> **数据桥接**。
2. 点击页面右上角的**创建**。
3. 在数据桥接类型中选择 Microsoft SQL Server，点击**下一步**。
4. 输入数据桥接名称，要求是大小写英文字母或数字组合。
5. 输入 SQL Server 连接信息。
   - **服务器地址**： `127.0.0.1:1433`，或使用实际的 SQL Server 地址和端口
   - **数据库名字**： `mqtt`
   - **用户名**： `sa`
   - **密码**： `mqtt_public1`
6. 配置 SQL 模板，使用如下 SQL 完成数据插入，此处为[预处理 SQL](./data-bridges.md#sql-预处理)，字段不应当包含引号，SQL 末尾不要带分号 `;`:

  ```sql
  insert into t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )
  ```
7. 配置 odbc 驱动。
您可以使用 freetds 或 Microsoft 发布的 msodbcsql17 作为 odbc 驱动。
要使用 msodbcsql17 驱动，请参考 [安装 Microsoft ODBC Driver for SQL Server (Linux)](https://learn.microsoft.com/zh-cn/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-ver16&tabs=alpine18-install%2Calpine17-install%2Cdebian8-install%2Credhat7-13-install%2Crhel7-offline)。

MacOS 上配置 FreeTDS odbc 驱动:
```
$ brew install unixodbc freetds
$ vim /usr/local/etc/odbcinst.ini

[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/local/lib/libtdsodbc.so
Setup       = /usr/local/lib/libtdsodbc.so
FileUsage   = 1
```

Centos 上配置 FreeTDS odbc 驱动:
```
$ yum install unixODBC unixODBC-devel freetds freetds-devel perl-DBD-ODBC perl-local-lib
$ vim /etc/odbcinst.ini
# 加入以下内容
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/lib64/libtdsodbc.so
Setup       = /usr/lib64/libtdsS.so.2
Driver64    = /usr/lib64/libtdsodbc.so
Setup64     = /usr/lib64/libtdsS.so.2
FileUsage   = 1
```

Ubuntu 上配置 FreeTDS odbc 驱动（以 Ubuntu20.04 为例，其他版本请参考 odbc 官方文档）:
```
$ apt-get install unixodbc unixodbc-dev tdsodbc freetds-bin freetds-common freetds-dev libdbd-odbc-perl liblocal-lib-perl
$ vim /etc/odbcinst.ini
# 加入以下内容
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so
Setup       = /usr/lib/x86_64-linux-gnu/odbc/libtdsS.so
FileUsage   = 1
```

8. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考[配置参数](#配置参数)。
9. 在点击 **创建** 按钮完成数据桥接创建之前，您可以使用 **测试连接** 来测试当前 EMQX 到 SQL Server 的连接是否成功。
10. 点击创建按钮完成数据桥接创建。

至此您已经完成数据桥接创建，SQL Server 数据桥接应该出现在数据桥接列表（**数据集成** -> **数据桥接**）中，**资源状态**为**已连接**。
接下来将继续创建一条规则来指定需要写入的数据。

## 为 SQL Server 创建规则

1. 转到 Dashboard **数据集成** -> **规则**页面。
2. 点击页面右上角的**创建**。
3. 输入规则 ID `my_rule`，在 SQL 编辑器中输入规则，此处选择将 `t/#` 主题的 MQTT 消息存储至 SQL Server，
   请确规则选择出来的字段（SELECT 部分）包含所有 SQL 模板中用到的变量，此处规则 SQL 如下：

  ```sql
  SELECT
    *
  FROM
    "t/#"
  ```
4. 添加动作，在动作下拉框中选择**使用数据桥接转发**选项，选择先前创建好的 SQL Server 数据桥接。
5. 点击最下方**创建**按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 SQL Server 存储。

## 测试连接和规则

使用 MQTTX 向 `t/1` 主题发布消息。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello SQL Server" }'
```

查看 SQL Server 数据桥接运行统计，命中、发送成功次数均 +1。

查看数据是否已经写入表中，`mqtt.dbo.t_mqtt_msg` 表：

```bash
1> SELECT * from mqtt.dbo.t_mqtt_msg
2> GO
id          msgid                                                            topic                                                                                                qos payload                                                                                              arrived
----------- ---------------------------------------------------------------- ---------------------------------------------------------------------------------------------------- --- ---------------------------------------------------------------------------------------------------- -----------------------
 1000000002 0005F995096D9466F442000010520002                                 t/1                                                                                                    0 { "msg": "Hello SQL Server" }                                                                        2023-04-18 04:49:47.170

(1 rows affected)
1>
```
