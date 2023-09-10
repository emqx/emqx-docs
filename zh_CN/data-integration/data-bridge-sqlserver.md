# Microsoft SQL Server

通过 Microsoft SQL Server 数据桥接可以将 MQTT 消息和客户端事件存储到 Microsoft SQL Server 中，也可以通过事件触发对 Microsoft SQL Server 中数据的更新或删除操作，从而实现对诸如设备在线状态、上下线历史等的记录。

{% emqxee %}

::: tip

仅 EMQX 5.0.3 及以上版本支持 Microsoft SQL Server 数据桥接功能。

:::

{% emqxee %}

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

## 快速开始教程

本节介绍如何配置 Microsoft SQL Server 数据桥接，包括如何设置 Microsoft SQL Server 服务器并创建数据库和数据表、安装并配置 ODBC 驱动程序、创建数据创建数据桥接和转发数据到 Microsoft SQL Server 的规则以及测试数据桥接和规则等主题。

本教程假设您在本地机器上同时运行 EMQX 和 Microsoft SQL Server。 如果您有远程运行的 EMQX 和 Microsoft SQL Server，请相应地调整设置。

### 安装并连接到 Microsoft SQL Server

本节描述如何使用 Docker 镜像在 Linux/MacOS 安装启动 Microsoft SQL Server 2019 以及如何使用 `sqlcmd` 连接到 Microsoft SQL Server。关于其他 Microsoft SQL Server 的安装方式，请参阅微软提供的 [Microsoft SQL Server 安装指南](https://learn.microsoft.com/zh-cn/sql/database-engine/install-windows/install-sql-server?view=sql-server-ver16)。

1. 通过 Docker 安装并启动 Microsoft SQL Server。

   Microsoft SQL Server 要求使用复杂密码，请参阅[使用复杂密码](https://learn.microsoft.com/zh-cn/sql/relational-databases/security/password-policy?view=sql-server-ver16#password-complexity)。
   使用环境变量 `ACCEPT_EULA=Y` 启动 Docker 容器代表您同意 Microsoft 的 EULA 条款，详情请参阅 [MICROSOFT 软件许可条款 MICROSOFT SQL SERVER 2019 STANDARD(ZH_CN)](https://www.microsoft.com/en-us/Useterms/Retail/SQLServerStandard/2019/Useterms_Retail_SQLServerStandard_2019_ChineseSimplified.htm)。

   ```bash
   # 启动一个 Microsoft SQL Server 容器并设置密码为 `mqtt_public1`
   $ docker run --name sqlserver -p 1433:1433 -e ACCEPT_EULA=Y -e MSSQL_SA_PASSWORD=mqtt_public1 -d mcr.microsoft.com/mssql/server:2019-CU19-ubuntu-20.04
   ```

2. 进入 Docker 容器。

   ```bash
   $ docker exec -it sqlserver bash
   ```

3. 在容器中连接到 Microsoft SQL Server 服务器，需要输入预设的密码。输入密码时字符不会回显。请输入密码后直接键入 `Enter`。

   ```bash
   $ /opt/mssql-tools/bin/sqlcmd -S 127.0.0.1 -U sa
   $ Password:
   1>
   ```

   ::: tip

   Microsoft 提供的 Microsoft SQL Server 容器内已安装 `mssql-tools`，但可执行文件并不在 `$PATH` 中，因此您需要指定可执行文件路径。在上述连接示例中，可执行文件路径为 `opt`。

   关于更多 `mssql-tools` 的使用，请阅读 [sqlcmd 实用工具](https://learn.microsoft.com/zh-cn/sql/tools/sqlcmd/sqlcmd-utility?view=sql-server-ver16)。

   :::

至此 Microsoft SQL Server 2019 实例已经完成部署并可以连接。

### 创建数据库和数据表

本节描述如何在 Microsoft SQL Server 中创建数据库与数据表。

1. 使用已创建的连接在 Microsoft SQL Server 中创建数据库 `mqtt`。

   ```bash
   ...
   Password:
   1> USE master
   2> GO
   Changed database context to 'master'.
   1> IF NOT EXISTS(SELECT name FROM sys.databases WHERE name = 'mqtt') BEGIN CREATE DATABASE mqtt END
   2> GO
   ```


2. 使用 SQL 语句在此数据库中创建数据表。

   - 如需用于 MQTT 消息存储，创建数据表 `t_mqtt_msg`。该表存储每条消息的 MsgID、主题、QoS、Payload 以及发布时间。

     ```sql
     CREATE TABLE mqtt.dbo.t_mqtt_msg (id int PRIMARY KEY IDENTITY(1000000001,1) NOT NULL,
                                       msgid   VARCHAR(64) NULL,
                                       topic   VARCHAR(100) NULL,
                                       qos     tinyint NOT NULL DEFAULT 0,
                                       payload VARCHAR(100) NULL,
                                       arrived DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP);
     GO
     ```

   - 如需用于设备上下线状态记录，创建数据表 `t_mqtt_events`。

     ```sql
     CREATE TABLE mqtt.dbo.t_mqtt_events (id int PRIMARY KEY IDENTITY(1000000001,1) NOT NULL,
                                          clientid VARCHAR(255) NULL,
                                          event_type VARCHAR(255) NULL,
                                          event_time DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP);
     GO
     ```


### 安装并配置 ODBC 驱动程序

为了能够访问 Microsoft SQL Server 数据库，您需要安装并配置 ODBC 驱动程序。您可以使用 Microsoft 发布的 msodbcsql17 (msodbcsql18 的连接属性仍未进行适配) 或者 FreeTDS 作为 ODBC 驱动程序。

EMQX 使用 `odbcinst.ini` 配置中的 DSN Name 来确定驱动动态库的路径，有关的详细信息请参考[连接属性](https://learn.microsoft.com/zh-cn/sql/connect/odbc/linux-mac/connection-string-keywords-and-data-source-names-dsns?view=sql-server-ver16#connection-properties)。

::: tip 注意：

您可以根据自己的喜好命名 DSN Name，但建议只使用英文字母。此外 DSN Name 大小写敏感。

:::

#### 安装配置 msodbcsql17 作为 ODBC 驱动程序

<!-- TODO: update tag version in command and dockerfile -->

如需安装配置 msodbcsql17 作为 ODBC 驱动程序，您需要参考微软的安装指导：

- [安装 Microsoft ODBC Driver for SQL Server (Linux)](https://learn.microsoft.com/zh-cn/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-ver16&tabs=alpine18-install%2Calpine17-install%2Cdebian8-install%2Credhat7-13-install%2Crhel7-offline)
- [安装 Microsoft ODBC Driver for SQL Server (macOS)](https://learn.microsoft.com/zh-cn/sql/connect/odbc/linux-mac/install-microsoft-odbc-driver-sql-server-macos?view=sql-server-ver16)

受限于 [Microsoft EULA 条款](https://www.microsoft.com/en-us/Useterms/Retail/SQLServerStandard/2019/Useterms_Retail_SQLServerStandard_2019_ChineseSimplified.htm)，EMQX 提供的 Docker 镜像不带有 msodbcsql17 驱动程序，如需在 Docker 或 Kubernetes 中使用该驱动程序，您需要基于 [EMQX-Enterprise](https://hub.docker.com/r/emqx/emqx-enterprise) 提供的镜像构建带有 ODBC 驱动程序的新镜像以便在连接 Microsoft SQL Server 数据库时使用 msodbcsql17 驱动程序。使用构建的新镜像，代表您同意 Microsoft SQL Server EULA。

1. 在 EMQX 的仓库中找到对应的 [Dockerfile](https://github.com/emqx/emqx/blob/master/deploy/docker/Dockerfile.msodbc)。您可以将该文件保存至本地。

   下面的示例中 Dockerfile 中的镜像版本为 `emqx/emqx-enterprise:5.0.3-alpha.2`，对于 EMQX-Enterprise 5.0.3 以后的版本，可以根据您需要的 EMQX-Enterprise 版本构建镜像，也可以使用 EMQX-Enterprise 最新版本镜像 `emqx/emqx-enterprise:latest` 进行构建。

   ```dockerfile
   # FROM emqx/emqx-enterprise:latest
   FROM emqx/emqx-enterprise:5.0.3-alpha.2

   USER root

   RUN apt-get update \
       && apt-get install -y gnupg2 curl apt-utils \
       && curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
       && curl https://packages.microsoft.com/config/debian/11/prod.list > /etc/apt/sources.list.d/mssql-mkc crelease.list \
       && apt-get update \
       && ACCEPT_EULA=Y apt-get install -y msodbcsql17 unixodbc-dev \
       && sed -i 's/ODBC Driver 17 for SQL Server/ms-sql/g' /etc/odbcinst.ini \
       && apt-get clean \
       && rm -rf /var/lib/apt/lists/*

   USER emqx
   ```

2. 使用命令 `docker build -f=Dockerfile.msodbc -t emqx-enterprise-with-msodbc:5.0.3-alpha.2 .` 构建镜像。

3. 构建完成后可以使用 `docker image ls` 来获取本地的 image 列表，您也可以将镜像上传或保存备用。

::: tip 注意：

使用上文给出的示例安装 msodbcsql17 驱动后，请确认 `odbcinst.ini` 中的 DSN Name 为 `ms-sql` 。您也可以根据需要修改 DSN Name。

:::

#### 安装配置 FreeTDS 作为 ODBC 驱动程序

本节介绍了在几种主流发行版上安装配置 FreeTDS 作为 ODBC 驱动程序的方式。在此处给出的示例中，DSN Name 均为 `ms-sql`。

在 MacOS 上安装配置 FreeTDS 作为 ODBC 驱动程序:
```bash
$ brew install unixodbc freetds
$ vim /usr/local/etc/odbcinst.ini

[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/local/lib/libtdsodbc.so
Setup       = /usr/local/lib/libtdsodbc.so
FileUsage   = 1
```

在 Centos 上安装配置 FreeTDS 作为 ODBC 驱动程序:
```bash
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

在 Ubuntu 上安装配置 FreeTDS 作为 ODBC 驱动程序（以 Ubuntu20.04 为例，其他版本请参考 ODBC 官方文档）:
```bash
$ apt-get install unixodbc unixodbc-dev tdsodbc freetds-bin freetds-common freetds-dev libdbd-odbc-perl liblocal-lib-perl
$ vim /etc/odbcinst.ini
# 加入以下内容
[ms-sql]
Description = ODBC for FreeTDS
Driver      = /usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so
Setup       = /usr/lib/x86_64-linux-gnu/odbc/libtdsS.so
FileUsage   = 1
```

### 创建 Microsoft SQL Server 数据桥接

本节介绍了如何在 EMQX Dashboard 上创建 Microsoft SQL Server 数据桥接以实现对客户端发布消息的存储或设备状态的记录。

1. 在 Dashboard 点击 **数据集成** -> **数据桥接**。

2. 点击页面右上角的**创建**。

3. 在数据桥接类型中选择 Microsoft SQL Server，点击**下一步**。

4. 输入数据桥接名称，要求是大小写英文字母和数字的组合。

5. 输入 Microsoft SQL Server 连接信息。
   - **服务器地址**： `127.0.0.1:1433`，或使用实际的 Microsoft SQL Server 地址和端口
   - **数据库名字**： `mqtt`
   - **用户名**： `sa`
   - **密码**： `mqtt_public1`
   - **SQL Server Driver 名称**： `ms-sql`，即您在 `odbcinst.ini` 中配置的 DSN Name

6. 根据业务实现需要配置 SQL 模板：

   - 如需实现对指定主题消息的转发，使用如下 SQL 语句完成数据插入。此处为[预处理 SQL](./data-bridges.md#sql-预处理)，字段不应当包含引号，SQL 末尾不要带分号 `;`。

     ```sql
     insert into t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )
     ```

   - 如需实现实现设备上下线状态记录，可使用如下 SQL 语句完成数据插入：

     ```sql
     insert into t_mqtt_events(clientid, event_type, event_time) values ( ${clientid}, ${event}, DATEADD(MS, ${ms_shift}, DATEADD(S, ${s_shift}, '19700101 00:00:00:000') ) )
     ```

7. 高级配置（可选），根据情况配置同步/异步模式，队列与批量等参数，详细请参考[配置参数](./data-bridges.md)。

8. 在点击 **创建** 按钮完成数据桥接创建之前，您可以使用 **测试连接** 来测试当前 EMQX 到 Microsoft SQL Server 的连接是否成功。

9. 点击**创建**按钮完成数据桥接创建。

   在弹出的**创建成功**对话框中您可以点击**创建规则**，继续创建规则以指定需要写入 Microsoft SQL Server 的数据。您也可以按照[创建 Microsoft SQL Server 数据桥接规则](#创建-microsoft-sql-server-数据桥接规则)章节的步骤来创建规则。

至此您已经完成数据桥接创建，Microsoft SQL Server 数据桥接应该出现在数据桥接列表（**数据集成** -> **数据桥接**）中，**资源状态**为**已连接**。

### 创建数据转发规则

本节介绍了如何为 Microsoft SQL Server 数据桥接创建规则。您需要为实现对客户端发布消息的存储或实现设备上下线状态的记录创建不同的规则。

1. 转到 Dashboard **数据集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 输入规则 ID `my_rule`，在 **SQL 编辑器**中根据业务实现需要输入规则：

   - 如需实现对指定主题消息的转发，例如将 `t/#` 主题的 MQTT 消息存储至 Microsoft SQL Server，输入以下 SQL 语句：
     注意：如果您希望制定自己的 SQL 语句，需要确保规则选出的字段（SELECT 部分）包含所有 SQL 模板中用到的变量。

     ```sql
      SELECT
        *
      FROM
        "t/#"
     ```

   - 如需实现设备上下线状态记录，输入以下 SQL 语句：

     ```sql
     SELECT
       *,
       floor(timestamp / 1000) as s_shift,
       timestamp div 1000 as ms_shift
     FROM
       "$events/client_connected", "$events/client_disconnected"
     ```

4. 点击**添加动作**，在动作下拉框中选择**使用数据桥接转发**选项，选择先前创建好的 Microsoft SQL Server 数据桥接。

5. 点击最下方**创建**按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果交由 Microsoft SQL Server 存储。

### 测试数据桥接和规则

使用 MQTTX 向 `t/1` 主题发布消息。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello SQL Server" }'
```

查看 Microsoft SQL Server 数据桥接运行统计。

- 用于消息存储的数据桥接，命中、发送成功次数均 +1。查看数据是否已经写入 `mqtt.dbo.t_mqtt_msg` 表中：

```bash
1> SELECT * from mqtt.dbo.t_mqtt_msg
2> GO
id          msgid                                                            topic                                                                                                qos payload                                                                                              arrived
----------- ---------------------------------------------------------------- ---------------------------------------------------------------------------------------------------- --- ---------------------------------------------------------------------------------------------------- -----------------------
 1000000001 0005F995096D9466F442000010520002                                 t/1                                                                                                    0 { "msg": "Hello SQL Server" }                                                                        2023-04-18 04:49:47.170

(1 rows affected)
1>
```

- 用于存储上下线事件的 Microsoft SQL Server 数据桥接，命中、发送次数均 +2，即一次上线和一次下线。查看设备状态是否已经写入 `mqtt.dbo.t_mqtt_events` 表中：

```bash
1> SELECT * from mqtt.dbo.t_mqtt_events
2> GO
id          clientid                                                         event_type                                                                                                                                                                                                    event_time
----------- ---------------------------------------------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- -----------------------
 1000000001 emqx_c                                                           client.connected                                                                                                                                                                                              2023-04-18 04:49:47.140
 1000000002 emqx_c                                                           client.disconnected                                                                                                                                                                                           2023-04-18 04:49:47.180

(2 rows affected)
1>
```

