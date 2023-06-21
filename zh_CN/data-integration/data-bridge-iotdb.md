# Apache IoTDB

[Apache IoTDB (物联网数据库)](https://iotdb.apache.org/zh/) 是一款高性能可扩展的物联网时序数据库，用于处理由各种物联网设备和系统产生的海量时序数据。通过数据桥接将数据导入 Apache IoTDB，您可以将来自其他系统的数据转发到 Apache IoTDB 进行存储和分析。

EMQX 支持与 Apache IoTDB 之间的数据集成, 使您能够通过它的 [REST API V2](https://iotdb.apache.org/zh/UserGuide/Master/API/RestServiceV2.html) 将时序数据转发到 Apache IoTDB。

{% emqxce %}
:::tip 

Apache IoTDB 数据集成是 EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。

::: 

{% endemqxce %}

::: tip 前置准备

- 了解 EMQX 数据集成[规则](./rules.md)

- 了解[数据桥接](./data-bridges.md)

- 了解 UNIX 终端和命令

:::

## 功能清单

- [连接池](./data-bridges.md#连接池)
- [异步请求模式](./data-bridges.md#异步请求模式)
  <!-- - [Batch mode](./data-bridges.md) -->
  <!-- - [Buffer mode](./data-bridges.md) -->

## 快速开始

本节为如何使用 Apache IoTDB 数据桥接提供了实用的教程，包括如何设置 Apache IoTDB 服务器、创建数据桥接和规则以将数据转发到 Apache IoTDB、以及如何测试数据桥接和规则。

本教程假定 EMQX 和 ApacheIoTDB 均在本地运行。如果您在远程运行 Apache IoTDB 和 EMQX，请根据实际情况调整相应配置。

### 启动 Apache IoTDB 服务器

本节将介绍如何通过 [Docker](https://www.docker.com/) 启动 Apache IoTDB 服务器。 确保在您的 IoTDB 配置中具备该字段： `enable_rest_service=true`。

在 REST 接口开启的情况下运行下面的命令启动 Apache IoTDB 服务器：

```bash
docker run -d --name iotdb-service \
              --hostname iotdb-service \
              -p 6667:6667 \
              -p 18080:18080 \
              -e enable_rest_service=true \
              -e cn_internal_address=iotdb-service \
              -e cn_target_config_node_list=iotdb-service:10710 \
              -e cn_internal_port=10710 \
              -e cn_consensus_port=10720 \
              -e dn_rpc_address=iotdb-service \
              -e dn_internal_address=iotdb-service \
              -e dn_target_config_node_list=iotdb-service:10710 \
              -e dn_mpp_data_exchange_port=10740 \
              -e dn_schema_region_consensus_port=10750 \
              -e dn_data_region_consensus_port=10760 \
              -e dn_rpc_port=6667 \
              apache/iotdb:1.1.0-standalone
```

有关如何通过 Docker 运行 IoTDB 的更多信息，请参阅： [IoTDB in Docker on Docker Hub](https://hub.docker.com/r/apache/iotdb)。

### 创建 Apache IoTDB 数据桥接

本节将通过 Dashboard 演示如何创建到 Apache IoTDB 的数据桥接。

1. 在 Dashboard 左侧导航目录中点击**数据集成** -> **数据桥接**。

2. 点击页面右上角的**创建**。

3. 在**创建数据桥接**页面, 点击选择 **Apache IoTDB** 作为数据桥接类型, 然后点击 **下一步**。

4. 输入数据桥接名称，要求是大小写英文字母和数字的组合。

5. 输入 Apache IoTDB 服务器链接信息：

   * **基础 URL**: 输入 `http://localhost:18080`；如果服务器在远程运行，须输入实际 IoTDB 服务器地址和端口。

   * **用户名**: 输入 IoTDB 用户名; 默认值为 `root`。

   * **密码**: 输入 IoTDB 密码; 默认值为 `root`。

   * **设备 ID** （可选）: 输入一个固定的设备名称，表明被插入到 IoTDB 数据库的时序数据来自该设备。

     :::tip

     如果留空，设备 ID 也可以在发布的消息中指定，通过规则配置，或从消息发送的主题中提取，将`/`转换为`.`。例如，向主题 `root/sg27` 发布一条消息将导致发送一个设备名称为 `root.sg27` 的消息。然而，相比此前提到的其他任何配置方式，在此字段中配置的固定设备名称将被优先使用。
     
     :::

7. 高级功能（可选）：根据情况配置同步/异步模式、批量模式。

8. 点击**创建**前，您可点击**测试连接**按钮确保能连接到 Apache IoTDB 服务器。

9. 点击**创建**按钮完成数据桥接创建。

   在弹出的**创建成功**对话框中您可以点击**创建规则**，继续创建规则以指定需要写入 Apache IoTDB 的数据。详细步骤可参照[创建数据转发规则](#创建数据转发规则)章节的步骤来创建规则。

至此，您已经完成数据桥接的创建，在 Dashboard 的数据桥接页面，可以看到 Apache IoTDB 数据桥接的状态为**已连接**。

### 创建数据转发规则

本节将介绍如何创建一条规则来指定需要转发至 Apache IoTDB 的数据。

1. 转到 Dashboard **数据集成** -> **规则**页面。

2. 点击页面右上角的**创建**。

3. 输入规则 ID，例如 `my_rule`.

4. 在 SQL 编辑器中输入规则，例如我们希望将 `root/#` 主题的 MQTT 消息转发至 Apache IoTDB，可通过如下规则 SQL 实现：

   ```sql
   SELECT
     *
   FROM
     "root/#"
   
   ```

   如果您想要创建自定义的规则，您需要在 SQL 语句的 `SELECT` 部分选择出桥接所需的所有字段。例如，客户端发送一条 payload 格式为 JSON 的消息，在 IoTDB 桥接中需要以下字段：

   ```json
   {
     "measurement": "temp",
     "data_type": "FLOAT",
     "value": "32.67",
     "device_id": "root.sg27" // optional
   }
   ```

   您可以使用以下 SQL 语句来选择出 `measurement`, `data_type` 和 `value` 这些字段：

   ```sql
   SELECT
     payload.measurement, payload.data_type, payload.value, clientid as payload.device_id
   FROM
     "root/#"
   ```

   对于不同结构的 payload，可以使用下面的规则对结构进行重写：

   ```sql
   SELECT
     payload.measurement, payload.dtype as payload.data_type, payload.val as payload.value
   FROM
     "root/#"
   ```

5. 点击**添加动作**按钮，在下拉框中选择**使用数据桥接转发**，选择之前创建好的 Apache IoTDB 数据桥接。

   :::tip

   如果您在创建完数据桥接后直接点击**创建规则**，这些选项已经过配置，您可以跳过这一步和下一步。

   :::

6. 点击**添加**按钮确认添加动作。

7. 点击最下方**创建**按钮完成规则创建。

至此我们已经完成数据桥接和转发规则的创建，您可前往 **数据集成** -> **Flows** 页面查看拓扑图，可看到 `root/#` 主题的消息被转发至 Apache IoTDB。

### 测试数据桥接和规则

您可通过 EMQX Dashboard 内置的 WebSocket 客户端进行规则和数据桥接的验证。

1. 在 Dashboard 页面，点击左侧导航目录中的 **问题分析** -> **WebSocket 客户端**。

2. 填写当前 EMQX 的连接信息。 

   - 如果 EMQX 在本地运行，可直接使用默认配置。
   - 如果您修改过 EMQX 的默认配置，如修改过访问规则的配置，则需要输入用户名和密码。

3. 点击**连接**，建立该 WebSocket 客户端与 EMQX 的连接。

4. 前往发布区域，在消息 payload 中设置设备 ID并发布消息：

   - **主题**：`root/test`

   - **Payload**: 

     ```json
     {
       "measurement": "temp",
       "data_type": "FLOAT",
       "value": "37.6",
       "device_id": "root.sg27"
     }
     ```
   
   - **QoS**: `2`

5. 点击**发布**完成消息的发送。

6. 您也可以在主题中设置设备 ID， 并再次发布消息：

   - **主题**：`root/sg27`


   - **Payload**:

     ```json
     {
       "measurement": "temp",
       "data_type": "FLOAT",
       "value": "36.6"
     }
     ```


   - **QoS**: `2`

   :::tip

   如果主题不以 `root` 开头，系统将自动为其添加前缀。例如，如果您将消息发布到 `test/sg27`，生成的设备名称将为 `root.test.sg27`。请确保您的规则和主题已正确配置，以便将来自该主题的消息转发到桥接。

   :::

7. 点击**发布**完成消息的发送。

   如果数据桥接和规则创建成功，消息应该已被转发至 Apache IoTDB 服务器里指定的时序数据表中。 

8. 您可以使用 IoTDB 的命令行查看。如果服务器在 docker 中运行，可以使用下面的命令连接服务器：

   ```shell
    $ docker exec -ti iotdb-service /iotdb/sbin/start-cli.sh -h iotdb-service
   ```

9. 在控制台中继续输入：

   ```sql
   IoTDB> select * from root.sg27
   ```

   您将能看到以下返回结果：
   
   ```
   +------------------------+--------------+
   |                    Time|root.sg27.temp|
   +------------------------+--------------+
   |2023-05-05T14:26:44.743Z|          37.6|
   |2023-05-05T14:27:44.743Z|          36.6|
   +------------------------+--------------+
   ```
   
   

