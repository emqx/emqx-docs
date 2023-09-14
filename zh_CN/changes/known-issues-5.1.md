# 已知问题

## e5.1.1

-   **滚动升级成功后显示不准确的错误消息**

    在完成滚动升级后，`emqx.log` 可能会显示一个错误消息。然而，此错误消息是无害的，升级已经成功完成。
    
    在核心+复制集群上执行滚动升级时，日志中可能会出现错误消息 `** ERROR ** Mnesia post_commit hook failed: error:badarg`。然而，需要注意这些错误不会影响升级过程，集群可以成功完成升级。

## e5.1.0

-   **MongoDB 数据桥接界面中重复的 "Connection Pool Size" 设置**

    MongoDB 数据桥接界面上显示了重复的 "Connection Pool Size" 参数。第二个实例用于配置 MongoDB 连接选项，导致混淆和潜在的配置错误。

    > **修复版本:** 5.1.0

-   **成功创建 TimescaleDB 数据桥接后状态仍保持断开**

    当为 TimescaleDB 建立无密码登录时，数据桥接成功创建但保持断开状态。在桥接创建过程中输入用户名可以解决此问题。

    > **解决方法:**
    > 配置 TimescaleDB 的用户名和密码，然后使用正确的用户名和密码创建数据桥接。

    > **修复版本:** 5.1.0

-   **命令行启动/停止后 Dashboard 未更新监听器状态**

    通过以下命令启动或停止监听器不会在 Dashboard 上更新监听器的状态，可能导致对其实际状态的困惑。

    ```
    ./bin/emqx ctl listeners stop tcp:we
    ./bin/emqx ctl listeners start tcp:we
    ```

-   **插件的启动/停止命令只影响执行节点，而不影响整个集群**

    使用下面的命令执行插件的启动/停止命令只影响执行节点，而不影响集群中的其他节点。

    ```
    ./bin/emqx ctl plugins stop emqx_plugin_template-5.0.0
    ./bin/emqx ctl plugins start emqx_plugin_template-5.0.0
    ```

    > **解决方法:**
    > 在集群中的所有节点上执行启动/停止命令，或使用 Dashboard 管理插件。

-   **CoAP 网关连接模式需要身份验证**

    CoAP 网关只允许启用身份验证的连接创建。启用任何身份验证方法可以解决此问题。

    > **解决方法:**
    > 启用任何身份验证方法，然后使用正确的身份验证信息创建连接。

    > **修复版本:** 5.1.0

-   **在 Dashboard 使用 "crt" 作为 "Use Peer Certificate field as ClientId" 选项时出现错误**

    在 Dashboard **MQTT 配置**的**通用**页签中选择 `crt` 作为 "Use Peer Certificate as Client ID" 选项会导致客户端 ID 显示为乱码，并在查看详细信息时出现错误。

-   **在带有文件传输功能的集群中无法在 replica 节点上查看和下载文件**

    使用集群中的文件传输功能将文件上传到某个 replica 节点后，仅可以从文件被上传至的节点使用文件列表 API 访问文件。

    > **解决方法:**
    > 仅从文件被上传到的节点查看和下载文件。

    > **修复版本:** 5.1.0

-   **Dashboard 未能反映修改后的监听器端口**

    在下面的 `emqx.conf` 配置文件中调整监听器端口，但不会更新 Dashboard 上显示的端口，导致显示的端口与实际端口不匹配。

    ```
    listeners.tcp.default {
        bind = "0.0.0.1884"
    }
    ```

    > **修复版本:** 5.1.0

-   **"quic" 或 "ws" 类型监听器的 "Max Connections" 参数无效**

    通过 Dashboard 或配置文件配置 "quic" 或 "ws" 类型监听器的 "Max Connections" 参数不会生效。

-   **规则中不准确的 `client.disconnected` 事件统计**

    设置 `clean_session = false` 的连接会生成两个 `client.disconnected` 事件，导致事件统计不准确。

-   **规则动作 "republish" payload 中的 UTF-8 字符解析问题**

    在规则动作为 "republish"，且在 payload 中使用 `${payload.'msg'}` 时，订阅重新发布主题的客户端会收到 `${payload.'msg'}` 而不是实际 `${payload.msg}`中的值。

-   **Oracle 数据库的 "Connection Pool Size" 的默认值无法更改**

    无论指定了什么值，"Connection Pool Size" 参数都会保持在默认值 8。

    > **修复版本:** 5.1.0

-   **无法在 Dashboard 上更新订阅主题的 QoS**

    当 CoAP 客户端订阅 QoS 为 0 的主题并将 QoS 更改为 1 时，Dashboard 仍然显示原始的 QoS 值，并且实际 QoS 仍保持在 0。
