## e5.1.1

-   ****Inaccurate Error Message Displayed After Successful Rolling Upgrade****

    Following a rolling upgrade, the `emqx.log` might present an error message. Nonetheless, this error message is harmless, and the upgrade has indeed succeeded.

    When performing a rolling upgrade on a core+replicant cluster, you may observe the error message `** ERROR ** Mnesia post_commit hook failed: error:badarg` in the logs. However, it is important to recognize that these errors do not affect the success of the upgrade process, and the cluster can be upgraded without any issues.

## e5.1.0

-   ****Duplicate "Connection Pool Size" Setting in MongoDB Bridge UI****

    The MongoDB Bridge UI displays duplicated entries for the "Connection Pool Size" parameter. The second instance is utilized for configuring MongoDB connection options, leading to confusion and potential configuration errors.

    > **Fixed Version:** 5.1.1

-   ****TimescaleDB Bridge Status Remains Disconnected after Successful Creation****

    When establishing a password-free login for TimescaleDB, the data bridge is created successfully but remains disconnected. Inputting the username during bridge creation resolves this issue.

    > **Workaround:**
    > Configure usernames and passwords for TimescaleDB, then create a data bridge using the correct credentials.

    > **Fixed Version:** 5.1.1

-   ****Listener Status Not Updated on Dashboard After Command Line Start/Stop****

    Starting or stopping a listener through the following commands does not update the listener's status on the Dashboard, causing confusion about its actual state.

    ```
    ./bin/emqx ctl listeners stop tcp:we
    ./bin/emqx ctl listeners start tcp:we
    ```

-   ****Plugin Start/Stop Command Only Affects Executing Node, Not Cluster-Wide****

    Executing the start/stop commands for a plugin as shown below affects only the executing node, not other nodes within the cluster.

    ```
    ./bin/emqx ctl plugins stop emqx_plugin_template-5.0.0
    ./bin/emqx ctl plugins start emqx_plugin_template-5.0.0
    ```

    > **Workaround:**
    > Execute the start/stop command on all nodes in the cluster or use the Dashboard to manage the plugin.

-   ****Authentication Required for Coap Gateway Connection Mode****

    The CoAP gateway only allows connection creation with authentication enabled. Enabling any authentication method resolves the issue.

    > **Workaround:**
    > Enable any authentication method and create a connection with the correct authentication information.

    > **Fixed Version:** 5.1.1

-   ****Dashboard Error When Using "crt" for "Use Peer Certificate field as ClientId" Option****

    Selecting `crt` as the option for **Use Peer Certificate as Client ID** on the **General** tab of the **MQTT Settings** page on the Dashboard causes the client ID to display garbled code on the Dashboard and leads to an error when viewing details.

-   ****Unable to View and Download Files on Replica Nodes in Cluster with File Transfer Feature****

    When using the file transfer function to upload files within a cluster, files uploaded to one of the replica nodes can only be accessed with files list API from the node to which they are uploaded.

    > **Workaround:**
    > Only view and download files from the node to which the file is uploaded.

    > **Fixed Version:** 5.1.1

-   ****Dashboard Fails to Reflect Modified Listener Port****

    Adjusting the listener port in the `emqx.conf` configuration file as shown below does not update the port displayed on the Dashboard, leading to a mismatch between the displayed and actual port.

    ```
    listeners.tcp.default {
        bind = "0.0.0.1884"
    }
    ```

    > **Fixed Version:** 5.1.1

-   ****"Max Connections" Parameter Ineffective for "quic" or "ws" Listeners****

    Configuring "Max Connections" for "quic" or "ws" type listeners through the Dashboard or configuration file does not take effect.

-   ****Inaccurate** `client.disconnected` Events Statistics in Rules**

    Channels with `clean_session = false` generate two `client.disconnected` events, leading to inaccurate event statistics.

-   ****Parsing Issue with UTF-8 Characters in Rule Action "republish" Payload****

    When creating a rule with the "republish" action and using `${payload.'msg'}` in the payload, clients subscribed to the republish topic receive `${payload.'msg'}` instead of the actual `${payload.msg}` value.

    > **Fixed Version:** 5.2.0

-   ****Default Value of "Connection Pool Size" for Oracle Database is Unchangeable****

    Irrespective of the specified value, the "Connection Pool Size" parameter remains fixed at the default value of 8.

    > **Fixed Version:** 5.1.1

-   ****Fail to Update for Subscribed Topic's QoS on Dashboard****

    When a CoAP client subscribes to a topic with QoS 0 and changes the QoS to 1, the Dashboard still displays the original QoS value and the actual QoS remains at 0.

-   **Log Files Deleted on EMQX Restart with Rotation Number > 10**

    When the log rotation_number is set to a value greater than 10, the log files with numbers higher than 10 (e.g., emqx.log.11, emqx.log.12, etc.) are still getting deleted upon restarting EMQX.

-   **EMQX Cluster Split-Brain Issue when Node Disconnected for >2 Minutes**

    EMQX 5.x upgrade to OTP25 has encountered a challenge due to the implementation of a more aggressive network fragmentation approach in Erlang OTP 25. Unlike the previous OTP 24 version, even minor network disturbances can now inaccurately trigger split-brain detection in the global component of EMQX.
