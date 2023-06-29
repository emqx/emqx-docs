# e5.1.0

-   **Duplicate "Connection Pool Size" Setting in MongoDB Bridge UI.**

    The MongoDB Bridge UI shows two duplicate parameters for "Connection Pool Size". The second one is used to set MongoDB connecting options, which causes confusion and potential configuration errors.

-   **State of TimescaleDB Bridge remains Disconnected after Successful Creation**

    When setting TimescaleDB to password-free login, the data bridge is created successfully but remains disconnected. Filling in the username during bridge creation resolves the issue.

    > **Workaround:**
    > Set up users and passwords for the TimescaleDB, and create a data bridge using the correct username and password.

-   **Listener Status Not Updated on Dashboard after Command Line Start/Stop**

    Starting or stopping a listener through following commands does not update the listener's status on the Dashboard, causing confusion about its actual state.
    ```
    ./bin/emqx ctl listeners stop tcp:we
    ./bin/emqx ctl listeners start tcp:we
    ```

-   **Plugin Start/Stop Command Only Affects Executing Node, Not Cluster-Wide**

    Starting or stopping a plugin through the following command lines affects only the executing node, not other nodes in the cluster. Executing the command on all nodes or using the Dashboard resolves the issue.
    ```
    ./bin/emqx ctl plugins stop emqx_plugin_template-5.0.0
    ./bin/emqx ctl plugins start emqx_plugin_template-5.0.0
    ```

    > **Workaround:**
    > Execute the start/stop command on all nodes in the cluster or use the Dashboard to manage the plugin.

-   **No Authentication Enabled in Connection Mode for Coap Gateway is Disallowed.**

    Coap gateway does not allow connection creation without authentication enabled. Enabling any authentication method resolves the issue.

    > **Workaround:**
    > Enable any authentication method and create a connection with the correct authentication information.

-   **Error in Dashboard when Using "crt" for "Use Peer Certificate field as ClientId" Option.**

    Selecting "Use Peer Certificate as Client ID" type as "crt" in MQTT causes the client ID to display garbled code on the Dashboard and leads to an error when viewing details.

-   **Unable to View and Download Files on Replica Nodes in Cluster with File Transfer Feature.**

    When uploading files using the file transfer function in a cluster, files uploaded to replica nodes can only be accessed from the node to which they were uploaded. Accessing files from the node to which the file is uplaoded resolves the issue.

    > **Workaround:**
    > Only view and download files from the node to which the file is uploaded.

-   **Listener Port Modification Not Reflected on Dashboard**

    Modifying the listener port in the following`emqx.conf` configuration file does not update the displayed port on the Dashboard, causing a discrepancy between the displayed and actual port.
    ```
    listeners.tcp.default {
        bind = "0.0.0.1884"
    }
    ```

-   **"Max Connections" Parameter Ineffective for "quic" or "ws" Listeners**

    Configuring "Max Connections" for "quic" or "ws" type listeners through the Dashboard or configuration file does not take effect.

-   **Inaccurate `client.disconnected` Events Statistics in Rules**

    Channels with `clean_session = false` emit two `client.disconnected` events, leading to inaccurate event statistics.
