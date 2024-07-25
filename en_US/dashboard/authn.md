# Authentication

EMQX Dashboard provides out-of-the-box authentication and user management capabilities. Users can quickly configure client authentication mechanisms through the user interface without writing code or manually editing configuration files. This allows integration with various data sources and authentication services to achieve secure configurations across different levels and scenarios, ensuring higher development efficiency with enhanced security guarantees. On the **Authentication** page, you can quickly create and manage various authentication resources.

:::tip
After configuring authentication backends, you must set up corresponding authentication information for devices or MQTT clients to securely connect to EMQX.
:::

## Create Authentication

Click the **Create** button to go to the **Create Authentication** page. To create an authentication, you need to select a mechanism and then select a backend to store or obtain authentication data (except JWT authentication). The data can be obtained from these backends, including databases or HTTP servers. Finally, you need to configure the connection information to connect to the backends.

### Mechanism

You can select from the following mechanisms provided by EMQX:

1. Password-Based, using the client ID or username and password.
2. JWT, where the client can carry a JWT Token in the username or password.
3. SCRAM, enhanced authentication in MQTT 5.0, which enables two-way authentication between client and server.

<img src="./assets/create-authn.png" alt="image" style="zoom:50%;" />

### Backend

In this step, you can select a backend based on the mechanism selected in last step.

::: tip

A backend that has been used for authentication cannot be reselected.

:::

For a comprehensive introduction to backends, refer to [EMQX Authenticators](../access-control/authn/authn.md#emqx-authenticator).

#### Password-Based

When the `Password-Based` is selected, the user can choose either the database that stores the data or the HTTP server that provides the data, which contains two types of databases.

- The built-in database of EMQX, i.e., the `Built-in Database`.
- External database, which supports selecting and connecting to some mainstream databases, including: `MySQL`, `PostgreSQL`, `MongoDB`, `Redis`, etc.

You can also directly use HTTP services that can provide authentication data, i.e., the `HTTP Server`.

#### JWT

If JWT is selected, there will be no need to select a backend.

#### SCRAM

The enhanced authentication feature in MQTT 5.0, if selected, currently only provides the ability to use the `Built-in Database`, which uses EMQX's built-in database (Mnesia) to store data.

Enhanced authentication enables two-way authentication of the client and server, where the server can verify that the connected client is the real client and the client can verify that the connected server is the real server, thus providing higher security.

For more details about MQTT 5.0 Enhanced Authentication, refer to [SCRAM Authentication](../access-control/authn/scram.md).

### Configuration

The final step is to configure the selected backend. Each backend has some connection and usage configuration that needs to be configured by the user. After completing the configuration, just click **Create**. 

#### Built-in Database

For example, if you use the `Built-in Database`, you need to choose whether to use the Username or the Client ID, seting the encryption method of the password, etc. If you use the enhanced authentication of MQTT 5.0 and use the built-in database, you only need to configure the encryption method.

For more details about Built-in Database, refer to [Use Built-in Database](../access-control/authn/mnesia.md).

#### External Database

If you use an external database, you need to configure the server address of the database, the database name, username and password, the authentication configuration, and the SQL statements or other query statements on how to get data from the database.

For more details about MySQL or other external databases, refer to [Integrate with MySQL](../access-control/authn/mysql.md) or configuration documents for other databases.

#### HTTP Server

To use HTTP Server, you need to configure the request method of the HTTP service, POST or GET. The request URL, note that the URL needs to fill in the protocol is http or https. Then there is the configuration of the HTTP request Headers. The authentication information is entered into the `Body` field, e.g. `username` and `password` are filled in the JSON data.

For more details about HTTP Server, refer to [Use HTTP Service](../access-control/authn/http.md).

#### JWT & JWKS

To use JWT, you can configure JWT directly without selecting a backend, and set whether the Token required for JWT comes from `username` or `password` of the client. This way the client only needs to enter the Token into the corresponding field when connecting, and JWT authentication can be performed. Then set `Secret` or `Public Key` depending on the encryption method, set `Secret` to Base64 encoding or not, and finally enter the information that needs to be verified in `Payload` to complete the configuration of JWT authentication.

You can get the latest JWKS from the `JWKS Endpoint` periodically, which is essentially a set of public keys that will be used to verify any JWT issued by the authorization server and signed using RSA or ECDSA algorithms, and configure the refresh interval (in seconds) for the JWKS. Finally, configure the `Payload` entry to complete the JWKS configuration.

For more details about JWT, refer to [JWT Authentication](../access-control/authn/jwt.md).

## Authentication List

After successfully creating an authenticator, you can view and manage it in the authenticator list.

In the list, you can see the backend and mechanism of each authenticator, and the status of the backend. For example, if the external database deployment fails to connect, the status will indicate `Disconnected`. Hovering over this field provides further details on the connection status of all nodes in the EMQX cluster linked to this data source. You can quickly enable or disable the authentication configuration by toggling the **Enable** switch.

Each entry in the authenticator list can be reordered by dragging with the mouse or by adjusting the sequence in the **Actions** column. The order in the authenticator list is significant because EMQX supports multiple authenticators that operate sequentially in the authentication chain. If the current authenticator fails to retrieve matching authentication information, the process continues with the next authenticator in the chain.

In the **Actions** column, you can also click to configure or delete an authenticator.

<img src="./assets/authn-list.png" alt="image" style="zoom:50%;" />

:::tip Note
Disabled authentication will not authenticate any client, which means all clients can connect to EMQX. Please proceed with caution.
:::

## User Management

For users using the built-in database, clicking **User Management** on the Authenticator List page allows you to manage authentication information. You can add or delete usernames and passwords, and also batch-create authentication-related user information by downloading a template, filling in the relevant authentication details, and clicking **Import**.

<img src="./assets/authn-users.png" alt="image" style="zoom:50%;" />

## Overview

You can click the authenticator name in the **Mechanism and Backend** list on the list page to go to the Overview page. This page provides some metrics of the authenticators in the EMQX cluster, such as the number of successes and failures of authentication, the number of mismatches and the rate of authentication currently being connected.

You can monitor the metrics data under each node from the list at the bottom of the page.

<img src="./assets/authn-overview.png" alt="image" style="zoom:50%;" />

## Settings

Click **Settings** in the list page to modify the authentication configuration.

In the settings page, you can modify the current authenticator configuration, such as when some connection information of the external database changes, when you need to modify the `UserID Type` of the built-in database as username or client ID, or modify the encryption method of the password, etc.

:::tip Note
When using the built-in database, updating the **Password Hash** or **Salt Position** will cause the added authentication data to be unavailable, please proceed with caution.
:::

<img src="./assets/authn-settings.png" alt="image" style="zoom:50%;" />

## More Information

For more details about authentication, refer to [Authentication](../access-control/authn/authn.md).
