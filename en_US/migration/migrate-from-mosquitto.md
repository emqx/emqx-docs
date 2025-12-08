# Migrate from Mosquitto to EMQX

This guide outlines the process of migrating an existing Eclipse Mosquitto deployment to EMQX. It is designed for administrators seeking to move from a lightweight, single-instance broker to a scalable, distributed MQTT platform. The migration leverages EMQX’s compatibility with standard MQTT protocols and provides a clear path for transferring configuration, security credentials, and integration logic.

## Migration at a Glance

The migration process consists of three main phases:

1. **Inventory Mosquitto Assets**: Collect configuration files (`mosquitto.conf`), security artifacts (password files, ACLs, certificates), and understand the current data flow.
2. **Configure EMQX**: Translate Mosquitto settings into EMQX’s configuration file (`emqx.conf`) in HOCON format, import user credentials, and recreate access controls and data integrations using the Rule Engine.
3. **Update Devices & Integrations**: Redirect devices to the EMQX cluster (often seamless due to port compatibility) and validate system behavior.

| Parameter / Artifact | Mosquitto (Example) | EMQX (Example) | Notes |
| :--- | :--- | :--- | :--- |
| **Main Configuration** | `/etc/mosquitto/mosquitto.conf` | `/etc/emqx/emqx.conf` | EMQX uses hierarchical HOCON format. |
| **Network Ports** | `1883` (TCP), `8883` (SSL) | `1883` (TCP), `8883` (SSL) | Standard ports match; no device reconfiguration usually needed. |
| **User Credentials** | `/etc/mosquitto/passwd` | Built-in Database (Mnesia) | Import existing password hashes via API. |
| **Access Control** | `/etc/mosquitto/acl_file` | `/etc/emqx/acl.conf` | Direct mapping of Allow/Deny rules. |
| **Bridges** | `connection bridge_name` | Data Connectors & Rules | Replaces static bridges with dynamic data routing. |
| **Persistence** | `mosquitto.db` | `data/` (Mnesia + RocksDB) | EMQX handles session persistence automatically. |

## Phase 1: Inventory Mosquitto Assets

### Collect Configuration and Certificates

Identify the locations of your key configuration files. These are typically defined in your `mosquitto.conf`:

* **Main Config:** `include_dir` or default `/etc/mosquitto/mosquitto.conf`.
* **Certificates:** Look for `certfile`, `keyfile`, and `cafile` paths.
* **Security:** Locate `password_file` and `acl_file`.

Copy your certificate files (`server.crt`, `server.key`, `ca.crt`) to the EMQX node, typically under `/etc/emqx/certs/`.

### Analyze Authentication and Authorization

Determine your authentication method:
*   **Password File:** Most common. You will migrate these to EMQX's internal database.
*   **Plugin (mosquitto-auth-plug):** If using SQL or LDAP, you will configure the corresponding EMQX authentication backend directly.

## Phase 2: Configure EMQX to Mirror Mosquitto Baseline

### Recreate MQTT Listeners

Mosquitto defines listeners sequentially. EMQX groups them by type (TCP, SSL, WebSocket) in `emqx.conf`.

**Mosquitto (`mosquitto.conf`):**

```properties
# Default listener
port 1883
max_connections -1

# SSL Listener
listener 8883
certfile /etc/mosquitto/certs/server.crt
keyfile /etc/mosquitto/certs/server.key
```

**EMQX (`emqx.conf`):**
```hocon
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = infinity
}

listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    certfile = "/etc/emqx/certs/server.crt"
    keyfile  = "/etc/emqx/certs/server.key"
  }
}
```

### Map MQTT Configuration Options

Translate core protocol settings to ensure consistent client behavior.

| Mosquitto Directive | EMQX HOCON Parameter | Description |
| :--- | :--- | :--- |
| `max_queued_messages` | `mqtt.max_mqueue_len` | Maximum offline messages buffered per client. |
| `persistent_client_expiration` | `mqtt.session_expiry_interval` | Time to keep session state after disconnect. |
| `message_size_limit` | `mqtt.max_packet_size` | Maximum allowed MQTT packet size. |
| `log_dest file` | `log.file.enable = true` | Enables file logging. |

**Note:** Mosquitto handles session expiration globally, whereas EMQX (MQTT 5.0) supports per-client session expiry intervals. For legacy MQTT 3.1.1 clients, you can set a global default in EMQX to match your Mosquitto policy.

### Migrate Authentication

EMQX supports multiple authentication backends. For most Mosquitto migrations, the goal is to preserve existing credentials without requiring user password resets.

#### Option 1: Recreate Users (Batch Import)

If you have access to the original plain-text passwords, you can batch import them via the EMQX HTTP API.

**Batch Import CSV Format:**
Create a file `users.csv` with the following columns:
```csv
user_id,password,is_superuser
device001,secret123,false
admin,adminPass,true
```

**Import Command:**
Use `curl` to upload the file. The `type=plain` parameter instructs EMQX to hash the passwords during import.

```bash
curl -v -u admin:public -X POST \
  -H "Content-Type: multipart/form-data" \
  -F "filename=@users.csv" \
  "http://localhost:18083/api/v5/authentication/password_based:built_in_database/import_users?type=plain"
```
* Replace `admin:public` with your Dashboard credentials.
* Ensure the authenticator (`password_based:built_in_database`) exists and matches your configuration.

#### Option 2: Import Mosquitto Password File (Advanced)

If you have a large number of users and only possess the `mosquitto.passwd` file (which contains hashed passwords), you can import them directly into EMQX's Built-in Database using an Erlang script.

**Step 1: Configure Authentication**

Before importing data, configure the [password-based authentication](../access-control/authn/pwoverview.md) in EMQX using the [built-in database](../access-control/authn/mnesia.md) backend. You must use the specific settings below to match Mosquitto's default hashing mechanism.

*   **Algorithm:** `pbkdf2`
*   **Mac Fun:** `sha512`
*   **Iterations:** 101
*   **DK Length:** 32

> **Note:** These parameters (`101` iterations, `sha512`) correspond exactly to Mosquitto's defaults. While they differ from EMQX's standard defaults (which prioritize stronger security), they are required to validate the imported credentials.

**Step 2: Copy Password File**

Copy your `mosquitto.passwd` file to the EMQX server (e.g., `/tmp/mosquitto.passwd`) and ensure the `emqx` user has read permissions.

**Step 3: Execute Import Script**

Run the following command on the EMQX node. This script reads the file, decodes the Base64 salt/hash, and inserts the user directly into the database.

```bash
emqx eval "
File = \"/tmp/mosquitto.passwd\",
{ok, Bin} = file:read_file(File),
Lines = binary:split(Bin, <<\"\n\">>, [global, trim]),
lists:foreach(fun(Line) ->
    case binary:split(Line, <<\":\">>) of
        [Username, <<\"\$7$\", Rest/binary>>] ->
            [_, SaltB64, HashB64] = binary:split(Rest, <<\"$\">>, [global]),
            Salt = base64:decode(SaltB64),
            Hash = binary:part(emqx_utils:bin_to_hexstr(base64:decode(HashB64), lower), 0, 64),
            Record = {user_info, {'mqtt:global', Username}, Hash, Salt, false},
            mnesia:dirty_write(emqx_authn_mnesia, Record);
        _ -> ok
    end
end, Lines)."
```

##### Alternative: External Database

For enterprise deployments requiring integration with existing user management systems, you can also migrate users to an external SQL database (MySQL, PostgreSQL). EMQX supports dynamic SQL queries, allowing flexible integration with various schema formats.

#### Option 3: Mutual TLS (mTLS)

If your Mosquitto setup relies on X.509 client certificates (Mutual TLS) for authentication, migration involves configuring the EMQX listeners to verify peer certificates.

**Mosquitto Config:**
```properties
require_certificate true
use_identity_as_username true
cafile /etc/mosquitto/ca.crt
```

**EMQX Config:**
```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    cacertfile = "/etc/emqx/certs/ca.crt"
    verify = verify_peer
    fail_if_no_peer_cert = true
  }
}
```

* Ensure you copy the same CA certificate (`ca.crt`) used by Mosquitto to EMQX.
* If `use_identity_as_username` was enabled, EMQX uses the Common Name (CN) as the username by default when `verify_peer` is active.

### Migrate Authorization (ACLs)

Once authentication is complete, migrate your topic-level access controls to match the original Mosquitto policy.

Mosquitto’s ACL syntax is very similar to EMQX’s `acl.conf`.

**Mosquitto (`acl_file`):**
```properties
user Alice
topic read sensors/#
pattern write devices/%u/data
```

**EMQX (`acl.conf`):**
```erlang
{allow, {user, "Alice"}, subscribe, ["sensors/#"]}.
{allow, all, publish, ["devices/${username}/data"]}.
```

* Replace `%u` with `${username}` (or `${clientid}`).
* Map `read` to `subscribe` and `write` to `publish`.

### Configure Data Integration (Replacing Bridges & Scripts)

Mosquitto uses bridges to forward messages and external scripts (Python/Node.js) for data processing. EMQX replaces these with the built-in [Rule Engine](../data-integration/rules.md) and [Data Integration](../data-integration/data-bridges.md).

> The EMQX Rule Engine allows you to select, filter, and transform messages before forwarding them to external systems through Connectors.

**Scenario: Forwarding Data to another Broker**
Instead of `connection bridge_name` in `mosquitto.conf`:

1. Create an **MQTT Broker Connector** in the EMQX Dashboard.
2. Create a **Rule** to select messages (e.g., `SELECT * FROM "#"`) and forward them to the connector.

**Scenario: Replacing a Python Processing Script**
If you have a script that subscribes to `sensors/+/temp`, filters values > 30, and writes to a database:
1. **Eliminate the script.**
2. **Create a Rule:**
    ```sql
    SELECT payload.temp as temperature, topic, timestamp
    FROM "sensors/+/temp"
    WHERE temperature > 30
    ```
3. **Add an Action:** Configure a Data Integration (e.g., InfluxDB, HTTP) to write the result directly.

## Phase 3: Update Devices and Integrations

### Update Client Connections

Since EMQX uses standard MQTT ports (1883/8883), most devices do not need configuration changes if they connect via DNS. Update your DNS records to point `mqtt.yourdomain.com` to the EMQX cluster load balancer or IP.

### Verify Connectivity

Monitor the EMQX Dashboard to ensure devices are connecting.
* Check the **Connections** count.

  > You can verify connections using:
  >
  > ```bash
  > emqx_ctl clients list
  > ```
  >
  > Or check the Dashboard under **Monitoring** -> **Clients**.

* Check **Logs** for authentication errors (often due to mismatched hashing algorithms or missing certificates).

## Advanced Migration Scenarios

This section is optional and applies if you require a zero-downtime migration.

### Bridge-Transition Strategy (Zero Downtime)

To migrate without service interruption:
1. **Deploy EMQX** alongside Mosquitto.
2. **Bridge Mosquitto to EMQX:** Configure Mosquitto to forward all messages to EMQX.
    ```properties
    # mosquitto.conf
    connection migrate_uplink
    address emqx-server:1883
    topic # out 0
    topic # in 0
    ```
3. **Migrate Consumers:** Point your backend applications to EMQX. They will receive data from both EMQX-connected and Mosquitto-connected devices.
4. **Migrate Devices:** Gradually move devices to the new EMQX endpoint.
5. **Decommission:** Once Mosquitto has no connections, remove the bridge and shut it down.

## Validation Checklist

Before switching production traffic, verify:

* [ ] **Listeners:** TCP (1883) and SSL (8883) ports are open and accepting connections.
* [ ] **Authentication:** Users can log in using existing credentials.
* [ ] **ACLs:** Users are restricted to their specific topics.
* [ ] **Data Flow:** Messages published by devices are received by subscribers/backend apps.
* [ ] **Persistence:** Retained messages are available after broker restart (ensure `retain_available = true`).

## Conclusion

Migrating from Mosquitto to EMQX provides a significant upgrade in scalability and reliability while maintaining protocol compatibility. By mapping your existing configuration and leveraging EMQX’s Rule Engine to replace external scripts and bridges, you can simplify your architecture and prepare your infrastructure for massive growth.
