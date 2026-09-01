# Migrate from HiveMQ to EMQX

This guide describes how to migrate an existing HiveMQ deployment to EMQX. It focuses on the common enterprise pattern where devices connect over TLS (port 8883) with either X.509 client certificates or username/password credentials managed by the HiveMQ Enterprise Security Extension (ESE). The objective is to reproduce equivalent connectivity, authentication, and data integration behaviors in EMQX using HOCON configuration and the rule engine.

## Migration at a Glance

The migration can be divided into three phases:

1. **Inventory HiveMQ Assets**: Collect the TLS keystores, `config.xml`, ESE files, and extension properties that define listeners, authentication, clustering, and data pipelines.
2. **Configure EMQX**: Translate HiveMQ settings into EMQX HOCON, convert keystores to PEM, recreate listener and cluster settings, and set up the authentication chain and rule engine.
3. **Update Devices and Integrations**: Redirect devices to the EMQX endpoint, deploy the EMQX server CA certificates, validate client identities, and migrate downstream integrations such as Kafka or Prometheus.

The following table summarizes key artifacts and their mapping between HiveMQ and EMQX:

| **Parameter / Artifact** | **HiveMQ (Example)** | **EMQX (Example)** | **Notes** |
| --- | --- | --- | --- |
| Endpoint hostname | `mqtt.internal.example.com` (as configured in load balancer / Control Center) | `mqtt.example.com` (EMQX load balancer / VIP) | Update device firmware or deployment manifests. |
| TLS assets | `conf/hivemq.jks` | `/etc/emqx/certs/server-cert.pem`, `/etc/emqx/certs/server-key.pem` | Convert JKS/PKCS12 to PEM with `keytool` + `openssl`. |
| Client authentication | ESE file realm (`credentials.xml`) | `authentication = [{mechanism = password_based, backend = built_in_database}]` | Import user list via REST API or Dashboard. |
| Client certificates | Stored as PEM in device fleet, validated by HiveMQ when mTLS enabled | Same device certs, EMQX listener `ssl_options.cacertfile = "device-ca.pem"` | No reprovisioning needed if using the same CA. |
| Cluster discovery | DNS or `extensions/*-discovery*/*.properties` | `cluster.discovery_strategy = dns` (or `static`, `etcd`, `k8s`) | Replace extension-based discovery with native EMQX strategy. |
| Kafka integration | `extensions/hivemq-kafka-extension/kafka-configuration.xml` | EMQX connectors + rules + actions (`SELECT ... FROM "device/+/data"`) | Use EMQX Data Integration instead of Java-based extensions for message transformation. |
| Rate limiting / restrictions | `<restrictions>` block + Overload Protection | `listeners.*.max_connections`, `messages_rate`, `bytes_rate`, `limiter.*` | Configure per-listener quotas and global limiters. |

## Phase 1: Inventory HiveMQ Configuration Artifacts

### Collect and Convert TLS Keystores

1. Locate the keystore referenced in `<tls-tcp-listener>` (for example, `/opt/hivemq/conf/hivemq.jks`).
2. Export the server certificate and key:

```
keytool -importkeystore \
  -srckeystore /opt/hivemq/conf/hivemq.jks \
  -destkeystore /tmp/hivemq.p12 \
  -deststoretype PKCS12

openssl pkcs12 -in /tmp/hivemq.p12 -nodes -nokeys -out /tmp/server-cert.pem
openssl pkcs12 -in /tmp/hivemq.p12 -nodes -nocerts -out /tmp/server-key.pem
```

3. Copy the resulting PEM files into `/etc/emqx/certs/` (or your container secret mount). Keep the device CA (`device-ca.pem`) trusted by HiveMQ; EMQX will reuse this certificate for mTLS validation.

### Export HiveMQ Configuration Files

Store the following artifacts in version control for traceability. Highlight environment-variable placeholders (e.g., `${ENV:HIVEMQ_PORT}`) so they can be remapped to EMQX’s double-underscore environment override syntax (`EMQX_LISTENERS__TCP__DEFAULT__BIND=0.0.0.0:1883`).

- `conf/config.xml`: Listeners, restrictions, clustering, persistence, Control Center users
- `conf/logback.xml`: Logging targets (translate to EMQX `log` section)
- `extensions/<name>/conf/*.xml` or `.properties`: Discovery, Kafka, Prometheus, custom auth
- `extensions/hivemq-enterprise-security-extension/enterprise-security-extension.xml`: Authentication realms and pipelines
- Any `credentials.xml` or custom user stores referenced by the ESE

### Classify Authentication Modes

Determine which authentication method you are using:

- **Username/password** via file realm or SQL realm
- **X.509 client certificates** (mTLS) with CN = client ID
- **Hybrid** (e.g., TLS + SASL plugin). 

Each path maps to a specific EMQX authenticator chain.

## Phase 2: Configure EMQX to Mirror HiveMQ Baseline

### Recreate MQTT Listeners

Translate each `<tcp-listener>`, `<tls-tcp-listener>`, `<websocket-listener>` and `<tls-websocket-listener>` element into HOCON.

Example HiveMQ configuration:

```xml
<hivemq>
    <listeners>
        <tcp-listener>
            <port>1883</port>
            <bind-address>0.0.0.0</bind-address>
        </tcp-listener>
        <tls-tcp-listener>
            <port>8883</port>
            <bind-address>0.0.0.0</bind-address>
            <tls>
                <keystore>
                    <path>/opt/hivemq/conf/keystore.jks</path>
                    <password>password</password>
                    <private-key-password>pkpassword</private-key-password>
                </keystore>
                <truststore>
                    <path>/opt/hivemq/conf/truststore.jks</path>
                    <password>password</password>
                </truststore>
                <client-authentication-mode>NONE</client-authentication-mode>
            </tls>
        </tls-tcp-listener>
        <tls-websocket-listener>
            <port>8084</port>
            <bind-address>0.0.0.0</bind-address>
            <path>/mqtt</path>
            <subprotocols>
                <subprotocol>mqttv3.1</subprotocol>
                <subprotocol>mqtt</subprotocol>
            </subprotocols>
            <tls>
                <keystore>
                    <path>/opt/hivemq/conf/keystore.jks</path>
                    <password>hivemq</password>
                </keystore>
                <truststore>
                    <path>/opt/hivemq/conf/truststore.jks</path>
                    <password>hivemq</password>
                </truststore>
            </tls>
        </tls-websocket-listener>
    </listeners>
</hivemq>
```

Equivalent EMQX configuration snippet:

```hocon
listeners.tcp.default {
  bind = "0.0.0.0:1883"
}

listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    certfile = "/etc/certs/server-cert.pem"
    keyfile  = "/etc/certs/server-key.pem"
  }
}

listeners.wss.default {
  bind = "0.0.0.0:8083"
  mqtt_path = "/mqtt"
  ssl_options {
    certfile = "/etc/certs/server-cert.pem"
    keyfile  = "/etc/certs/server-key.pem"
  }
}
```

To convert `truststore.jks` and `keystore.jks` to PEM, follow the steps in [Collect and Convert TLS Keystores](#collect-and-convert-tls-keystores).

### Map MQTT Configuration Options

HiveMQ settings such as queue size, QoS, and retained message behavior map directly to EMQX’s `mqtt` section.

Example HiveMQ configuration:

```xml
<queued-messages>
    <max-queue-size>1000</max-queue-size>
    <strategy>discard</strategy>
</queued-messages>

<topic-alias>
    <enabled>true</enabled>
    <max-per-client>5</max-per-client>
</topic-alias>

<message-expiry>
    <max-interval>4294967296</max-interval>
</message-expiry>

<session-expiry>
    <max-interval>4294967295</max-interval>
</session-expiry>

<packets>
    <max-packet-size>268435460</max-packet-size>
</packets>

<receive-maximum>
    <server-receive-maximum>10</server-receive-maximum>
</receive-maximum>

<quality-of-service>
    <max-qos>2</max-qos>
</quality-of-service>

<wildcard-subscriptions>
    <enabled>true</enabled>
</wildcard-subscriptions>

<shared-subscriptions>
    <enabled>true</enabled>
</shared-subscriptions>

<subscription-identifier>
    <enabled>true</enabled>
</subscription-identifier>

<retained-messages>
    <enabled>true</enabled>
</retained-messages>
```

Corresponding EMQX configuration:

```hocon
mqtt {
  max_mqueue_len          = 1000
  mqueue_priorities       = disabled
  max_topic_alias         = 5
  message_expiry_interval = infinity   # 4294967296 in HiveMQ
  session_expiry_interval = infinity
  max_packet_size         = "256MB"
  max_inflight            = 10
  max_qos_allowed         = 2
  wildcard_subscription   = true
  shared_subscription     = true
  retain_available        = true
  # subscription_identifier is enabled by default
}
```

### Map the `<restrictions>` Block

HiveMQ collects global limits under `<restrictions>`. EMQX splits these values between the global `mqtt` section and each listener.

Example HiveMQ configuration:

```xml
<restrictions>
    <max-client-id-length>65535</max-client-id-length>
    <max-connections>-1</max-connections>
    <incoming-bandwidth-throttling>0</incoming-bandwidth-throttling>
    <no-connect-idle-timeout>10000</no-connect-idle-timeout>
</restrictions>
```

Equivalent EMQX configuration snippet:

```hocon
listeners.ssl.default {
  bind             = "0.0.0.0:8883"
  max_connections  = infinity
  bytes_rate       = "0"        # 'incoming-bandwidth-throttling'
  bytes_burst      = "0"
}

mqtt {
  max_clientid_len = 65535
  idle_timeout     = "10s"      # no-connect-idle-timeout
}
```

### Configure Clustering

Replace HiveMQ discovery extension and other discovery methods with EMQX’s native strategies.

Example HiveMQ cluster configuration:

```xml
<cluster>
    <enabled>true</enabled>
    <transport>
        <tcp>
            <bind-address>127.0.0.1</bind-address>
            <bind-port>7800</bind-port>
        </tcp>
    </transport>
    <discovery>
        <static>
            <node>
                <host>127.0.0.1</host>
                <port>7800</port>
            </node>
            <node>
                <host>127.0.0.1</host>
                <port>7801</port>
            </node>
        </static>
    </discovery>
</cluster>
```

Equivalent EMQX configuration:

```
cluster {
  discovery_strategy = static
  static {
    seeds = [
      "emqx1@127.0.0.1",
      "emqx2@127.0.0.1"
    ]
  }
}
```

EMQX automatically assigns the Erlang distribution port when more than one node runs on the same machine; no need to select `bind-port` manually.

For alternative discovery methods (etcd, Kubernetes, static files, etc.), see [Create and Manage Cluster](../deploy/cluster/create-cluster.md).

### Translate Authentication and Authorization

HiveMQ manages security through the Enterprise Security Extension (ESE), which defines **Realms** (data sources) and **Pipelines** (logic), or through legacy plugins. EMQX uses **Authentication Chains** (ordered backends) and **Authorization sources** (ACLs).

| HiveMQ ESE Component | EMQX Equivalent | Migration Strategy |
| :--- | :--- | :--- |
| **File Realm** (`credentials.xml`) | [**Built-in Database**](../access-control/authn/mnesia.md) | Export HiveMQ users and import via the EMQX REST API. |
| **SQL Realm** (JDBC) | [**MySQL**](../access-control/authn/mysql.md) / [**PostgreSQL**](../access-control/authn/postgresql.md) | Configure password-based authentication with a `mysql` or `postgresql` backend. Reuse existing user tables. |
| **LDAP Realm** / AD | [**LDAP**](../access-control/authn/ldap.md) | Configure password-based authentication with an LDAP backend. Map HiveMQ DN patterns to EMQX filter templates. |
| **OAuth / JWT** | [**JWT**](../access-control/authn/jwt.md) | Configure JWT authentication mechanism. Configure public keys or JWKS endpoint. |
| **HTTP / Webhooks** | [**HTTP Server**](../access-control/authn/http.md) | Configure password-based authentication with an HTTP backend to delegate credentials to your external auth service. |
| **X.509 Certs** | [**X.509**](../access-control/authn/x509.md) / [**mTLS**](../network/emqx-mqtt-tls.md#enable-ssl-tls-with-two-way-authentication) | Use `TLS` listener and mutual (two-way) authentication, reusing existing CA and client certificates. |

#### Migrate File Realm Users

**Source:** HiveMQ `conf/credentials.xml` (encrypted/hashed)

**Destination:** EMQX Built-in Database

1. **Export:** Extract users from the HiveMQ File Realm (`credentials.xml`). This file typically contains hashed passwords and salts. You will need to parse this XML to generate a JSON or CSV import file for EMQX.
2. **Import:** Use the EMQX REST API to create users. EMQX supports bulk import of users with password hashes (e.g., bcrypt, pbkdf2). See [Importing Users](../access-control/authn/user_management.md#importing-users) for file format details.

```bash
# Example: Import a user with a plain password
curl -u admin:public -X POST \
  http://emqx-node:18083/api/v5/authentication/password_based:built_in_database/users \
  -d '{"user_id":"device-001","password":"StrongPass!"}'
```

#### Migrate External Integrations (SQL, LDAP, HTTP)

Translate your `enterprise-security-extension.xml` pipelines into EMQX HOCON `authentication` blocks.

**Example: SQL Realm to EMQX MySQL**

HiveMQ uses a [fixed database schema](https://docs.hivemq.com/hivemq-enterprise-security-extension/latest/ese.html#table_users) for its SQL realm. In contrast, EMQX allows you to [define your own schema and queries](../access-control/authn/mysql.md). **This means you do not need to modify your existing MySQL or PostgreSQL database.** 

The EMQX configuration example below uses a query (`SELECT password_hash, salt ...`) that is specifically adapted to work with the standard HiveMQ `users` table structure.

Use the following EMQX configuration to authenticate against your existing HiveMQ MySQL database without modifying the schema:

```hocon
authentication = [
  {
    mechanism = "password_based"
    backend = "mysql"
    server = "127.0.0.1:3306"
    database = "mqtt"
    username = "root"
    password = ""
    query = "SELECT password_hash, salt FROM users WHERE username = ${username}"
    password_hash_algorithm {
        name = "sha256"
        salt_position = "suffix"
    }
  }
]
```

**Example: LDAP Realm**

```hocon
authentication = [
  {
    mechanism = "password_based"
    backend = "ldap"
    server = "ldap.example.com:636"
    ssl {
      enable = true
    }
    method {
      type = bind
      bind_password = "${password}"
    }
    username = "root"
    password = "root password"
    base_dn = "uid=${username},ou=testdevice,dc=emqx,dc=io"
    filter = "(objectClass=mqttUser)"
  }
]
```

#### Migrate Authorization (ACLs)

HiveMQ defines access policies in `enterprise-security-extension.xml` (File Realm) or external databases. EMQX uses a flexible **Authorization Chain** supporting multiple backends simultaneously (File, Redis, MySQL, PostgreSQL, MongoDB, HTTP, etc).

**HiveMQ XML Policy:**

```xml
<permission>
    <topic>device/${clientid}/#</topic>
    <activity>ALL</activity>
</permission>
```

**EMQX Equivalent:**

- [**File (`acl.conf`)**](../access-control/authz/file.md): `{allow, all, subscribe, ["device/${clientid}/#"]}.`
- [**Built-in Database**](../access-control/authz/mnesia.md): Configure rules via Dashboard or API based on Client ID, Username, or Topic.
- [**MySQL**](../access-control/authz/mysql.md): `SELECT action, permission, topic, ipaddress, qos, retain FROM mqtt_acl where clientid = ${clientid} and ipaddress = ${peerhost}`.
- [**PostgreSQL**](../access-control/authz/postgresql.md): `SELECT action, permission, topic, ipaddress, qos, retain FROM mqtt_acl where clientid = ${clientid} and ipaddress = ${peerhost}`.

For more information, refer to the [**Authorization**](../access-control/authz/authz.md) documentation.

### Configure Data Integration

HiveMQ relies on individual extensions (e.g., Kafka Extension) for data integration. In EMQX, all data integrations are built in and enabled out of the box.

Before configuring specific integrations, familiarize yourself with these core concepts:
- [**Data Integration Overview**](../data-integration/data-bridges.md)
- [**Rule Engine**](../data-integration/rules.md)
- [**Flow Designer**](../flow-designer/introduction.md)

#### Example: Migrate Kafka Extension

HiveMQ Kafka extension configuration:
```xml
<kafka-configuration xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:noNamespaceSchemaLocation="config.xsd">
    <kafka-clusters>
        <kafka-cluster>
            <id>cluster01</id>
            <bootstrap-servers>127.0.0.1:9092</bootstrap-servers>
        </kafka-cluster>
    </kafka-clusters>

    <mqtt-to-kafka-mappings>
        <mqtt-to-kafka-mapping>
            <id>mapping01</id>
            <cluster-id>cluster01</cluster-id>
            <mqtt-topic-filters>
                <mqtt-topic-filter>#</mqtt-topic-filter>
            </mqtt-topic-filters>
            <kafka-topic>emqx</kafka-topic>
        </mqtt-to-kafka-mapping>
    </mqtt-to-kafka-mappings>

    <kafka-to-mqtt-mappings>
        <kafka-to-mqtt-mapping>
            <id>mapping02</id>
            <cluster-id>cluster01</cluster-id>
            <kafka-topics>
                <kafka-topic>topic1</kafka-topic>
                <kafka-topic>topic2</kafka-topic>
            </kafka-topics>
        </kafka-to-mqtt-mapping>
    </kafka-to-mqtt-mappings>
</kafka-configuration>
```

Equivalent EMQX configuration:

```hocon
connectors {
  kafka_producer {
    cluster01 {
      bootstrap_hosts = "127.0.0.1:9092"
      enable = true
    }
  }
  kafka_consumer {
    cluster01 {
      bootstrap_hosts = "127.0.0.1:9092"
      enable = true
    }
  }
}
actions {
  kafka_producer {
    mapping01 {
      connector = "cluster01"
      enable = true
      parameters {
        message {
          value = "${.}"
        }
        topic = "emqx"
      }
    }
  }
}
rule_engine {
  rules {
    mqtt-to-kafka-mapping-mapping01 {
      sql = "SELECT * FROM '#'"
      actions = [
        "kafka_producer:mapping01"
      ]
      enable = true
    }
    kafka-to-mqtt-mapping-mapping02 {
      actions = [
        {
          args {
            topic = "kafka"
          }
          function = "republish"
        }
      ]
      enable = true
      sql = "SELECT * FROM '$bridges/kafka_consumer:cluster01-topic1','$bridges/kafka_consumer:cluster01-topic2'"
    }
  }
}
sources {
  kafka_consumer {
    cluster01-topic1 {
      connector = "cluster01"
      parameters {
        topic = "topic1"
      }
      enable = true
    }
    cluster01-topic2 {
      connector = "cluster01"
      parameters {
        topic = "topic2"
      }
      enable = true
    }
  }
}
```

### Configure Observability

#### Prometheus

HiveMQ uses the "Prometheus Monitoring HiveMQ Extension". EMQX comes with native Prometheus support.

Endpoint for Prometheus to scrape metrics is enabled by default: `http://emqx-node:18083/api/v5/prometheus/stats`.

Starting from EMQX 6.3.0, Prometheus scrape endpoints require authentication by default. Configure the scraper with an API key and secret key, or explicitly disable authentication. For details, see [Integrate with Prometheus](../observability/prometheus.md#authentication).

If you want to use Pushgateway, it can be configured as follows:

```hocon
prometheus {
  push_gateway {
    enable = true
    url = "http://127.0.0.1:9091"
  }
}
```

For Pushgateway configuration details, see [Integrate with Prometheus](../observability/prometheus.md#configure-push-mode-integration).

#### Logging

HiveMQ uses `logback.xml` (Java standard). EMQX uses a built-in logging facility configured in HOCON.

HiveMQ (`logback.xml`) configuration:
```xml
<appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
    <encoder>
        <pattern>%-30(%d %level)- %msg%n%ex</pattern>
    </encoder>
</appender>

<appender name="FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
    <file>${hivemq.log.folder}/hivemq.log</file>
    <append>true</append>

    <rollingPolicy class="ch.qos.logback.core.rolling.TimeBasedRollingPolicy">
        <!-- daily rollover -->
        <fileNamePattern>${hivemq.log.folder}/hivemq.%d{yyyy-MM-dd}.log</fileNamePattern>

        <!-- keep 30 days' worth of history -->
        <maxHistory>30</maxHistory>
    </rollingPolicy>
    <encoder>
        <pattern>%-30(%d %level)- %msg%n%ex</pattern>
    </encoder>
</appender>
```

Equivalent EMQX configuration:
```hocon
log {
  file {
    default {
      enable = true
      level = warning
      path = "/var/log/emqx/emqx.log"
      rotation_count = 30
      rotation_size = "50MB"

    }
  }
  console {
    enable = true
    level = warning
  }
}
```

See [Logs](../observability/log.md) for configuring log levels, rotation, and formatters (text/JSON).

#### Log Trace

HiveMQ's "Trace Recordings" are often used to debug specific client sessions. EMQX provides a built-in **Log Trace** feature (Dashboard or CLI) to filter logs for specific Client IDs, Topics, or IPs in real-time.

To start a trace for a specific client, use the following command:

```bash
emqx ctl trace start client device-001 trace.log
```

See [Log Trace](../observability/tracer.md) for advanced debugging.

## Phase 3: Update Devices and Integrations

### Deploy EMQX Server CA to Devices

- If EMQX uses an internal CA, install `device-ca.pem` on each device (system trust store or application bundle).
- If EMQX uses a public CA (e.g., Let’s Encrypt), no device action is needed.

### Update Device Connection Parameters

**Example (mqtt-cli)**

```bash
# Before (HiveMQ)
mqtt pub -h mqtt.internal.example.com -p 8883 \
  -u device-001 -pw StrongPass! \
  --cafile device-ca.pem --topic device/001/data --message test

# After (EMQX)
mqtt pub -h mqtt.example.com -p 8883 \
  -u device-001 -pw StrongPass! \
  --cafile device-ca.pem --topic device/001/data --message test
```

**Example (Python paho-mqtt with mTLS)**

```hocon
client.tls_set(
    ca_certs="certs/device-ca.pem",
    certfile="certs/device-001.cert.pem",
    keyfile="certs/device-001.key.pem",
    tls_version=ssl.PROTOCOL_TLS_CLIENT
)
client.connect("mqtt.example.com", 8883)
```

Only the endpoint hostname and server CA file change. Device certificates and private keys continue to work if they were signed by the same CA referenced in EMQX `ssl_options.cacertfile`.

### Validate Integrations

- Ensure Kafka topics receive messages by checking EMQX rule metrics (`emqx ctl rule show`).
- Update monitoring dashboards to scrape EMQX metrics.
- Reconfigure alerting systems (Splunk, ELK) to parse EMQX log format.

## Advanced Migration Scenarios

### Retained Messages and Sessions

HiveMQ persistence files cannot be imported directly. Use a migration script:

1. Keep HiveMQ running temporarily.
2. Run a bridge client that subscribes to `#` on HiveMQ and republishes retained messages to EMQX.
3. For queued QoS 1/2 messages, complete in-flight transactions before switching DNS.

### Shared Subscriptions

HiveMQ’s `$share/group/topic` syntax is fully supported by EMQX. If you previously used `$queue/topic`, map it to `$share/queue/topic`. Tune `broker.shared_subscription_strategy` (e.g., `round_robin`, `hash_clientid`) to mimic the load-balancing behavior your consumers expect.

### HTTP/API-Driven Configuration

HiveMQ relies on static XML plus extension-specific reload semantics. EMQX offers dynamic configuration APIs:

```bash
curl -s -H "Authorization: Bearer $TOKEN" \
  -H "Content-type: application/json" \
  -X PUT "http://emqx-node:18083/api/v5/listeners/ssl:default" \
  -d '{"type": "ssl", "bind": "0.0.0.0:8883", "id": "ssl:default", "max_connections": 200000}'
```

This writes to `data/configs/cluster.hocon`. Decide whether to keep configuration immutable (only `emqx.conf`) or adopt EMQX’s dual-layer model for per-environment overrides.

## Validation Checklist

Before switching production traffic, verify:

- All EMQX listeners report `running` (`emqx ctl listeners list`).
- TLS handshake succeeds and fails when no client certificate is provided (for mTLS devices).
- Device IDs in EMQX sessions match original HiveMQ client IDs.
- ACLs enforce the same topic access you enforced in HiveMQ.
- Cluster nodes auto-heal after simulated network partitions.
- Kafka integration receives data without transformation regressions.
- Metrics are visible in Prometheus.

## Conclusion

Migrating from HiveMQ to EMQX is primarily a configuration translation process: converting Java-centric artifacts (XML, JKS, extensions) into EMQX’s HOCON configuration, flexible Authentication Chains, and the Data Integration framework. 

By following the three phases: inventory, configure, and update, you can preserve device credentials, topic structures, and integration flows while gaining EMQX’s high-concurrency Erlang runtime and dynamic configuration capabilities. 

Plan the migration carefully, validate each listener and integration, and execute the cutover with confidence.
