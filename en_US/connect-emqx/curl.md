# Use curl with EMQX

Curl is a widely used command-line tool for data transfer and automation. Since 2020, curl has supported the MQTT protocol, and starting from curl 8.19.0 (expected March 2026), it also supports MQTTS (MQTT over TLS).

With curl, developers can connect to EMQX, publish messages, and subscribe to topics directly from the command line, without installing any language-specific MQTT client SDKs. This makes curl a convenient choice for quick testing, scripting, and IoT prototyping.

This page explains how to use curl with EMQX for MQTT and MQTTS communication, including connection, publish/subscribe, authentication, TLS configuration, and common troubleshooting scenarios.

## curl Version Requirements

| Feature            | Minimum Version | Release          |
| ------------------ | --------------- | ---------------- |
| MQTT (`mqtt://`)   | 7.70.0          | April 2020       |
| MQTTS (`mqtts://`) | 8.19.0          | Early March 2026 |

Check your installed curl version:

```
curl --version
```

Ensure that `mqtt` (and `mqtts` for curl ≥ 8.19.0) appears in the **Protocols** list.

> If your curl version is too old, upgrade via your package manager or download from https://curl.se/download.
>
> On macOS, use `brew install curl` to install the latest version.

## MQTT Broker Setup

You need an MQTT broker to connect to. This guide uses EMQX, which supports both MQTT and MQTTS.

### EMQX Public Broker (Testing)

For quick testing without deploying your own broker, you can use the EMQX public broker.

| Parameter  | Value            |
| ---------- | ---------------- |
| Broker     | `broker.emqx.io` |
| MQTT Port  | `1883`           |
| MQTTS Port | `8883`           |

The public broker is intended for testing and demonstration purposes only.

### EMQX Enterprise Deployment

For production scenarios, connect curl to your own EMQX Enterprise deployment using the broker address, ports, authentication credentials, and TLS configuration defined in your environment.

Typical configurations include:

- Custom broker hostname or IP address
- MQTT and/or MQTTS listeners enabled in EMQX Enterprise
- Username/password authentication, token-based authentication, or mutual TLS
- Access control rules (ACLs) applied to topics

Refer to your EMQX Enterprise listener, authentication, and TLS configuration when constructing curl commands.

**Notes**

- Replace `broker.emqx.io` with your EMQX Enterprise broker address in all examples when testing against your own deployment.
- Ensure the corresponding MQTT or MQTTS listener is enabled in EMQX Enterprise before connecting.

### Connect to EMQX Enterprise with curl

In MQTT, clients establish a connection to the broker as part of an operation, such as subscribing to a topic or publishing a message. There is no separate “connect” command.

When using curl with EMQX Enterprise, a connection is established automatically when you run a subscribe or publish command using your Enterprise broker address (and authentication or TLS settings, if configured).

For example, the following command connects to EMQX Enterprise and subscribes to a topic in a single step:

```powershell
curl -N mqtts://your-enterprise-broker.example.com/curl/test
```

> **Note**
>
> In addition to self-managed EMQX Enterprise deployments, you can also use curl to connect to the fully managed MQTT service [EMQX Cloud](https://docs.emqx.com/en/cloud/latest/overview.html) (Serverless or Dedicated).
>
> The curl MQTT/MQTTS usage remains the same. Use the broker address, ports, and authentication information provided by EMQX Cloud.

## Understand curl MQTT URL Scheme

curl uses a URL-based syntax for MQTT operations:

```
mqtt[s]://[user:password@]broker[:port]/topic
```

| Component        | Description                   | Example                  |
| ---------------- | ----------------------------- | ------------------------ |
| `mqtt://`        | MQTT over TCP                 | `mqtt://broker.emqx.io`  |
| `mqtts://`       | MQTT over TLS (curl ≥ 8.19.0) | `mqtts://broker.emqx.io` |
| `user:password@` | Optional authentication       | `admin:secret@`          |
| `broker`         | Broker hostname               | `broker.emqx.io`         |
| `:port`          | Optional port                 | `:1883`, `:8883`         |
| `/topic`         | MQTT topic                    | `/sensor/temperature`    |

If no port is specified, curl uses the default:

- `1883` for `mqtt://`
- `8883` for `mqtts://`

## curl MQTT Output Format

When subscribing to a topic, curl outputs raw MQTT message data in the following format:

```
[2 bytes: topic length] [topic] [payload]
```

For example, a message `"hello"` on the topic `curl/test` appears as:

```
curl/testhello
```

This output is binary and not human-readable by default. See [Parse MQTT Messages](#parse-mqtt-messages) below for conversion examples.

## Subscribe to Topics

Subscribing keeps the connection open and prints incoming messages to `stdout`.

### Basic Subscription (Unencrypted)

```powershell
curl -N mqtt://broker.emqx.io/curl/test
```

The `-N` option disables output buffering, allowing messages to appear immediately.

### Secure Subscription with MQTTS (curl ≥ 8.19.0)

```powershell
curl -N mqtts://broker.emqx.io/curl/test
```

### Subscription with Authentication

```powershell
curl -N -u "username:password" \
  mqtts://your-broker.emqxsl.com/curl/test
```

## Parse MQTT Messages

When subscribing to a topic, curl outputs MQTT messages in a **binary format** rather than a formatted text representation.

Each incoming message is structured as:

- 2 bytes: topic length (big-endian)
- topic string
- payload (raw bytes)

As a result, the output appears as a concatenation of topic and payload and is not human-readable by default.

### Bash One-Liner

To make subscription output readable, pipe curl output through a shell parser:

```bash
curl -sN mqtt://broker.emqx.io/curl/test | \
  while IFS= read -r -d $'\0' d; do
    [ -n "$d" ] && \
      l=$(printf "%d" "'${d:0:1}") && \
      echo "[${d:1:$l}] ${d:1+$l}"
  done
```

Output format:

```
[curl/test] hello
```

This parser works as follows:

1. The stream is split on null bytes.
2. The topic length is extracted from the first byte (valid for topics shorter than 256 bytes).
3. The topic string and payload are extracted using the length prefix.
4. Each message is printed as `[topic] payload`.

### Save Raw Output for Inspection

To better understand the binary structure of MQTT messages, you can save the raw subscription output to a file and inspect it manually.

Save the output:

```powershell
curl -sN mqtt://broker.emqx.io/curl/test > messages.bin
```

Inspect the file using `hexdump`:

```
hexdump -C messages.bin
```

This allows you to clearly see the topic length prefix, topic bytes, and payload layout.

### Reusable Shell Function

For repeated use, the parser can be wrapped in a reusable shell function:

```bash
mqtt_subscribe() {
  curl -sN "$1" | while IFS= read -r -d $'\0' d; do
    [ -n "$d" ] && \
      l=$(printf "%d" "'${d:0:1}") && \
      echo "[${d:1:$l}] ${d:1+$l}"
  done
}
```

Usage example:

```
mqtt_subscribe "mqtt://broker.emqx.io/curl/test"
```

> For production use or complex parsing requirements, consider [MQTTX CLI](https://mqttx.app/cli), which provides properly formatted output, full MQTT 5.0 support, QoS handling, and wildcard subscriptions.

## Publish Messages

To publish, use curl's `-d` (data) flag with the message payload.

### Basic Publish (Unencrypted)

```powershell
curl -d "Hello from curl" \
  mqtt://broker.emqx.io/curl/test
```

### Secure Publish with MQTTS (curl ≥ 8.19.0)

```powershell
curl -d "Secure message from curl" \
  mqtts://broker.emqx.io/curl/test
```

### Publish JSON Payloads

```powershell
curl -d '{"sensor_id":"temp-001","value":23.5}' \
  mqtt://broker.emqx.io/sensors/temperature
```

### Publish with Authentication

```powershell
curl -u "username:password" \
  -d '{"status":"online"}' \
  mqtts://your-broker.emqxsl.com/devices/status
```

## Relevant curl Options

The following table summarizes curl command-line options used throughout this document.

| Option         | Description                                           | Typical Usage   |
| -------------- | ----------------------------------------------------- | --------------- |
| `-N`           | Disable output buffering (required for subscriptions) | Subscribe       |
| `-d`           | Message payload to publish                            | Publish         |
| `-u user:pass` | Username and password authentication                  | Authentication  |
| `-v`           | Verbose output (shows MQTT handshake)                 | Troubleshooting |
| `-s`           | Silent mode (suppress progress output)                | Scripts         |
| `--cacert`     | CA certificate for TLS verification                   | MQTTS           |
| `--cert`       | Client certificate for mutual TLS                     | MQTTS           |
| `--key`        | Client private key for mutual TLS                     | MQTTS           |
| `-k`           | Skip TLS verification (testing only)                  | Troubleshooting |

> For a complete list of curl options, see the official curl documentation.

## TLS Configuration (MQTTS)

### CA Certificate Verification

```bash
curl --cacert /path/to/ca.crt \
  -d "TLS verified message" \
  mqtts://your-broker.emqxsl.com/secure/topic
```

### Mutual TLS (mTLS)

```bash
curl --cacert /path/to/ca.crt \
  --cert /path/to/client.crt \
  --key /path/to/client.key \
  -d "mTLS message" \
  mqtts://your-broker.emqxsl.com/secure/topic
```

> Use `-k` to skip certificate verification for testing only.

## Common Use Cases

### Broker Connectivity Testing

```powershell
curl -v mqtt://broker.emqx.io/curl/test
```

This verifies DNS resolution, TCP connectivity, and MQTT handshake.

**Successful connection indicators**

When the connection is successful, the verbose output typically shows:

- The broker hostname resolving to one or more IP addresses
- A successful TCP connection to port `1883` (MQTT) or `8883` (MQTTS)
- An MQTT handshake completed without errors

If no errors are printed and the command exits normally, the connection to the EMQX broker is established.

### Shell Scripting and IoT Prototyping

Example: Simulate a temperature sensor publishing data every 5 seconds.

```bash
#!/bin/bash
BROKER="mqtt://broker.emqx.io"
TOPIC="sensors/room1/temperature"

while true; do
  TEMP=$(awk -v min=20 -v max=30 'BEGIN{srand(); print min+rand()*(max-min)}')
  PAYLOAD="{\"temperature\": $TEMP, \"timestamp\": $(date +%s)}"
  curl -s -d "$PAYLOAD" "$BROKER/$TOPIC"
  sleep 5
done
```

## Limitations of curl for MQTT

While curl is useful for testing and scripting, it has the following limitations:

| Limitation             | Description                          |
| ---------------------- | ------------------------------------ |
| QoS 0 only             | No support for QoS 1 or 2            |
| Binary output          | Subscription output is not formatted |
| No wildcards           | Cannot subscribe using `+` or `#`    |
| Single topic           | One topic per command                |
| No persistent sessions | Stateless connections                |

For advanced MQTT features, use [MQTTX CLI](https://mqttx.app/cli) or EMQX client SDKs.

## Verify MQTT Support in curl

```powershell
curl --version | grep -i mqtt
```

If the command produces output containing `mqtt` or `mqtts`, your curl build includes MQTT support. If MQTT is missing, you may need to:

- Upgrade curl
- Install a build with MQTT enabled
- Compile curl with `--enable-mqtt`

## Troubleshooting

This section lists common issues when using curl with EMQX and how to resolve them.

### Connection Refused or Timeout

**Description**

- `Connection refused`
- `Failed to connect to broker`
- Connection hangs and then times out

**Possible causes**

- Incorrect broker address or port
- Network firewall blocking MQTT/MQTTS ports
- Broker is not running or is not listening on the specified port

**Solution**

- Verify the broker address and port:
  - MQTT: `1883`
  - MQTTS: `8883`
- Check network connectivity using verbose mode:

```powershell
curl -v mqtt://broker.emqx.io/curl/test
```

### MQTT or MQTTS Not Supported by curl

**Description**

- `Protocol "mqtt" not supported`
- `Unknown protocol`

**Possible causes**

- curl was built without MQTT support
- curl version is too old

**Solution**

- Verify protocol support:

```powershell
curl --version
```

Ensure `mqtt` (and `mqtts` for TLS) appears in the **Protocols** list.

- Upgrade curl or install a build with MQTT enabled.

### TLS Handshake or Certificate Errors (MQTTS)

**Description**

- `SSL certificate problem`
- `TLS handshake failed`
- `Unable to get local issuer certificate`

**Possible causes**

- Missing or incorrect CA certificate
- Broker uses a private or self-signed certificate

**Solution**

- Specify the CA certificate explicitly:

```powershell
curl --cacert /path/to/ca.crt \
  mqtts://your-broker.emqxsl.com/topic
```

- For testing only, skip verification (not recommended for production):

```powershell
curl -k mqtts://your-broker.emqxsl.com/topic
```

### No Messages Received When Subscribing

**Description**

- Subscription command runs, but no output is displayed

**Possible causes**

- Output buffering enabled
- No messages published to the topic
- Topic name mismatch

**Solution**

- Always use `-N` for subscriptions:

```powershell
curl -N mqtt://broker.emqx.io/curl/test
```

- Verify that messages are being published to the same topic.

### Authentication Failed

**Description**

- Connection closes immediately
- Authorization or authentication errors in broker logs

**Possible causes**

- Incorrect username or password
- ACL restrictions on the topic

**Solution**

- Verify credentials:

```powershell
curl -u "username:password" \
  mqtts://your-broker.emqxsl.com/topic
```

- Check authentication and ACL configuration in EMQX.

## More Information

For a detailed, step-by-step walkthrough of using curl with MQTT and MQTTS, including background explanations, additional examples, and usage considerations, see the blog post: [Using curl for MQTT: Connect, Publish, and Subscribe with Secure IoT Communication](https://www.emqx.com/en/blog/using-curl-for-mqtt).

The blog complements this Enterprise-focused guide by providing deeper explanations and extended examples.

