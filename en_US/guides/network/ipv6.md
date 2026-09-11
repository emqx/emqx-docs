# IPv6

EMQX fully supports IPv6 for client connections, the Dashboard, inter-node clustering, and outbound connections to external services. This page explains how to configure EMQX for IPv6 environments, from single-stack (IPv6-only) to dual-stack deployments.

## MQTT Listeners

To accept MQTT client connections over IPv6, bind the listener to an IPv6 address. EMQX automatically enables the `inet6` socket option when it detects an IPv6 bind address.

### Dual-Stack (IPv4 and IPv6)

Bind to `[::]` to accept both IPv4 and IPv6 connections on the same port:

```bash
listeners.tcp.default {
  bind = "[::]:1883"
}
```

::: tip

On most operating systems, binding to `[::]` accepts both IPv4 and IPv6 connections by default (dual-stack). This is the simplest configuration for environments that need to support both protocols.

:::

### IPv6-Only

To restrict the listener to IPv6 connections only, set `ipv6_v6only = true`:

```bash
listeners.tcp.default {
  bind = "[::]:1883"
  ipv6_v6only = true
}
```

This sets the `IPV6_V6ONLY` socket option, which prevents IPv4-mapped IPv6 addresses from being accepted.

### Bind to a Specific IPv6 Address

You can bind to a specific IPv6 address:

```bash
listeners.tcp.default {
  bind = "[::1]:1883"
}
```

The same configuration applies to SSL, WebSocket, and Secure WebSocket listeners:

```bash
listeners.ssl.default {
  bind = "[::]:8883"
  ssl_options {
    certfile = "etc/certs/cert.pem"
    keyfile = "etc/certs/key.pem"
    cacertfile = "etc/certs/cacert.pem"
  }
}

listeners.ws.default {
  bind = "[::]:8083"
}

listeners.wss.default {
  bind = "[::]:8084"
}
```

## Dashboard HTTP/HTTPS Listeners

The EMQX Dashboard HTTP/HTTPS listener also supports IPv6.

### Use an IPv6 Bind Address

When the `bind` address is an IPv6 address, EMQX automatically enables IPv6 for the Dashboard listener:

```bash
dashboard.listeners.http {
  bind = "[::]:18083"
}
```

### Use the `inet6` Flag

Alternatively, if using a port-only bind (without an explicit IP address), you can enable IPv6 explicitly:

```bash
dashboard.listeners.http {
  bind = 18083
  inet6 = true
}
```

| Parameter    | Type    | Default | Description                                                                 |
| ------------ | ------- | ------- | --------------------------------------------------------------------------- |
| `inet6`      | boolean | `false` | Enable IPv6 support. When `false`, the listener accepts only IPv4 traffic.  |
| `ipv6_v6only`| boolean | `false` | Disable IPv4-to-IPv6 mapping. Only effective when `inet6` is `true`.        |

## Cluster Communication

When nodes in an EMQX cluster communicate over an IPv6 network, two components need configuration: the Erlang distribution protocol (used for cluster coordination) and the Gen RPC channel (used for data forwarding between nodes).

### Erlang Distribution Protocol

Set `cluster.proto_dist` to use IPv6 for inter-node communication:

```bash
cluster.proto_dist = inet6_tcp
```

Available options:

| Value        | Description                                              |
| ------------ | -------------------------------------------------------- |
| `inet_tcp`   | TCP over IPv4 (default)                                  |
| `inet6_tcp`  | TCP over IPv6                                            |
| `inet_tls`   | TLS over IPv4, configured via `etc/ssl_dist.conf`        |
| `inet6_tls`  | TLS over IPv6, configured via `etc/ssl_dist.conf`        |

::: warning Important Notice

When using IPv6 node names (for example, `emqx@::1`), you **must** set `cluster.proto_dist` to `inet6_tcp` or `inet6_tls`. Otherwise, the node will fail to start with errors such as "not responding to pings".

:::

### Gen RPC

Configure the Gen RPC channel for IPv6:

```bash
rpc.listen_address = "::"
rpc.ipv6_only = true
```

| Parameter           | Type    | Default     | Description                                                                                  |
| ------------------- | ------- | ----------- | -------------------------------------------------------------------------------------------- |
| `rpc.listen_address`| string  | `0.0.0.0`   | IP address for the RPC server. Use `0.0.0.0` for IPv4 or `::` for IPv6.                     |
| `rpc.ipv6_only`     | boolean | `false`     | When `listen_address` is IPv6, setting this to `true` forces the RPC client to use IPv6 only. |

## Outbound Connections

EMQX makes outbound connections to external services for features such as HTTP authentication, webhook actions, and database integrations.

### Automatic IPv6 Detection

EMQX provides IPv6 support for different outbound connection types:

- For HTTP-based connectors (authentication backends, webhook actions, etc.), EMQX automatically probes whether the target host supports IPv6 and selects the appropriate address family. No manual configuration is required in most cases.
- For MQTT connectors, EMQX can connect to IPv6-only brokers, including brokers whose hostnames resolve only to IPv6 `AAAA` records. The MQTT broker address can also use a bracketed IPv6 literal, such as `[::1]:1883` or `mqtt://[::1]:1883`.

### Manual Override

Some connector types expose an `ipv6_probe` toggle in their configuration. When enabled (the default for HTTP connectors), EMQX attempts an IPv6 connection first. If your network is IPv4-only and DNS returns both A and AAAA records, you can disable the probe to avoid connection delays:

```bash
# Example: HTTP authentication backend
authentication {
  backend = "http"
  method = "post"
  url = "http://auth-server.example.com:8080/auth"

  # Disable IPv6 auto-detection if not needed
  pool_size = 8
}
```

## Complete IPv6-Only Example

Below is a minimal `emqx.conf` for an IPv6-only deployment:

```bash
# Node name using IPv6 address
node.name = "emqx@::1"

# Cluster distribution over IPv6
cluster.proto_dist = inet6_tcp

# Gen RPC over IPv6
rpc.listen_address = "::"
rpc.ipv6_only = true

# MQTT listener on IPv6
listeners.tcp.default {
  bind = "[::]:1883"
  ipv6_v6only = true
}

# Dashboard on IPv6
dashboard.listeners.http {
  bind = "[::]:18083"
}
```

## Troubleshooting

### Node Not Responding to Pings

**Symptom**: When starting a cluster node with an IPv6 node name, the node reports "not responding to pings" and fails to start.

**Cause**: The Erlang distribution protocol defaults to `inet_tcp` (IPv4). An IPv6 node name requires `inet6_tcp`.

**Solution**: Set `cluster.proto_dist = inet6_tcp` in `emqx.conf`.

### `enetunreach` Errors in Outbound Connections

**Symptom**: Outbound HTTP requests (e.g., to authentication backends) fail with `enetunreach` (network unreachable).

**Cause**: The connection is attempting to use IPv4 to reach an IPv6-only service, or vice versa.

**Solution**: Verify that the target service is reachable from the EMQX host using the correct address family. For HTTP connectors, the automatic IPv6 probe should handle this. If the service is behind a DNS name, ensure the DNS returns the correct record type (A for IPv4, AAAA for IPv6).

### Dashboard Unreachable on IPv6

**Symptom**: The Dashboard is not accessible when EMQX is running in an IPv6-only environment.

**Cause**: The Dashboard listener defaults to IPv4 (`0.0.0.0:18083`).

**Solution**: Either bind the Dashboard to an IPv6 address (`bind = "[::]:18083"`), or explicitly enable IPv6 with `inet6 = true`.
