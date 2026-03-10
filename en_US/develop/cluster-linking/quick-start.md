# Quick Start with Cluster Linking

This page provides a quick start guide to setting up cluster linking between two distant EMQX clusters.

## Prerequisites

Ensure you have compute resources in two different regions, each hosting an EMQX cluster. For this example, you can use `us-east` and `eu-west` regions, naming the clusters `cluster-us-east` and `cluster-eu-west`, respectively.

### Requirements

- EMQX version 5.8.0 or later
- Unique cluster names
- Network communication between clusters

Cluster Linking requires the MQTT listener of each cluster to be reachable from the other cluster's network. It is recommended that these MQTT listeners be placed behind a load balancer for even traffic distribution. For security, if using the public internet, ensure communication between clusters is secured using [TLS](./configuration.md) and strict [TLS or MQTT client authentication](../access-control/authn/authn.md).

## Set Up the First Cluster (cluster-us-east)

Set up the first cluster `cluster-us-east` using the following configuration snippet in the cluster's configuration file:

```bash
# Cluster Linking configuration
cluster {
  # This cluster's name
  name = "cluster-us-east"
  links = [
    {
      # Name of the second cluster
      name = "cluster-eu-west"
      # Endpoint of the second cluster's MQTT listener
      server = "emqx.us-east.myinfra.net:11883"
      clientid = "clink-us-east"
      topics = ["#"]
    }
  ]
}

# Dedicated listener for Cluster Linking connections
listeners {
  tcp.clink {
    bind = 11883
  }
}
```

This configuration specifies the following:
- The remote cluster is named `cluster-eu-west`.
- The cluster can be accessed at `emqx.us-east.myinfra.net:11883`.
- The Client ID prefix for Cluster Linking MQTT connections is `clink-us-east`.
- All messages (matching the `#` wildcard topic) will be forwarded to the local cluster.
- A dedicated listener for Cluster Linking connections is enabled on port `11883`.

## Set Up the Second Cluster (cluster-eu-west)

Set up the second cluster `cluster-eu-west` using the following configuration snippet in the cluster's configuration file:
```bash
# Cluster Linking configuration
cluster {
  name = "cluster-eu-west"
  links = [
    {
      name = "cluster-us-east"
      server = "emqx.eu-west.myinfra.net:11883"
      clientid = "clink-eu-west"
      topics = ["#"]
    }
  ]
}

# Dedicated listener for Cluster Linking connections
listeners {
  tcp.clink {
    bind = 11883
  }
}
```

This configuration is symmetrical to that of the first cluster. With both configurations in place, a symmetrical, bidirectional Cluster Link will be established between the two clusters once they are up and running. It is also possible to create _asymmetrical_ links, which will be covered later.

## Verify Cluster Linking

To confirm that clients connected to different clusters can now communicate using standard MQTT mechanisms, you can use the [MQTTX CLI](https://mqttx.app/cli) tool to publish messages from one cluster and subscribe to them from the other.

1. Start a subscriber on `cluster-us-east`:

   ```bash
   mqttx sub -h emqx.us-east.myinfra.net --topic linked/# --qos 1 --verbose
   [6/4/2024] [3:53:32 PM] › …  Connecting...
   [6/4/2024] [3:53:32 PM] › ✔  Connected
   [6/4/2024] [3:53:32 PM] › …  Subscribing to linked/#...
   [6/4/2024] [3:53:32 PM] › ✔  Subscribed to linked/#
   ```

2. Publish a message from `cluster-eu-west`:

   ```bash
   mqttx pub -h emqx.eu-west.myinfra.net --topic linked/42 --message "Hello from the other side!"
   [6/4/2024] [3:53:35 PM] › …  Connecting...
   [6/4/2024] [3:53:35 PM] › ✔  Connected
   [6/4/2024] [3:53:35 PM] › …  Message publishing...
   [6/4/2024] [3:53:35 PM] › ✔  Message published
   ```

3. Observe that the message is received by the subscriber:

   ```bash
   [6/4/2024] [3:53:35 PM] › topic: linked/42
   payload: Hello from the other side!
   ```

4. Repeat the process in the opposite direction:

   - Start a subscriber on `cluster-eu-west`:

     ```bash
     mqttx sub -h emqx.eu-west.myinfra.net --topic linked/# --qos 1 --verbose
     [6/4/2024] [3:54:12 PM] › …  Connecting...
     [6/4/2024] [3:54:12 PM] › ✔  Connected
     [6/4/2024] [3:54:12 PM] › …  Subscribing to linked/#...
     [6/4/2024] [3:54:12 PM] › ✔  Subscribed to linked/#
     ```

   - Publish a message from `cluster-us-east`:

     ```bash
     mqttx pub -h emqx.us-east.myinfra.net --topic linked/1 --message "Hello from US!"
     [6/4/2024] [3:54:15 PM] › …  Connecting...
     [6/4/2024] [3:54:15 PM] › ✔  Connected
     [6/4/2024] [3:54:15 PM] › …  Message publishing...
     [6/4/2024] [3:54:15 PM] › ✔  Message published
     ```

   - Observe that the message is received by the subscriber:

     ```bash
     [6/4/2024] [3:54:15 PM] › topic: linked/1
     payload: Hello from US!
     ```

Perfect! The Cluster Linking is working.

::: tip

Cluster Linking involves the propagation of subscription information between clusters, which is an asynchronous process. This typically takes only a few milliseconds, but it may cause a slight delay in message delivery if you publish a message immediately after subscribing.

:::

## Set Up Asymmetrical Links

To create an asymmetrical link, you need to slightly modify the `cluster-eu-west` configuration:

```bash
cluster {
  name = "cluster-eu-west"
  links = [
    {
      name = "cluster-us-east"
      server = "emqx.eu-west.myinfra.net:11883"
      clientid = "clink-eu-west"
      topics = []
    }
  ]
}
```

As you can see, the configuration is almost identical to the previous one, except the `topics` field is empty. This means that `cluster-eu-west` is now not interested in _any_ messages from `cluster-us-east`. This makes the Cluster Link _asymmetrical_, which is useful for one-way message forwarding between clusters.

If you repeat the message publishing and subscribing steps outlined above, you will notice that the message published from `cluster-us-east` is not received by the subscriber on `cluster-eu-west`.
