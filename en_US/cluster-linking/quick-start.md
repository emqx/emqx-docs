# Setup Cluster Link

This page provides a quick start guide to set up a Cluster Link between two distant EMQX clusters.

## Prerequisites

Suppose you have compute resources in two different regions and you want to have a EMQX cluster in each region, with the ability to link the two clusters together. Let's say the regions are `us-east` and `eu-west`, thus the clusters are named `cluster-us-east` and `cluster-eu-west` respectively.

This is easily achievable if those clusters:
1. Are running EMQX 5.8.0 or later.
2. Have distinct, unique Cluster names.
3. Are able to communicate with each other over the network.

Last point is crucial. Cluster Linking requires both `cluster-us-east` MQTT listener to be reachable from the `cluster-eu-west` network, and vice versa, otherwise the link will not be fully operational. While not strictly necessary, it is strongly recommended to put those MQTT listeners behind a load balancer, to ensure even distribution of Cluster Linking protocol traffic.

For simplicity, suppose also that the clusters will talk to each other over a VPN or similar mechanism, essentially taking care of most of the security concerns, thus simplifying all our example configurations. If you are going to use the public internet in your setup, make sure to secure the communication between the clusters using [TLS][1], and strict [TLS or MQTT client authentication][2].

[1]: ./configuration.md
[2]: ../access-control/authn/authn.md

## Spin up first cluster

Let's start by setting up the first cluster, `cluster-us-east`.

Here's a configuration snippet that does the job:
```
# Cluster Linking configuration
cluster {
  # This cluster's name
  name = "cluster-us-east"
  links = [
    { enable = true
      # Name of the second cluster
      upstream = "cluster-eu-west"
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
    enable = true
    bind = 11883
  }
}
```

This configuration basically states that:
1. The remote cluster is named `cluster-eu-west`.
2. It's reachable at `emqx.us-east.myinfra.net:11883`.
3. Use `clink-us-east` as the Client ID prefix for Cluster Linking MQTT connections.
4. Indicate that _all_ messages (i.e. having topics matching `#` wildcard) should be forwarded to the local cluster.
5. Enable a dedicated listener for Cluster Linking connections on port 11883.

## Spin up second cluster

Next, let's set up the second cluster, `cluster-eu-west`. The configuration snippet is similar to the first cluster:
```
# Cluster Linking configuration
cluster {
  name = "cluster-eu-west"
  links = [
    { enable = true
      upstream = "cluster-us-east"
      server = "emqx.eu-west.myinfra.net:11883"
      clientid = "clink-eu-west"
      topics = ["#"]
    }
  ]
}

# Dedicated listener for Cluster Linking connections
listeners {
  tcp.clink {
    enable = true
    bind = 11883
  }
}
```

As you can see, the configuration is almost symmetrical to the first cluster. With this configuration in place, once both clusters are up a _symmetrical_ bidirectional Cluster Link should be established between these two clusters. Actually, _asymmetrical_ links are also possible, but more on that later.

## Verify Cluster Link works

The expectation is that the clients talking to different clusters can now communicate with each other through usual MQTT mechanisms. To verify this, let's use `mqttx` CLI tool to publish messages from one cluster and subscribe to them from the other.

1. Start a subscriber on `cluster-us-east`:
```bash
$ mqttx sub -h emqx.us-east.myinfra.net --topic linked/# --qos 1 --verbose
[6/4/2024] [3:53:32 PM] › …  Connecting...
[6/4/2024] [3:53:32 PM] › ✔  Connected
[6/4/2024] [3:53:32 PM] › …  Subscribing to linked/#...
[6/4/2024] [3:53:32 PM] › ✔  Subscribed to linked/#
```

2. Publish a message from `cluster-eu-west`:
```bash
$ mqttx pub -h emqx.eu-west.myinfra.net --topic linked/42 --message "Hello from the other side!"
[6/4/2024] [3:53:35 PM] › …  Connecting...
[6/4/2024] [3:53:35 PM] › ✔  Connected
[6/4/2024] [3:53:35 PM] › …  Message publishing...
[6/4/2024] [3:53:35 PM] › ✔  Message published
```

3. Observe the message being received by the subscriber.
```
[6/4/2024] [3:53:35 PM] › topic: linked/42
payload: Hello from the other side!
```

4. Do the same in the opposite direction.
```bash
$ mqttx sub -h emqx.eu-west.myinfra.net --topic linked/# --qos 1 --verbose
[6/4/2024] [3:54:12 PM] › …  Connecting...
[6/4/2024] [3:54:12 PM] › ✔  Connected
[6/4/2024] [3:54:12 PM] › …  Subscribing to linked/#...
[6/4/2024] [3:54:12 PM] › ✔  Subscribed to linked/#
```

```bash
$ mqttx pub -h emqx.us-east.myinfra.net --topic linked/1 --message "Hello from US!"
[6/4/2024] [3:54:15 PM] › …  Connecting...
[6/4/2024] [3:54:15 PM] › ✔  Connected
[6/4/2024] [3:54:15 PM] › …  Message publishing...
[6/4/2024] [3:54:15 PM] › ✔  Message published
```

```
[6/4/2024] [3:54:15 PM] › topic: linked/1
payload: Hello from US!
```

Perfect! The Cluster Link is working.

::: tip
Cluster Linking protocol involves propagation of subscription information between clusters, which is an asynchronous process. It is unlikely to take more than a few tens of milliseconds, but it still means that the message may not be delivered if you publish it _immediately_ after subscribing.
:::

## Make the Link asymmetrical

Next, let's slightly modify the `cluster-eu-west` configuration.

```
cluster {
  name = "cluster-eu-west"
  links = [
    { enable = true
      upstream = "cluster-us-east"
      server = "emqx.eu-west.myinfra.net:11883"
      clientid = "clink-eu-west"
      topics = []
    }
  ]
}
```

As you can see, the configuration is almost identical to the previous one, with the exception that the `topics` field is empty. This means that `cluster-eu-west` is not now interested in _any_ messages from `cluster-us-east`. This makes the Cluster Link _asymmetrical_, and is useful when you want to have a one-way forwarding of messages between clusters.

If you were to repeat the message publishing and subscribing steps outlined above, you would notice that the message published from `cluster-us-east` is not received by the subscriber on `cluster-eu-west`.
