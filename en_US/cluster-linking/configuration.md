# Configuration

You can set up zero or more links between clusters by populating the `cluster.links` list in EMQX configuration. Each link must have unique remote cluster name, and can be enabled or disabled individually.

Cluster Linking employs regular MQTT as the underlying protocol, so you have to specify the remote cluster's MQTT listener endpoint as `server`. Several aspects of MQTT protocol related to authentication and authorization (`username`, `password`) are configurable as well. Depending on the cluster size and configuration, several MQTT client connections may be established, `clientid` is used as a _ClientID prefix_ for these connections.

In addition to that, it's important to have cluster names consistent across each link, otherwise the link may not work as expected. In the example below, the remote cluster name should be set to `emqx-eu-west` in its respective configuration file.

```
cluster {
  name = "emqx-us-east"
  links = [
    { enable = true
      name = "emqx-eu-west"
      server = "emqx.us-east.myinfra.net"
      username = "clink-user:us-east"
      password = "clink-password-no-one-knows"
      clientid = "clink-us-east"
      topics = ["global/#", "fwd/#", "cluster/+/status", ...]
      ssl {
        enable = true
        verify = verify_peer
        certfile = "etc/certs/client/emqx-us-east.pem"
        ...
      }
    }
    ...
  ]
}
```

Keep in mind that the remote `emqx-eu-west` cluster should have a similarly configured link to `emqx-us-east` in its configuration file for the link to work.

## Enabling and Disabling

A configured link is enabled by default. You can disable it by setting the `enable` parameter to `false`. Disabling a link will stop EMQX from communicating with the remote cluster. However, this will not automatically stop the remote cluster from communicating with this cluster, and will likely manifest as warnings and raised alarms on the remote cluster's side. Always make sure to disable the link on both sides to avoid this.

## Topics

The `topics` parameter is a list of MQTT topic filters that the local cluster is interested in. The cluster expects to receive messages published to these topics from the remote cluster. This list can be empty, in which case the local cluster will not receive any messages from the remote cluster.

## TLS

Cluster Linking supports TLS connections. If you plan to have clusters communicate over the public internet, or any other untrusted network in general, TLS is a must. EMQX also supports mutual TLS authentication, which is a nice mechanism to ensure that the communication is both secure, confidential, and trusted.
