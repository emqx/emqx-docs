# Configure Cluster Linking

This page provides guidelines for configuring the Cluster Linking feature through the configuration file and EMQX Dashboard.

## Configure Cluster Linking via Configuration File

You can set up one or more links between clusters by populating the `cluster.links` list in the EMQX configuration. Each link must have a unique remote cluster name and can be enabled or disabled individually.

Maintaining consistent cluster names across each link is important to ensure proper functionality. In the example below, the remote cluster name should be `emqx-eu-west` in its corresponding configuration file.

```bash
cluster {
  name = "emqx-us-east"
  links = [
    {
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

Ensure that the remote `emqx-eu-west` cluster has a similarly configured link to `emqx-us-east` in its configuration file for the link to function properly.

### Enable and Disable Links

A configured link is enabled by default. You can disable it by setting the `enable` parameter to `false`.

Disabling a link will prevent EMQX from communicating with the remote cluster. However, this action does not automatically stop the remote cluster from communicating with this cluster, which can result in warnings and raised alarms on the remote cluster's side. To avoid these issues, always ensure that the link is disabled on both sides.

### Configure Topics

The `topics` parameter is a list of MQTT topic filters that specify which topics the local cluster is interested in. The local cluster expects to receive messages published to these topics from the remote cluster. This list can be empty, meaning the local cluster will not receive any messages from the remote cluster if no topics are specified.

### Configure MQTT Connections

Cluster Linking uses standard MQTT as the underlying protocol, requiring you to specify the remote cluster's MQTT listener endpoint as `server`. 

Depending on the cluster size and configuration, multiple MQTT client connections may be established to the remote cluster, and each client must have a unique ClientID. You can control how these ClientIDs are allocated by setting the `clientid` parameter, which serves as a *ClientID prefix* for these connections. 

Other MQTT protocol aspects, such as authentication and authorization parameters (`username`, `password`), are also configurable. The remote cluster must be able to [authenticate](../access-control/authn/authn.md) these connections and [authorize](../access-control/authz/authz.md) them to publish messages to the specific MQTT topics designated for inter-cluster communication by the Cluster Linking setup. For example, with the configuration above, the remote cluster can have the following [ACL rule](../access-control/authz/file.md) to function correctly:

```erlang
%% Allow Cluster Linking MQTT clients to operate with "$LINK/#" topics
{allow, {clientid, {re, "^clink-us-east"}}, all, ["$LINK/#"]}.
...
```

This rule allows MQTT clients with ClientIDs that match the regex pattern `^clink-us-east` to publish and subscribe to any topic that starts with `$LINK/`. The `$LINK/` is the control topic prefix used for Cluster Linking-related messages. This ensures that the subscribing entity receives all relevant messages under the `$LINK/` namespace, which are necessary for maintaining and managing the cluster link.

Cluster Linking supports TLS connections. If you plan to have clusters communicate over the public internet, or any other untrusted network in general, TLS is a must. EMQX also supports mutual TLS authentication, ensuring that communication is secure, confidential, and trusted.

## Configure Cluster Linking via Dashboard

<!-- to do -->
