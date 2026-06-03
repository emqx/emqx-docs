# Secure Cluster Linking

Cluster Linking uses standard MQTT under the hood: each cluster connects to its peer as one or more MQTT clients, carrying forwarded user messages and control-plane traffic (route synchronization and response channels). Because these connections cross the network boundary between clusters, the listener that accepts them requires the same hardening as any other public-facing MQTT listener.

The configuration below is recommended for every production deployment. Each cluster must apply it to the listener that accepts incoming link connections from its peers. The cluster being connected *to* is the one that enforces these checks.

## Plan ClientIDs and Usernames

Every Cluster Linking MQTT connection uses a ClientID derived from the `clientid` prefix configured on the link. EMQX appends suffixes such as `:msg:<node>` to form the final ClientID. The prefix must meet the following requirements:

- It is unique to the source cluster (for example, `clink-A-` on a cluster whose `cluster.name` is `A`).
- It ends with a separator character such as `-` so an anchored regex like `^clink-A-` matches only that cluster's connections without accidentally matching a hypothetical `clink-AB-...` peer.
- It does not overlap with any prefix used by application clients.

If you use password authentication, allocate a dedicated username for each peer cluster (for example, `clink-user:A`) and never reuse it for regular MQTT clients.

These ClientIDs and usernames are the identifiers your authentication and authorization rules match against, so decide on them before configuring either layer.

## Enable Authentication

Enable authentication on the listener that accepts incoming Cluster Linking connections. Without it, any party that can reach the listener can impersonate a peer cluster, inject traffic into the `$LINK/` control namespace, and disrupt or eavesdrop on inter-cluster communication.

Two mechanisms are commonly used for Cluster Linking:

- **TLS mutual authentication (mTLS):** the peer cluster presents a client certificate issued by a CA you control; the listener verifies it with `verify = verify_peer` and `fail_if_no_peer_cert = true`. This is the strongest option because it pins the peer's identity at the transport layer and doubles as the authentication check, removing the need for a separate password authenticator. See [X.509 Certificate Authentication](../access-control/authn/x509.md).
- **Username and password:** configure `username` and `password` on the link, and set up a matching authenticator on the peer's listener. Store credentials securely and rotate them on a regular schedule.

You can also combine the two: mTLS at the transport layer with password authentication layered on top.

For any link traversing untrusted networks (public internet, cross-cloud peering, partner networks), TLS is mandatory. mTLS additionally pins the peer cluster's identity at the transport layer, complementing the credential checks above.

For the full list of supported mechanisms, see the [Authentication](../access-control/authn/authn.md) overview. TLS configuration for the link connection itself is covered in [Configure MQTT Connections](./configuration.md#configure-mqtt-connections).

## Enable Authorization

Once authenticated, Cluster Linking clients must be restricted to the `$LINK/` namespace, and no other client should be permitted to use it. Without this boundary, an authenticated but unrelated client could inject forged route updates or forwarded messages into the link.

A peer cluster communicates with the local broker over the following control topics. `<Cluster>` is the peer's own `cluster.name` (the value of `cluster.name` configured on the side that initiated the link); it appears verbatim in the topic and is not a wildcard or runtime substitution. `<Actor>` is an internal sub-identifier assigned per replication actor; match it with `+` in your rules.

| Operation | Topic | Purpose |
| --- | --- | --- |
| Publish | `$LINK/cluster/msg/<Cluster>` | Forwarded user messages |
| Publish | `$LINK/cluster/route/<Cluster>` | Route (subscription) synchronization |
| Subscribe | `$LINK/cluster/resp/<Cluster>/<Actor>` | Responses from the local broker |

### ACL Configuration

The examples below use the [ACL file](../access-control/authz/file.md) source. The same rules apply equally with any other authorizer.

Granting publish and subscribe access on the catch-all `$LINK/#` is the recommended starting point. The wildcard covers all current and future control topics, so you do not need to update rules when upgrading EMQX.

Assume this broker accepts links from two peer clusters whose `cluster.name` values are `A` and `C`, and the link configuration on each peer sets `clientid` to `clink-A-` and `clink-C-` respectively. The rules below allow each peer to use the `$LINK/` namespace, deny anyone else from touching it, and finish with a default-deny so unrelated clients cannot publish or subscribe anywhere unless an earlier `allow` rule matches:

```erlang
%% Allow each peer cluster to use the $LINK control namespace.
{allow, {clientid, {re, "^clink-A-"}}, all, ["$LINK/#"]}.
{allow, {clientid, {re, "^clink-C-"}}, all, ["$LINK/#"]}.

%% Deny any other client from touching the $LINK namespace.
{deny, all, all, ["$LINK/#"]}.

%% ... your application's allow rules go here ...

%% Catch-all: deny everything not matched by an earlier allow.
{deny, all}.
```

Pair the catch-all `{deny, all}` with the deny-by-default authorizer setting so non-matching authorization checks fail closed:

```hocon
authorization {
  no_match = deny
}
```

If you prefer an enumerated allow list over the wildcard (more restrictive but more fragile: new control topics introduced in future EMQX versions would have to be added by hand), the equivalent rules for the same two peers `A` and `C` look like this:

```erlang
{allow, {clientid, {re, "^clink-A-"}}, publish,   ["$LINK/cluster/msg/A", "$LINK/cluster/route/A"]}.
{allow, {clientid, {re, "^clink-A-"}}, subscribe, ["$LINK/cluster/resp/A/+"]}.
{allow, {clientid, {re, "^clink-C-"}}, publish,   ["$LINK/cluster/msg/C", "$LINK/cluster/route/C"]}.
{allow, {clientid, {re, "^clink-C-"}}, subscribe, ["$LINK/cluster/resp/C/+"]}.
{deny, all}.
```

Notice how each `<Cluster>` in the topic table is replaced with the peer's actual `cluster.name` (`A` and `C` here), and each ClientID regex is the prefix you configured in the peer's `clientid` field. The two values are independent and you must keep them in sync yourself when naming a new peer.

For the available authorization sources and configuration options, see the [Authorization](../access-control/authz/authz.md) overview.

## See Also

- [Authentication](../access-control/authn/authn.md)
- [Authorization](../access-control/authz/authz.md)
- [Use ACL File](../access-control/authz/file.md)
- [Security Checklist](../access-control/security-checklist.md)
