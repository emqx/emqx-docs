# Secure Cluster Linking

Cluster Linking uses standard MQTT under the hood: each cluster connects to its peer as one or more MQTT clients, carrying not only forwarded user messages but also control-plane traffic (route synchronization and response channels). Because these connections cross the network boundary between clusters, they must be hardened with the same authentication and authorization rigor as any other public MQTT listener.

The hardening below is recommended for every production deployment. Each cluster must apply it to the listener that accepts incoming link connections from its peers — the cluster being connected *to* is the one that enforces these checks.

## 1. Plan ClientIDs and Usernames

Every Cluster Linking MQTT connection uses a ClientID derived from the `clientid` prefix configured on the link — EMQX appends suffixes such as `:msg:<node>` to produce the final ClientID. Choose a prefix that:

- is unique to the source cluster (for example, `clink-A-` on a cluster whose `cluster.name` is `A`),
- ends with a separator character such as `-` so the anchored regex `^clink-A-` matches every connection from cluster `A` without accidentally matching a hypothetical `clink-AB-...` peer,
- does not collide with any prefix used by ordinary application clients.

If you use username-based authentication, allocate a dedicated username for Cluster Linking (for example, `clink-user:A`) and never reuse it for regular MQTT clients. These ClientIDs and usernames are the identifiers your authentication and authorization layers will key off of.

## 2. Enable Authentication

Authentication must be enabled on the listener that accepts Cluster Linking connections. Without it, any party that can reach the listener can impersonate a peer cluster, publish into the `$LINK/` control namespace, and disrupt or eavesdrop on inter-cluster traffic.

For the full list of supported mechanisms and how to configure them, start from the [Authentication](../access-control/authn/authn.md) overview. Two options are commonly used for Cluster Linking:

- **TLS mutual authentication (mTLS)** — the strongest guarantee. The peer cluster presents a client certificate issued by a CA you control; the listener verifies it with `verify = verify_peer` and `fail_if_no_peer_cert = true`. See [X.509 Certificate Authentication](../access-control/authn/x509.md).
- **Username and password** — set `username` / `password` on the link and configure a matching authenticator on the peer listener. Store credentials securely and rotate them regularly.

You can also combine the two: mTLS at the transport layer plus password authentication on top.

## 3. Enable Authorization

Once authenticated, Cluster Linking clients must only be allowed to use the `$LINK/` namespace, and **only** Cluster Linking clients should be allowed to use it. Without these restrictions, an authenticated but unrelated client could inject forged route updates or forwarded messages into the link.

For the available authorization sources and how to enable them, start from the [Authorization](../access-control/authz/authz.md) overview. The examples below use the [ACL file](../access-control/authz/file.md) source — the same rules express equally well via any other authorizer.

A peer cluster uses these control topics. `<Cluster>` is the peer's own `cluster.name` (the value of `cluster.name` configured on the side that initiated the link), so it appears verbatim in the topic — it is **not** a wildcard or runtime substitution. `<Actor>` is an internal sub-identifier assigned per replication actor; treat it as opaque and match it with `+` in ACL rules.

| Operation | Topic | Purpose |
| --- | --- | --- |
| Publish | `$LINK/cluster/msg/<Cluster>` | Forwarded user messages |
| Publish | `$LINK/cluster/route/<Cluster>` | Route (subscription) synchronization |
| Subscribe | `$LINK/cluster/resp/<Cluster>/<Actor>` | Responses from the local broker |

Granting publish + subscribe on the catch-all `$LINK/#` is the recommended starting point — it covers all current and future control topics without you having to track schema changes between EMQX versions.

Assume this broker accepts links from two peer clusters whose `cluster.name` values are `A` and `C`, and the link configuration on each peer sets `clientid` to `clink-A-` and `clink-C-` respectively. The rules below allow each peer to use the `$LINK/` namespace, deny anyone else from touching it, and finish with a default-deny so unrelated clients cannot publish or subscribe anywhere unless an earlier `allow` rule matches:

```erlang
%% Allow each peer cluster to use the $LINK control namespace.
{allow, {clientid, {re, "^clink-A-"}}, all, ["$LINK/#"]}.
{allow, {clientid, {re, "^clink-C-"}}, all, ["$LINK/#"]}.

%% Disallow any other client from touching the $LINK namespace.
{deny, all, all, ["$LINK/#"]}.

%% ... your application's allow rules go here ...

%% Catch-all: deny everything that no earlier rule allowed.
{deny, all}.
```

Pair the catch-all `{deny, all}` with the deny-by-default authorizer setting so non-matching authorization checks fail closed:

```bash
authorization {
  no_match = deny
}
```

If you prefer an enumerated allow list over the wildcard (more restrictive but more fragile — new control topics introduced in future EMQX versions would have to be added by hand), the equivalent rules for the same two peers `A` and `C` look like:

```erlang
{allow, {clientid, {re, "^clink-A-"}}, publish,   ["$LINK/cluster/msg/A", "$LINK/cluster/route/A"]}.
{allow, {clientid, {re, "^clink-A-"}}, subscribe, ["$LINK/cluster/resp/A/+"]}.
{allow, {clientid, {re, "^clink-C-"}}, publish,   ["$LINK/cluster/msg/C", "$LINK/cluster/route/C"]}.
{allow, {clientid, {re, "^clink-C-"}}, subscribe, ["$LINK/cluster/resp/C/+"]}.
{deny, all}.
```

Notice how each `<Cluster>` in the topic table is replaced with the peer's actual `cluster.name` (`A` and `C` here), and each ClientID regex is the prefix you configured in the peer's `clientid` field — the two values are independent and you must keep them in sync yourself when naming a new peer.

## 4. Use TLS, Prefer mTLS

For any link traversing untrusted networks (public Internet, cross-cloud peering, partner networks), TLS is mandatory. mTLS additionally pins the peer cluster's identity at the transport layer, complementing the credential checks above. See [Configure MQTT Connections](./configuration.md#configure-mqtt-connections) for the link-side TLS settings.

## See Also

- [Authentication](../access-control/authn/authn.md)
- [Authorization](../access-control/authz/authz.md)
- [Use ACL File](../access-control/authz/file.md)
- [Security Checklist](../access-control/security-checklist.md)
