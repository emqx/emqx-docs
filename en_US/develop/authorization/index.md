---
description: "Choose an MQTT authorization method in EMQX and verify allowed and denied operations with MQTTX CLI or MQTTX Desktop."
---

# Control MQTT Publish and Subscribe Permissions

Choose which topics a connected client can publish to or subscribe to. Authentication establishes the client's identity; authorization checks each requested operation against your policy.

## Choose an Authorization Method

Start with the built-in database when you want to manage a small policy in the EMQX Dashboard. Choose another method when permission data already lives in that system or requires application logic.

| Method | Use It When | Task |
| --- | --- | --- |
| Built-in database | You want to manage per-user, per-client, and shared rules in EMQX. | [Create rules in EMQX](built-in-database.md) |
| ACL file | You maintain a compact, ordered policy as a file. | [Use an ACL file](../../guides/access-control/authz/file.md) |
| MySQL | Permission records are in MySQL. | [Query MySQL rules](../../guides/access-control/authz/mysql.md) |
| PostgreSQL | Permission records are in PostgreSQL. | [Query PostgreSQL rules](../../guides/access-control/authz/postgresql.md) |
| MongoDB | Permission records are MongoDB documents. | [Query MongoDB rules](../../guides/access-control/authz/mongodb.md) |
| Redis | You maintain an allow list in Redis hashes. | [Read Redis permissions](../../guides/access-control/authz/redis.md) |
| LDAP | Topic permissions are attributes in an LDAP directory. | [Read LDAP permissions](../../guides/access-control/authz/ldap.md) |
| HTTP service | An application makes the permission decision at request time. | [Call an HTTP authorization service](../../guides/access-control/authz/http.md) |

You can combine methods in an ordered chain. A matching allow or deny stops the chain; no match can continue to the next authorizer. See [evaluation and failure behavior](../../guides/access-control/authz/authz.md).

If your HTTP authenticator or JWT already supplies client ACL rules, see [ACLs returned during authentication](../../guides/access-control/authn/acl.md). These rules have higher priority than the configured authorizers. A superuser bypasses authorization.

## Prepare a Controlled Test

Use a development broker or a dedicated test deployment. Changing the authorization chain or its fallback policy affects connected clients as well as new connections.

1. Create a normal authenticated user named `mqtt-user`, or reuse the account from [password authentication](../../get-started/password-authentication.md). Keep **Is superuser** unchecked. Do not attach an authentication-time ACL to this user for the following test.
2. Inspect **Access Control** -> **Authorization**. Before making changes, record the authorizers, their order and enabled states, **No Match Action**, **Deny Action**, and cache settings so that you can restore them if needed.
3. Open **Settings** and explicitly set **No Match Action** to `deny`. Use **Deny Action** `ignore` for the test so that a denied operation does not close the connection.
4. In the same **Settings** panel, turn off **Enable Client Cache** and save. Use **Clear Client Cache** to clear existing cached decisions. If you use an external backend, also disable its external resource cache, or clear both caches before each policy change. See [cache controls](../../guides/access-control/authz/authz.md#client-authorization-cache).
5. Disable unrelated authorizers in this test deployment. In particular, an enabled file authorizer with a final allow rule can permit a request that your new authorizer does not match.

`no_match = deny` rejects an operation only when no authorizer returns a matching rule. The shipped file policy permits otherwise unmatched operations under the `legacy` security profile. Existing files may also end in `{allow, all}.` Inspect the effective policy; changing the fallback alone does not override an allow rule.

## Verify Authorization Rules

Before running these checks, [create or configure the built-in database authorizer](built-in-database.md#create-the-authorizer-in-the-dashboard) and [add the example username rule](built-in-database.md#add-a-username-rule). Then return here to verify the policy.

The test policy grants `mqtt-user` publish and subscribe access to `docs/authz/allowed/#`. All other topics remain denied in the isolated test. Use an MQTT 5 client so that you can inspect authorization reason codes.

Choose one of the following methods. Use MQTTX CLI for the local learning path, or MQTTX Desktop if you prefer a graphical client. Both methods check allowed message delivery and rejected subscriptions and publications. The CLI uses fresh connections for denied operations; the Desktop procedure also verifies that the same connections remain usable after a denial when **Deny Action** is `ignore`.

A successful connection proves authentication, not topic access. Confirm the exact allowed payload and an explicit error for each denied operation. A missing received message alone does not prove rejection.

### Verify with MQTTX CLI

These commands use MQTTX CLI from Quick Start, MQTT 5, and the disposable Quick Start endpoint and account. Replace `1883` in every command if you changed the MQTT host port. For another broker, use its endpoint, credentials, and TLS settings.

1. In Terminal 1, subscribe and wait for `Subscribed`:

   ```bash
   mqttx sub -h 127.0.0.1 -p 1883 -V 5.0 -i docs-authz-sub \
     -u mqtt-user -P 'Example-only-password!' --reconnect-period 0 \
     -t 'docs/authz/allowed/#' -q 1
   ```

2. In Terminal 2, publish and confirm the exact payload in Terminal 1:

   ```bash
   mqttx pub -h 127.0.0.1 -p 1883 -V 5.0 -i docs-authz-pub \
     -u mqtt-user -P 'Example-only-password!' --reconnect-period 0 \
     -t docs/authz/allowed/check -q 1 -m '{"test_id":"authz-allowed-001"}'
   ```

3. Keep Terminal 1 running. In Terminal 2, try each denied subscription:

   ```bash
   mqttx sub -h 127.0.0.1 -p 1883 -V 5.0 -i docs-authz-denied-sub \
     -u mqtt-user -P 'Example-only-password!' --reconnect-period 0 \
     -t 'docs/authz/denied/#' -q 1
   mqttx sub -h 127.0.0.1 -p 1883 -V 5.0 -i docs-authz-broad-sub \
     -u mqtt-user -P 'Example-only-password!' --reconnect-period 0 \
     -t '#' -q 1
   ```

   MQTTX CLI 1.13.0 should report `Subscription negated` with code `135`, the decimal form of `0x87`.

4. Test a denied publish:

   ```bash
   mqttx pub -h 127.0.0.1 -p 1883 -V 5.0 -i docs-authz-denied-pub \
     -u mqtt-user -P 'Example-only-password!' --reconnect-period 0 \
     -t docs/authz/denied/check -q 1 -m '{"test_id":"authz-denied-001"}'
   ```

   Expect `Publish error: Not authorized`. A missing received message alone does not prove that the broker rejected the publication.

5. Repeat the allowed publish with `authz-allowed-002` and confirm that new payload arrives in Terminal 1. Then stop the subscriber with Ctrl+C.

These CLI denial checks use fresh connections; to observe the same connection surviving a denial, use [MQTTX Desktop](#verify-with-mqttx-desktop).

### Verify with MQTTX Desktop

In MQTTX, create two connections to your broker with username `mqtt-user`, its existing password, and distinct client IDs: `docs-authz-sub` and `docs-authz-pub`. Use the TLS settings required by your listener. In MQTTX Desktop 1.13.0, turn **Auto Reconnect** off so that you can distinguish a surviving connection from a reconnect.

1. Connect both clients.
2. On `docs-authz-sub`, subscribe to `docs/authz/allowed/#` at QoS 1. Confirm that the subscription succeeds.
3. On `docs-authz-pub`, replace the compose area's topic and payload with `docs/authz/allowed/check` and `{"test_id":"authz-allowed-001"}`. Publish at QoS 1, with Retain off. Confirm the exact payload in the subscriber's **Received** tab.
4. Try to subscribe to `docs/authz/denied/#` and then `#`. Each must fail with MQTT 5 reason code `0x87` (Not authorized). Desktop reports **Failed to Subscribe** with **Not authorized (Code: 135)**; `135` is decimal `0x87`. The subscription dialog stays open on failure: select **Cancel** before starting the next action.
5. Publish `{"test_id":"authz-denied-001"}` to `docs/authz/denied/check` at QoS 1. Desktop reports **Error: Publish error: Not authorized**, corresponding to PUBACK `0x87`. It can still add a message bubble under **Published**; that record does not prove the broker accepted the publication.
6. Without disconnecting either client, publish `{"test_id":"authz-allowed-002"}` to `docs/authz/allowed/check`. Confirm that this new payload arrives in **Received**. With **Deny Action** `ignore`, both connections should survive the denied operations.

### Check Broker-Side Results

For a broker-side check, inspect the authorizer's **Overview** or a client trace. This policy has an allow rule and relies on **No Match Action** `deny` for other topics: rejected requests increase the authorizer's **No match** count, while its **Deny** count can remain zero. Authorizer metrics describe that authorizer's result, not the final chain decision; use the client error or trace to confirm the final rejection.

## Optional: Verify Permissions for Another User

Verify that the permissions granted to `mqtt-user` do not also allow another authenticated user without matching authorization rules to access the same topics.

Create a normal MQTT user named `docs-other` with a different disposable password and **Is superuser** unchecked. If that name already exists, choose a new test username instead of changing the existing account. Do not give this user topic rules or an authentication-time ACL. Connect with its credentials and client ID `docs-authz-other`; the connection should succeed, but subscribing to `docs/authz/allowed/#` and publishing to `docs/authz/allowed/check` at QoS 1 should be rejected. A CONNECT failure is an authentication result and does not prove that authorization works. After this check, disconnect the test client and delete only the authentication account you created for it. Keep `mqtt-user` and its password for the next exercise.

For MQTT 3.1.1, QoS 0, or **Deny Action** `disconnect`, observable behavior differs. See [denied operation behavior](../../guides/access-control/authz/authz.md).

## Next Steps

Disconnect the test clients when finished. If you changed the configuration only for a temporary test, restore the recorded authorizers, their order and enabled states, **No Match Action**, **Deny Action**, and cache settings. Clear affected cached decisions before reconnecting, then verify the intended policy. Keep rules and accounts that you still need; remove only test data you created and no longer need.

- [Order authorizers and inspect metrics](../../guides/access-control/authz/authz.md#manage-authorizers).
- [Handle policy changes and cached results](../../guides/access-control/authz/authz.md#client-authorization-cache).
- [Review topic filters, placeholders, and failure behavior](../../guides/access-control/authz/authz.md).
- [Select a security profile](../../guides/access-control/security-profile.md).
