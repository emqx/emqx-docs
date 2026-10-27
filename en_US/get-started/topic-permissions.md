---
description: "Configure topic permissions on a local EMQX broker and use MQTTX CLI to verify allowed and denied publish and subscribe operations."
next: false
---

# Control Access to Topics

Configure topic permissions for MQTT user `mqtt-user`. Allow the user to publish and subscribe within `docs/authz/allowed/#`, then verify that operations outside this subtree are rejected.

First, complete [Authenticate with a Username and Password](password-authentication.md). Reuse the account and password you created.

## Before You Change the Policy

Use only the disposable, loopback-bound Quick Start broker. Keep the EMQX Dashboard open and use MQTTX CLI from the previous task. These authorization changes affect existing connections as well as new ones.

1. Reuse `mqtt-user` from the previous task. Keep **Is superuser** unchecked and do not attach an authentication-time ACL to this account.
2. Open **Access Control** -> **Authorization**. Record the authorizers, their order and enabled states, **No Match Action**, **Deny Action**, and client authorization cache settings. If the `mqtt-user` rule set already exists, record its contents too. You will restore these values after the test.
3. Open **Settings**, set **No Match Action** to `deny` and **Deny Action** to `ignore`, and save. A denied operation will not close the connection.
4. Turn off **Enable Client Cache** and save. Use **Clear Client Cache** to clear cached decisions. If you use an external backend, also disable its external resource cache, or clear both caches before each policy change. See [cache controls](../guides/access-control/authz/authz.md#client-authorization-cache).
5. Disable unrelated authorizers for this test. An enabled file authorizer with a final allow rule can permit a request that the built-in database authorizer does not match.

`no_match = deny` rejects an operation only when no authorizer returns a matching rule. The shipped file policy permits otherwise unmatched operations under the `legacy` security profile. Existing files may also end in `{allow, all}.` Changing the fallback alone does not override an allow rule.

<!-- include: ../develop/authorization/built-in-database.md#create-the-authorizer-in-the-dashboard -->

## Configure Authorization Rules

<!-- include: ../develop/authorization/built-in-database.md#add-a-username-rule -->

## Verify Authorization Rules

<!-- include: ../develop/authorization/index.md#verify-with-mqttx-cli -->

## Restore the Local Learning Policy

1. Disconnect the test clients.
2. Delete only the `mqtt-user` authorization rule set created for this task, or restore its previous contents if it already existed. Keep the authentication account and its password.
3. Restore the authorizers, their order and enabled states, **No Match Action**, **Deny Action**, and cache settings to the recorded values. Remove an authorizer only if you created it for this task. Clear any remaining affected cached decisions before reconnecting.
4. Reconnect with the valid `mqtt-user` credentials and verify the expected local policy. Before a later TLS test on this broker, confirm that publish and subscribe are permitted on `docs/tls`.

The original Quick Start policy is intentionally permissive for a disposable broker bound to your computer. Restoring it is not a production authorization configuration. Keep the ports bound to `127.0.0.1`.

## Continue Learning

For a TLS connection, follow [Enable SSL/TLS Connection](../guides/network/emqx-mqtt-tls.md) after preparing the listener, trusted certificates, and Docker port mappings. Changing the client port alone is not enough. For MQTT concepts and session behavior, see [MQTT Basics](../develop/mqtt/basics.md).

<!-- Migration follow-up: Replace the TLS and MQTT concept links with the approved introductory task routes after those tasks are migrated. -->

For other rule scopes, backend choices, and policy evaluation, see [MQTT Authorization](../develop/authorization/index.md). When you finish using the test broker, follow [Quick Start: Clean Up](quick-start.md#clean-up).
