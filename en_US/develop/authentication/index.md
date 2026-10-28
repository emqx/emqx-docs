---
description: "Choose an MQTT client authentication method in EMQX and understand authentication chain behavior and configuration prerequisites."
---

# Authenticate MQTT Clients

Choose how EMQX verifies a client's identity before it opens an MQTT session. Then configure [authorization](../authorization/index.md) to control which topics that client can publish to or subscribe to.

For a first local setup, follow [Authenticate with a Username and Password](../../get-started/password-authentication.md). For an existing identity service, choose its method below.

## Before You Configure Authentication

- Identify the EMQX version and active [security profile](../../guides/access-control/security-profile.md). An empty or fully disabled authentication chain accepts connections under `legacy` and rejects them under `hardened`. A listener with `enable_authn = false` explicitly bypasses MQTT authentication.
- Use TLS when credentials cross a network. Password authentication does not encrypt the connection.
- Use a disposable broker for the examples. Changing a shared authentication chain affects new connections across its scope.
- Create ordinary test users with `is_superuser = false`. A superuser bypasses topic authorization; it is not a Dashboard administrator.

## Choose an Authentication Method

Choose a guide below. TLS methods are listed alongside MQTT methods for discovery, but run at a different layer.

| Identity Source or Requirement | Guide | Where the Check Runs |
| --- | --- | --- |
| A username or client ID and a password | [Password-based authentication](password-backends.md) | MQTT authentication chain |
| Signed tokens issued by your identity service | [JWT](../../guides/access-control/authn/jwt.md) | MQTT authentication chain |
| MQTT 5 challenge/response using SCRAM | [SCRAM with built-in database](../../guides/access-control/authn/scram.md) or [SCRAM with HTTP](../../guides/access-control/authn/scram_restapi.md) | MQTT enhanced authentication |
| Kerberos tickets and compatible MQTT 5 clients | [Kerberos](../../guides/access-control/authn/kerberos.md) | MQTT enhanced authentication |
| Rules over connection metadata | [Client info](../../guides/access-control/authn/cinfo.md) | MQTT authentication chain |
| Client certificates issued by a trusted CA | [X.509 client certificates](../../guides/access-control/authn/x509.md) | TLS handshake, before MQTT authentication |
| Clients that support TLS pre-shared keys | [TLS-PSK](../tls/psk.md) | TLS handshake, before MQTT authentication |

## Combine Methods Deliberately

An authentication chain tries authenticators in order. A successful authenticator ends the chain; an explicit rejection also ends it. Combining two methods does not require the client to pass both.

Use [chain evaluation](../../guides/access-control/authn/authn.md#authentication-chain) to decide which clients reach each method, then [manage the chain](../../guides/access-control/authn/authn.md#configure-authentication). When JWT and password-based clients share the MQTT `password` field, set the JWT authenticator's precondition to `is_jwt(password)` so that ordinary passwords skip it and reach a later password authenticator.

Backend failures are different from “user not found” or an explicit `ignore`. Their treatment depends on the security profile and `authentication_settings.ignore_backend_failures`.

## Verify Acceptance and Rejection

For each method, test a valid identity, an incorrect credential, and an unknown identity. When methods are chained, also test a missing credential, a skipped precondition, and an unavailable backend. Confirm that a failed check cannot unexpectedly reach a permissive later method.

An accepted MQTT connection proves authentication only. Follow [the authorization guide](../authorization/index.md) to prove that an ordinary client can deliver an allowed message and cannot publish or subscribe outside its permissions.

## Operate Authentication

- [Create, reorder, and inspect authenticators](../../guides/access-control/authn/authn.md#configure-authentication).
- [Manage built-in users through the API](../../guides/access-control/authn/user_management.md).
- [Control external authentication caching](../../guides/access-control/authn/authn.md#external-resource-cache).
- [Return a trusted replacement Client ID](../../guides/access-control/authn/authn.md#override-client-ids-from-authentication-results).
- [Return permissions with HTTP authentication or JWT](../../guides/access-control/authn/acl.md).
