---
description: "Choose where EMQX checks MQTT client passwords and review the prerequisites for configuring a password authentication backend."
---

# Choose a Password Authentication Backend

Use password authentication when your MQTT clients send a username or client ID and a password in their CONNECT packet. Choose where EMQX should check those credentials, then follow the corresponding setup guide.

Password authentication establishes identity; configure [authorization](../authorization/index.md) separately to control publishing and subscribing.

## Choose Where Credentials Live

| Situation | Start Here | What EMQX Checks |
| --- | --- | --- |
| You want EMQX to store and manage MQTT users. | [Built-in database](built-in-database.md) | The supplied password against a credential managed through the EMQX Dashboard or the REST API. |
| Your application already stores credentials in MySQL. | [MySQL](../../guides/access-control/authn/mysql.md) | A password hash selected by your SQL query. |
| Your application already stores credentials in PostgreSQL. | [PostgreSQL](../../guides/access-control/authn/postgresql.md) | A password hash selected by your SQL query. |
| Your identity records are MongoDB documents. | [MongoDB](../../guides/access-control/authn/mongodb.md) | A password hash selected by your document filter, with configurable field names. |
| Your credentials are Redis hashes. | [Redis](../../guides/access-control/authn/redis.md) | Fields returned by an `HGET` or `HMGET` command. |
| Your users are in an LDAP directory. | [LDAP](../../guides/access-control/authn/ldap.md) | An LDAP bind using the client password, or a supported password hash retrieved from the directory. |
| Your application owns the authentication decision. | [HTTP service](../../guides/access-control/authn/http.md) | The decision returned by your service under the EMQX HTTP response contract. |

For an external database, the configured database username is EMQX's service account. The MQTT username identifies the device or application being authenticated. They are different credentials.

## Before Enabling a Backend

1. Choose the identity field. A username identifies the authentication account; a client ID identifies the client and its MQTT session. Choose the field that matches your identity model. The built-in database lets you choose either. External backends use your query, filter, or request template.
2. Match the existing credential format. For database backends that compare passwords locally, match the configured password algorithm and salt settings to the stored credential format. EMQX does not create or update external user records. See [Password Hashing](../../guides/access-control/authn/authn.md#password-hashing) and [Authentication Placeholders](../../guides/access-control/authn/authn.md#authentication-placeholders).
3. Protect both network paths: MQTT client → EMQX and EMQX → external service. Password authentication does not itself encrypt either path.
4. Decide the chain order, preconditions, and backend-failure behavior. An unknown user, a rejected password, and an unavailable backend are distinct outcomes. Review [authentication evaluation](../../guides/access-control/authn/authn.md#authentication-chain) before combining backends. Backend-failure handling also depends on the [security profile](../../guides/access-control/security-profile.md#authentication) and `authentication_settings.ignore_backend_failures`.
5. Keep ordinary clients non-superusers. A successful result with `is_superuser = true` bypasses topic authorization; it does not create a Dashboard administrator.

Use an isolated test listener or instance for the first setup. Verify a valid user, a wrong password, an unknown user, and an unavailable backend before moving existing clients to the new chain. Test authorization separately with one permitted and one forbidden topic.

## Other Authentication Methods

For JWTs, X.509 certificates, or MQTT 5 enhanced authentication, see [Authenticate MQTT Clients](index.md). You do not need to move credentials into a password database to use those methods.
