---
description: "Configure built-in database authentication in EMQX, manage MQTT user credentials, and verify accepted and rejected connections."
---

# Authenticate Clients with the Built-in Database

Create an MQTT user in EMQX, then confirm that its password permits a connection and an incorrect password is rejected. The built-in database stores client credentials and supports EMQX Dashboard and REST API user management.

These are MQTT client accounts, separate from Dashboard administrators and API keys.

## Prerequisites

- Use a test instance or listener and retain administrative access to Dashboard.
- Have an MQTT client such as MQTTX available. Use TLS for remote connections carrying credentials.
- Review the [authentication chain](../../guides/access-control/authn/authn.md#authentication-chain). For this test, ensure another authenticator cannot accept the same test user before this one runs.
- Decide whether users are identified by `username` or `clientid`. This walkthrough uses `username`.

An enabled authenticator with no users cannot authenticate your test client. Provision and verify the user before switching existing clients to this authentication chain.

## Create the Authenticator

Use an isolated test deployment. Enabling an authenticator with no users can reject new connections; add and verify the test user before switching other clients to this chain.

1. Open **Access Control** -> **Authentication** -> **Create** in Dashboard.
2. Select **Password-Based** -> **Next** -> **Built-in Database** -> **Next**.
3. Set **UserID Type** to `username`.
4. Set **Password Hash** to `bcrypt` and **Salt Rounds** to `10` for this example.
5. Leave **Precondition** empty for the isolated test, then click **Create**.

If this authenticator already exists, open its **Settings** instead of creating it again. For this exercise, confirm the settings above before using its **Users** view.

The authenticator hashes passwords supplied through user management before storing them. Hash creation and verification settings must remain consistent with existing users; changing the algorithm is not a password migration. See [password formats](../../guides/access-control/authn/authn.md#password-hashing) for supported algorithms and migration constraints.

A precondition only selects whether an authenticator runs. If you later add one, an expression that does not return the string `"true"` skips this authenticator; it does not reject the client by itself.

## Add an MQTT User

Open **Users** in the authenticator's **Actions** column, then select **Add**. Leave **Namespace** off for this global test account and add:

| Field | Test Value |
| --- | --- |
| Username | `mqtt-user` |
| Password | `Example-only-password!` |
| Is superuser | Unchecked / `false` |

Click **Save**. In **User Management**, confirm that `mqtt-user` appears with **Is superuser** set to **No**.

This password is a disposable example. Use a separate generated secret for each production identity. `is_superuser` grants an authorization bypass, so leave it disabled for ordinary users.

For repeatable provisioning, use [Manage built-in users through the REST API](../../guides/access-control/authn/user_management.md).

## Verify Accepted and Rejected Connections

Use MQTTX CLI from Quick Start. Run the following command on your computer against the disposable broker. If you changed the MQTT host port, replace `1883`. For another deployment, use its endpoint, TLS settings, and the account you created.

```bash
mqttx conn -h 127.0.0.1 -p 1883 -V 5.0 -i docs-authn-good \
  -u mqtt-user -P 'Example-only-password!' --reconnect-period 0
```

1. Wait for `Connected`, then press Ctrl+C to disconnect.
2. Repeat the command for each row below, using a different Client ID each time. For the missing-password test, remove `-P` and its value. For the no-credentials test, remove both `-u` and `-P` with their values.

   | Credentials | Expected Result in This Isolated Chain |
   | --- | --- |
   | `mqtt-user` / `Example-only-password!` | Connection accepted. |
   | `mqtt-user` / `wrong-password` | Connection rejected. |
   | `mqtt-user` / no password | Connection rejected. |
   | `docs-missing` / any password | No matching user in this authenticator; another backend must not accept the client. |
   | No credentials | Connection rejected under the isolated listener and chain configuration. |

3. Check the client message for each rejected attempt: it must report `Connection refused`, without `Connected`. In MQTTX CLI 1.13.0, `conn` can exit with code `0` after a refusal; the exit code alone does not prove authentication succeeded.
4. Restore the correct credentials, reconnect, and confirm `Connected` again. Then press Ctrl+C.

A successful connection verifies authentication only; it does not verify permission to publish or subscribe.

## Optional: Verify with MQTTX Desktop

Create a connection to the test listener with MQTT 5, Client ID `docs-authn-good`, username `mqtt-user`, and the test password. Confirm that it connects and appears in the Dashboard client list. Turn **Auto Reconnect** off; disconnect before editing the credentials or Client ID with the pencil icon or disconnected connection form.

Repeat the credential cases in [Verify Accepted and Rejected Connections](#verify-accepted-and-rejected-connections), using a different Client ID each time. The following messages apply to MQTTX Desktop 1.13.0 with this isolated chain:

| Credentials | Desktop Result |
| --- | --- |
| Correct username and password | Connection accepted. |
| Wrong password | `Connection refused: Bad User Name or Password`. |
| Missing password | `Connection refused: Not authorized`. |
| Unknown username | `Connection refused: Not authorized`. |
| No credentials | `Connection refused: Not authorized`. |

Restore the correct credentials and verify that the account still connects. Disconnect when finished. For general Desktop connection steps, see [Test MQTT Connections and Messaging](../clients/test-clients.md#test-with-mqttx-desktop).

## Bootstrap Users During Deployment

Use a bootstrap file to seed users when the authenticator is created. Use the runtime import API for subsequent bulk changes.

The following HOCON object is one item in the `authentication` array. Merge it into your managed configuration; do not replace an existing chain unintentionally. See [configuration management](../../guides/configuration/configuration.md) for file precedence and applying changes.

```hocon
{
  mechanism = password_based
  backend = built_in_database
  user_id_type = username
  password_hash_algorithm {
    name = bcrypt
    salt_rounds = 10
  }
  bootstrap_file = "${EMQX_ETC_DIR}/auth-built-in-db-bootstrap.csv"
  bootstrap_type = plain
}
```

For `bootstrap_type = plain`, create a protected CSV file on the EMQX host:

```csv
user_id,password,is_superuser
mqtt-user,Example-only-password!,false
```

| Bootstrap Type | Required Fields | Optional Fields |
| --- | --- | --- |
| `plain` | `user_id`, `password` | `is_superuser`, default `false` |
| `hash` | `user_id`, `password_hash` | `salt`, default empty; `is_superuser`, default `false` |

A `.json` file contains an array of objects with the same fields. `plain` means EMQX hashes the supplied password; `hash` means the supplied hash must already match the configured algorithm and parameters. It does not mean the stored password should be plaintext.

Bootstrap skips existing user IDs. It is not a continuous synchronization mechanism. A missing or invalid file can leave the authenticator created without the intended users: inspect its user list and logs, then perform the positive and negative connection tests. Protect and remove disposable plaintext seed files according to your deployment process.

## Migrate Existing Credentials

Export user IDs, password hashes, salts, and superuser flags from the old system. Configure the matching hash algorithm in the target authenticator, then [import users](../../guides/access-control/authn/user_management.md#import-users). Do not import unsupported hash formats as if they were plaintext passwords. Runtime import can replace existing records; validate the export and target user IDs before importing.

For namespaced deployments, verify the intended namespace as well as the user ID. The global user-management example is not a tenant-isolation policy.

## Troubleshoot and Clean Up

| Symptom | Check |
| --- | --- |
| Correct password is rejected | User ID type, exact user ID, selected namespace, hash algorithm, and whether a preceding authenticator rejects the attempt. |
| Unknown user connects | Whether the listener bypasses authentication, the authenticator is disabled or skipped, or another authenticator accepts the client. |
| User connects but publish/subscribe fails | Authorization rules; keep the user non-superuser while testing them. |
| Bootstrap reports no users | File path inside the EMQX host/container, file permissions, format, and bootstrap warnings. Creating the authenticator alone does not prove import success. |

After the test, delete `mqtt-user` through user management and remove the disposable bootstrap entry. Reconnect to verify it can no longer authenticate through this backend. Credential deletion does not substitute for disconnecting an already connected client when immediate revocation is required.
