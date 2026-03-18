# Dashboard Users and Role Management

EMQX Dashboard supports multi-user access with role-based access control (RBAC). Each Dashboard account is assigned a role that determines what it can view and modify. This page describes how to manage Dashboard users and roles via the Dashboard and API.

## Roles

EMQX Dashboard has two built-in roles:

| Role | Description |
|------|-------------|
| `administrator` | Full access to all Dashboard features and REST APIs. Can manage users, modules, rules, clients, and all cluster configurations. |
| `viewer` | Read-only access. Can view monitoring data, client lists, subscriptions, and statistics, but cannot modify any configuration. |

:::tip
The default `admin` account has the `administrator` role and cannot be deleted. Make sure to change its password before deploying in production.
:::

## Call the Dashboard API

Dashboard user credentials (username and password) can be used directly to call the Dashboard API (port 18083), for scenarios that require programmatic Dashboard operations, such as automated user management or API Key management.

:::tip
For programmatic access to client management, rule engine, plugins, and other integration endpoints, use the **Management API (port 8081)** with API Keys. See [API Key](../advanced/api-key.md).
:::

Two authentication methods are supported:

### Basic Auth

Pass the username and password in the `Authorization` header, encoded as Base64:

```
Authorization: Basic base64(username:password)
```

Example:

```bash
curl -u admin:public "http://127.0.0.1:18083/api/v4/users/"
```

### Bearer Token

Bearer Token is only available to users with MFA enabled. After completing the MFA login flow, you receive a session token. Pass it in the `Authorization` header for subsequent API calls:

```
Authorization: Bearer <token>
```

Example:

```bash
curl -H "Authorization: Bearer <token>" \
  "http://127.0.0.1:18083/api/v4/users/"
```

::: warning Note
Bearer tokens are invalidated when you log out or when the user account is deleted. Store tokens securely and avoid embedding them in client-side code.
:::

The API examples in the sections below use Basic Auth (`-u admin:public`). You can substitute a Bearer Token as needed.

## User Management

This section covers creating, viewing, updating, and deleting Dashboard users, and changing passwords. All operations require the `administrator` role and can be performed via the Dashboard or REST API.

### Create a User

**Via Dashboard:**

1. Click **Users** from the left navigation menu.

2. Click **Create**.

3. Fill in the username, password, role, and optional description.

4. Click **Confirm**.

   ![create_user](./assets/create_user.png)

**Via API:**

```bash
curl -i -X POST "http://127.0.0.1:18083/api/v4/users/" \
  -u admin:public \
  -H "Content-Type: application/json" \
  -d '{"username":"newuser","password":"Password123!","role":"viewer"}'
```

**Request parameters:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `username` | String | Yes | Username. Alphanumeric characters, underscores, and hyphens are allowed. |
| `password` | String | Yes | Password. Must be 8-64 characters and contain at least 2 of the following: letters, numbers, special characters. ASCII only. |
| `role` | String | No | `administrator` or `viewer`. Defaults to `viewer`. |
| `tags` | String | No | Optional description or label for the user. |
| `enable_mfa` | Boolean | No | When set to `true`, the user will be prompted to set up MFA on their first login. See [Dashboard Multi-Factor Authentication](./dashboard-mfa.md). |

**Response:**

```json
{
  "code": 0
}
```

### List Users

**Via Dashboard:**

Click **Users** from the left navigation menu to view all Dashboard users and their roles.

**Via API:**

```bash
curl -u admin:public "http://127.0.0.1:18083/api/v4/users/"
```

**Example response:**

```json
{
  "code": 0,
  "data": [
    {
      "username": "admin",
      "tags": "administrator",
      "role": "administrator",
      "mfa_enabled": false,
      "mfa_setup_required": false
    }
  ]
}
```

**Response fields:**

| Field | Type | Description |
|-------|------|-------------|
| `username` | String | Username |
| `tags` | String | User description |
| `role` | String | Assigned role: `administrator` or `viewer` |
| `mfa_enabled` | Boolean | Whether MFA is currently active for this user |
| `mfa_setup_required` | Boolean | Whether the user must complete MFA setup on next login |

### Update User

Use this endpoint to update a user's role or description. You cannot update the username or password through this endpoint.

**Via Dashboard:**

1. Click **Users** from the left navigation menu.
2. Click the **Edit** button for the target user.
3. Update the role or description and click **Confirm**.

**Via API:**

```bash
curl -i -X PUT "http://127.0.0.1:18083/api/v4/users/newuser" \
  -u admin:public \
  -H "Content-Type: application/json" \
  -d '{"role":"administrator"}'
```

**Request parameters:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `role` | String | No | New role: `administrator` or `viewer` |
| `tags` | String | No | Updated description |

**Response:**

```json
{
  "code": 0
}
```

### Delete User

**Via Dashboard:**

1. Click **Users** from the left navigation menu.
2. Click the **Delete** button for the target user.
3. Click **Confirm** in the confirmation dialog.

**Via API:**

```bash
curl -i -X DELETE "http://127.0.0.1:18083/api/v4/users/newuser" \
  -u admin:public
```

:::danger
The built-in `admin` user cannot be deleted. Attempting to delete it will return an error.
:::

:::warning
Deleting a user immediately removes their MFA configuration and invalidates all their active tokens. Any active sessions for that user will be terminated.
:::

### Change Password

Users can change their own passwords. Administrators can change the password for any user.

**Via Dashboard:**

1. Click **Users** from the left navigation menu.
2. Click the **Edit** button for the target user.
3. Enter the new password and click **Confirm**.

**Via API:**

```bash
curl -i -X PUT "http://127.0.0.1:18083/api/v4/change_pwd/newuser" \
  -u admin:public \
  -H "Content-Type: application/json" \
  -d '{"old_pwd":"OldPassword1!","new_pwd":"NewPassword2@"}'
```

**Request parameters:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `old_pwd` | String | Yes | Current password |
| `new_pwd` | String | Yes | New password |

**Password requirements:**
- 8 to 64 characters
- Must contain at least 2 different character types from: letters, numbers, special characters
- ASCII characters only

## SSO Users

When a user logs in through SAML Single Sign-On (SSO) for the first time, EMQX automatically creates a Dashboard account for that user with the `viewer` role.

- SSO users are assigned a random internal password and cannot log in with a username and password directly.
- Administrators can change an SSO user's role using the update user API or through the Dashboard Users page.
- See the [SAML 2.0 Single Sign-On](../modules/saml_sso.md) documentation for configuration details.

:::tip
To grant an SSO user administrator access, update their role via `PUT /api/v4/users/:username` with `"role": "administrator"`.
:::

## API Reference

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/v4/auth` | POST | Login |
| `/api/v4/auth` | DELETE | Logout |
| `/api/v4/users/` | GET | List all users |
| `/api/v4/users/` | POST | Create a user |
| `/api/v4/users/:username` | PUT | Update a user |
| `/api/v4/users/:username` | DELETE | Delete a user |
| `/api/v4/change_pwd/:username` | PUT | Change a user's password |

For MFA-related endpoints, see [Dashboard MFA — API Reference](./dashboard-mfa.md#api-reference).
