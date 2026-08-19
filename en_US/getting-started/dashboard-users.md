# Dashboard Users and Role Management

EMQX Dashboard supports multi-user access with role-based access control (RBAC). Each Dashboard account is assigned a role that determines what it can view and modify. This page describes how to manage Dashboard users and roles via the Dashboard.

## Roles

EMQX Dashboard has two built-in roles:

| Role | Description |
|------|-------------|
| `administrator` | Full access to all Dashboard features and REST APIs. Can manage users, modules, rules, clients, and all cluster configurations. |
| `viewer` | Read-only access. Can view monitoring data, client lists, subscriptions, and statistics, but cannot modify any configuration. |

:::tip
The default `admin` account has the `administrator` role and cannot be deleted. Make sure to change its password before deploying in production.
:::

## User Management

This section covers creating, viewing, updating, and deleting Dashboard users, and changing passwords. All operations require the `administrator` role.

### Create a User

1. Click **General** -> **Users** from the left navigation menu.

2. Click **Create**.

3. Fill in the username, role, optional remark, optional scopes, and password.

   | Field | Description |
   |-------|-------------|
   | Username | Alphanumeric characters, underscores, and hyphens are allowed. |
   | Role | `administrator` or `viewer`. Defaults to `viewer`. |
   | Remark | Optional note or label for the user. |
   | Scopes | Optional permission categories used to restrict the user's access. When empty, the user keeps the role default. See [Category-Based Permission Control](#category-based-permission-control). |
   | Password | Must be 8-64 characters and contain at least 2 of the following: letters, numbers, special characters. ASCII only. |

4. Click **Confirm**.

   ![create_user](./assets/create_user.png)

### List Users

Click **General** -> **Users** from the left navigation menu to view all Dashboard users and their roles.

### Update User

1. Click **General** -> **Users** from the left navigation menu.
2. Click the **Edit** button for the target user.
3. Update the role, remark, or scopes, and click **Confirm**.

You cannot update the username or password through this operation. To change a password, see [Change Password](#change-password).

### Delete User

1. Click **General** -> **Users** from the left navigation menu.
2. Click the **Delete** button for the target user.
3. Click **Confirm** in the confirmation dialog.

::: warning Important Notice
The built-in `admin` user cannot be deleted. Attempting to delete it will return an error.
:::

::: warning Important Notice
Deleting a user immediately removes their MFA configuration and invalidates all their active tokens. Any active sessions for that user will be terminated.
:::

### Change Password

Users can change their own passwords. Administrators can change the password for any user.

1. Click **General** -> **Users** from the left navigation menu.
2. Click the **Edit** button for the target user.
3. Enter the new password and click **Confirm**.

**Password requirements:**
- 8 to 64 characters
- Must contain at least 2 different character types from: letters, numbers, special characters
- ASCII characters only

## Category-Based Permission Control

In addition to the role-based access control described above, EMQX Enterprise supports category-based fine-grained permission control for Dashboard users. This allows administrators to narrow a user's access beyond their role by assigning specific permission categories.

### Permission Categories

EMQX defines 9 permission categories. The first 6 categories apply to both API keys and Dashboard users. The last 3 categories apply only to Dashboard users and cannot be assigned to API keys.

| Category | Description |
|----------|-------------|
| `banned` | Blacklist management |
| `rule_engine` | Rule engine and actions |
| `resources` | Connectors and bridges |
| `plugins` | Plugin management |
| `modules` | Module configuration |
| `others` | Miscellaneous endpoints |
| `user_management` | Manage other Dashboard accounts |
| `mfa_management` | Manage MFA. Administrators can manage other users' MFA; viewers can only exempt themselves from a forced-MFA lock. |
| `app_management` | Manage API keys |

### Role-Permission Compatibility

| Role | Allowed Permission Categories | Role Default (no explicit categories) |
|------|---------------|-----------------------------------|
| `administrator` | All 9 categories | Pre-upgrade behaviour: access to all endpoints |
| `viewer` | 6 common categories and `mfa_management`. `user_management` and `app_management` are **not** allowed for viewers. | 6 common categories: `banned`, `rule_engine`, `resources`, `plugins`, `modules`, and `others`. |

If explicit scopes are set for an administrator, both read and write operations are restricted to those scopes. Viewers remain read-only regardless of scopes. For viewers, GET requests are also scope-checked, and non-self non-GET requests are denied. Viewers cannot download backup archives from `/api/v4/data/file/*`; listing exports remains available. Self-service paths, for example, a user changing their own password, managing their own MFA, and logging out, are always permitted regardless of scopes.

### Set Permissions for a User

When creating or updating a Dashboard user, administrators can select one or more **Scopes** to restrict the user's permissions. In API and backup data, these values are stored as the `scopes` array in the user's `tags` field:

- **Omitted** (no change / use role default): The user receives the pre-upgrade role-based behaviour.
- **Empty array `[]`**: The user is denied access to all scope-gated endpoints, including GET requests (self-service paths remain available).
- **Non-empty array**: The user can only access endpoints that belong to the listed categories. For viewers, the role's read-only restriction still applies.

If the selected scopes are not compatible with the user's role, the operation returns an error. For example, viewers cannot be assigned `user_management` or `app_management`.

:::tip Example
For example, select **Modules** to allow the user to access only module-related API paths. In API and backup data, this selection is stored as `scopes: ["modules"]`. Self-service operations, such as changing the user's own password, managing their own MFA, and logging out, remain available.
:::

### Default Administrator Protection

The default administrator account configured by `dashboard.default_user.login` has additional safeguards:

- It cannot be demoted to the `viewer` role.
- It cannot have an explicit `scopes` field (it always holds the full category set).
- It cannot be deleted.

These protections ensure the cluster always has a break-glass administrator account that can recover from accidental permission misconfigurations.

## SSO Users

When a user logs in through SAML Single Sign-On (SSO) for the first time, EMQX automatically creates a Dashboard account for that user with the `viewer` role.

- SSO users are assigned a random internal password and cannot log in with a username and password directly.
- Administrators can change an SSO user's role through the Dashboard Users page.
- See the [SAML 2.0 Single Sign-On](../modules/saml_sso.md) documentation for configuration details.

:::tip
To grant an SSO user administrator access, edit the user on the **Users** page and change their role to `administrator`.
:::
