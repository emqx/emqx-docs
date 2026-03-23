# API Key

EMQX Enterprise provides a Management API (port 8081 by default) for programmatic access to cluster management operations. API Keys, identified by an AppID and AppSecret pair, authenticate requests to this Management API. Each key can be restricted to specific API categories, giving you fine-grained control over what each integration or automation tool is allowed to do.

This page covers how to create API Keys, configure permissions, and authenticate requests.

![API Keys](./assets/api_key.png)

## Quick Start

This section walks through the basic flow: create an API Key and use it to call the Management API.

1. In the Dashboard, click **Manage** -> **Application** in the left navigation panel, then click **Create**.

2. Fill in the required fields (name, permissions, etc.) and save. Record the generated AppID and AppSecret.

3. Use the key to call the Management API (port 8081):

   ```bash
   curl -u <app_id>:<app_secret> "http://127.0.0.1:8081/api/v4/clients"
   ```

4. Configure which write operations the key can perform via [Permission Categories](#permission-categories). For example, to allow only rule engine write access, set `rule_engine` to `true` in the permission settings.

## Authenticate to the Management API

All requests to the Management API (port `8081`) must authenticate with an API Key.

An API Key consists of an `AppID` and `AppSecret`, authenticated via HTTP Basic Auth:

```
Authorization: Basic base64(AppID:AppSecret)
```

Most HTTP clients handle this automatically with the `-u` flag:

```bash
curl -u my_app_id:my_app_secret "http://127.0.0.1:8081/api/v4/clients"
```

### Authentication Failure

Requests return HTTP `401` in the following cases:

- `AppID` or `AppSecret` is invalid
- The API Key is disabled (`status: false`)
- The API Key has expired

## Create an API Key

To create an API Key for accessing the Management API:

1. In the left navigation panel, click **Manage**, then click **Apps** (HTTP API).
2. Click **Add App**.
3. Fill in the fields and configure permissions as needed. For details on permissions, see [Permission Model](#permission-model).
4. Click **Confirm** to save.

::: warning Note

The AppSecret is shown only at creation time. Store it securely.

:::

## Permission Model

API Key permissions control write operations (`PUT`, `POST`, `DELETE`) to the corresponding endpoints. Read (`GET`) requests to all APIs are always allowed, regardless of permission settings.

**New API Keys deny all write operations by default. Enable write access only as needed.**

### Permission Categories

Each API Key has an independent boolean permission for each of the following five categories.

Setting a category to `true` allows the key to perform write operations on the corresponding endpoints. Setting it to `false` denies write operations (GET requests are still allowed).

| Category | Permission Key | Endpoints Controlled |
|----------|---------------|----------------------|
| Banned | `banned` | `/api/v4/banned/` (client blacklist management) |
| Rule Engine | `rule_engine` | `/api/v4/rules/`, `/api/v4/actions/`, `/api/v4/rule_events/` |
| Resources | `resources` | `/api/v4/resources/`, `/api/v4/resource_types/` |
| Plugins | `plugins` | `/api/v4/plugins/` |
| Modules | `modules` | `/api/v4/modules/`, `/api/v4/trace/`, `/api/v4/topic-metrics/`, `/api/v4/quota/`, `/api/v4/client_tags/` |

New API Keys default all five categories to `false`, following the principle of least privilege. Enable only the write permissions the key actually needs. All keys can always read (GET) from any endpoint.

### The `fallback` Setting

Many commonly used endpoints do not belong to any of the five named categories, such as:

- `/api/v4/clients/`
- `/api/v4/subscriptions/`
- `/api/v4/stats/`
- `/api/v4/metrics/`
- `/api/v4/nodes/`

The `fallback` setting controls **write access** when a key tries to call write operations on these endpoints:

- `false` (default): Write access is denied.
- `true`: Write access is allowed.

Read (GET) requests to these endpoints are always allowed regardless of the `fallback` setting.

::: tip

Most read-only monitoring APIs (clients, subscriptions, stats, metrics, nodes) fall into the uncategorized group governed by `fallback`. Since GET is always allowed, you can read monitoring data without setting `fallback` to `true`. Only set `fallback: true` if you need to perform write operations on uncategorized endpoints.

:::

### Compatibility Mode

API Keys created before the permission system was introduced operate in compatibility mode. A compatibility mode key has full read and write access to all APIs, equivalent to all categories set to `true` and `fallback` set to `true`.

To apply permission restrictions to a compatibility mode key, update the key via the Dashboard with an explicit permissions configuration. This exits compatibility mode and applies the permissions you specify.

::: warning Note

Exiting compatibility mode is irreversible. Once a key exits compatibility mode, it operates under the normal permission system.

:::

## Manage API Keys

API Keys can be managed through the Dashboard under **Manage** -> **Apps** (HTTP API). You can view, update, disable, or delete any key from this page.

- **View details**: Click the key name to see its AppID, permissions, status, and expiration.
- **Update**: Click **Edit** to modify the name, description, status, expiration, or permissions.
- **Disable**: Toggle the key's status to disabled. Disabled keys return HTTP `401` on any API request.
- **Delete**: Click **Delete** to permanently remove a key.

## Pre-configure API Keys with a Bootstrap File

You can pre-configure API Keys before EMQX starts using a bootstrap file. This is useful for initial deployments or containerized environments where credentials must be available before any API calls are possible.

**Configuration:**

Set the environment variable pointing to the file path:

```bash
EMQX_API_KEY__BOOTSTRAP_FILE=/path/to/bootstrap_keys.txt
```

**File format:**

One key per line, with the AppID and AppSecret separated by a colon:

```
my_app_id:my_app_secret
another_app:another_secret
```

Bootstrap keys are created with full access — no permission restrictions and `fallback` set to `true`. They carry the description tag `Bootstrapped From File`. After EMQX starts, you can update these keys via the Dashboard to apply permission restrictions.

::: tip

Use the bootstrap file to create an initial admin key for managing other API Keys. After startup, manage all subsequent keys through the Dashboard.

:::

## Security Recommendations

- **Principle of least privilege:** Grant only the write permissions a key actually needs. A CI/CD pipeline that only manages rules should have `rule_engine: true` and everything else `false`. All keys can still read (GET) any endpoint.
- **Control `fallback` carefully:** Leave `fallback` as `false` unless the key specifically needs write access to uncategorized endpoints. GET requests are always allowed regardless.
- **Use expiration dates:** Set an expiration for temporary keys used in short-lived pipelines or test environments.
- **Rotate secrets:** Delete and recreate keys periodically via the Dashboard.
- **Bootstrap for setup, Dashboard for ongoing management:** Use the bootstrap file to create your initial management key, then manage all subsequent keys through the Dashboard.
