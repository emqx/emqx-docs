# API Key Permissions Management

EMQX Enterprise provides a Management API (port 8081 by default) for programmatic access to cluster management operations. API Keys, identified by an AppID and AppSecret pair, authenticate requests to this Management API. Each key can be restricted to specific API categories, giving you fine-grained control over what each integration or automation tool is allowed to do.

![API Keys](./assets/api_key_permissions.png)

## Management API vs Dashboard API

EMQX exposes two separate HTTP API services with independent authentication systems:

| | Management API | Dashboard API |
|---|---|---|
| **Port** | 8081 | 18083 |
| **Authentication** | AppID + AppSecret (Basic Auth) | Dashboard user credentials (Basic Auth) |
| **Used for** | Plugin/module APIs, automation, integration, CI/CD | Dashboard web UI, API Key management |
| **Path prefix** | `/api/v4/` | `/api/v4/` |

Both services share the same `/api/v4/` path namespace, but they authenticate separately. Credentials for one do not work on the other.

**Important:** The API Key management endpoints (`/api/v4/apps/`) are part of the **Dashboard API on port 18083**, not the Management API on port 8081. Use Dashboard user credentials to create and manage API keys.

## Creating an API Key

### Via Dashboard

1. In the left-hand navigation panel, click **Manage** then **Apps** (HTTP API).
2. Click **Add App**.
3. Fill in the fields and configure permissions as needed.
4. Click **Confirm** to save.

### Via API

**Endpoint:** `POST /api/v4/apps/`

**Request Parameters (JSON):**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `app_id` | string | Yes | Unique identifier for the API key |
| `name` | string | No | Display name |
| `secret` | string | No | Custom secret. Auto-generated if omitted |
| `desc` | string | No | Description |
| `status` | boolean | No | Enabled or disabled. Defaults to `true` |
| `expired` | integer | No | Expiration timestamp in seconds since epoch. Omit for no expiration |
| `permissions` | object | No | Permission map keyed by category name. See [Permission Categories](#permission-categories) |
| `fallback` | boolean | No | Default behavior for API paths not covered by any category. Defaults to `false` (deny) |

**Example:**

```bash
curl -i -X POST "http://127.0.0.1:18083/api/v4/apps/" \
  -u admin:public \
  -H "Content-Type: application/json" \
  -d '{
    "app_id": "my_automation",
    "name": "CI/CD Pipeline",
    "desc": "Used by CI/CD for rule engine management",
    "status": true,
    "permissions": {
      "rule_engine": true,
      "resources": true,
      "plugins": false,
      "modules": false,
      "banned": false
    },
    "fallback": false
  }'
```

**Response:**

```json
{
  "code": 0,
  "data": {
    "secret": "<generated_secret_token>"
  }
}
```

::: warning

The AppSecret is visible in API responses (both creation and lookup). Store it securely. The `/api/v4/apps/` endpoints are only accessible via the Dashboard API (port 18083) with Dashboard user credentials, not via API Keys.

:::

## Permission Categories

Each API Key has an independent boolean permission for each of the following categories. The permission controls **write access** (PUT, POST, DELETE) to the corresponding endpoints. **Read (GET) access to all APIs is always allowed**, regardless of permission settings.

Setting a category to `true` allows the key to call write operations on the corresponding endpoints. Setting it to `false` denies write operations (GET requests are still allowed).

| Category | Permission Key | Endpoints Controlled |
|----------|---------------|----------------------|
| Banned | `banned` | `/api/v4/banned/` — Client blacklist management |
| Rule Engine | `rule_engine` | `/api/v4/rules/`, `/api/v4/actions/`, `/api/v4/rule_events/` |
| Resources | `resources` | `/api/v4/resources/`, `/api/v4/resource_types/` |
| Plugins | `plugins` | `/api/v4/plugins/` |
| Modules | `modules` | `/api/v4/modules/`, `/api/v4/trace/`, `/api/v4/topic-metrics/`, `/api/v4/quota/`, `/api/v4/client_tags/` |

New API keys are created with all five categories set to `false` by default. This follows the principle of least privilege — you grant write access only for what the key actually needs. All keys can always read (GET) from any endpoint.

### The `fallback` Setting

Many commonly used endpoints — such as `/api/v4/clients/`, `/api/v4/subscriptions/`, `/api/v4/stats/`, `/api/v4/metrics/`, and `/api/v4/nodes/` — do not belong to any of the five named categories. The `fallback` setting controls **write access** when a key tries to call write operations on these endpoints:

- `false` (default): Write access is denied.
- `true`: Write access is allowed.

Read (GET) requests to these endpoints are always allowed regardless of the fallback setting.

::: tip

Most read-only monitoring APIs (clients, subscriptions, stats, metrics, nodes) fall into the unrecognized category governed by `fallback`. Since GET is always allowed, you can read monitoring data without setting `fallback` to `true`. Only set `fallback: true` if you need to make write operations on uncategorized APIs.

:::

## Compatibility Mode

API keys created before the permission system was introduced operate in compatibility mode. A compatibility mode key has full read and write access to all APIs, equivalent to setting all categories to `true` and `fallback` to `true`.

You can identify a compatibility mode key by the `compatibility_mode: true` field in its API response.

To apply permission restrictions to a compatibility mode key, update the key with an explicit `permissions` object. This exits compatibility mode and applies the permissions you specify.

::: warning

Updating a compatibility mode key with a `permissions` object is irreversible in terms of mode transition. Once a key exits compatibility mode, it operates under the normal permission system.

:::

## Managing API Keys

### List All Keys

**Endpoint:** `GET /api/v4/apps/`

**Example:**

```bash
curl -u admin:public "http://127.0.0.1:18083/api/v4/apps/"
```

**Response Example:**

```json
{
  "code": 0,
  "data": [
    {
      "status": true,
      "permissions": {
        "rule_engine": true,
        "resources": true,
        "plugins": false,
        "modules": false,
        "banned": false
      },
      "name": "Documentation Test",
      "expired": "undefined",
      "desc": "Created for documentation examples",
      "compatibility_mode": false,
      "app_id": "doc_test_key"
    }
  ]
}
```

### Get Key Details

**Endpoint:** `GET /api/v4/apps/:appid`

**Example:**

```bash
curl -u admin:public "http://127.0.0.1:18083/api/v4/apps/my_automation"
```

**Response Example:**

```json
{
  "code": 0,
  "data": {
    "status": true,
    "secret": "<secret>",
    "permissions": {
      "rule_engine": true,
      "resources": true,
      "plugins": false,
      "modules": false,
      "banned": false
    },
    "name": "Documentation Test",
    "expired": "undefined",
    "desc": "Created for documentation examples",
    "compatibility_mode": false,
    "app_id": "doc_test_key"
  }
}
```

::: tip

The `secret` field is only included when fetching a single key's details (lookup). When listing all keys, the `secret` field is not returned for security reasons.

:::

### Update a Key

**Endpoint:** `PUT /api/v4/apps/:appid`

You can update `name`, `desc`, `status`, `expired`, `permissions`, and `fallback` independently. Only the fields you include in the request body are changed.

**Example — disable a key:**

```bash
curl -i -X PUT "http://127.0.0.1:18083/api/v4/apps/my_automation" \
  -u admin:public \
  -H "Content-Type: application/json" \
  -d '{"status": false}'
```

**Example — update permissions only:**

```bash
curl -i -X PUT "http://127.0.0.1:18083/api/v4/apps/my_automation" \
  -u admin:public \
  -H "Content-Type: application/json" \
  -d '{
    "permissions": {
      "rule_engine": true,
      "resources": true,
      "plugins": true,
      "modules": false,
      "banned": false
    }
  }'
```

**Response:**

```json
{"code": 0}
```

### Delete a Key

**Endpoint:** `DELETE /api/v4/apps/:appid`

```bash
curl -i -X DELETE "http://127.0.0.1:18083/api/v4/apps/my_automation" \
  -u admin:public
```

**Response:**

```json
{"code": 0}
```

## Bootstrap File

You can pre-configure API keys before EMQX starts using a bootstrap file. This is useful for initial deployments or containerized environments where you need credentials available before any API calls are possible.

**Configuration:**

Set the environment variable pointing to your file:

```bash
EMQX_API_KEY__BOOTSTRAP_FILE=/path/to/bootstrap_keys.txt
```

**File format:**

One key per line, with the AppID and AppSecret separated by a colon:

```
my_app_id:my_app_secret
another_app:another_secret
```

Bootstrap keys are created with full access — no permission restrictions and `fallback` set to `true`. They carry the description tag `Bootstrapped From File`. After EMQX starts, you can update these keys via the API to apply permission restrictions.

::: tip

Use the bootstrap file for the initial admin key that manages other API keys. After startup, use that key to create more restricted keys for specific integrations.

:::

## Authentication

All Management API requests require HTTP Basic Authentication:

```
Authorization: Basic base64(AppID:AppSecret)
```

Most HTTP clients handle this automatically with a `-u` flag:

```bash
curl -u my_app_id:my_app_secret "http://127.0.0.1:8081/api/v4/clients"
```

Requests with an invalid AppID or AppSecret return HTTP `401`. Requests using a disabled key (`status: false`) or an expired key also return `401`.

## API Reference Summary

| Method | Endpoint | Description |
|--------|----------|-------------|
| `POST` | `/api/v4/apps/` | Create an API key |
| `GET` | `/api/v4/apps/` | List all API keys |
| `GET` | `/api/v4/apps/:appid` | Get API key details |
| `PUT` | `/api/v4/apps/:appid` | Update an API key |
| `DELETE` | `/api/v4/apps/:appid` | Delete an API key |

## Security Recommendations

- **Principle of least privilege:** Grant only the write permissions a key actually needs. A CI/CD pipeline that only manages rules should have `rule_engine: true` and everything else `false`. All keys can still read (GET) any endpoint.
- **Control `fallback` carefully:** Leave `fallback` as `false` unless the key specifically needs write access to uncategorized endpoints. GET requests are always allowed regardless.
- **Use expiration dates:** Set the `expired` field for temporary keys used in short-lived pipelines or test environments.
- **Rotate secrets:** Delete and recreate keys periodically, or update them with a new `secret` value.
- **Bootstrap for setup, API for operations:** Use the bootstrap file to create your initial management key, then manage all subsequent keys through the API.
