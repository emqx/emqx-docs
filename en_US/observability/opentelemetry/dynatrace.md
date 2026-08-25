# Integrate OpenTelemetry with Dynatrace

Starting from EMQX 6.3.0, EMQX supports exporting OpenTelemetry logs and traces directly to Dynatrace. This integration uses the OTLP HTTP/protobuf protocol and OAuth2 client credentials authentication.

::: tip Note

In EMQX 6.3.0, Dynatrace integration supports only OpenTelemetry logs and traces. Metrics are not supported for `type = dynatrace`.

:::

## How It Works

When `opentelemetry.type` is set to `dynatrace`, EMQX uses a Dynatrace-specific exporter configuration. EMQX obtains an OAuth2 access token from the configured Dynatrace token endpoint, adds it as a `Bearer` token in the `Authorization` header, and exports enabled logs and traces to Dynatrace through OTLP HTTP/protobuf.

For OTLP HTTP endpoints, EMQX appends the signal-specific path automatically. These paths match the standard Dynatrace OTLP ingest paths:

- Logs: `/v1/logs`
- Traces: `/v1/traces`

Therefore, set `opentelemetry.exporter.endpoint` to the Dynatrace OTLP base URL, without the signal-specific path.

## Prerequisites

Before configuring EMQX, prepare the following information in Dynatrace. For endpoint formats and ingest scopes, see [Dynatrace OTLP API endpoints](https://docs.dynatrace.com/docs/ingest-from/opentelemetry/otlp-api).

- The Dynatrace OTLP base URL.
  - Dynatrace SaaS: `https://{your-environment-id}.live.dynatrace.com/api/v2/otlp`
  - Environment ActiveGate: `https://{your-activegate-domain}:9999/e/{your-environment-id}/api/v2/otlp`
- An OAuth2 client ID and client secret.
- The OAuth2 token endpoint, for example `https://sso.dynatrace.com/sso/oauth2/token`.
- The OAuth2 resource value, for example `urn:dtaccount:{your-account-uuid}`.
- The scopes required by Dynatrace for the signals you want to export, such as `openTelemetryTrace.ingest` for traces and `logs.ingest` for logs.

## Configure Dynatrace in Dashboard

You can configure the Dynatrace integration from the EMQX Dashboard:

1. In the left navigation menu, click **Management** -> **Monitoring**.
2. On the **Monitoring** page, click the **Integration** tab.
3. Under **Monitoring Platform**, select **OpenTelemetry**.
4. Under **OpenTelemetry Type**, select **Dynatrace**.
5. Under **Feature Selection**, select **Traces**, **Logs**, or both. Dynatrace integration does not support metrics.
6. In **Endpoint**, enter the Dynatrace OTLP base URL. Do not append `/v1/logs` or `/v1/traces`.
7. Optional: In **Headers**, click **Add** to add extra HTTP headers that EMQX should send to the Dynatrace OTLP endpoint. You do not need to add an `Authorization` header; EMQX obtains the access token and adds this header automatically.
8. If the Dynatrace OTLP endpoint uses HTTPS, turn on **Enable TLS** for the exporter.

In the **OAuth2 Authentication** section, configure the following fields:

| Field | Description |
| --- | --- |
| **Token Endpoint** | Dynatrace OAuth2 token endpoint. |
| **Client ID** | OAuth2 client ID. |
| **Client Secret** | OAuth2 client secret. |
| **Resource** | OAuth2 resource value required by Dynatrace. Use the format `urn:dtaccount:{your-account-uuid}`. |
| **Scope** | Optional OAuth2 scopes. Configure scopes that match the enabled signals when Dynatrace requires them. |
| **Timeout** | Timeout for token requests. |
| **Enable TLS** | Enable TLS for token requests when the token endpoint uses HTTPS. |

If **Traces** is selected, configure **Trace Mode**, **Traces All Messages**, **Traces Export Interval**, and **Max Queue Size** as needed.

If **Logs** is selected, configure **Logs Level** and **Logs Export Interval** as needed.

Click **Save Changes** to apply the configuration.

## Configure Dynatrace with HOCON

Add the following configuration to `etc/base.hocon`, or apply the equivalent configuration through the REST API:

```hocon
opentelemetry {
  type = dynatrace

  exporter {
    endpoint = "https://{your-environment-id}.live.dynatrace.com/api/v2/otlp"

    auth {
      kind = dynatrace_oauth2
      enable = true
      token_endpoint = "https://sso.dynatrace.com/sso/oauth2/token"
      client_id = "{your-client-id}"
      client_secret = "{your-client-secret}"
      resource = "urn:dtaccount:{your-account-uuid}"
      scope = "openTelemetryTrace.ingest logs.ingest"
    }

    ssl_options {
      enable = true
    }
  }

  logs {
    enable = true
    level = warning
  }

  traces {
    enable = true
    scheduled_delay = "5s"
  }
}
```

Replace the placeholders with your Dynatrace environment, account, and OAuth2 client information.

## Configuration Notes

- `opentelemetry.type`: Set to `dynatrace` to use the Dynatrace-specific OpenTelemetry configuration.
- `opentelemetry.exporter.endpoint`: Set this to the Dynatrace OTLP base URL. Do not append `/v1/logs` or `/v1/traces`; EMQX appends the path automatically.
- `opentelemetry.exporter.auth.kind`: Set to `dynatrace_oauth2`.
- `opentelemetry.exporter.auth.enable`: Set to `true` to enable OAuth2 token retrieval.
- `opentelemetry.exporter.auth.resource`: Required by Dynatrace OAuth2 client credentials flow. EMQX sends it as the `resource` parameter when requesting an access token.
- `opentelemetry.exporter.auth.scope`: Optional in EMQX, but Dynatrace may require scopes that match the enabled signals.
- `opentelemetry.exporter.ssl_options.enable`: Enable TLS when exporting to Dynatrace SaaS or an HTTPS ActiveGate endpoint.

You can enable either `logs`, `traces`, or both. Do not configure `metrics` when `opentelemetry.type = dynatrace`.

## Verify the Integration

After restarting EMQX or applying the configuration, generate log events or MQTT trace events, then check Dynatrace for incoming OpenTelemetry logs or traces.

If data does not appear in Dynatrace:

- Verify that the endpoint is the OTLP base URL and does not include `/v1/logs` or `/v1/traces`.
- Verify that the OAuth2 client ID, client secret, token endpoint, resource, and scopes are correct.
- Verify that the network allows EMQX to connect to the Dynatrace endpoint.
- Check the EMQX logs for OAuth2 token request or export errors.
