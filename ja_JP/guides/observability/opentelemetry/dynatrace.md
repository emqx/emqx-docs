# OpenTelemetry と Dynatrace の統合

EMQX 6.3.0 以降、EMQX は OpenTelemetry のログおよびトレースを直接 Dynatrace にエクスポートすることをサポートしています。この統合は OTLP HTTP/protobuf プロトコルと OAuth2 クライアント認証情報認証を使用します。

::: tip 注意

EMQX 6.3.0 では、Dynatrace 統合は OpenTelemetry のログおよびトレースのみをサポートし、`type = dynatrace` の場合、メトリクスはサポートされていません。

:::

## 動作概要

`opentelemetry.type` を `dynatrace` に設定すると、EMQX は Dynatrace 専用のエクスポーター設定を使用します。EMQX は設定された Dynatrace のトークンエンドポイントから OAuth2 アクセストークンを取得し、`Authorization` ヘッダーに `Bearer` トークンとして追加し、有効なログおよびトレースを OTLP HTTP/protobuf 経由で Dynatrace にエクスポートします。

OTLP HTTP エンドポイントの場合、EMQX はシグナル固有のパスを自動的に付加します。これらのパスは標準の Dynatrace OTLP 受信パスと一致します：

- ログ: `/v1/logs`
- トレース: `/v1/traces`

したがって、`opentelemetry.exporter.endpoint` にはシグナル固有のパスを含まない Dynatrace OTLP のベース URL を設定してください。

## 前提条件

EMQX を設定する前に、Dynatrace で以下の情報を準備してください。エンドポイント形式および受信スコープについては、[Dynatrace OTLP API endpoints](https://docs.dynatrace.com/docs/ingest-from/opentelemetry/otlp-api) を参照してください。

- Dynatrace OTLP ベース URL
  - Dynatrace SaaS: `https://{your-environment-id}.live.dynatrace.com/api/v2/otlp`
  - Environment ActiveGate: `https://{your-activegate-domain}:9999/e/{your-environment-id}/api/v2/otlp`
- OAuth2 クライアント ID とクライアントシークレット
- OAuth2 トークンエンドポイント（例: `https://sso.dynatrace.com/sso/oauth2/token`）
- OAuth2 リソース値（例: `urn:dtaccount:{your-account-uuid}`）
- エクスポートしたいシグナルに必要な Dynatrace のスコープ（例: トレース用の `openTelemetryTrace.ingest`、ログ用の `logs.ingest`）

## ダッシュボードでの Dynatrace 設定

EMQX ダッシュボードから Dynatrace 統合を設定できます：

1. 左側のナビゲーションメニューで **Management** -> **Monitoring** をクリックします。
2. **Monitoring** ページで **Integration** タブをクリックします。
3. **Monitoring Platform** で **OpenTelemetry** を選択します。
4. **OpenTelemetry Type** で **Dynatrace** を選択します。
5. **Feature Selection** で **Traces**、**Logs**、または両方を選択します。Dynatrace 統合はメトリクスをサポートしていません。
6. **Endpoint** に Dynatrace OTLP ベース URL を入力します。`/v1/logs` や `/v1/traces` は付加しないでください。
7. 任意で **Headers** にて **Add** をクリックし、EMQX が Dynatrace OTLP エンドポイントに送信する追加の HTTP ヘッダーを設定できます。`Authorization` ヘッダーは不要です。EMQX がアクセストークンを取得して自動的に追加します。
8. Dynatrace OTLP エンドポイントが HTTPS を使用している場合は、エクスポーターの **Enable TLS** をオンにします。

**OAuth2 Authentication** セクションでは、以下の項目を設定します：

| 項目 | 説明 |
| --- | --- |
| **Token Endpoint** | Dynatrace の OAuth2 トークンエンドポイント |
| **Client ID** | OAuth2 クライアント ID |
| **Client Secret** | OAuth2 クライアントシークレット |
| **Resource** | Dynatrace が要求する OAuth2 リソース値。`urn:dtaccount:{your-account-uuid}` 形式で指定します。 |
| **Scope** | 任意の OAuth2 スコープ。OAuth2 クライアント作成時にスコープが固定されている場合は省略可能です。そうでなければ、有効にしたシグナルに必要なスコープを設定してください。 |
| **Timeout** | トークンリクエストのタイムアウト |
| **Enable TLS** | トークンエンドポイントが HTTPS の場合に TLS を有効化 |

**Traces** を選択した場合は、**Trace Mode**、**Traces All Messages**、**Traces Export Interval**、**Max Queue Size** を必要に応じて設定してください。

**Logs** を選択した場合は、**Logs Level** と **Logs Export Interval** を必要に応じて設定してください。

設定が完了したら、**Save Changes** をクリックして適用します。

## HOCON での Dynatrace 設定

`etc/base.hocon` に以下の設定を追加するか、REST API で同等の設定を適用してください：

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

プレースホルダーはご自身の Dynatrace 環境、アカウント、OAuth2 クライアント情報に置き換えてください。

## 設定に関する注意点

- `opentelemetry.type`: Dynatrace 専用の OpenTelemetry 設定を使用するために `dynatrace` に設定します。
- `opentelemetry.exporter.endpoint`: Dynatrace OTLP ベース URL を設定します。`/v1/logs` や `/v1/traces` は付加しないでください。EMQX が自動でパスを付加します。
- `opentelemetry.exporter.auth.kind`: `dynatrace_oauth2` に設定します。
- `opentelemetry.exporter.auth.enable`: OAuth2 トークン取得を有効にするために `true` に設定します。
- `opentelemetry.exporter.auth.resource`: Dynatrace OAuth2 クライアント認証情報フローで必須です。EMQX はアクセストークン取得時に `resource` パラメータとして送信します。
- `opentelemetry.exporter.auth.scope`: 任意です。OAuth2 クライアント作成時にスコープが固定されている場合は省略可能です。そうでなければ、有効なシグナルに必要なスコープを設定してください。
- `opentelemetry.exporter.ssl_options.enable`: Dynatrace SaaS または HTTPS の ActiveGate エンドポイントにエクスポートする場合は TLS を有効にします。

`logs`、`traces` のいずれか、または両方を有効にできます。`opentelemetry.type = dynatrace` の場合は `metrics` を設定しないでください。

## 統合の検証

EMQX を再起動するか設定を適用した後、ログイベントや MQTT トレースイベントを生成し、Dynatrace に OpenTelemetry のログやトレースが届いているか確認してください。

データが Dynatrace に表示されない場合は以下を確認してください：

- エンドポイントが OTLP ベース URL であり、`/v1/logs` や `/v1/traces` を含んでいないか
- OAuth2 クライアント ID、クライアントシークレット、トークンエンドポイント、リソース、スコープが正しいか
- ネットワークが EMQX から Dynatrace エンドポイントへの接続を許可しているか
- EMQX のログに OAuth2 トークンリクエストやエクスポートのエラーがないか
