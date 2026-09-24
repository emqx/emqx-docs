# OpenTelemetry を統合したログ管理

ファイルログと同様に、OpenTelemetry ログは重要なイベント、ステータス情報、エラーメッセージを記録し、開発者や運用チームがアプリケーションの動作を理解しトラブルシューティングを行うのに役立ちます。ただし、OpenTelemetry ログは標準化されたログフォーマットを採用しているため、ログの解析や分析、処理が容易です。さらに、OpenTelemetry ログは Trace ID、タグ、属性などの豊富なコンテキスト情報をレコードに追加することをサポートしています。

本ページでは、EMQX と OpenTelemetry ログハンドラーを統合して高度なログ管理を実現するための包括的なガイドを提供します。OpenTelemetry Collector のセットアップ、EMQX における OpenTelemetry ログハンドラーの設定とログのエクスポート、ログの過負荷管理について説明します。この統合により、EMQX のログイベントを [OpenTelemetry ログデータモデル](https://opentelemetry.io/docs/specs/otel/logs/data-model/) に準拠した形式でフォーマットし、設定済みの OpenTelemetry Collector またはバックエンドシステムにエクスポートできるため、監視やデバッグ機能が向上します。

## OpenTelemetry Collector のセットアップ

EMQX の OpenTelemetry ログを有効にする前に、OpenTelemetry Collector と OpenTelemetry 互換のログ収集システムをデプロイおよび設定する必要があります。本ガイドでは、[OpenTelemetry Collector](https://opentelemetry.io/docs/collector/getting-started) のデプロイ方法と、デバッグエクスポーターを使用してログを `stdout` に転送する設定方法を説明します。

1. `otel-logs-collector-config.yaml` という名前で OpenTelemetry Collector の設定ファイルを作成します。

   ```yaml
   receivers:
     otlp:
       protocols:
         grpc:

   exporters:
     logging:
       verbosity: detailed

   processors:
     batch:

   extensions:
     health_check:

   service:
     extensions: [health_check]
     pipelines:
       logs:
         receivers: [otlp]
         processors: [batch]
         exporters: [logging]
   ```

2. 同じディレクトリに `docker-compose-otel-logs.yaml` という Docker Compose ファイルを作成します。

   ```yaml
   version: '3.9'

   services:
     # Collector
     otel-collector:
       image: otel/opentelemetry-collector:0.90.0
       restart: always
       command: ["--config=/etc/otel-collector-config.yaml", "${OTELCOL_ARGS}"]
       volumes:
         - ./otel-logs-collector-config.yaml:/etc/otel-collector-config.yaml
       ports:
         - "13133:13133" # Health check extension
         - "4317:4317"   # OTLP gRPC receiver
   ```

3. Docker Compose を使って Collector を起動します。

   ```bash
   docker compose -f docker-compose-otel-logs.yaml up
   ```

4. 起動後、OpenTelemetry Collector は [http://localhost:4317](http://localhost:4317/) でアクセス可能になります。


## EMQX で OpenTelemetry ログハンドラーを有効化

`opentelemetry.exporter.endpoint` は 1 つの URL を受け入れます。URL は `http` または `https` スキームを使用し、明示的なポート番号を含める必要があります。例えば `http://localhost:4317` は有効ですが、`localhost:4317` や `http://localhost` は無効です。

1. EMQX がローカルで動作していることを想定し、`cluster.hocon` ファイルに以下の設定を追加します。

   ```bash
   opentelemetry {
     exporter {
       endpoint = "http://localhost:4317"
       headers {
         authorization = ""Basic dXNlcjpwYXNzd29yZA=="
       }
     }
     logs {enable = true, level = warning}
   }
   ```

   また、ダッシュボードの **Management** -> **Monitoring** にある **Integration** タブから OpenTelemetry ログ統合の設定も可能です。

   ::: tip 注意事項

   `opentelemetry.logs.level` の設定は、[EMQX ログハンドラー](../log.md) で設定されたデフォルトのログレベルによって上書きされます。例えば、OpenTelemetry のログレベルが `info` でも、EMQX のコンソールログレベルが `error` に設定されている場合は、`error` レベル以上のイベントのみがエクスポートされます。

   :::

2. EMQX ノードを起動します。

3. ダッシュボードからアクセスできない HTTP サービスへのブリッジ作成など、EMQX のログイベントを発生させます。

   <img src="./assets/otel-logs-bridge-example-en.png" alt="Otel-logs-HTTP-bridge-example" style="zoom:67%;" />

4. 数秒以内（デフォルトは約1秒）に、Otel Collector が HTTP ブリッジ接続失敗を示す EMQX ログイベントを受信していることを確認できます。

   ![Otel-collector-logs-debug-output](./assets/otel-collector-logs-debug-output.png)

## ログの過負荷管理

EMQX はログイベントを蓄積し、定期的にバッチでエクスポートします。
このエクスポート頻度は `opentelemetry.logs.scheduled_delay` パラメータで制御され、デフォルトは 1 秒です。
バッチングログハンドラーは過負荷保護機能を備えており、蓄積可能なイベント数の上限を持ち、デフォルトは 2048 です。以下の設定でこの上限を変更できます。

```bash
opentelemetry {
  logs { max_queue_size = 2048 }
}
```

`max_queue_size` の上限に達すると、新しいログイベントは現在のキューがエクスポートされるまで破棄されます。

::: tip 注意事項

OpenTelemetry ログの過負荷保護は、デフォルトの [EMQX ログハンドラー](../log.md) の過負荷保護とは独立して動作します。
そのため、設定によっては同じログイベントが OpenTelemetry ハンドラーで破棄され、デフォルトの EMQX ログハンドラーでは記録される場合や、その逆もあり得ます。

:::
