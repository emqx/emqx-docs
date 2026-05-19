# OpenTelemetry を統合したログ管理

ファイルログと同様に、OpenTelemetry ログは重要なイベント、ステータス情報、エラーメッセージを記録し、開発者や運用チームがアプリケーションの挙動を理解しトラブルシューティングを行うのに役立ちます。ただし、OpenTelemetry ログは標準化されたログフォーマットを採用しているため、ログの解析、分析、処理が容易になります。さらに、OpenTelemetry ログは Trace ID、タグ、属性などの豊富なコンテキスト情報をレコードに追加することが可能です。

本ページでは、EMQX と OpenTelemetry ログハンドラーを統合して高度なログ管理を実現するための包括的なガイドを提供します。OpenTelemetry コレクターのセットアップ、EMQX における OpenTelemetry ログハンドラーの設定およびログのエクスポート、ログ過負荷の管理方法について解説します。この統合により、EMQX のログイベントを [OpenTelemetry ログデータモデル](https://opentelemetry.io/docs/specs/otel/logs/data-model/) に準拠した形式でフォーマットし、設定済みの OpenTelemetry コレクターやバックエンドシステムへエクスポートでき、監視やデバッグ機能が向上します。

## OpenTelemetry コレクターのセットアップ

EMQX の OpenTelemetry ロギングを有効にする前に、OpenTelemetry コレクターおよび OpenTelemetry 対応のログ収集システムをデプロイし設定する必要があります。本ガイドでは、[OpenTelemetry コレクター](https://opentelemetry.io/docs/collector/getting-started)のデプロイと、デバッグエクスポーターを使用してログを `stdout` にリダイレクトする設定方法を説明します。

1. `otel-logs-collector-config.yaml` という名前で OpenTelemetry コレクターの設定ファイルを作成します。

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

2. 同じディレクトリに Docker Compose ファイル `docker-compose-otel-logs.yaml` を作成します。

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
         - "13133:13133" # ヘルスチェック拡張
         - "4317:4317"   # OTLP gRPC レシーバー
   ```

3. Docker Compose を使ってコレクターを起動します。

   ```bash
   docker compose -f docker-compose-otel-logs.yaml up
   ```

4. 起動後、OpenTelemetry コレクターは [http://localhost:4317](http://localhost:4317/) でアクセス可能になります。


## EMQX で OpenTelemetry ログハンドラーを有効化

1. EMQX がローカルで動作していることを前提に、`cluster.hocon` ファイルに以下の設定を追加します。

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

   また、ダッシュボードの **Management** -> **Monitoring** に移動し、ページ内の **Integration** タブから OpenTelemetry ログ統合を設定することも可能です。

   ::: tip 補足

   `opentelemetry.logs.level` の設定は、[EMQX ログハンドラー](../../observability/log.md)で設定されているデフォルトのログレベルにより上書きされます。例えば、OpenTelemetry のログレベルが `info` でも、EMQX のコンソールログレベルが `error` に設定されている場合は、`error` レベル以上のイベントのみがエクスポートされます。

   :::

2. EMQX ノードを起動します。

3. ダッシュボードからアクセスできない HTTP サービスへのブリッジを作成するなどして、EMQX のログイベントを発生させます。

   <img src="./assets/otel-logs-bridge-example-en.png" alt="Otel-logs-HTTP-bridge-example" style="zoom:67%;" />

4. しばらくすると（デフォルトで約1秒）、Otel コレクターに HTTP ブリッジ接続失敗を示す EMQX ログイベントが表示されます。

   ![Otel-collector-logs-debug-output](./assets/otel-collector-logs-debug-output.png)

## ログ過負荷の管理

EMQX はログイベントを蓄積し、定期的にバッチでエクスポートします。
このエクスポート頻度は `opentelemetry.logs.scheduled_delay` パラメーターで制御され、デフォルトは1秒です。
バッチログハンドラーには過負荷保護機構が組み込まれており、蓄積可能なイベント数の上限が設定されています。デフォルトは2048です。以下の設定でこの上限を変更できます。

```bash
opentelemetry {
  logs { max_queue_size = 2048 }
}
```

`max_queue_size` の上限に達すると、新しいログイベントは現在のキューがエクスポートされるまで破棄されます。

::: tip 補足

OpenTelemetry ログの過負荷保護は、デフォルトの [EMQX ログハンドラー](../log.md) の過負荷保護とは独立して動作します。
そのため、設定によっては同じログイベントが OpenTelemetry ハンドラーで破棄される一方、デフォルトの EMQX ログハンドラーでは記録される場合や、その逆もあり得ます。

:::
