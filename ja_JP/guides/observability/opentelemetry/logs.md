# OpenTelemetryを統合したログ管理

ファイルログと同様に、OpenTelemetryログは重要なイベント、状態情報、およびエラーメッセージを記録し、開発者や運用チームがアプリケーションの挙動を理解し、トラブルシューティングを行うのに役立ちます。ただし、OpenTelemetryログは標準化されたログフォーマットを採用しているため、ログの解析や分析、処理が容易です。さらに、OpenTelemetryログはTrace ID、タグ、属性などの豊富なコンテキスト情報をレコードに追加することが可能です。

本ページでは、EMQXとOpenTelemetryログハンドラーを統合して高度なログ管理を実現するための包括的なガイドを提供します。OpenTelemetry Collectorのセットアップ、EMQXでのOpenTelemetryログハンドラーの設定およびログのエクスポート方法、ログの過負荷管理について解説します。この統合により、EMQXのログイベントを[OpenTelemetryログデータモデル](https://opentelemetry.io/docs/specs/otel/logs/data-model/)に準拠した形式でフォーマットし、設定されたOpenTelemetry Collectorやバックエンドシステムにエクスポートできるため、監視やデバッグ機能が向上します。

OpenTelemetryログを直接Dynatraceにエクスポートする方法については、[OpenTelemetryとDynatraceの統合](./dynatrace.md)をご参照ください。

## OpenTelemetry Collectorのセットアップ

EMQXのOpenTelemetryログ機能を有効にする前に、OpenTelemetry CollectorおよびOpenTelemetry対応のログ収集システムをデプロイし、設定する必要があります。本ガイドでは、[OpenTelemetry Collector](https://opentelemetry.io/docs/collector/getting-started)のデプロイと、デバッグエクスポーターを使用してログを`stdout`にリダイレクトする設定方法を説明します。

1. `otel-logs-collector-config.yaml`という名前でOpenTelemetry Collectorの設定ファイルを作成します。

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

2. 同じディレクトリにDocker Composeファイル`docker-compose-otel-logs.yaml`を作成します。

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
         - "4317:4317"   # OTLP gRPCレシーバー
   ```

3. Docker Composeを使ってCollectorを起動します。

   ```bash
   docker compose -f docker-compose-otel-logs.yaml up
   ```

4. 起動後、OpenTelemetry Collectorは[http://localhost:4317](http://localhost:4317/)でアクセス可能になります。


## EMQXでOpenTelemetryログハンドラーを有効化

`opentelemetry.exporter.endpoint`は1つのURLを受け付けます。URLは`http`または`https`スキームを使用し、明示的なポート番号を含む必要があります。例えば、`http://localhost:4317`は有効ですが、`localhost:4317`や`http://localhost`は無効です。

1. EMQXがローカルで動作している場合、`cluster.hocon`ファイルに以下の設定を追加します。

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

   また、ダッシュボードの **Management** -> **Monitoring** に移動し、ページの **Integration** タブからOpenTelemetryログ統合を設定することも可能です。

   ::: tip 注意事項

   `opentelemetry.logs.level`の設定は、[EMQXログハンドラー](../log.md)で設定されたデフォルトのログレベルによって上書きされます。例えば、OpenTelemetryのログレベルが`info`でも、EMQXのコンソールログレベルが`error`の場合、`error`以上のレベルのイベントのみがエクスポートされます。

   :::

2. EMQXノードを起動します。

3. ダッシュボードからアクセスできないHTTPサービスへのブリッジを作成するなどして、EMQXログイベントを発生させます。

   <img src="./assets/otel-logs-bridge-example-en.png" alt="Otel-logs-HTTP-bridge-example" style="zoom:67%;" />

4. しばらくすると（デフォルトで約1秒）、Otel CollectorにHTTPブリッジ接続失敗などのEMQXログイベントが表示されます。

   ![Otel-collector-logs-debug-output](./assets/otel-collector-logs-debug-output.png)

## ログの過負荷管理

EMQXはログイベントを蓄積し、定期的にバッチでエクスポートします。エクスポートの頻度は`opentelemetry.logs.scheduled_delay`パラメータで制御され、デフォルトは1秒です。バッチ処理のログハンドラーには過負荷保護機構が組み込まれており、蓄積可能なイベント数の上限が設定されています。デフォルトは2048件です。この上限は以下の設定で変更可能です。

```bash
opentelemetry {
  logs { max_queue_size = 2048 }
}
```

`max_queue_size`の上限に達すると、現在のキューがエクスポートされるまで新しいログイベントは破棄されます。

::: tip 注意事項

OpenTelemetryログの過負荷保護は、デフォルトの[EMQXログハンドラー](../log.md)の過負荷保護とは独立して動作します。そのため、設定によっては同じログイベントがOpenTelemetryハンドラーで破棄される一方、デフォルトのEMQXログハンドラーでは記録される、またはその逆のケースが発生する可能性があります。

:::
