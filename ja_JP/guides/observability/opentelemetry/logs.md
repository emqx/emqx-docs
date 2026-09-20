# OpenTelemetryを統合したログ管理

ファイルログと同様に、OpenTelemetryログは重要なイベント、ステータス情報、およびエラーメッセージを記録し、開発者や運用チームがアプリケーションの動作を理解しトラブルシューティングを行うのに役立ちます。ただし、OpenTelemetryログは標準化されたログフォーマットを採用しているため、ログの解析や分析、処理が容易です。さらに、OpenTelemetryログはTrace ID、タグ、属性などの豊富なコンテキスト情報をレコードに追加することをサポートしています。

本ページでは、EMQXとOpenTelemetryログハンドラーを統合して高度なログ管理を実現するための包括的なガイドを提供します。OpenTelemetry Collectorのセットアップ、EMQXでのOpenTelemetryログハンドラーの設定とログのエクスポート、ログの過負荷管理について説明します。この統合により、EMQXのログイベントを[OpenTelemetryログデータモデル](https://opentelemetry.io/docs/specs/otel/logs/data-model/)に沿ってフォーマットし、設定済みのOpenTelemetry Collectorまたはバックエンドシステムにエクスポートでき、監視とデバッグ能力が向上します。

OpenTelemetryログを直接Dynatraceにエクスポートする方法については、[OpenTelemetryとDynatraceの統合](./dynatrace.md)をご覧ください。

## OpenTelemetry Collectorのセットアップ

EMQXのOpenTelemetryログを有効にする前に、OpenTelemetry CollectorおよびOpenTelemetry対応のログ収集システムをデプロイし設定する必要があります。本ガイドでは、[OpenTelemetry Collector](https://opentelemetry.io/docs/collector/getting-started)のデプロイと、デバッグエクスポーターを使用してログを`stdout`にリダイレクトする設定方法を説明します。

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

1. EMQXがローカルで動作していることを前提に、`cluster.hocon`ファイルに以下の設定を追加します。

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

   また、ダッシュボードの **Management** -> **Monitoring** にある **Integration** タブからOpenTelemetryログ統合の設定も可能です。

   ::: tip 補足

   `opentelemetry.logs.level`の設定は、[EMQXログハンドラー](../log.md)で設定されているデフォルトのログレベルにより上書きされます。例えば、OpenTelemetryのログレベルが`info`でも、EMQXのコンソールログレベルが`error`の場合、`error`以上のレベルのイベントのみがエクスポートされます。

   :::

2. EMQXノードを起動します。

3. ダッシュボードからアクセス不能なHTTPサービスへのブリッジ作成など、EMQXのログイベントを発生させます。

   <img src="./assets/otel-logs-bridge-example-en.png" alt="Otel-logs-HTTP-bridge-example" style="zoom:67%;" />

4. 数秒後（デフォルトは約1秒）、Otel CollectorにHTTPブリッジ接続失敗を示すEMQXログイベントが表示されるはずです。

   ![Otel-collector-logs-debug-output](./assets/otel-collector-logs-debug-output.png)

## ログ過負荷の管理

EMQXはログイベントを蓄積し、定期的にバッチでエクスポートします。  
このエクスポート頻度は`opentelemetry.logs.scheduled_delay`パラメータで制御され、デフォルトは1秒です。  
バッチングログハンドラーには過負荷保護機能があり、蓄積可能なイベント数の上限（デフォルト2048）を超えると新規ログイベントを破棄します。  
この上限は以下の設定で変更可能です。

```bash
opentelemetry {
  logs { max_queue_size = 2048 }
}
```

`max_queue_size`の上限に達すると、現在のキューがエクスポートされるまで新しいログイベントは破棄されます。

::: tip 補足

OpenTelemetryログの過負荷保護は、デフォルトの[EMQXログハンドラー](../log.md)の過負荷保護とは独立して動作します。  
そのため、設定によっては同じログイベントがOpenTelemetryハンドラーで破棄される一方、EMQXのデフォルトログハンドラーでは記録される、またはその逆のケースもあります。

:::
