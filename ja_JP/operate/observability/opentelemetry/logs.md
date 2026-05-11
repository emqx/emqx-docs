# OpenTelemetryを使ったログ管理の統合
ファイルログと同様に、OpenTelemetryログは重要なイベント、ステータス情報、エラーメッセージを記録し、開発者や運用チームがアプリケーションの動作を理解しトラブルシューティングを行うのに役立ちます。ただし、OpenTelemetryログは標準化されたログフォーマットを採用しているため、ログの解析や分析、処理が容易です。さらに、Trace ID、タグ、属性などの豊富なコンテキスト情報をログに付加できる点も特徴です。

本ページでは、EMQXとOpenTelemetryログハンドラーを統合して高度なログ管理を実現する方法を詳しく解説します。OpenTelemetry Collectorのセットアップ、EMQXでのOpenTelemetryログハンドラーの設定およびログのエクスポート方法、ログ過負荷の管理について説明します。この統合により、EMQXのログイベントを[OpenTelemetryログデータモデル](https://opentelemetry.io/docs/specs/otel/logs/data-model/)に準拠した形式で出力し、設定したOpenTelemetry Collectorやバックエンドにエクスポートできるため、監視やデバッグの効率が向上します。

## OpenTelemetry Collectorのセットアップ

EMQXのOpenTelemetryログを有効にする前に、OpenTelemetry CollectorおよびOpenTelemetry対応のログ収集システムをデプロイし設定する必要があります。本ガイドでは、[OpenTelemetry Collector](https://opentelemetry.io/docs/collector/getting-started)のデプロイ方法と、debugエクスポーターを使ってログを`stdout`にリダイレクトする設定手順を説明します。

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

1. EMQXの`cluster.hocon`ファイルに以下の設定を追加します（EMQXがローカルで動作している想定です）。

   ```bash
   opentelemetry {
     exporter {endpoint = "http://localhost:4317"}
     logs {enable = true, level = warning}
   }
   ```

   または、ダッシュボードの **Management** -> **Monitoring** にある **Integration** タブからOpenTelemetryログ統合を設定することも可能です。

   ::: tip 補足

   `opentelemetry.logs.level`の設定は、[EMQXログハンドラー](../../observability/log.md)で設定されたデフォルトのログレベルにより上書きされます。例えば、OpenTelemetryのログレベルが`info`でも、EMQXのコンソールログレベルが`error`の場合は、`error`以上のレベルのイベントのみがエクスポートされます。

   :::

2. EMQXノードを起動します。

3. ダッシュボードからアクセスできないHTTPサービスへのブリッジ作成など、EMQXのログイベントを発生させます。

   <img src="./assets/otel-logs-bridge-example-en.png" alt="Otel-logs-HTTP-bridge-example" style="zoom:67%;" />

4. 数秒以内（デフォルトは約1秒）に、Otel CollectorがHTTPブリッジ接続失敗などのEMQXログイベントを受信していることを確認できます。

   ![Otel-collector-logs-debug-output](./assets/otel-collector-logs-debug-output.png)

## ログ過負荷の管理

EMQXはログイベントを蓄積し、一定間隔でバッチ処理としてエクスポートします。  
このエクスポートの頻度は`opentelemetry.logs.scheduled_delay`パラメータで制御され、デフォルトは1秒です。  
バッチ処理のログハンドラーは過負荷保護機構を備えており、蓄積できるイベント数に上限を設けています。デフォルトの上限は2048件です。以下の設定でこの上限を変更できます。

```bash
opentelemetry {
  logs { max_queue_size = 2048 }
}
```

`max_queue_size`の上限に達すると、新しいログイベントは現在のキューがエクスポートされるまで破棄されます。

::: tip 補足

OpenTelemetryログの過負荷保護は、デフォルトの[EMQXログハンドラー](../log.md)の過負荷保護とは独立して動作します。  
そのため、設定によっては同じログイベントがOpenTelemetryハンドラーで破棄される一方、EMQXのデフォルトログハンドラーでは記録される場合や、その逆もあり得ます。

:::
