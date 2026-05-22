# OpenTelemetryを統合したログ管理

<<<<<<< HEAD
ファイルログと同様に、OpenTelemetryログは重要なイベント、ステータス情報、エラーメッセージを記録し、開発者や運用チームがアプリケーションの動作を理解しトラブルシューティングを行うのに役立ちます。ただし、OpenTelemetryログは標準化されたログフォーマットを採用しているため、ログの解析、分析、処理が容易になります。さらに、OpenTelemetryログはTrace ID、タグ、属性などの豊富なコンテキスト情報をレコードに追加することをサポートしています。

本ページでは、EMQXとOpenTelemetryログハンドラーを統合して高度なログ管理を実現するための包括的なガイドを提供します。OpenTelemetry Collectorのセットアップ、EMQXでのOpenTelemetryログハンドラーの設定およびログのエクスポート、ログ過負荷の管理方法について説明します。この統合により、EMQXのログイベントを[OpenTelemetryログデータモデル](https://opentelemetry.io/docs/specs/otel/logs/data-model/)に準拠した形式でフォーマットし、設定済みのOpenTelemetry Collectorやバックエンドシステムにエクスポートできるため、監視やデバッグ機能が向上します。

## OpenTelemetry Collectorのセットアップ

EMQXのOpenTelemetryログを有効化する前に、OpenTelemetry CollectorおよびOpenTelemetry対応のログ収集システムをデプロイし設定する必要があります。本ガイドでは、[OpenTelemetry Collector](https://opentelemetry.io/docs/collector/getting-started)のデプロイと、デバッグエクスポーターを使用してログを`stdout`にリダイレクトする設定方法を説明します。
=======
ファイルログと同様に、OpenTelemetryログは重要なイベント、ステータス情報、エラーメッセージを記録し、開発者や運用チームがアプリケーションの挙動を理解しトラブルシューティングを行うのに役立ちます。ただし、OpenTelemetryログは標準化されたログフォーマットを採用しているため、ログの解析、分析、処理が容易です。さらに、OpenTelemetryログはTrace ID、タグ、属性などの豊富なコンテキスト情報をレコードに追加することをサポートしています。

本ページでは、EMQXにOpenTelemetryログハンドラーを統合して高度なログ管理を実現するための包括的なガイドを提供します。OpenTelemetry Collectorのセットアップ、EMQXでのOpenTelemetryログハンドラーの設定およびログのエクスポート、ログ過負荷の管理方法について説明します。この統合により、EMQXのログイベントを[OpenTelemetryログデータモデル](https://opentelemetry.io/docs/specs/otel/logs/data-model/)に準拠した形式でフォーマットし、設定済みのOpenTelemetry Collectorやバックエンドシステムにエクスポートできるようになり、監視やデバッグ機能が向上します。

## OpenTelemetry Collectorのセットアップ

EMQXでOpenTelemetryログを有効にする前に、OpenTelemetry CollectorおよびOpenTelemetry対応のログ収集システムをデプロイし設定する必要があります。本ガイドでは、[OpenTelemetry Collector](https://opentelemetry.io/docs/collector/getting-started)のデプロイ方法と、debugエクスポーターを使ってログを`stdout`にリダイレクトする設定方法を説明します。
>>>>>>> origin/release-6.1

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

2. 同じディレクトリに、`docker-compose-otel-logs.yaml`というDocker Composeファイルを作成します。

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

<<<<<<< HEAD
   または、ダッシュボードの **Management** -> **Monitoring** に移動し、ページ内の **Integration** タブからOpenTelemetryログ統合の設定を行うことも可能です。
=======
   また、ダッシュボードの **Management** -> **Monitoring** にアクセスし、ページ内の **Integration** タブからOpenTelemetryログ統合の設定も可能です。
>>>>>>> origin/release-6.1

   ::: tip 注意

<<<<<<< HEAD
   `opentelemetry.logs.level` の設定は、[EMQXログハンドラー](../../observability/log.md)で設定されたデフォルトのログレベルによって上書きされます。たとえば、OpenTelemetryのログレベルが`info`でも、EMQXのコンソールログレベルが`error`の場合、`error`以上のレベルのイベントのみがエクスポートされます。
=======
   `opentelemetry.logs.level`の設定は、[EMQXログハンドラー](../../observability/log.md)で設定されたデフォルトのログレベルによって上書きされます。例えば、OpenTelemetryのログレベルが`info`であっても、EMQXのコンソールログレベルが`error`の場合、`error`以上のレベルのイベントのみがエクスポートされます。
>>>>>>> origin/release-6.1

   :::

2. EMQXノードを起動します。

<<<<<<< HEAD
3. ダッシュボードからアクセス不能なHTTPサービスへのブリッジを作成するなどして、EMQXのログイベントを発生させます。

   <img src="./assets/otel-logs-bridge-example-en.png" alt="Otel-logs-HTTP-bridge-example" style="zoom:67%;" />

4. しばらくすると（デフォルトで約1秒後）、Otel CollectorにHTTPブリッジ接続失敗を示すEMQXログイベントが表示されます。
=======
3. ダッシュボードからアクセスできないHTTPサービスへのブリッジを作成するなどして、EMQXのログイベントを発生させます。

   <img src="./assets/otel-logs-bridge-example-en.png" alt="Otel-logs-HTTP-bridge-example" style="zoom:67%;" />

4. しばらくすると（デフォルトで約1秒後）、Otel CollectorにHTTPブリッジ接続失敗を示すようなEMQXログイベントが表示されます。
>>>>>>> origin/release-6.1

   ![Otel-collector-logs-debug-output](./assets/otel-collector-logs-debug-output.png)

## ログの過負荷管理

<<<<<<< HEAD
EMQXはログイベントを蓄積し、定期的にバッチでエクスポートします。このエクスポートの頻度は`opentelemetry.logs.scheduled_delay`パラメータで制御され、デフォルトは1秒です。バッチログハンドラーには過負荷保護機構が組み込まれており、蓄積可能なイベント数の上限が設定されています。デフォルトは2048です。以下の設定でこの上限を変更可能です。
=======
EMQXはログイベントを蓄積し、定期的にバッチでエクスポートします。
このエクスポート頻度は`opentelemetry.logs.scheduled_delay`パラメータで制御され、デフォルトは1秒です。
バッチログハンドラーには過負荷保護機構が組み込まれており、蓄積できるイベント数の上限が設定されています。デフォルトは2048です。この上限は以下の設定で変更可能です。
>>>>>>> origin/release-6.1

```bash
opentelemetry {
  logs { max_queue_size = 2048 }
}
```

`max_queue_size`の上限に達すると、現在のキューがエクスポートされるまで新しいログイベントは破棄されます。

::: tip 注意

<<<<<<< HEAD
OpenTelemetryログの過負荷保護は、デフォルトの[EMQXログハンドラー](../log.md)の過負荷保護とは独立して動作します。そのため、設定によっては同じログイベントがOpenTelemetryハンドラーで破棄される一方、デフォルトのEMQXログハンドラーでは記録される場合や、その逆もあり得ます。
=======
OpenTelemetryログの過負荷保護は、デフォルトの[EMQXログハンドラー](../log.md)の過負荷保護とは独立して動作します。
そのため、設定によっては同じログイベントがOpenTelemetryハンドラーで破棄される一方、デフォルトのEMQXログハンドラーでは記録される場合や、その逆もあり得ます。
>>>>>>> origin/release-6.1

:::
