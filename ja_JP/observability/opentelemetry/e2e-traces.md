# OpenTelemetryベースのエンドツーエンドMQTTトレーシング

現代の分散システムにおいて、リクエストの流れを追跡しパフォーマンスを分析することは、信頼性と可観測性を確保するために不可欠です。エンドツーエンドトレーシングは、リクエストの開始から終了までの全経路をキャプチャすることを目的とした概念であり、システムの挙動やパフォーマンスに関する深い洞察を得ることができます。

EMQXはバージョン5.8.3以降、MQTTプロトコルに特化したOpenTelemetryベースのエンドツーエンドトレーシング機能を統合しています。この機能により、特にマルチノードクラスター環境において、メッセージのパブリッシュ、ルーティング、配信の流れを明確にトレースできます。これにより、システムパフォーマンスの最適化だけでなく、迅速な障害箇所の特定やシステム信頼性の向上に寄与します。

本ページでは、MQTTメッセージのフローを包括的に可視化するために、EMQXでエンドツーエンドトレーシング機能を有効化する手順を詳しく解説します。

## OpenTelemetryコレクターのセットアップ

設定の詳細については、[OpenTelemetryコレクターのセットアップ](./traces.md#setting-up-opentelemetry-collector)を参照してください。

## EMQXでのエンドツーエンドトレーシングの有効化

::: tip

エンドツーエンドトレーシングはシステムパフォーマンスに影響を与える可能性があるため、必要な場合のみ有効にしてください。

:::

本節では、EMQXでOpenTelemetryベースのエンドツーエンドトレーシングを有効にする方法を案内し、マルチノード環境におけるMQTT分散トレーシング機能を紹介します。

### ダッシュボードからエンドツーエンドトレーシングを設定する

1. ダッシュボードの左メニューから **Management** -> **Monitoring** をクリックします。

2. Monitoringページで **Integration** タブを選択します。

3. 以下の設定を行います：
   - **Monitoring platform**：`OpenTelemetry` を選択します。

   - **Feature Selection**：`Traces` を選択します。

   - **Endpoint**：トレースデータのエクスポート先アドレスを設定します。デフォルトは `http://localhost:4317` です。

   - **Headers**：トレースエクスポートリクエストにカスタムHTTPヘッダーを追加します。OpenTelemetryコレクターが認証やAPIキー、トークンなどのカスタムヘッダーを要求する場合に有効です。各ヘッダーはキーと値のペアで指定します。

     OpenTelemetryコレクターがBasic認証を使用する場合は、`authorization` ヘッダーを以下の形式で追加してください：

     ```
     Key: authorization
     Value: Basic dXNlcjpwYXNzd29yZA==
     ```

     これにより、HTTPベースの認証を要求するコレクターとの互換性が向上します。

   - **Enable TLS**：必要に応じてTLS暗号化を有効にします。通常は本番環境のセキュリティ要件に応じて設定します。

   - **Trace Mode**：`End-to-End` を選択してエンドツーエンドトレーシング機能を有効化します。

   - **Cluster Identifier**：span属性にクラスタ識別用のプロパティ値を追加します。プロパティキーは `cluster.id` です。通常はシンプルで識別しやすい名前やクラスター名を設定し、複数のEMQXクラスターを区別します。デフォルトは `emqxcl` です。

   - **Traces Export Interval**：トレースデータのエクスポート間隔を秒単位で設定します。デフォルトは `5` 秒です。

   - **Max Queue Size**：トレースデータのキュー最大サイズを設定します。デフォルトは `2048` エントリです。

4. 必要に応じて **Trace Advanced Configuration** をクリックし、詳細設定を行います。

   - **Trace Configuration**：クライアント接続やメッセージ送受信、ルールエンジンの実行など、特定イベントのトレースオプションを設定します。
     - **Follow Traceparent**：`traceparent` を追従するかどうかを設定します。`true` に設定すると、EMQXはクライアントが送信する `User-Property` から `traceparent` 識別子を取得し、それに紐づけてエンドツーエンドトレーシングを行います。`false` の場合は新規トレースを生成します。デフォルトは `true` です。
   - **Client ID White List**：トレース対象とするクライアントの接続やメッセージを制限するホワイトリストを設定します。不要なトレースを避け、システムリソースの消費を抑制できます。
   - **Topic White List**：トレース対象のトピックを制限するホワイトリストを設定します。クライアントホワイトリストと同様にトレース範囲を制御できます。

   設定を保存後、**Confirm** をクリックしてウィンドウを閉じます。

5. 最後に **Save Changes** をクリックして設定を保存します。

<img src="./assets/e2e-dashboard-conf-en.png" alt="Otel-E2E-Trace-dashboard-page" style="zoom:67%;" />

### 設定ファイルからエンドツーエンドトレーシングを設定する

EMQXがローカルで稼働している前提で、`cluster.hocon` ファイルに以下の設定を追加します。

設定オプションの詳細は、[EMQXダッシュボードのモニタリング統合](http://localhost:18083/#/monitoring/integration)のOpenTelemetry節を参照してください。

```bash
opentelemetry {
  exporter {
    endpoint = "http://localhost:4317"
    headers {
      authorization = ""Basic dXNlcjpwYXNzd29yZA=="
    }
  }
  traces {
    enable = true
    # エンドツーエンドトレーシングモード
    trace_mode = e2e
    # エンドツーエンドトレーシングオプション
    e2e_tracing_options {
      ## クライアント接続/切断イベントのトレース
      client_connect_disconnect = true
      ## クライアントメッセージングイベントのトレース
      client_messaging = true
      ## クライアントのサブスクライブ/アンマウントイベントのトレース
      client_subscribe_unsubscribe = true
      ## クライアントIDホワイトリストの最大長
      clientid_match_rules_max = 30
      ## トピックフィルタホワイトリストの最大長
      topic_match_rules_max = 30
      ## クラスター識別子
      cluster_identifier = emqxcl
      ## メッセージトレースレベル（QoS）
      msg_trace_level = 2
      ## ホワイトリスト外イベントのサンプリング率
      ## 注意：トレーシングが有効な場合のみ適用
      sample_ratio = "100%"
      ## traceparentの追従
      ## クライアントから渡された `traceparent` をエンドツーエンドトレーシングが追従するかどうか
      follow_traceparent
    }
  }
  max_queue_size = 50000
  scheduled_delay = 1000
}
```

## EMQXでのエンドツーエンドトレーシングのデモ

1. EMQXノードを起動します。例として、ノード名 `emqx@172.19.0.2` と `emqx@172.19.0.3` の2ノードクラスターを起動し、分散トレーシング機能をデモします。

2. MQTTX CLIをクライアントとして使用し、異なるノードで同じトピックをサブスクライブします。

   - `emqx@172.19.0.2` ノードでサブスクライブ：

     ```bash
     mqttx sub -t t/1 -h 172.19.0.2 -p 1883
     ```

   - `emqx@172.19.0.3` ノードでサブスクライブ：

     ```bash
     mqttx sub -t t/1 -h 172.19.0.3 -p 1883
     ```

3. 約5秒後（EMQXのトレースデータエクスポートのデフォルト間隔）、[http://localhost:16686](http://localhost:16686/) のJaeger WEB UIにアクセスしてトレースデータを確認します。

   `emqx` サービスを選択し、**Find Traces** をクリックします。`emqx` サービスがすぐに表示されない場合は、少し待ってページを更新してください。クライアントの接続およびサブスクライブイベントのトレースが表示されます：

   ![Jaeger-WEB-UI-e2e-Client-Events](./assets/e2e-client-events.png)

4. メッセージをパブリッシュします：

   ```bash
   mqttx pub -t t/1 -h 172.19.0.2 -p 1883
   ```

5. 少し待つと、Jaeger WEB UIでMQTTメッセージの詳細なトレースが確認できます。

   トレースをクリックすると、詳細なspan情報やトレースタイムラインが表示されます。サブスクライバー数、クロスノードメッセージルーティング、QoSレベル、`msg_trace_level` 設定により、MQTTメッセージトレースに含まれるspan数は異なります。

   以下は、2クライアントがQoS 2でサブスクライブし、パブリッシャーがQoS 2メッセージを送信、`msg_trace_level` が2に設定されている場合のトレースタイムラインとspan情報の例です。

   特に、クライアント `mqttx_9137a6bb` はパブリッシャーとは異なるEMQXノードに接続されているため、クロスノード伝送を表す2つの追加span（`message.forward` と `message.handle_forward`）が存在します。

   ![Jaeger-WEB-UI-e2e-Message](./assets/e2e-message.png)

   また、メッセージやイベントがルールエンジンの実行をトリガーする場合、ルールエンジントラッキングオプションを有効にすると、ルールおよびアクション実行のトラッキング情報も取得できます。

   ![Jaeger-WEB-UI-e2e-With-Rule-Engine](./assets/e2e-with-rule-engine.png)

   ::: tip

   ルールエンジン実行を含むエンドツーエンドトレーシング機能は、EMQXバージョン5.9.0以降でサポートされています。

   :::

   ::: warning 重要なお知らせ

   この機能は慎重に有効化してください。メッセージやイベントが複数のルールやアクションをトリガーすると、1つのトレースで大量のspanが生成され、システム負荷が増大します。
   メッセージ量やルール・アクション数に基づき、適切なサンプリング率を見積もってください。

   :::

## トレースspanの過負荷管理

EMQXはトレースspanを蓄積し、定期的にバッチでエクスポートします。エクスポート間隔は `opentelemetry.trace.scheduled_delay` パラメータで制御され、デフォルトは5秒です。バッチトレースspanプロセッサには過負荷保護機能があり、デフォルトで2048spanまで蓄積可能です。この制限は以下の設定で調整できます：

```yaml
opentelemetry {
  traces {
    max_queue_size = 50000
    scheduled_delay = 1000
  }
}
```

`max_queue_size` の制限に達すると、新規のトレースspanは現在のキューがエクスポートされるまで破棄されます。

::: tip 補足

トレース対象メッセージが多数のサブスクライバーに配信される場合や、メッセージ量が多くサンプリング率が高すぎる場合、過負荷保護により多くのspanが破棄され、エクスポートされるspanはごく一部になることがあります。

エンドツーエンドトレーシングモードでは、メッセージ量やサンプリング率に応じて `max_queue_size` を増やし、`scheduled_delay` を短縮してspanのエクスポート頻度を上げることを検討してください。これにより過負荷保護によるspanの損失を防げます。

**ただし、エクスポート頻度の増加やキューサイズの拡大はシステムリソース消費を増加させるため、メッセージTPSや利用可能なシステムリソースを十分に見積もった上で適切な設定を行ってください。**

:::
