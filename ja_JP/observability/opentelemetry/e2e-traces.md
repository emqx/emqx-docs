# OpenTelemetryベースのエンドツーエンドMQTTトレーシング

現代の分散システムにおいて、リクエストの流れを追跡しパフォーマンスを分析することは、信頼性と可観測性を確保するために不可欠です。エンドツーエンドトレーシングは、リクエストの開始から終了までの全経路を捕捉することを目的とした概念であり、システムの挙動やパフォーマンスに関する深い洞察を得ることができます。

EMQXはバージョン5.8.3以降、MQTTプロトコルに特化したOpenTelemetryベースのエンドツーエンドトレーシング機能を統合しています。この機能により、特にマルチノードクラスター環境において、メッセージのパブリッシュ、ルーティング、配信の過程を明確にトレースできます。これによりシステムパフォーマンスの最適化だけでなく、迅速な障害箇所の特定やシステム信頼性の向上に役立ちます。

本ページでは、MQTTメッセージのフローを包括的に可視化するために、EMQXでエンドツーエンドトレーシング機能を有効化する方法を詳細に解説します。

## OpenTelemetry Collectorのセットアップ

設定の詳細については、[OpenTelemetry Collectorのセットアップ](./traces.md#setting-up-opentelemetry-collector)を参照してください。

## EMQXでのエンドツーエンドトレーシングの有効化

::: tip

エンドツーエンドトレーシングはシステムパフォーマンスに影響を与える可能性があるため、必要な場合のみ有効にしてください。

:::

このセクションでは、EMQXでOpenTelemetryベースのエンドツーエンドトレーシングを有効化する手順を案内し、マルチノード環境におけるMQTT分散トレーシング機能を紹介します。

### ダッシュボードからのエンドツーエンドトレーシング設定

1. ダッシュボードの左メニューから **Management** -> **Monitoring** をクリックします。

2. Monitoringページの **Integration** タブを選択します。

3. 以下の設定を行います：
   - **Monitoring platform**：`OpenTelemetry` を選択します。

   - **Feature Selection**：`Traces` を選択します。

   - **Endpoint**：トレースデータのエクスポート先アドレスを設定します。デフォルトは `http://localhost:4317` です。

   - **Headers**：トレースエクスポートリクエストにカスタムHTTPヘッダーを追加します。OpenTelemetry Collectorが認証やAPIキー、トークンなどのカスタムヘッダーを必要とする場合に使用します。各ヘッダーはキーと値のペアで指定してください。

     OpenTelemetry CollectorがBasic認証を使用する場合は、`authorization` ヘッダーに以下の形式で値を設定する必要があります：

     ```
     Key: authorization
     Value: Basic dXNlcjpwYXNzd29yZA==
     ```

     この設定により、HTTPベースの認証を強制するCollectorとの互換性が向上します。

   - **Enable TLS**：必要に応じてTLS暗号化を有効にします。主に本番環境のセキュリティ要件に対応します。

   - **Trace Mode**：`End-to-End` を選択し、エンドツーエンドトレーシング機能を有効にします。

   - **Cluster Identifier**：スパン属性に追加するプロパティ値で、どのEMQXクラスターからのデータかを識別するのに役立ちます。プロパティキーは `cluster.id` です。通常は単純で識別しやすい名前やクラスター名を設定して、EMQXクラスター間の区別に利用します。デフォルトは `emqxcl` です。

   - **Traces Export Interval**：トレースデータのエクスポート間隔を秒単位で設定します。デフォルトは `5` 秒です。

   - **Max Queue Size**：トレースデータキューの最大サイズを設定します。デフォルトは `2048` エントリです。

4. 必要に応じて **Trace Advanced Configuration** をクリックし、高度な設定を行います。

   - **Trace Configuration**：クライアント接続やメッセージ送受信、ルールエンジンの実行など、特定のイベントをトレースするかどうかの追加オプションを設定します。
     - **Follow Traceparent**：`traceparent` を追従するかどうかを設定します。`true` に設定すると、EMQXはクライアントから送信された `User-Property` 内の `traceparent` 識別子を取得し、エンドツーエンドトレーシングに関連付けます。`false` の場合は新しいトレースを生成します。デフォルトは `true` です。
   - **Client ID White List**：トレース対象とするクライアント接続やメッセージを制限するホワイトリストを設定します。不要なトレースを避け、システムリソースの消費を抑制できます。
   - **Topic White List**：トレース対象とするトピックのホワイトリストを設定します。クライアントホワイトリストと同様にトレーシングの範囲を制御します。

   設定を保存後、**Confirm** をクリックしてウィンドウを閉じます。

5. 最後に **Save Changes** をクリックして設定を保存します。

<img src="./assets/e2e-dashboard-conf-en.png" alt="Otel-E2E-Trace-dashboard-page" style="zoom:67%;" />

### 設定ファイルによるエンドツーエンドトレーシング設定

EMQXの `cluster.hocon` ファイルに以下の設定を追加します（EMQXがローカルで稼働している場合の例です）。

設定オプションの詳細は、[EMQXダッシュボード監視統合](http://localhost:18083/#/monitoring/integration) のOpenTelemetryセクションを参照してください。

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
      ## クライアント接続/切断イベントをトレース
      client_connect_disconnect = true
      ## クライアントメッセージングイベントをトレース
      client_messaging = true
      ## クライアントのサブスクライブ/アン・サブスクライブイベントをトレース
      client_subscribe_unsubscribe = true
      ## クライアントIDホワイトリストの最大長
      clientid_match_rules_max = 30
      ## トピックフィルターホワイトリストの最大長
      topic_match_rules_max = 30
      ## クラスター識別子
      cluster_identifier = emqxcl
      ## メッセージトレースレベル（QoS）
      msg_trace_level = 2
      ## ホワイトリスト外イベントのサンプリング率
      ## 注意：トレースが有効な場合のみサンプリングが適用されます
      sample_ratio = "100%"
      ## traceparentの追従
      ## クライアントから渡された`traceparent`をエンドツーエンドトレーシングが追従するかどうか
      follow_traceparent
    }
  }
  max_queue_size = 50000
  scheduled_delay = 1000
}
```

## EMQXでのエンドツーエンドトレーシングのデモ

1. EMQXノードを起動します。例えば、`emqx@172.19.0.2` と `emqx@172.19.0.3` の2ノードクラスターを起動し、分散トレーシング機能を実演します。

2. MQTTX CLIをクライアントとして使用し、異なるノードで同じトピックをサブスクライブします。

   - `emqx@172.19.0.2` ノードでサブスクライブ：

     ```bash
     mqttx sub -t t/1 -h 172.19.0.2 -p 1883
     ```

   - `emqx@172.19.0.3` ノードでサブスクライブ：

     ```bash
     mqttx sub -t t/1 -h 172.19.0.3 -p 1883
     ```

3. 約5秒後（EMQXのトレースデータエクスポートのデフォルト間隔）、[http://localhost:16686](http://localhost:16686/) のJaeger WEB UIにアクセスし、トレースデータを確認します。

   `emqx` サービスを選択し、**Find Traces** をクリックします。`emqx` サービスがすぐに表示されない場合は、少し待ってページを更新してください。クライアントの接続やサブスクライブイベントのトレースが表示されます：

   ![Jaeger-WEB-UI-e2e-Client-Events](./assets/e2e-client-events.png)

4. メッセージをパブリッシュします：

   ```bash
   mqttx pub -t t/1 -h 172.19.0.2 -p 1883
   ```

5. 少し待つと、Jaeger WEB UIでMQTTメッセージの詳細なトレースを確認できます。

   トレースをクリックすると、詳細なスパン情報やトレースタイムラインが表示されます。サブスクライバー数、ノード間のメッセージルーティング、QoSレベル、`msg_trace_level` 設定に応じて、MQTTメッセージのトレースに含まれるスパン数は異なります。

   以下は、2人のクライアントがQoS 2でサブスクライブし、パブリッシャーがQoS 2のメッセージを送信し、`msg_trace_level` が2に設定されている場合のトレースタイムラインとスパン情報の例です。

   特に、クライアント `mqttx_9137a6bb` はパブリッシャーとは異なるEMQXノードに接続しているため、ノード間送信を表す2つの追加スパン（`message.forward` と `message.handle_forward`）が存在します。

   ![Jaeger-WEB-UI-e2e-Message](./assets/e2e-message.png)

   また、メッセージやイベントがルールエンジンの実行をトリガーした場合、ルールエンジントラッキングオプションが有効であれば、ルールおよびアクションの実行トラッキング情報も取得可能です。

   ![Jaeger-WEB-UI-e2e-With-Rule-Engine](./assets/e2e-with-rule-engine.png)

   ::: tip

   ルールエンジン実行を含むエンドツーエンドトレーシング機能は、EMQXバージョン5.9.0以降でサポートされています。

   :::

   ::: warning 重要なお知らせ

   この機能は慎重に有効化してください。メッセージやイベントが複数のルールやアクションをトリガーすると、1つのトレースで大量のスパンが生成され、システム負荷が増加します。
   メッセージ量やルール・アクション数に基づき、適切なサンプリング率を見積もってください。

   :::

## トレーススパンの過負荷管理

EMQXはトレーススパンを蓄積し、定期的にバッチでエクスポートします。エクスポート間隔は `opentelemetry.trace.scheduled_delay` パラメータで制御され、デフォルトは5秒です。バッチトレーススパンプロセッサには過負荷保護機能があり、スパンの蓄積上限（デフォルト2048スパン）を超えると新規スパンを破棄します。この上限は以下の設定で調整可能です。

```yaml
opentelemetry {
  traces {
    max_queue_size = 50000
    scheduled_delay = 1000
  }
}
```

`max_queue_size` の上限に達すると、現在のキューがエクスポートされるまで新しいトレーススパンは破棄されます。

::: tip 補足

トレース対象メッセージが多数のサブスクライバーに配信される場合や、メッセージ量が多くサンプリング率が高い場合、過負荷保護により多くのスパンが破棄され、エクスポートされるスパンはごく一部になる可能性があります。

エンドツーエンドトレーシングモードでは、メッセージ量やサンプリング率に応じて `max_queue_size` を増やし、`scheduled_delay` を短縮してスパンのエクスポート頻度を上げることを検討してください。これにより過負荷保護によるスパンの損失を軽減できます。

**ただし、エクスポート頻度の増加やキューサイズの拡大はシステムリソース消費の増加を招くため、メッセージTPSや利用可能なシステムリソースを十分に見積もった上で設定を適用してください。**

:::
