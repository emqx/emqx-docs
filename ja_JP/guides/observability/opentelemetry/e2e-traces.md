# OpenTelemetryベースのエンドツーエンドMQTTトレーシング

現代の分散システムにおいて、リクエストの流れを追跡しパフォーマンスを分析することは、信頼性と可観測性を確保するために不可欠です。エンドツーエンドトレーシングは、リクエストの開始から終了までの全経路をキャプチャすることを目的とした概念であり、システムの挙動やパフォーマンスに関する深い洞察を得ることができます。

EMQXはバージョン5.8.3以降、MQTTプロトコルに特化したOpenTelemetryベースのエンドツーエンドトレーシング機能を統合しています。この機能により、特にマルチノードクラスター環境において、メッセージのパブリッシュ、ルーティング、配信の流れを明確にトレースできます。システムパフォーマンスの最適化だけでなく、迅速な障害箇所の特定やシステム信頼性の向上にも役立ちます。

本ページでは、EMQXでエンドツーエンドトレーシング機能を有効化し、MQTTメッセージフローの包括的な可視化を実現する方法について詳しく解説します。

## OpenTelemetryコレクターのセットアップ

設定の詳細については、[OpenTelemetryコレクターのセットアップ](./traces.md#setting-up-opentelemetry-collector)を参照してください。

## EMQXでエンドツーエンドトレーシングを有効化する

::: tip

エンドツーエンドトレーシングはシステムパフォーマンスに影響を与える可能性があるため、必要な場合のみ有効化してください。

:::

このセクションでは、EMQXでOpenTelemetryベースのエンドツーエンドトレーシングを有効化する手順を案内し、マルチノード環境でのMQTT分散トレーシング機能を紹介します。

### ダッシュボードからエンドツーエンドトレーシングを設定する

1. ダッシュボードの左メニューから **Management** -> **Monitoring** をクリックします。

2. Monitoringページで **Integration** タブを選択します。

3. 以下の設定を行います：
   - **Monitoring platform**：`OpenTelemetry` を選択します。

   - **Feature Selection**：`Traces` を選択します。

   - **Endpoint**：トレースデータをエクスポートするURLを1つ入力します。URLは `http` または `https` スキームを使用し、明示的なポート番号を含める必要があります。デフォルトは `http://localhost:4317` です。`localhost:4317` や `http://localhost` のような値は無効です。

   - **Headers**：トレースエクスポートリクエストにカスタムHTTPヘッダーを追加します。OpenTelemetryコレクターが認証やAPIキー、トークンなどのカスタムヘッダーを必要とする場合に有効です。各ヘッダーはキーと値のペアで指定します。

     OpenTelemetryコレクターがBasic認証を使用する場合は、`authorization` ヘッダーに `Basic <base64エンコードされたユーザー名:パスワード>` の形式で値を追加してください。例：

     ```
     Key: authorization
     Value: Basic dXNlcjpwYXNzd29yZA==
     ```

     このオプションはHTTPベースの認証を強制するコレクターとの互換性を高めます。

   - **Enable TLS**：必要に応じてTLS暗号化を有効にします。通常は本番環境のセキュリティ要件に応じて設定します。

   - **Trace Mode**：`End-to-End` を選択し、エンドツーエンドトレーシング機能を有効にします。

   - **Cluster Identifier**：span属性にクラスタ識別用のプロパティ値を追加します。プロパティキーは `cluster.id` です。通常はシンプルで識別しやすい名前やクラスター名を設定し、EMQXクラスター間の区別に利用します。デフォルトは `emqxcl` です。

   - **Traces Export Interval**：トレースデータのエクスポート間隔を秒単位で設定します。デフォルトは `5` 秒です。

   - **Max Queue Size**：トレースデータのキューの最大サイズを設定します。デフォルトは `2048` エントリです。

4. 必要に応じて **Trace Advanced Configuration** をクリックし、高度な設定を行います。

   - **Trace Configuration**：クライアント接続、メッセージ送受信、ルールエンジン実行など特定イベントのトレース設定を追加で行えます。
     - **Follow Traceparent**：`traceparent` を追従するかどうかを設定します。`true` に設定すると、EMQXはクライアントから送信された `User-Property` 内の `traceparent` 識別子を取得し、それに関連付けてエンドツーエンドトレーシングを行います。`false` の場合は新規トレースを生成します。デフォルトは `true` です。
   - **Client ID White List**：トレース対象とするクライアントを制限するホワイトリストを設定します。不要なトレースを避け、システムリソースの消費を抑制できます。
   - **Topic White List**：トレース対象のトピックを制限するホワイトリストを設定します。クライアントホワイトリストと同様にトレース範囲を制御できます。

   設定を保存後、ウィンドウを閉じて **Confirm** をクリックします。

5. 最後に **Save Changes** をクリックして設定を保存します。

<img src="./assets/e2e-dashboard-conf-en.png" alt="Otel-E2E-Trace-dashboard-page" style="zoom:67%;" />

### 設定ファイルからエンドツーエンドトレーシングを設定する

EMQXの `cluster.hocon` ファイルに以下の設定を追加します（EMQXがローカルで動作している前提）。

`opentelemetry.exporter.endpoint` は1つのURLを受け入れます。URLは `http` または `https` スキームを使用し、明示的なポート番号を含める必要があります。例：`http://localhost:4317` は有効ですが、`localhost:4317` や `http://localhost` は無効です。

設定オプションの詳細は、[EMQX Dashboard Monitoring Integration](http://localhost:18083/#/monitoring/integration) のOpenTelemetryセクションを参照してください。

```bash
opentelemetry {
  exporter {
    endpoint = "http://localhost:4317"
    headers {
      authorization = "Basic dXNlcjpwYXNzd29yZA=="
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
      ## クライアントメッセージイベントのトレース
      client_messaging = true
      ## クライアントサブスクライブ/アンインサブスクライブイベントのトレース
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
      ## 注意：トレースが有効な場合のみ適用
      sample_ratio = "100%"
      ## traceparentの追従
      ## クライアントから渡された `traceparent` をエンドツーエンドトレーシングで追従するかどうか
      follow_traceparent
    }
  }
  max_queue_size = 50000
  scheduled_delay = 1000
}
```

## EMQXでエンドツーエンドトレーシングを実演する

1. EMQXノードを起動します。例として、`emqx@172.19.0.2` と `emqx@172.19.0.3` というノード名の2ノードクラスターを起動し、分散トレーシング機能を実演します。

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

   `emqx` サービスを選択し、**Find Traces** をクリックします。`emqx` サービスがすぐに表示されない場合は少し待ってページを更新してください。クライアントの接続やサブスクライブイベントのトレースが表示されます：

   ![Jaeger-WEB-UI-e2e-Client-Events](./assets/e2e-client-events.png)

4. メッセージをパブリッシュします：

   ```bash
   mqttx pub -t t/1 -h 172.19.0.2 -p 1883
   ```

5. 少し待つと、Jaeger WEB UIでMQTTメッセージの詳細なトレースを確認できます。

   トレースをクリックすると、詳細なspan情報とトレースタイムラインが表示されます。サブスクライバー数、ノード間のメッセージルーティング、QoSレベル、`msg_trace_level` の設定に応じて、MQTTメッセージトレースに含まれるspan数は異なります。

   以下は、2人のクライアントがQoS 2でサブスクライブし、パブリッシャーがQoS 2のメッセージを送信し、`msg_trace_level` が2に設定されている場合のトレースタイムラインとspan情報の例です。

   特に、クライアント `mqttx_9137a6bb` がパブリッシャーとは異なるEMQXノードに接続されているため、ノード間の転送を表す2つの追加span（`message.forward` と `message.handle_forward`）が表示されています。

   ![Jaeger-WEB-UI-e2e-Message](./assets/e2e-message.png)

   さらに、メッセージやイベントがルールエンジンの実行をトリガーした場合、ルールエンジンのトレースオプションを有効にしていれば、ルールおよびアクションの実行トレース情報も取得できます。

   ![Jaeger-WEB-UI-e2e-With-Rule-Engine](./assets/e2e-with-rule-engine.png)

   ::: tip

   ルールエンジン実行を含むエンドツーエンドトレーシング機能は、EMQXバージョン5.9.0以降でサポートされています。

   :::

   ::: warning 重要なお知らせ

   この機能は慎重に有効化してください。メッセージやイベントが複数のルールやアクションをトリガーすると、1つのトレースで大量のspanが生成され、システム負荷が増加します。
   メッセージ量やルール・アクション数に応じて適切なサンプリング率を見積もってください。

   :::

## トレースspanの理解

EMQXはエンドツーエンドトレーシング中にさまざまな種類のspanを生成し、ブローカー内部の詳細な動作を可視化します。これらのspanはクライアントのライフサイクル、メッセージのライフサイクル、認証・認可、ルールエンジン、ブローカー内部処理など多岐にわたります。

主なspanタイプの概要は以下の通りです：

- **クライアントライフサイクルspan**：クライアントの接続、切断、サブスクライブ、アンインサブスクライブなど主要なライフサイクルイベントをトレースします。
- **認証・認可span**：クライアントに対して行われる認証および認可処理の可視化を提供します。
- **メッセージライフサイクルspan**：MQTTメッセージがブローカー内を通過する過程（受信、ルーティング、転送、QoSごとのアックフロー）をトレースします。
- **ルールエンジンスパン**：ルールエンジンの処理および実行をトレースします。
- **ブローカー内部span**：クライアントの強制切断や内部サブスクライブなど、ブローカー内部の操作をトレースします。

各spanの詳細については、[エンドツーエンドトレーシングspan詳細](./e2e_span_details.md)を参照してください。

## トレースspanの過負荷管理

EMQXはトレースspanを蓄積し、一定間隔でバッチエクスポートします。エクスポート間隔は `opentelemetry.trace.scheduled_delay` パラメータで制御され、デフォルトは5秒です。バッチトレースspanプロセッサには過負荷保護機能があり、蓄積できるspan数の上限（デフォルト2048span）を超えると新規spanは破棄されます。以下の設定で上限を調整可能です：

```yaml
opentelemetry {
  traces {
    max_queue_size = 50000
    scheduled_delay = 1000
  }
}
```

`max_queue_size` の上限に達すると、現在のキューがエクスポートされるまで新規トレースspanは破棄されます。

::: tip 補足

トレース対象メッセージが多数のサブスクライバーに配信される場合や、メッセージ量が多くサンプリング率が高い場合、過負荷保護により多くのspanが破棄され、エクスポートされるspanはごく一部になる可能性があります。

エンドツーエンドトレーシングモードでは、メッセージ量やサンプリング率に応じて `max_queue_size` を増やし、`scheduled_delay` を短く設定してspanのエクスポート頻度を上げることを検討してください。これにより過負荷保護によるspanの損失を防げます。

**ただし、エクスポート頻度の増加やキューサイズの拡大はシステムリソースの消費増加を招くため、メッセージTPSや利用可能なシステムリソースを十分に見積もった上で適切に設定してください。**

:::
