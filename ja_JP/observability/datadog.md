# Datadogとの連携

[Datadog](https://www.datadoghq.com/)は、クラウドベースのオブザーバビリティおよびセキュリティプラットフォームであり、自動化されたインフラ監視、アプリケーションパフォーマンス監視、ログ管理、リアルユーザーモニタリングを提供します。これらの機能をリアルタイムのアプリケーションソリューションとして統合し、開発者がパフォーマンスと信頼性を容易に監視、分析、最適化できるようにします。

EMQXは、EMQXの稼働状況をよりよく理解し、システムパフォーマンスの監視およびトラブルシューティングを支援するために、標準搭載の[Datadog連携](https://docs.datadoghq.com/integrations/emqx/)を提供しています。これにより、ユーザーはより効率的で信頼性が高く、リアルタイムなデータ伝送を実現するIoTアプリケーションを構築できます。

## Datadog連携の仕組み

EMQXとDatadogの連携は新機能ではなく、EMQXの既存機能をフル活用しています。動作の原理は以下の通りです：

1. EMQXクラスター側にDatadog Agentをインストールし、DatadogがEMQX向けに提供する標準搭載の拡張プラグインである[Datadog - EMQX連携](https://docs.datadoghq.com/integrations/emqx/)を追加します。

2. 連携のプリセット設定を変更し、Datadog AgentがEMQXのPrometheusプルREST APIから定期的に指標データを取得できるようにします。取得した指標データはDatadog Agentで処理され、Datadogプラットフォームにアップロードされます。

3. Datadogクラウドプラットフォーム上で、連携のプリセットダッシュボードチャートを通じて各種指標データを閲覧できます。

以下では、上記の手順に従って設定を進めます。

## Datadog Agentのインストール

[Datadog Agent](https://docs.datadoghq.com/getting_started/agent/)はEMQXのメトリクスを収集し、Datadogクラウドへ送信します。EMQXクラスターが稼働するサーバー、またはEMQXノードにアクセス可能なサーバーにデプロイする必要があります。

初めてDatadogを利用する場合は、[Datadog](https://www.datadoghq.com/)にアクセスしてアカウントを作成し、Datadogコンソールにログインしてください。次に、EMQXが稼働するサーバーにDatadog Agentをインストールします。手順は以下の通りです：

1. メニューバーの**Integrations** → **Agent**に移動し、Agentインストール手順のページを開きます。

2. ご利用のOSバージョンを選択し、表示される指示に従ってください。

![Datadog Agentのインストール](./assets/datadog-agent-install.png)

## DatadogにEMQX連携を追加

EMQXは標準搭載の[Datadog連携](https://docs.datadoghq.com/integrations/emqx/)を提供しており、以下の手順でDatadogコンソールに簡単に組み込めます：

1. Datadogコンソールを開き、メニューバーの**Integrations** → **Integrations**に移動します。

2. 検索ボックスに`EMQX`と入力し、同名かつ同じ作者の連携を探します。

3. ポップアップの右上にある**Install Integration**ボタンをクリックし、EMQX連携をDatadogに追加します。

![Datadog EMQX連携のインストール](./assets/datadog-search-emqx-intergration.png)

4. インストール完了後、**Configure**タブに移動し、EMQX連携の設定ガイドラインを確認します。Datadog Agent内で必要な設定を行います。

![EMQX Datadog連携の設定](./assets/datadog-integration-configuration.png)

## Datadog AgentにEMQX連携を追加・有効化

設定ガイドラインに従い、Datadog AgentにEMQX連携を追加してEMQXメトリクスの収集および報告を設定します。

1. Datadog Agentが稼働するサーバーで以下のコマンドを実行し、EMQX連携を追加します。例ではバージョン1.1.0を使用していますが、常に最新のガイドラインで適切なバージョンを確認してください：

    ```bash
    datadog-agent integration install -t datadog-emqx==1.1.0
    ```

2. インストール完了後、`monitoring`スコープを持つ専用の[EMQX APIキー](../admin/api.md#authentication)を作成します。次にAgentの設定ファイルを編集してEMQX連携を有効化します。

    Agent設定ディレクトリ（通常は`/opt/datadog-agent/etc/conf.d/`）に移動し、その中の`emqx.d`ディレクトリを探します。`emqx.d`内に`conf.yaml.example`というサンプル設定ファイルがあります。

    このファイルを同じディレクトリにコピーし、`conf.yaml`にリネームしてください。`conf.yaml`を編集し、以下の設定項目を調整します：

    ```yaml
    instances:
      - openmetrics_endpoint: http://localhost:18083/api/v5/prometheus/stats?mode=all_nodes_aggregated
        username: '<API_KEY>'
        password: '<SECRET_KEY>'
    ```

    `openmetrics_endpoint`はDatadog AgentがOpenMetrics形式でメトリクスデータを取得するアドレスを指定します。ここではEMQXのHTTP APIアドレスを設定していますが、Datadog Agentからアクセス可能なアドレスに置き換えてください。`username`にはAPIキー、`password`には対応するシークレットキーを設定します。EMQX 6.3.0以降、PrometheusスクレイプAPIはデフォルトで認証が必要です。

    APIは`mode`クエリパラメータで取得するメトリクスの範囲を指定できます。各パラメータの意味は以下の通りです：

    | **パラメータ**           | **説明**                                                    |
    | ------------------------ | ------------------------------------------------------------ |
    | node                     | 現在リクエストしているノードのメトリクスを返します。`mode`パラメータが指定されない場合のデフォルトです。 |
    | all_nodes_unaggregated    | クラスター内の各ノードのメトリクスを個別に返し、メトリクスの独立性を維持します。結果にはノード名が含まれ区別可能です。 |
    | all_nodes_aggregated      | クラスター内の全ノードのメトリクス値を集約して返します。       |

    統一的なビューを得るには`mode=all_nodes_aggregated`オプションを使用してください。これによりDatadog側ではEMQXクラスター全体の値として認識されます。

3. Agentの再起動については[こちらのドキュメント](https://docs.datadoghq.com/agent/guide/agent-commands/#start-stop-and-restart-the-agent)を参照してください。macOSを例にすると以下のようになります：

    ```bash
    launchctl stop com.datadoghq.agent
    launchctl start com.datadoghq.agent
    ```

4. システム再起動後、以下のコマンドでEMQX連携が正常に有効化されているか確認します。`Instance ID: ... [OK]`と表示されれば連携が有効化されています。

    ```bash
    $ datadog-agent status | grep emqx -A 4
        emqx (1.1.0)
        ------------
          Instance ID: emqx:1865f3a06d300ccc \[OK\]
          Configuration Source: file:/opt/datadog-agent/etc/conf.d/emqx.d/conf.yaml
          Total Runs: 17
          Metric Samples: Last Run: 166, Total: 2,822
          Events: Last Run: 0, Total: 0
          Service Checks: Last Run: 1, Total: 17
          Average Execution Time : 43ms
          Last Execution Date : 2024-05-11 17:35:41 CST / 2024-05-11 09:35:41 UTC (1715420141000)
          Last Successful Execution Date : 2024-05-11 17:35:41 CST / 2024-05-11 09:35:41 UTC (1715420141000)
    
    ```

これでDatadog Agent側の必要な設定は完了です。AgentはEMQXの稼働データを定期的に収集し、Datadogに送信します。

次にDatadogコンソールでメトリクスが正しく収集されているか確認しましょう。

## DatadogコンソールでのEMQXメトリクスの確認

Datadog AgentのEMQX連携は、ノード状態やメッセージ状態などの詳細なオブザーバビリティメトリクスを表示する使いやすいダッシュボードチャートを提供しています。利用手順は以下の通りです：

1. Datadogコンソールを開き、メニューバーの**Integrations** → **Integrations**に移動します。

2. インストール済みのEMQX連携を探してクリックします。

3. ポップアップ内の**Monitoring Resources**タブに切り替え、**Dashboards**の下にある**EMQX Overview**チャートを開きます。

    ![Monitoring Resourcesタブ](./assets/datadog-dashboard-overview.png)

**チャートが提供する情報は以下の通りです：**

- **OpenMetrics Health**：アクティブなメトリクスコレクターの数
- **Total Connections**：接続数の合計（切断されてもセッションが維持されているものを含む）
- **NodeRunning**：クラスター内で稼働中のノード数
- **Active Topics**：現在アクティブなトピック数
- **NodeStopped**：停止中のノード数
- **Connection**
  - **Total**：接続の総数（切断されてもセッションが維持されているものを含む）
  - **Live**：アクティブに維持されているTCP接続数
- **Topic**
  - **Total**：トピックの総数
  - **Shared**：共有トピックの数
- **Session**：セッションの総数
- **Erlang VM**：Erlang仮想マシンのCPU、メモリ、キュー使用状況
- **Retainer & Delayed**
  - **Retained**：保持されているメッセージ数
  - **Delayed**：遅延メッセージ数
- **Message**
  - **Sent & Received**：送受信メッセージのレート
  - **Delayed & Retained**：遅延および保持メッセージのレート
  - **Publish & Delivered**：メッセージのパブリッシュおよび配信レート
  - **Delivery Dropped**：配信がドロップされたメッセージ数
- **Client**
  - **Connected & Disconnected**：接続確立および切断のレート
  - **Sub & UnSub**：サブスクライブおよびサブスクライブ解除のレート
  - **AuthN & AuthZ**：認証および認可のレート情報
  - **Delivery Dropped**：ドロップされた配信メッセージ数
- **Mria**：Mriaトランザクションの総数

以下は概要メトリクスチャートのスクリーンショットの一部です。値はEMQXの負荷やクライアントのアクティビティに応じて動的に変化します。

![メトリクス概要](./assets/datadog-dashboard-detail.png)

![接続、トピック、セッション](./assets/datadog-dashboard-conn.png)

![送受信メッセージのレート、保持／遅延／ドロップメッセージ数](./assets/datadog-dashboard-msg-rate.png)

![クライアイベント](./assets/datadog-dashboard-events.png)

## 次のステップ

DatadogのEMQX連携に組み込まれているチャートは主要なメトリクスの一部のみを表示しています。すべての報告されるEMQXメトリクスについては[こちらのドキュメント](https://docs.datadoghq.com/integrations/emqx/#metrics)を参照し、それらを基に独自の監視チャートを作成できます。

これらのメトリクスに基づいてDatadogでアラートルールを設定することも可能です。特定のメトリクスが設定した閾値に達したり異常が発生した場合、Datadogが通知を送信し、迅速な対応を促すことでシステム障害がビジネスに与える影響を最小限に抑えられます。
