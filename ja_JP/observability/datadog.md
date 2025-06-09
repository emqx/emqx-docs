# Datadogとの統合

[Datadog](https://www.datadoghq.com/) は、クラウドベースのオブザーバビリティおよびセキュリティプラットフォームであり、自動化されたインフラストラクチャ監視、アプリケーションパフォーマンス監視、ログ管理、リアルユーザーモニタリングを提供します。これらの機能をリアルタイムのアプリケーションソリューションとして統合し、開発者がパフォーマンスと信頼性を簡単に監視、分析、最適化できるようにします。

EMQXは、EMQXの稼働状況をよりよく理解し、システムパフォーマンスの監視やトラブルシューティングを支援するための、[Datadog統合](https://docs.datadoghq.com/integrations/emqx/)を標準で提供しています。これにより、ユーザーはより効率的で信頼性が高くリアルタイムなデータ伝送が可能なIoTアプリケーションを構築できます。

## Datadog統合の仕組み

EMQXとDatadogの統合は新機能ではなく、EMQXの既存機能をフル活用しています。動作原理は以下の通りです。

1. EMQXクラスター側にDatadog Agentをインストールし、DatadogがEMQX向けに提供する標準プラグインである[Datadog - EMQX統合](https://docs.datadoghq.com/integrations/emqx/)を追加します。

2. 統合のプリセット設定を変更し、Datadog AgentがEMQXのPrometheusプル型REST APIから定期的に指標データを取得できるようにします。取得した指標データはDatadog Agentで処理され、Datadogプラットフォームにアップロードされます。

3. Datadogクラウドプラットフォーム上で、統合のプリセットダッシュボードチャートを通じて各種指標データを確認できます。

以下では、上記の手順に従ってセットアップを行います。

## Datadog Agentのインストール

[Datadog Agent](https://docs.datadoghq.com/getting_started/agent/)はEMQXのメトリクスを収集し、Datadogクラウドに送信します。EMQXクラスターが稼働するサーバー、またはEMQXノードにアクセスできるサーバーにデプロイする必要があります。

初めてDatadogを利用する場合は、[Datadog](https://www.datadoghq.com/)にアクセスしてアカウントを作成し、Datadogコンソールにログインしてください。次に、EMQXが稼働するサーバーにDatadog Agentをインストールします。手順は以下の通りです。

1. メニューバーの**Integrations** → **Agent**に移動し、Agentインストール手順ページを開きます。

2. ご利用のOSバージョンを選択し、表示された手順に従ってください。

![Install Datadog Agent](./assets/datadog-agent-install.png)

## DatadogにEMQX統合を追加

EMQXは標準で[Datadog統合](https://docs.datadoghq.com/integrations/emqx/)を提供しており、以下の手順でDatadogコンソールに簡単に組み込めます。

1. Datadogコンソールを開き、メニューバーの**Integrations** → **Integrations**に移動します。

2. 検索ボックスに `EMQX` と入力し、同名かつ同じ作者の統合を探します。

3. ポップアップ右上の**Install Integration**ボタンをクリックし、EMQX統合をDatadogに追加します。

![Install Datadog EMQX Integration](./assets/datadog-search-emqx-intergration.png)

4. インストール完了後、**Configure**タブに移動し、EMQX統合の設定ガイドラインを確認します。Datadog Agent内で必要な設定を行います。

![Config EMQX Datadog Integration](./assets/datadog-integration-configuration.png)

## Datadog AgentにEMQX統合を追加・有効化

設定ガイドラインに従い、Datadog AgentにEMQX統合を追加して、EMQXメトリクスの収集とレポートを設定します。

1. Datadog Agentが稼働するサーバー上で以下のコマンドを実行し、EMQX統合を追加します。例ではバージョン1.1.0を使用していますが、常に最新のガイドラインに従い適切なバージョンを指定してください。

    ```bash
    datadog-agent integration install -t datadog-emqx==1.1.0
    ```

2. インストール完了後、Agentの設定ファイルを編集してEMQX統合を有効化します。

    Agentの設定ディレクトリ（通常は `/opt/datadog-agent/etc/conf.d/`）に移動し、その中の `emqx.d` ディレクトリを探します。`emqx.d` 内にある `conf.yaml.example` というサンプル設定ファイルを同じディレクトリにコピーし、`conf.yaml` にリネームします。

    `conf.yaml` ファイルを編集し、以下の設定項目を調整します。

    ```bash
    instances:
      - openmetrics_endpoint: http://localhost:18083/api/v5/prometheus/stats?mode=all_nodes_aggregated
    ```

    `openmetrics_endpoint` はDatadog AgentがOpenMetrics形式のメトリクスデータを取得するためのアドレスを指定します。ここではEMQXのHTTP APIアドレスを指定しています。Datadog Agentからアクセス可能なアドレスに置き換えてください。

    APIでは、`mode` クエリパラメータで取得するメトリクスの範囲を指定できます。各パラメータの意味は以下の通りです。

    | **パラメータ**           | **説明**                                                      |
    | ------------------------ | ------------------------------------------------------------- |
    | node                     | 現在リクエストしているノードのメトリクスを返します。modeパラメータが指定されていない場合のデフォルトです。 |
    | all_nodes_unaggregated    | クラスター内の各ノードごとのメトリクスを返し、メトリクスの独立性を保ちます。結果にはノード名が含まれ区別可能です。 |
    | all_nodes_aggregated      | クラスター内の全ノードのメトリクスを集約した値を返します。         |

    統一的なビューを得るには `mode=all_nodes_aggregated` オプションを使用してください。これによりDatadog側でEMQXクラスター全体の値を確認できます。

3. [こちらのドキュメント](https://docs.datadoghq.com/agent/guide/agent-commands/#start-stop-and-restart-the-agent)を参照し、Agentを再起動してください。macOSの場合の例は以下の通りです。

    ```bash
    launchctl stop com.datadoghq.agent
    launchctl start com.datadoghq.agent
    ```

4. システム再起動後、以下のコマンドでEMQX統合が正常に有効化されているか確認します。`Instance ID: ... [OK]` と表示されれば成功です。

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

これでDatadog Agent側の設定は完了です。Agentは定期的にEMQXの稼働データを収集し、Datadogに送信します。

次にDatadogコンソールでメトリクスが正しく収集されているか確認しましょう。

## DatadogコンソールでEMQXメトリクスを確認

Datadog AgentのEMQX統合は、ノード状態やメッセージ状態などの詳細なオブザーバビリティメトリクスを表示する使いやすいダッシュボードチャートを提供しています。利用手順は以下の通りです。

1. Datadogコンソールを開き、メニューバーの**Integrations** → **Integrations**に移動します。

2. インストール済みのEMQX統合を探してクリックします。

3. ポップアップ内の**Monitoring Resources**タブに切り替え、**Dashboards**の下にある**EMQX Overview**チャートを開きます。

    ![Monitoring Resources Tab](./assets/datadog-dashboard-overview.png)

**チャートで確認できる情報は以下の通りです。**

- **OpenMetrics Health**: アクティブなメトリクスコレクターの数
- **Total Connections**: セッションを維持したまま切断された接続も含む、全接続数
- **NodeRunning**: クラスター内の稼働中ノード数
- **Active Topics**: 現在アクティブなトピック数
- **NodeStopped**: クラスター内の停止ノード数
- **Connection**
  - **Total**: セッションを維持したまま切断された接続も含む、全接続数
  - **Live**: アクティブなTCP接続数
- **Topic**
  - **Total**: 全トピック数
  - **Shared**: 共有トピック数
- **Session**: セッション総数
- **Erlang VM**: Erlang仮想マシンのCPU、メモリ、キュー使用状況
- **Retainer & Delayed**
  - **Retained**: 保持中のメッセージ数
  - **Delayed**: 遅延メッセージ数
- **Message**
  - **Sent & Received**: 送受信メッセージのレート
  - **Delayed & Retained**: 遅延および保持メッセージのレート
  - **Publish & Delivered**: メッセージのパブリッシュおよび配信レート
  - **Delivery Dropped**: 配信がドロップされたメッセージ数
- **Client**
  - **Connected & Disconnected**: 接続確立および切断のレート
  - **Sub & UnSub**: サブスクライブおよびサブスクライブ解除のレート
  - **AuthN & AuthZ**: 認証および認可のレート情報
  - **Delivery Dropped**: ドロップされた配信メッセージ数
- **Mria**: Mriaトランザクションの総数

以下は概要メトリクスチャートのスクリーンショットです。値はEMQXの負荷やクライアントのアクティビティに応じて動的に変化します。

![Metrics Overview](./assets/datadog-dashboard-detail.png)

![Connection, Topic, and Session](./assets/datadog-dashboard-conn.png)

![The Rate of Sent and Received Messages, the Number of Retained/Delayed/Dropped Messages](./assets/datadog-dashboard-msg-rate.png)

![Client Event](./assets/datadog-dashboard-events.png)

## 次のステップ

DatadogのEMQX統合に組み込まれているチャートは主要なメトリクスの一部のみを表示しています。すべてのレポートされるEMQXメトリクスは[こちらのドキュメント](https://docs.datadoghq.com/integrations/emqx/#metrics)を参照し、独自の監視チャートを作成できます。

これらのメトリクスを基にDatadogでアラートルールを設定可能です。特定のメトリクスが設定した閾値に達したり異常が発生した場合、Datadogが通知を送信し、迅速な対応を促すことでシステム障害によるビジネスへの影響を最小限に抑えられます。
