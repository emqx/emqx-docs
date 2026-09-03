# PrometheusでEMQXを監視する

EMQXはPrometheusが収集できるランタイムメトリクスを公開しており、これによりクエリ、アラート、可視化が可能です。メトリクスの収集方法は以下のいずれかです。

- **プルモード（推奨）**：PrometheusがREST APIエンドポイントから直接EMQXのメトリクスをスクレイプします。このモードで利用可能なメトリクスをすべて収集できます。
- **プッシュモード**：EMQXがPushgatewayに基本的なメトリクスを送信し、PrometheusがPushgatewayからそれらをスクレイプします。Prometheusが直接EMQXに接続できない場合に使用します。

収集したEMQXメトリクスはGrafanaを使って[可視化](#visualize-emqx-metrics-in-grafana)できます。

::: tip
EMQX 6.3.0以降、Prometheusメトリクスは`metrics`機能ゲートで制御されます。`EMQX_FEATURES`を手動で設定する場合、`metrics`を有効にすると依存する`dashboard`と`auth`も有効になります。詳細は[機能ゲート](../deploy/feature-gates.md)をご覧ください。
:::

## EMQXでのメトリクス収集設定

DashboardのPrometheus統合ページで認証、Pushgateway配信、レイテンシーバケット、ネームスペースのリクエスト制限を制御できます。本節では各設定の内容を説明し、後続の節でプルモードとプッシュモードの設定手順を示します。

Prometheus統合設定を開くには：

1. EMQX Dashboardで**管理** -> **監視**に移動します。
2. **統合**タブを選択します。
3. **Prometheus**を選択します。

<img src="./assets/enable-push-gateway.png" alt="Prometheus統合設定" style="zoom: 67%;" />

以下のエンドポイントで公開されるメトリクスシリーズの厳選リファレンス（アラートに適したものを含む）は、[ブローカーのヘルス指標](./broker-health-indicators.md)を参照してください。

### スクレイプリクエストの認証を要求する

**Basic Authを有効にする**は、`/api/v5/prometheus/*`以下のすべてのPrometheusスクレイプAPIの認証を制御します。Dashboard上のラベルに関わらず、この設定はHTTP Basic認証とBearer認証の両方を制御します。

EMQX 6.3.0以降、認証はデフォルトで有効です。認証情報なしのリクエストは`401`を返します。PrometheusはAPIキーとシークレットキーを使ったHTTP Basic認証を利用できます。EMQXはDashboardのログイントークンをBearerトークンとしても受け入れますが、トークンは期限切れになるため長期間のスクレイプには適しません。

長期間稼働するPrometheusサーバーには、`monitoring`スコープの専用APIキーを作成し、[Prometheusのスクレイプリクエスト認証を設定](#authentication)してください。

認証なしのスクレイプを許可するには、**Basic Authを有効にする**をオフにするか、`prometheus.enable_basic_auth = false`を設定します。この設定はプルモードにのみ影響し、Pushgateway配信には影響しません。

::: warning 重要なお知らせ
認証を無効にすると、Dashboardリスナーに到達可能な任意のクライアントがEMQXメトリクスをスクレイプ可能になります。アップグレード後、明示的に`prometheus.enable_basic_auth = false`を設定した設定や旧形式のPrometheus設定は引き続き認証なしスクレイプを許可します。アップグレード後はDashboardで**Basic Authを有効にする**を必ず確認してください。
:::

### Pushgatewayへのメトリクス配信を設定する

**Pushgatewayを有効にする**をオンにすると、EMQXがPushgatewayインスタンスにメトリクスを送信します。プッシュ配信はデフォルトで無効です。以下の項目を設定してください。

| 項目 | 説明 |
| --- | --- |
| **間隔** | EMQXがメトリクスをプッシュする頻度。デフォルトは`15`秒です。 |
| **Pushgatewayサーバー** | PushgatewayのURL。デフォルトは`http://127.0.0.1:9091`です。 |
| **ジョブ名** | プッシュされるメトリクスのジョブラベル。デフォルトは`${name}/instance/${name}~${host}`で、`${name}`は`@`の前のノード名、`${host}`は`@`の後のホスト名です。例えば`emqx@127.0.0.1`の場合、`emqx`と`127.0.0.1`になります。 |
| **ヘッダー** | Pushgatewayに送信する任意のHTTPヘッダー。各ヘッダーはキーと値のペアで追加します。例：`Authorization = "some-auth-token"` |

完全な手順は[Pushモード統合の設定](#configure-push-mode-integration)を参照してください。

### レイテンシーヒストグラムのバケットを定義する

**Latency Buckets**にはカンマ区切りの期間リストを入力します。例：

```text
10ms, 100ms, 1s, 5s, 30s
```

これらの値はプルモードとプッシュモード両方のレイテンシーヒストグラムのバケット境界を定義します。バケット数が多いほど粒度は細かくなりますが、メトリクスのカーディナリティやストレージ使用量が増加する可能性があります。

### すべてのネームスペースをスクレイプするリクエストの制限

**Namespace Data Scraping Rate Limit**は、すべてのネームスペースのメトリクスをスクレイプするリクエストの最大レートを設定します。特定のネームスペースのリクエストは制限されません。`<requests>/<duration>`形式で値を入力します。デフォルトの`1/5s`は5秒に1回のリクエストを許可し、それ以外のリクエストは拒否されます。

## PrometheusでEMQXメトリクスをスクレイプする設定

プルモードでは、PrometheusがEMQX Dashboardリスナーに接続し、1つ以上のREST APIエンドポイントをスクレイプします。

### スクレイプするメトリクスエンドポイントを選択する

収集したいメトリクスカテゴリごとにPrometheusのスクレイプジョブを追加します。

| エンドポイント | メトリクス内容 |
| --- | --- |
| `/api/v5/prometheus/stats` | 基本的なEMQXメトリクスとカウンター |
| `/api/v5/prometheus/namespaced_stats` | ネームスペース別に集約されたメトリクス |
| `/api/v5/prometheus/auth` | 認証、認可、禁止クライアントのメトリクス |
| `/api/v5/prometheus/data_integration` | ルール、コネクター、アクション、Sink/Source、エンコード/デコードのメトリクス |
| `/api/v5/prometheus/schema_validation` | スキーマ検証のメトリクス |
| `/api/v5/prometheus/message_transformation` | メッセージ変換のメトリクス |
| `/api/v5/prometheus/topic_metrics` | トピックメトリクスの収集カウンター |

完全なAPIリファレンスは[EMQX Enterprise APIドキュメント](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)を参照してください。

### ネームスペース別にデータ統合メトリクスをスクレイプする

EMQX 6.3.0以降、`GET /api/v5/prometheus/data_integration`は認証ユーザーのネームスペースに応じてルール、アクション、コネクタのメトリクスを制限します。

- ネームスペースユーザーは割り当てられたネームスペースのメトリクスのみ受け取ります。`ns=<namespace>`で別のネームスペースを指定すると`403`が返されます。
- グローバル管理者はデフォルトで全ネームスペースのメトリクスを受け取ります。`ns=<namespace>`を指定すると特定のネームスペースをスクレイプし、`only_global=true`を`ns`なしで指定するとグローバルネームスペースのみをスクレイプします。
- 認証が無効な場合、グローバル管理者と同様の可視性が適用され、デフォルトで全ネームスペースのメトリクスが返されます。

グローバル以外のネームスペースのリソース別メトリクスには`namespace`ラベルが含まれます。グローバルネームスペースのリソース別メトリクスにはこのラベルは含まれません。`emqx_schema_registrys_count`メトリクスはスキーマレジストリリソースがネームスペーススコープ外のためクラスター全体のままです。

すべてのネームスペースをスクレイプするリクエストは[ネームスペースデータスクレイプリクエスト制限](#limit-requests-that-scrape-all-namespaces)の対象です。

### メトリクス収集モードを選択する

対応するエンドポイントでは、`mode`クエリパラメータで現在のノードかクラスターのメトリクスを返すか制御できます。

:::: tabs type: card

::: tab 現在のノード

```text
mode=node
```

リクエストを受けたノードのメトリクスを返します。デフォルトのモードです。

:::

::: tab 集約されたクラスター

```text
mode=all_nodes_aggregated
```

全稼働ノードのメトリクスを以下の集約ルールで返します。

- 状態メトリクスは論理集約されます。例えば、すべてのノードで状態が有効または稼働中の場合に`1`、それ以外は`0`になります。
- CPUやメモリ使用率などノード固有のメトリクスは集約されず、`node`ラベルが付きます。

  ```text
  emqx_vm_cpu_use{node="emqx@172.17.0.2"} 7.6669163995887715
  emqx_vm_cpu_idle{node="emqx@172.17.0.2"} 92.33308360041123
  emqx_vm_cpu_use{node="emqx@172.17.0.3"} 7.676007766679973
  emqx_vm_cpu_idle{node="emqx@172.17.0.3"} 92.32399223332003
  ```

- クラスター全体で一貫するメトリクスはリクエストを受けたノードの値を返し、合計はせず`node`ラベルも付きません。

  ```text
  emqx_topics_count 3
  emqx_cert_expiry_at{listener_type="ssl",listener_name="default"} 1904285225
  emqx_cert_expiry_at{listener_type="wss",listener_name="default"} 1904285225
  ```

- その他のメトリクスは全稼働ノードの算術合計を返します。

:::

::: tab 非集約のクラスター

```text
mode=all_nodes_unaggregated
```

全稼働ノードの個別メトリクスを返します。ノード固有の値には`node`ラベルが付きます。

```text
emqx_connections_count{node="emqx@127.0.0.1"} 0
```

クラスター全体で一貫するメトリクスはリクエストを受けたノードの値を返し、`node`ラベルは付きません。

```text
emqx_retained_count 3
```

:::

::::

### トピックメトリクス

EMQX 6.3以降、`GET /api/v5/prometheus/topic_metrics`はトピックメトリクスREST APIで作成された名前付きコレクションのカウンターを公開します。このエンドポイントをスクレイプする前に少なくとも1つのコレクションを作成してください。手順は[REST APIでトピックメトリクスコレクションを管理する](./topic-metrics.md#manage-topic-metric-collections-with-the-rest-api)を参照してください。

公開されるカウンターは以下の通りです。

| メトリクス | 説明 |
| --- | --- |
| `emqx_topic_metric_messages_in_count` | コレクションフィルターにマッチするトピックにパブリッシュされたメッセージ数 |
| `emqx_topic_metric_messages_out_count` | マッチしたメッセージがサブスクライバーに配信された数 |
| `emqx_topic_metric_messages_dropped_count` | EMQXによってドロップされたマッチしたメッセージ数 |
| `emqx_topic_metric_bytes_in` | マッチしたパブリッシュメッセージのトピックとペイロードの合計サイズ |
| `emqx_topic_metric_bytes_out` | マッチした配信メッセージのトピックとペイロードの合計サイズ |

各時系列には`name`と`topic_filter`ラベルが含まれます。ネームスペース所有のコレクションには`namespace`ラベルも含まれます。`mode=all_nodes_unaggregated`の場合、各シリーズに`node`ラベルも含まれます。

すべてのトピックメトリクス値は単調増加カウンターです。`rate()`などのPromQL関数を使い、1秒あたりのレートを計算してください。例：

```text
rate(emqx_topic_metric_messages_in_count[5m])
```

::: warning 重要なお知らせ

各コレクションは5つのカウンターを公開します。非集約モードではEMQXはノードごとに別の時系列を作成するため、Prometheusの時系列が過剰に増えないようコレクション数を制限してください。

:::

<a id="authentication"></a>

### Prometheusスクレイプリクエストの認証設定

EMQX 6.3.0以降、PrometheusスクレイプAPIはデフォルトで認証を要求します。長期間のスクレイプには専用APIキーを使ったHTTP Basic認証を利用してください。

1. EMQXで`monitoring`スコープの[APIキー](../admin/api.md#authentication)を作成します。
2. `prometheus.yaml`の各EMQXスクレイプジョブにAPIキーとシークレットキーを追加します。

   ```yaml
   basic_auth:
     username: '<API_KEY>'
     password: '<SECRET_KEY>'
   ```

EMQXは`POST /api/v5/login`で取得するBearerトークンも受け入れますが、Dashboardログイントークンは期限切れになるため長期間のスクレイプにはAPIキーを推奨します。

### PrometheusにEMQXスクレイプジョブを追加する

以下の`prometheus.yaml`例は3つの一般的なメトリクスカテゴリを収集します。ターゲットアドレスと認証情報を置き換え、必要に応じて他の[メトリクスエンドポイント](#select-metrics-endpoints-to-scrape)のジョブを追加してください。ファイル変更後はPrometheusを再起動してください。

```yaml
global:
  scrape_interval: 10s
  evaluation_interval: 10s
  external_labels:
    monitor: 'emqx-monitor'

scrape_configs:
  - job_name: 'emqx_stats'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/stats'
    scheme: 'http'
    basic_auth:
      username: '<API_KEY>'
      password: '<SECRET_KEY>'

  - job_name: 'emqx_auth'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/auth'
    scheme: 'http'
    basic_auth:
      username: '<API_KEY>'
      password: '<SECRET_KEY>'

  - job_name: 'emqx_data_integration'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/data_integration'
    scheme: 'http'
    basic_auth:
      username: '<API_KEY>'
      password: '<SECRET_KEY>'

  - job_name: 'emqx_topic_metrics'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/topic_metrics'
    scheme: 'http'
    basic_auth:
      username: '<API_KEY>'
      password: '<SECRET_KEY>'
```

<a id="configure-push-mode-integration"></a>

## EMQXをPushgatewayにメトリクス送信するよう設定する

プッシュモードは`/api/v5/prometheus/stats`で利用可能な基本的なメトリクスとカウンターのみを送信します。他のエンドポイントのメトリクスが必要な場合はプルモードを使用してください。

### DashboardでPushgateway配信を有効にする

1. DashboardでPrometheus統合設定を開きます。
2. **Pushgatewayを有効にする**をオンにします。
3. Pushgatewayサーバー、プッシュ間隔、ジョブ名、必要なHTTPヘッダーを入力します。
4. **変更を保存**をクリックします。

PrometheusもPushgatewayインスタンスをスクレイプするよう設定してください。

### 設定ファイルでPushgateway配信を有効にする

代わりに、`etc/base.hocon`に以下の推奨ネスト設定を追加します。

```hocon
prometheus {
  push_gateway {
    enable = true
    url = "http://127.0.0.1:9091"
    interval = 15s
    headers {}
    job_name = "${name}/instance/${name}~${host}"
  }
}
```

## GrafanaでEMQXメトリクスを可視化する

PrometheusがEMQXメトリクスの収集を開始したら、[EMQX Grafanaダッシュボード](https://grafana.com/grafana/dashboards/17446-emqx/)をインポートして可視化できます。このテンプレートはDashboardのPrometheus統合の**ヘルプ**ページからも入手可能です。

完全な例は[PrometheusとGrafanaでMQTTブローカーを監視する](https://www.emqx.com/en/blog/emqx-prometheus-grafana)をご覧ください。
