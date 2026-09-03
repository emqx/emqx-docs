# PrometheusでEMQXを監視する

EMQXは、Prometheusがクエリ、アラート、可視化のために収集できるランタイムメトリクスを公開しています。これらのメトリクスは、以下のいずれかの方法で収集できます。

- **プルモード（推奨）**: PrometheusがREST APIエンドポイントから直接EMQXのメトリクスをスクレイプします。このモードを使用すると、利用可能なメトリクスの完全なセットを収集できます。
- **プッシュモード**: EMQXが基本的なメトリクスをPushgatewayに送信し、PrometheusはPushgatewayからそれらをスクレイプします。Prometheusが直接EMQXに接続できない場合にこのモードを使用します。

収集したEMQXメトリクスは、Grafanaを使って[可視化することができます](#visualize-emqx-metrics-in-grafana)。

::: tip
EMQX 6.3.0以降、Prometheusメトリクスは`metrics`機能ゲートで制御されます。`EMQX_FEATURES`を手動で設定する場合、`metrics`を有効にすると、依存する`dashboard`および`auth`も有効になります。詳細は[機能ゲート](../deploy/feature-gates.md)を参照してください。
:::

## EMQXでのメトリクス収集の設定

DashboardのPrometheus統合ページで認証、Pushgatewayへの送信、レイテンシーバケット、ネームスペースのリクエスト制限を制御できます。本節では各設定の内容を説明し、後続のセクションでプルモードとプッシュモードの設定手順を示します。

Prometheus統合設定を開くには：

1. EMQX Dashboardで **Management** -> **Monitoring** に移動します。
2. **Integration** タブを選択します。
3. **Prometheus** を選択します。

<img src="./assets/enable-push-gateway.png" alt="Prometheus統合設定" style="zoom: 67%;" />

以下のエンドポイントで公開されるメトリクスシリーズの厳選されたリファレンス（アラートに適したものを含む）は、[Broker Health Indicators](./broker-health-indicators.md)を参照してください。

### スクレイプリクエストの認証を必須にする

**Enable Basic Auth** は、`/api/v5/prometheus/*` 以下のすべてのPrometheusスクレイプAPIに対する認証を制御します。Dashboardのラベルとは異なり、この設定はHTTP Basic認証とBearer認証の両方を制御します。

EMQX 6.3.0以降、認証はデフォルトで有効です。認証情報なしのリクエストは`401`を返します。PrometheusはAPIキーとシークレットキーを使ったHTTP Basic認証を利用できます。EMQXはDashboardのログイントークンをBearerトークンとしても受け入れますが、トークンは期限切れになるため、継続的なスクレイプには適していません。

長時間稼働するPrometheusサーバーには、`monitoring`スコープを持つ専用のAPIキーを作成し、[Prometheusのスクレイプリクエスト認証を設定](#authentication)してください。

認証なしでスクレイプを許可するには、**Enable Basic Auth** をオフにするか、`prometheus.enable_basic_auth = false`に設定します。この設定はプルモードにのみ影響し、Pushgatewayへの送信には影響しません。

::: warning 重要なお知らせ
認証を無効にすると、Dashboardリスナーにアクセスできる任意のクライアントがEMQXメトリクスをスクレイプ可能になります。アップグレード後、`prometheus.enable_basic_auth = false`を明示的に設定した設定や旧形式のPrometheus設定は引き続き認証なしスクレイプを許可します。アップグレード後はDashboardで**Enable Basic Auth**を確認してください。
:::

### Pushgatewayへのメトリクス送信を設定する

**Enable Pushgateway** をオンにすると、EMQXがPushgatewayインスタンスにメトリクスを送信します。プッシュ送信はデフォルトで無効です。以下の項目を設定してください。

| 項目 | 説明 |
| --- | --- |
| **Interval** | EMQXがメトリクスをプッシュする間隔。デフォルトは`15`秒です。 |
| **Pushgateway Server** | PushgatewayのURL。デフォルトは`http://127.0.0.1:9091`です。 |
| **Job Name** | プッシュされるメトリクスのジョブラベル。デフォルトは`${name}/instance/${name}~${host}`で、`${name}`は`@`の前のノード名、`${host}`は`@`の後のホスト名です。例：`emqx@127.0.0.1`の場合、`emqx`と`127.0.0.1`になります。 |
| **Headers** | Pushgatewayに送信する任意のHTTPヘッダー。各ヘッダーはキーと値のペアで追加します。例：`Authorization = "some-auth-token"`。 |

完全な手順は[Pushgatewayへのメトリクス送信設定](#configure-push-mode-integration)を参照してください。

### レイテンシーヒストグラムのバケットを定義する

**Latency Buckets** にカンマ区切りの時間値リストを入力します。例：

```text
10ms, 100ms, 1s, 5s, 30s
```

これらの値はプルモードとプッシュモード両方のレイテンシーヒストグラムのバケット境界を定義します。バケット数が多いほど粒度は細かくなりますが、メトリクスのカーディナリティやストレージ使用量が増加する可能性があります。

### すべてのネームスペースをスクレイプするリクエストを制限する

**Namespace Data Scraping Rate Limit** は、すべてのネームスペースにまたがるメトリクススクレイプリクエストの最大レートを設定します。特定のネームスペースに対するリクエストは制限されません。`<requests>/<duration>`形式で値を入力します。デフォルトの`1/5s`は5秒に1回のリクエストを許可し、それ以外のリクエストは拒否されます。

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

APIの完全なリファレンスは[EMQX Enterprise APIドキュメント](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)を参照してください。

### ネームスペース別のデータ統合メトリクスをスクレイプする

EMQX 6.3.0以降、`GET /api/v5/prometheus/data_integration`は認証ユーザーのネームスペースに応じてルール、アクション、コネクタのメトリクスを制限します。

- ネームスペースユーザーは割り当てられたネームスペースのメトリクスのみを受け取ります。`ns=<namespace>`で別のネームスペースを指定すると`403`が返されます。
- グローバル管理者はデフォルトで全ネームスペースのメトリクスを受け取ります。`ns=<namespace>`で特定のネームスペースをスクレイプするか、`ns`なしで`only_global=true`を指定するとグローバルネームスペースのみをスクレイプします。
- 認証が無効の場合、グローバル管理者と同じ可視性が適用され、デフォルトで全ネームスペースのメトリクスが返されます。

グローバルネームスペース以外のルール、アクション、コネクタのリソース単位メトリクスには`namespace`ラベルが付きます。グローバルネームスペースのリソース単位メトリクスにはこのラベルは付きません。`emqx_schema_registrys_count`メトリクスはスキーマレジストリリソースがネームスペースにスコープされていないためクラスター全体のままです。

すべてのネームスペースをスクレイプするリクエストは[Namespace Data Scraping Rate Limit](#limit-requests-that-scrape-all-namespaces)の対象となります。

### メトリクス収集モードを選択する

対応するエンドポイントでは、`mode`クエリパラメータで現在のノードかクラスターのどちらのメトリクスを返すかを制御できます。

:::: tabs type: card

::: tab 現在のノード

```text
mode=node
```

リクエストを受け取ったノードのメトリクスを返します。デフォルトのモードです。

:::

::: tab 集約されたクラスター

```text
mode=all_nodes_aggregated
```

すべての稼働中ノードのメトリクスを以下の集約方法で返します。

- 状態メトリクスは論理集約されます。例えば、あるメトリクスはすべてのノードで有効または稼働中の場合に`1`、そうでなければ`0`となります。
- CPUやメモリ使用率などノード固有のメトリクスは集約されず、`node`ラベルを保持します。

  ```text
  emqx_vm_cpu_use{node="emqx@172.17.0.2"} 7.6669163995887715
  emqx_vm_cpu_idle{node="emqx@172.17.0.2"} 92.33308360041123
  emqx_vm_cpu_use{node="emqx@172.17.0.3"} 7.676007766679973
  emqx_vm_cpu_idle{node="emqx@172.17.0.3"} 92.32399223332003
  ```

- クラスター全体で一貫するメトリクスはリクエストを受けたノードの値を返し、合計はせず`node`ラベルは付きません。

  ```text
  emqx_topics_count 3
  emqx_cert_expiry_at{listener_type="ssl",listener_name="default"} 1904285225
  emqx_cert_expiry_at{listener_type="wss",listener_name="default"} 1904285225
  ```

- その他のメトリクスはすべての稼働中ノードの算術和を返します。

:::

::: tab 非集約クラスター

```text
mode=all_nodes_unaggregated
```

すべての稼働中ノードの個別メトリクスを返します。ノード固有の値には`node`ラベルが付きます。

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

EMQX 6.3以降、`GET /api/v5/prometheus/topic_metrics`はトピックメトリクスREST APIで作成された名前付きコレクションのカウンターを公開します。このエンドポイントをスクレイプする前に、少なくとも1つのコレクションを作成してください。作成手順は[REST APIでトピックメトリクスコレクションを管理する](./topic-metrics.md#manage-topic-metric-collections-with-the-rest-api)を参照してください。

このエンドポイントは以下のカウンターを公開します。

| メトリクス | 説明 |
| --- | --- |
| `emqx_topic_metric_messages_in_count` | コレクションフィルターにマッチするトピックにパブリッシュされたメッセージ数 |
| `emqx_topic_metric_messages_out_count` | マッチしたメッセージがサブスクライバーに配信された数 |
| `emqx_topic_metric_messages_dropped_count` | EMQXによってドロップされたマッチしたメッセージ数 |
| `emqx_topic_metric_bytes_in` | マッチしたパブリッシュメッセージのトピックとペイロードの合計サイズ |
| `emqx_topic_metric_bytes_out` | マッチした配信メッセージのトピックとペイロードの合計サイズ |

各時系列には`name`と`topic_filter`ラベルが含まれます。ネームスペース所有のコレクションは`namespace`ラベルも含みます。`mode=all_nodes_unaggregated`の場合は各時系列に`node`ラベルも含まれます。

すべてのトピックメトリクス値は単調増加カウンターです。`rate()`などのPromQL関数を使い、1秒あたりのレートを計算してください。例：

```text
rate(emqx_topic_metric_messages_in_count[5m])
```

::: warning 重要なお知らせ

各コレクションは5つのカウンターを公開します。非集約モードではEMQXはノードごとに別々の時系列を作成します。過剰なPrometheus時系列の作成を避けるため、コレクション数を制限してください。

:::

<a id="authentication"></a>

### Prometheusスクレイプリクエストの認証設定

EMQX 6.3.0以降、PrometheusスクレイプAPIはデフォルトで認証を要求します。継続的なスクレイプには専用APIキーを使ったHTTP Basic認証を利用してください。

1. EMQXで`monitoring`スコープを持つ[APIキー](../admin/api.md#authentication)を作成します。
2. `prometheus.yaml`の各EMQXスクレイプジョブにAPIキーとシークレットキーを追加します。

   ```yaml
   basic_auth:
     username: '<API_KEY>'
     password: '<SECRET_KEY>'
   ```

EMQXは`POST /api/v5/login`で取得したBearerトークンも受け入れますが、Dashboardログイントークンは期限切れになるため、長時間稼働するPrometheusスクレイパーにはAPIキーを使用してください。

### PrometheusにEMQXスクレイプジョブを追加する

以下の`prometheus.yaml`例は、3つの一般的なメトリクスカテゴリを収集します。ターゲットアドレスと認証情報を置き換え、必要に応じて他の[メトリクスエンドポイント](#select-metrics-endpoints-to-scrape)のジョブを追加してください。ファイル変更後はPrometheusを再起動してください。

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

プッシュモードは`/api/v5/prometheus/stats`で利用可能な基本的なメトリクスとカウンターのみを送信します。その他のエンドポイントのメトリクスが必要な場合はプルモードを使用してください。

### DashboardでPushgateway送信を有効にする

1. DashboardのPrometheus統合設定を開きます。
2. **Enable Pushgateway** をオンにします。
3. Pushgatewayサーバー、プッシュ間隔、ジョブ名、必要なHTTPヘッダーを入力します。
4. **Save Changes** をクリックします。

PrometheusもPushgatewayインスタンスをスクレイプするよう設定する必要があります。

### 設定ファイルでPushgateway送信を有効にする

代わりに、`etc/base.hocon`に以下の推奨設定を追加します。

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

PrometheusがEMQXメトリクスの収集を開始したら、[EMQX Grafanaダッシュボード](https://grafana.com/grafana/dashboards/17446-emqx/)をインポートして可視化できます。このテンプレートはDashboardのPrometheus統合の**Help**ページからも入手可能です。

完全な例は[PrometheusとGrafanaでMQTTブローカーを監視する](https://www.emqx.com/en/blog/emqx-prometheus-grafana)を参照してください。
