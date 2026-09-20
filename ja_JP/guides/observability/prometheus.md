# PrometheusでEMQXをモニタリングする

EMQXは、Prometheusがクエリ、アラート、可視化のために収集できるランタイムメトリクスを公開しています。これらのメトリクスは、以下のいずれかの方法で収集できます。

- **プルモード（推奨）**：PrometheusがREST APIエンドポイントから直接EMQXのメトリクスをスクレイプします。このモードを使用すると、利用可能なメトリクスの完全なセットを収集できます。
- **プッシュモード**：EMQXが基本的なメトリクスをPushgatewayに送信し、PrometheusがPushgatewayからそれらをスクレイプします。PrometheusがEMQXに直接接続できない場合にこのモードを使用します。

収集したEMQXメトリクスは、Grafanaを使って[可視化](#visualize-emqx-metrics-in-grafana)できます。

::: tip
EMQX 6.3.0以降、Prometheusメトリクスは`metrics`機能ゲートで制御されます。`EMQX_FEATURES`を手動で設定する場合、`metrics`を有効にすると、その依存である`dashboard`も有効になります。認証と認可はコア機能であり、機能ゲートで制御されません。詳細は[機能ゲート](../../get-started/deploy/feature-gates.md)をご覧ください。
:::

## EMQXでのメトリクス収集の設定

DashboardのPrometheus統合ページで、認証、Pushgatewayへの送信、レイテンシーバケット、ネームスペースのリクエスト制限を制御できます。このセクションでは各設定項目の内容を説明します。プルモードおよびプッシュモードの設定手順は後述します。

Prometheus統合設定を開くには：

1. EMQX Dashboardの**Management** -> **Monitoring**に移動します。
2. **Integration**タブを選択します。
3. **Prometheus**を選択します。

<img src="./assets/enable-push-gateway.png" alt="Prometheus統合設定" style="zoom: 67%;" />

以下のエンドポイントで公開されるメトリクスシリーズの厳選されたリファレンス（アラートに適したものを含む）は、[Broker Health Indicators](./broker-health-indicators.md)をご覧ください。

### スクレイプリクエストの認証を必須にする

**Enable Basic Auth**は、`/api/v5/prometheus/*`以下のすべてのPrometheusスクレイプAPIの認証を制御します。Dashboardのラベルにかかわらず、この設定はHTTP Basic認証とBearer認証の両方を制御します。

EMQX 6.3.0以降、認証はデフォルトで有効です。認証情報なしのリクエストは`401`を返します。PrometheusはAPIキーとシークレットキーを用いたHTTP Basic認証を使用できます。EMQXはDashboardのログイントークンをBearerトークンとしても受け入れますが、トークンは期限切れになるため、継続的なスクレイプには適しません。

長時間稼働するPrometheusサーバーの場合は、`monitoring`スコープの専用APIキーを作成し、[Prometheusのスクレイプリクエスト認証を設定](#authentication)してください。

認証なしのスクレイプを許可するには、**Enable Basic Auth**をオフにするか、`prometheus.enable_basic_auth = false`に設定します。この設定はプルモードにのみ影響し、Pushgatewayへの送信には影響しません。

::: warning 重要なお知らせ
認証を無効にすると、Dashboardリスナーにアクセス可能な任意のクライアントがEMQXメトリクスをスクレイプ可能になります。アップグレード後、明示的に`prometheus.enable_basic_auth = false`に設定された構成や旧形式のPrometheus構成は、認証なしスクレイプを継続して許可します。アップグレード後はDashboardで**Enable Basic Auth**を必ず確認してください。
:::

### Pushgatewayへのメトリクス送信を設定する

**Enable Pushgateway**をオンにすると、EMQXがPushgatewayインスタンスにメトリクスを送信します。プッシュ送信はデフォルトで無効です。以下の項目を設定してください。

| 項目 | 説明 |
| --- | --- |
| **Interval** | EMQXがメトリクスをプッシュする間隔。デフォルトは`15`秒です。 |
| **Pushgateway Server** | PushgatewayのURL。デフォルトは`http://127.0.0.1:9091`です。 |
| **Job Name** | プッシュされるメトリクスのジョブラベル。デフォルトは`${name}/instance/${name}~${host}`で、`${name}`は`@`の前のノード名、`${host}`は`@`の後のホスト名です。例えば`emqx@127.0.0.1`の場合、`emqx`と`127.0.0.1`になります。 |
| **Headers** | Pushgatewayに送信する任意のHTTPヘッダー。各ヘッダーはキーと値のペアで追加します。例：`Authorization = "some-auth-token"` |

完全な手順は[Pushgatewayへのメトリクス送信設定](#configure-push-mode-integration)をご覧ください。

### レイテンシーヒストグラムのバケットを定義する

**Latency Buckets**には、カンマ区切りの期間値を入力します。例：

```text
10ms, 100ms, 1s, 5s, 30s
```

これらの値はプルモード・プッシュモード両方のレイテンシーヒストグラムのバケット境界を定義します。バケット数が多いほど粒度は細かくなりますが、メトリクスのカーディナリティとストレージ使用量が増加する可能性があります。

### すべてのネームスペースをスクレイプするリクエストの制限

**Namespace Data Scraping Rate Limit**は、すべてのネームスペースにまたがるメトリクススクレイプリクエストの最大レートを設定します。特定のネームスペースを指定したリクエストは制限されません。`<requests>/<duration>`形式で値を入力します。デフォルトの`1/5s`は5秒ごとに1リクエストを許可し、それ以外のリクエストは拒否されます。

## PrometheusでEMQXメトリクスをスクレイプする設定

プルモードでは、PrometheusがEMQX Dashboardリスナーに接続し、1つ以上のREST APIエンドポイントをスクレイプします。

### スクレイプするメトリクスエンドポイントの選択

収集したいメトリクスカテゴリごとにPrometheusのスクレイプジョブを追加します。

| エンドポイント | メトリクス内容 |
| --- | --- |
| `/api/v5/prometheus/stats` | 基本的なEMQXメトリクスとカウンター |
| `/api/v5/prometheus/namespaced_stats` | ネームスペースごとに集約されたメトリクス |
| `/api/v5/prometheus/auth` | 認証、認可、禁止クライアントのメトリクス |
| `/api/v5/prometheus/data_integration` | ルール、コネクター、アクション、Sink/Source、エンコード/デコードのメトリクス |
| `/api/v5/prometheus/schema_validation` | スキーマ検証のメトリクス |
| `/api/v5/prometheus/message_transformation` | メッセージ変換のメトリクス |
| `/api/v5/prometheus/topic_metrics` | トピックメトリクス収集のカウンター |

完全なAPIリファレンスは[EMQX Enterprise APIドキュメント](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)をご覧ください。

### ネームスペースごとのデータ統合メトリクスのスクレイプ

EMQX 6.3.0以降、`GET /api/v5/prometheus/data_integration`は認証ユーザーのネームスペースに応じてルール、アクション、コネクターメトリクスを制限します。

- ネームスペースユーザーは割り当てられたネームスペースのメトリクスのみ受け取ります。`ns=<namespace>`で別のネームスペースを指定すると`403`が返されます。
- グローバル管理者はデフォルトで全ネームスペースのメトリクスを受け取ります。`ns=<namespace>`で特定のネームスペースをスクレイプ、または`only_global=true`（`ns`なし）でグローバルネームスペースのみをスクレイプできます。
- 認証が無効の場合、グローバル管理者と同様の可視性が適用され、デフォルトで全ネームスペースのメトリクスが返されます。

グローバル以外のネームスペースのルール、アクション、コネクターのリソース単位メトリクスには`namespace`ラベルが付きます。グローバルネームスペースのリソース単位メトリクスにはこのラベルは付きません。`emqx_schema_registrys_count`メトリクスはスキーマレジストリリソースがネームスペースでスコープされないためクラスター全体のままです。

すべてのネームスペースをスクレイプするリクエストは[Namespace Data Scraping Rate Limit](#limit-requests-that-scrape-all-namespaces)の対象となります。

### メトリクス収集モードの選択

対応するエンドポイントでは、`mode`クエリパラメータで現在のノードのメトリクスを返すかクラスター全体のメトリクスを返すかを制御できます。

:::: tabs type: card

::: tab 現在のノード

```text
mode=node
```

リクエストを受け取ったノードのメトリクスを返します。デフォルトモードです。

:::

::: tab 集約されたクラスター

```text
mode=all_nodes_aggregated
```

すべての稼働中ノードのメトリクスを以下の集約ルールで返します。

- 状態メトリクスは論理集約されます。例えば、あるメトリクスが全ノードで有効または稼働中の場合に`1`となり、それ以外は`0`です。
- CPUやメモリ使用率などノード固有のメトリクスは集約されず、`node`ラベルを保持します。

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

- その他のメトリクスはすべての稼働ノードの算術和を返します。

:::

::: tab 集約されていないクラスター

```text
mode=all_nodes_unaggregated
```

すべての稼働ノードの個別メトリクスを返します。ノード固有の値には`node`ラベルが付きます。

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

EMQX 6.3以降、`GET /api/v5/prometheus/topic_metrics`はトピックメトリクスREST APIで作成された名前付きコレクションのカウンターを公開します。このエンドポイントをスクレイプする前に、少なくとも1つのコレクションを作成してください。作成手順は[REST APIによるトピックメトリクスコレクションの管理](./topic-metrics.md#manage-topic-metric-collections-with-the-rest-api)をご覧ください。

公開されるカウンターは以下の通りです。

| メトリクス | 説明 |
| --- | --- |
| `emqx_topic_metric_messages_in_count` | コレクションフィルターにマッチするトピックにパブリッシュされたメッセージ数 |
| `emqx_topic_metric_messages_out_count` | マッチしたメッセージがサブスクライバーに配信された数 |
| `emqx_topic_metric_messages_dropped_count` | EMQXによってドロップされたマッチしたメッセージ数 |
| `emqx_topic_metric_bytes_in` | マッチしたパブリッシュメッセージのトピックとペイロードの合計サイズ |
| `emqx_topic_metric_bytes_out` | マッチした配信メッセージのトピックとペイロードの合計サイズ |

各時系列には`name`と`topic_filter`ラベルが付きます。ネームスペース所有のコレクションには`namespace`ラベルも付きます。`mode=all_nodes_unaggregated`の場合は各時系列に`node`ラベルも付きます。

すべてのトピックメトリクス値は単調増加カウンターです。`rate()`などのPromQL関数を使って1秒あたりのレートを計算してください。例：

```text
rate(emqx_topic_metric_messages_in_count[5m])
```

::: warning 重要なお知らせ

各コレクションは5つのカウンターを公開します。集約されていないモードでは、EMQXはノードごとに別々の時系列を作成します。過剰なPrometheus時系列の作成を避けるため、コレクション数を制限してください。

:::

<a id="authentication"></a>

### Prometheusスクレイプリクエストの認証設定

EMQX 6.3.0以降、PrometheusスクレイプAPIはデフォルトで認証を要求します。継続的なスクレイプには専用APIキーを用いたHTTP Basic認証を使用してください。

1. EMQXで`monitoring`スコープの[APIキー](../api.md#authentication)を作成します。
2. `prometheus.yaml`の各EMQXスクレイプジョブにAPIキーとシークレットキーを追加します。

   ```yaml
   basic_auth:
     username: '<API_KEY>'
     password: '<SECRET_KEY>'
   ```

PrometheusはDashboardのBearerトークンでも認証可能です。`dashboard.password_login`が`both`の場合は`POST /api/v5/login`でトークンを取得します。`scram_only`モードでは[SCRAMチャレンジレスポンス認証](../api.md#bearer-token-authentication)で取得します。Dashboardログイントークンは期限切れになるため、長期間稼働するPrometheusスクレイパーにはAPIキーを推奨します。

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

## EMQXでPushgatewayにメトリクスをプッシュする設定

プッシュモードは`/api/v5/prometheus/stats`で提供される基本的なメトリクスとカウンターのみを送信します。他のエンドポイントのメトリクスが必要な場合はプルモードを使用してください。

### DashboardでPushgateway送信を有効にする

1. DashboardのPrometheus統合設定を開きます。
2. **Enable Pushgateway**をオンにします。
3. Pushgatewayサーバー、プッシュ間隔、ジョブ名、および必要なHTTPヘッダーを入力します。
4. **Save Changes**をクリックします。

PrometheusもPushgatewayインスタンスをスクレイプするよう設定する必要があります。

### 設定ファイルでPushgateway送信を有効にする

代わりに、推奨されるネストされた設定を`etc/base.hocon`に追加します。

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

完全な例は[PrometheusとGrafanaでMQTTブローカーをモニタリングする](https://www.emqx.com/en/blog/emqx-prometheus-grafana)をご覧ください。
