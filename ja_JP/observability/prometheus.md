# Prometheusとの連携

EMQXは、SoundCloudによって開発されたオープンソースの監視ソリューションである[Prometheus](https://prometheus.io/)などのサードパーティ監視システムとの連携をサポートしています。Prometheusは多次元データモデル、柔軟なクエリ言語（PromQL）、強力なアラート機能を提供します。

サードパーティ監視システムを利用することで、以下のような利点があります。

- EMQXの監視データを他のシステムの監視データと統合した完全な監視システムを構築可能。例えば、サーバーホストの監視情報も取得できます。
- [Grafanaダッシュボード](#use-grafana-to-visualize-EMQX-metrics)などを使って、EMQXのメトリクスを図やグラフで直感的に可視化できます。
- Prometheus Alertmanagerを利用したアラームルールや通知方法の多様な設定が可能です。

EMQXはPrometheusメトリクス監視の統合方法として、以下の2つの方式をサポートしています。

- **Pullモード**：PrometheusがEMQXのREST APIを通じてメトリクスを直接収集する方式。
- **Pushモード**：EMQXがメトリクスをPushgatewayサービスにプッシュし、Prometheusがそこからメトリクスを収集する方式。

Prometheus連携の設定手順は以下の通りです。

1. EMQXダッシュボードの **Management** -> **Monitoring** に移動します。
2. **Integration** タブに切り替えます。
3. 監視プラットフォームとして **Prometheus** を選択します。

選択したモードに応じて、一部の設定項目はPullモードのみ適用されるものや、両モードに影響するものがあります。ダッシュボードの **Help** ボタンをクリックすると、各モードの詳細な設定手順が確認できます。

<img src="./assets/enable-push-gateway.png" alt="Pushgatewayの有効化" style="zoom:40%;" />

## Prometheus設定オプション

このセクションでは、ダッシュボードで **Prometheus** を選択した際に利用可能な設定オプションを説明します。

### 共通オプション（Pullモード・Pushモード両方に影響）

#### レイテンシーバケット

レイテンシー関連のメトリクスのヒストグラムバケット境界を指定します。

**フォーマット**

カンマ区切りの期間リスト：

```
10ms, 100ms, 1s, 5s, 30s
```

**説明**

これらの値は、Prometheus上でレイテンシーメトリクスをどのようにヒストグラムバケットに分類するかを定義します。小さいバケット間隔は詳細な粒度を提供しますが、メトリクスのカーディナリティ（種類数）やストレージ使用量が増加する可能性があります。

この設定は内部的にレイテンシーヒストグラムメトリクスの生成に影響し、以下に適用されます。

- Pullモードのメトリクス
- Pushモードのメトリクス（Pushgateway経由）

### Pullモード設定

以下の設定は、PrometheusがREST API経由でEMQXメトリクスをスクレイピングするPullモード時にのみ適用されます。

#### Basic Authの有効化

PrometheusスクレイプAPIに対するHTTP Basic認証の有効化・無効化を設定します。

デフォルトでは、PrometheusのPullモードAPIは認証を必要としません。このオプションを有効にすると：

- Prometheusは以下のAPIにアクセスする際にHTTP Basic認証を使用する必要があります。
  - `/api/v5/prometheus/stats`
  - `/api/v5/prometheus/auth`
  - `/api/v5/prometheus/data_integration`
- EMQXで[APIキー](../admin/api.md#authentication)を作成する必要があります。
- `prometheus.yaml`の`basic_auth`セクションを設定します。

このオプションはPullモードにのみ適用され、Pushgateway連携には影響しません。詳細は[Pullモード連携の設定](#configure-pull-mode-integration)を参照してください。

#### ネームスペースデータスクレイピングレート制限

ネームスペース関連メトリクスのスクレイピング時の最大リクエストレートを制限します。

ネームスペースレベルのメトリクスはマルチテナント環境でサポートされており、ネームスペース単位で公開または集約可能です。詳細は[Prometheusメトリクスの分離](../multi-tenancy/namespace-overview.md#multi-tenancy-capability-support)を参照してください。

**フォーマット**：`<リクエスト数>/<期間>`

**例**：`1/5s` は5秒あたり最大1リクエストを許可し、それを超えるリクエストは拒否されます。

**挙動**：

- ネームスペースレベルのメトリクススクレイピングリクエストにのみ適用されます。
- 特定のネームスペースを対象としたリクエストは制限されません。
- Pullモードにのみ適用されます。

このオプションは大規模またはマルチネームスペース環境での過負荷防止に役立ちます。

### Pushモード設定

Pushモードでは、EMQXがメトリクスをPushgatewayインスタンスに送信します。デフォルトではPushモードは無効です。

#### Pushgatewayの有効化

Pushgatewayへのメトリクスプッシュを有効または無効にします。有効にした場合、以下の項目を設定します。

#### プッシュ間隔

EMQXがPushgatewayにメトリクスをプッシュする間隔を指定します。デフォルトは`15`秒です。

#### Pushgatewayサーバー

PushgatewayサーバーのURLを指定します。デフォルトは`http://127.0.0.1:9091`です。

#### ジョブ名

Pushgatewayにメトリクスをプッシュする際のジョブラベルを指定します。

EMQXのノード名やホスト名に由来する変数を使ってジョブラベルを構築可能です。デフォルト値は`${name}/instance/${name}~${host}`です。

**変数**：

- `${name}`：EMQXノード名（例：`emqx`）
- `${host}`：ホストIPアドレス（例：`127.0.0.1`）

例えば、ノード名が`emqx@127.0.0.1`の場合：

- `${name}` = `emqx`
- `${host}` = `127.0.0.1`

#### ヘッダー

Pushgatewayにメトリクスをプッシュする際に送信する任意のHTTPヘッダーを設定できます。

値の型は文字列で、キーと値のペアで設定します。例：

```
Authorization = "some-auth-token"
```

追加のヘッダーは **Add** ボタンで挿入できます。

## Pullモード連携の設定

Pullモードでは、PrometheusがREST APIを通じてEMQXからメトリクスをスクレイピングします。

EMQXは以下のエンドポイントを提供しています。

- `/api/v5/prometheus/stats`：EMQXの基本的なメトリクスとカウンター。
- `/api/v5/prometheus/auth`：認証・認可を含むアクセス制御関連の主要メトリクスとカウンター。
- `/api/v5/prometheus/data_integration`：ルールエンジン、コネクター、アクション、Sink/Source、エンコード/デコードに関連するメトリクスとカウンター。

### メトリクス収集モード

上記APIを呼び出す際にURLクエリパラメータ`mode`を指定することで、異なる種類のメトリクスデータを取得できます。各パラメータの意味は以下の通りです。

:::: tabs type: card

::: tab シングルノードモード

```
mode=node
```

デフォルトのモードで、リクエストされたノード自身のメトリクスを返します。特に指定しない場合はこのモードが適用されます。

:::

::: tab クラスター集約モード

```
mode=all_nodes_aggregated
```

クラスター全体のメトリクスを集約し、すべての稼働ノードのメトリクスの**算術和**または**論理和**を返します。

- 「オン状態」や「稼働状態」などのメトリクスは論理和で返されます。すべてのノードがオンまたは稼働中なら1、それ以外は0を返します。

- CPUやメモリ使用率などノードごとに独立したメトリクスは集約されず、ノード名をラベルに含めて区別されます。例：

  ```bash
  emqx_vm_cpu_use{node="emqx@172.17.0.2"} 7.6669163995887715
  emqx_vm_cpu_idle{node="emqx@172.17.0.2"} 92.33308360041123
  
  emqx_vm_cpu_use{node="emqx@172.17.0.3"} 7.676007766679973
  emqx_vm_cpu_idle{node="emqx@172.17.0.3"} 92.32399223332003
  ```

- クラスター内のどのノードでも値が一貫しているべきメトリクスは、APIリクエストを受けたノードの値をそのまま返します。これらは集約されず、ノード名ラベルも含みません。例：

  ```bash
  emqx_topics_count 3
  emqx_cert_expiry_at{listener_type="ssl",listener_name="default"} 1904285225
  emqx_cert_expiry_at{listener_type="wss",listener_name="default"} 1904285225
  ```

- その他のメトリクスは算術和で返されます。

:::

::: tab クラスター非集約モード

```
mode=all_nodes_unaggregated
```

クラスター内のすべての稼働ノードの個別メトリクスを返します。

- ノード名をラベルに含めてノードごとのメトリクスを区別します。例：

  ```bash
  emqx_connections_count{node="emqx@127.0.0.1"} 0
  ```

- クラスター内のどのノードでも値が一貫しているべきメトリクスは、APIリクエストを受けたノードの値をそのまま返します。ノード名ラベルは含みません。例：

  ```bash
  emqx_retained_count 3
  ```

:::

::::

PrometheusのPullエンドポイントの詳細は、[EMQX Enterprise APIドキュメント](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)を参照してください。

### 認証（任意）

デフォルトでは、PrometheusのPullモードAPIは認証を必要としません。

EMQXダッシュボードで **Basic Authの有効化** をオンにした場合、PrometheusはHTTP Basic認証で認証を行う必要があります。

その場合は以下の手順を行います。

1. EMQXで[APIキー](../admin/api.md#authentication)を作成します。
2. 生成されたAPIキーとシークレットキーをPrometheus設定に使用します。

Prometheus設定例：

```yaml
basic_auth:
  username: '<API_KEY>'
  password: '<SECRET_KEY>'
```

- `username` はAPIキー
- `password` は対応するシークレットキー

Prometheusはこれらの認証情報を使ってEMQXメトリクスをスクレイピングします。

### Prometheusサーバー設定例

PrometheusがEMQXメトリクスをスクレイピングできるように、Prometheusサーバーの設定を行います。

以下の設定をPrometheus設定ファイルに追加し、Prometheusサービスを再起動してください。

```yaml
# prometheus.yaml
global:
  scrape_interval:     10s # デフォルトのスクレイプ間隔は10秒
  evaluation_interval: 10s # デフォルトの評価間隔は10秒
  # このマシン上のすべての時系列はデフォルトでエクスポートされます
  external_labels:
    monitor: 'emqx-monitor'
scrape_configs:
  - job_name: 'emqx_stats'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/stats'
    scheme: 'http'
    basic_auth:
      username: ''
      password: ''

  - job_name: 'emqx_auth'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/auth'
    scheme: 'http'
    basic_auth:
      username: ''
      password: ''

  - job_name: 'emqx_data_integration'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/data_integration'
    scheme: 'http'
    basic_auth:
      username: ''
      password: ''
```

## Pushモード連携の設定

PushモードはEMQXからPushgatewayへメトリクスを送信します。

ダッシュボードで **Pushgatewayの有効化** をオンにし、必要な項目を設定後、**Save Changes** をクリックしてください。

Pushモードは現在、`/api/v5/prometheus/stats` エンドポイントの基本的なメトリクスとカウンターのみを含みます。包括的な監視にはPullモードの利用が推奨されます。

### 設定ファイル例

設定ファイルに以下のように記述してPushgatewayを有効化・設定することも可能です。設定項目の詳細は[設定 - Prometheus](../configuration/prometheus.md)を参照してください。

```bash
prometheus {
  push_gateway_server = "http://127.0.0.1:9091"
  interval = 15s
  headers {}
  job_name = "${name}/instance/${name}~${host}"
}
```

## GrafanaでEMQXメトリクスを可視化する

GrafanaとPrometheusを組み合わせてEMQXメトリクスを可視化することも可能です。GrafanaにEMQXのテンプレートファイルをインポートすることで実現できます。テンプレートは[EMQX | Grafana Dashboard](https://grafana.com/grafana/dashboards/17446-emqx/)からダウンロードするか、**Monitoring** ページの **Integration** タブ下部の **Help** ボタンから入手できます。

::: tip

詳細な操作手順は[Monitoring MQTT broker with Prometheus and Grafana](https://www.emqx.com/en/blog/emqx-prometheus-grafana)をご参照ください。

:::
