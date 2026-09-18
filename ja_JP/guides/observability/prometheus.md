# Prometheusとの統合

EMQXは、[Prometheus](https://prometheus.io/)などのサードパーティ監視システムとの統合をサポートしています。PrometheusはSoundCloudがオープンソース化した監視ソリューションで、多次元データモデルのサポート、柔軟なクエリ言語、強力なアラーム管理など多彩な機能を提供します。

サードパーティ監視システムを利用することで、以下のような利点があります。

- EMQXの監視データを他のシステムの監視データと統合した完全な監視システムを構築可能。例えば、サーバーホストの監視情報も取得できます。
- [Grafanaダッシュボード](#use-grafana-to-visualize-EMQX-metrics)などを用いた、図表によるより直感的な監視レポートの作成。
- Prometheus Alertmanagerを使ったアラームルールや通知方法の設定など、多様なアラーム通知オプション。

EMQXはPrometheusメトリクス監視の統合方法として、以下の2つの方式をサポートしています。

- **Pullモード**：PrometheusがEMQXのREST APIを通じて直接メトリクスを収集する方式。
- **Pushモード**：EMQXがPushgatewayサービスにメトリクスをプッシュし、PrometheusがPushgatewayからメトリクスを収集する方式。

本ページでは両方式の設定手順を紹介します。EMQXダッシュボードの左側ナビゲーションメニューから **Management** -> **Monitoring** をクリックし、**Integration** タブで **Prometheus** を選択して設定を行えます。また、ページ内の **Help** ボタンをクリックすると各モードの具体的な設定手順を確認できます。

## Pullモード統合の設定

EMQXはPrometheusがシステムメトリクスを収集するために、以下のREST APIを提供しています。

- `/api/v5/prometheus/stats`：EMQXの基本的なメトリクスとカウンター。
- `/api/v5/prometheus/auth`：認証・認可を含むアクセス制御に関する主要なメトリクスとカウンター。
- `/api/v5/prometheus/data_integration`：ルールエンジン、コネクター、アクション、Sink/Source、エンコード/デコードに関連するメトリクスとカウンター。

これらのAPIを呼び出してメトリクスを取得する際、URLのクエリパラメータ `mode` を用いて異なる種類のメトリクスデータを取得できます。各パラメータの意味は以下の通りです。

:::: tabs type: card

::: tab シングルノードモード

```
mode=node
```

デフォルトモードで、現在のリクエストノードのメトリクスを返します。特に指定しない場合はこのモードが適用されます。

:::

::: tab クラスター集約モード

```
mode=all_nodes_aggregated
```

クラスターのメトリクスを集約し、クラスター内の全稼働ノードのメトリクスの*算術和*または*論理和*を返します。

- 「オン状態」や「稼働状態」などのメトリクスは論理和を返します。すべてのノードがオンまたは稼働中なら1を返し、そうでなければ0を返します。

- CPUやメモリ使用率などノードごとに独立したメトリクスは集約値を返しません。ノード名をラベルとして付与し、ノードごとのメトリクスを区別します。例：

  ```bash
  emqx_vm_cpu_use{node="emqx@172.17.0.2"} 7.6669163995887715
  emqx_vm_cpu_idle{node="emqx@172.17.0.2"} 92.33308360041123

  emqx_vm_cpu_use{node="emqx@172.17.0.3"} 7.676007766679973
  emqx_vm_cpu_idle{node="emqx@172.17.0.3"} 92.32399223332003
  ```

- クラスター内のどのノードでも値が一貫しているべきメトリクスは、APIリクエストを受けたノードの値を直接返します。これらは集約せず、ノード名ラベルも付きません。例：

  ```bash
  emqx_topics_count 3
  emqx_cert_expiry_at{listener_type="ssl",listener_name="default"} 1904285225
  emqx_cert_expiry_at{listener_type="wss",listener_name="default"} 1904285225
  ```

- その他のメトリクスは算術和を返します。つまり、返されるメトリクスは全ノードのメトリクスの合計です。

:::

::: tab クラスター非集約モード

```
mode=all_nodes_unaggregated
```

クラスター非集約メトリクスモードで、クラスター内の全稼働ノードの個別メトリクスを返します。

- ノード名をラベルとして付与し、ノードごとのメトリクスを区別します。例：

  ```bash
  emqx_connections_count{node="emqx@127.0.0.1"} 0
  ```

- クラスター内のどのノードでも値が一貫しているべきメトリクス（例：「ブラックリスト数」「保持メッセージ数」など）は、APIリクエストを受けたノードの値を直接返します。ノード名ラベルは付きません。例：

  ```bash
  emqx_retained_count 3
  ```

:::

::::

PrometheusのPullエンドポイントの詳細については、[EMQX Enterprise APIドキュメント](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)を参照してください。

::: tip

PullモードAPIはデフォルトで認証不要です。ページ上の **Enable Basic Auth** スイッチを設定すると、インターフェースにベーシック認証を有効化できます。有効化した場合は、EMQXで[APIキー](../../guides/api.md#authentication)を作成し、Prometheus設定に適用してメトリクスを取得してください。

:::

### Prometheus設定例

```yaml
# prometheus.yaml
global:
  scrape_interval:     10s # デフォルトのスクレイプ間隔は10秒ごと
  evaluation_interval: 10s # デフォルトの評価間隔は10秒ごと
  # このマシン上のすべての時系列がデフォルトでエクスポートされる
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

## Pushモード統合の設定

EMQXはPushgatewayにメトリクスをプッシュし、PrometheusがPushgatewayからメトリクスを収集する方式をサポートしています。Pushgatewayへのプッシュはデフォルトで無効化されています。Pushgatewayサービスを有効にするには、ダッシュボードのPrometheus設定ページで **Enable Pushgateway** トグルスイッチをクリックしてください。

<img src="./assets/enable-push-gateway.png" alt="Pushgatewayを有効にする" style="zoom:40%;" />

ビジネスニーズに応じて以下の項目を設定し、**Save Changes** をクリックします。

- **Interval**：Pushgatewayへ監視メトリクスデータを報告する時間間隔を指定します。デフォルトは`15`秒です。
- **Pushgateway Server**：PrometheusサーバーのURLを入力します。デフォルトは `http://127.0.0.1:9091` です。
- **Job Name**：EMQXクラスター名、ノード名、ホスト名を含む変数を指定します。デフォルトは `${name}/instance/${name}~${host}` です。例えば、EMQXノード名が `emqx@127.0.0.1` の場合、`name` は `emqx`、`host` は `127.0.0.1` となります。
- **Headers**：Pushgatewayにプッシュする監視メトリクスのHTTPヘッダーのキーと値を入力します。**Add** ボタンで複数のヘッダーを追加可能です。型は文字列で、例：{ Authorization = "some-authz-tokens"}。

同時に、**Help** ボタンをクリックし、**Use Pushgateway** タブの手順を参照して設定できます。

::: tip

Pushモードは現状、`/api/v5/prometheus/stats` エンドポイントのEMQX基本メトリクスとカウンターのみを含むため、Pullモードの利用を推奨します。

:::

Pushgatewayの有効化と設定は設定ファイルに以下を追加することでも可能です。設定項目の詳細は[Configuration - Prometheus](../configuration/prometheus.md)を参照してください。

```bash
prometheus {
  push_gateway_server = "http://127.0.0.1:9091"
  interval = 15s
  headers {}
  job_name = "${name}/instance/${name}~${host}"
}
```

## GrafanaでEMQXメトリクスを可視化する

GrafanaとPrometheusを組み合わせてEMQXメトリクスを可視化することも可能です。GrafanaにEMQXのテンプレートファイルをインポートすることで実現できます。テンプレートのダウンロードは、[EMQX | Grafana Dashboard](https://grafana.com/grafana/dashboards/17446-emqx/) をクリックするか、**Monitoring** ページの **Integration** タブ下部の **Help** ボタンから行えます。

::: tip

詳細な操作手順は [Monitoring MQTT broker with Prometheus and Grafana](https://www.emqx.com/en/blog/emqx-prometheus-grafana) を参照してください。

:::
