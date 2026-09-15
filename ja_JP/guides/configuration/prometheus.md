# Prometheus の設定

[Prometheus](https://prometheus.io/) は SoundCloud がオープンソース化したモニタリングソリューションで、多次元データモデルのサポート、柔軟なクエリ言語、強力なアラーム管理機能を備えています。EMQX はサードパーティのモニタリングシステムとして Prometheus との統合をサポートしています。本機能の詳細については、[Prometheus との統合](../observability/prometheus.md) をご参照ください。

`base.hocon` 設定ファイルを通じて Pushgateway の有効化および設定が可能です。例：

```bash
prometheus {
  push_gateway_server = "http://127.0.0.1:9091"
  interval = 15s
  headers {}
  job_name = "${name}/instance/${name}~${host}"
}
```

ここで、

- `push_gateway_server` は Prometheus のプッシュゲートウェイサーバーの URL を設定します。メトリクスを Prometheus にプッシュするために使用され、デフォルトは `http://127.0.0.1:9091` です。
- `interval` はメトリクスを収集し Prometheus にエクスポートする間隔を設定します。デフォルトは `15s` です。
- `headers` は Prometheus プッシュゲートウェイサーバーへの HTTP リクエストに含める追加のヘッダーを設定します。
- `job_name` はメトリクスをエクスポートする Prometheus 上のジョブ名を設定します。デフォルトは `"${name}/instance/${name}~${host}"` です。

::: tip

左側のナビゲーションメニューから **Management** -> **Monitoring** -> **Integration** をクリックすると、ダッシュボードを使って Prometheus とのプッシュモード統合を設定することも可能です。

ダッシュボードで統合を設定すると、その設定が設定ファイル内の同じ項目より優先されます。  
設定ファイルから Prometheus を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
これは、`emqx.conf` に設定した場合、ダッシュボード経由での変更は一時的なものとなり、EMQX 再起動時に失われるためです。

:::

::: tip

EMQX はよりカスタマイズされたニーズに対応するため、さらに多くの設定項目を提供しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご参照ください。

:::
