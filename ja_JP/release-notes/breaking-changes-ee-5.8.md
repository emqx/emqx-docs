# EMQX 5.8 の互換性のない変更点

## e5.8.9

- [#16062](https://github.com/emqx/emqx/pull/16062) RocketMQ アクションが指定されたペイロードテンプレートを無視し、ルールの出力全体をレンダリングしてしまう問題を修正しました。

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 13（Ventura）向けのパッケージリリースを停止しました。

## e5.8.6

- [#14802](https://github.com/emqx/emqx/pull/14802) このバージョン以降、REST API またはダッシュボード経由でのプラグインインストールには明示的な許可が必要になります。インストール前に以下の CLI コマンドで許可を取得してください。

  ```bash
  emqx ctl plugins allow NAME-VSN
  ```

  この変更は、不正なプラグインインストールを防ぐためのセキュリティ強化です。API やダッシュボードでプラグインを管理しているユーザーは、ワークフローの調整が必要です。

## e5.8.5

- [#14703](https://github.com/emqx/emqx/pull/14703) `force_shutdown.max_heap_size` の最大許容値を `128GB` に変更しました。以前に 128GB を超える値に設定していた場合、アップグレード後の設定リロードや更新時に問題が発生する可能性があります。

## e5.8.4

- [#14360](https://github.com/emqx/emqx/pull/14360) Prometheus メトリクスを JSON 形式で要求した際、`client` トップレベルキーは単一の JSON オブジェクトではなく、常に JSON オブジェクトの配列となります。この変更により、監視ツールのデータ処理に影響が出る可能性があります。

- [#14370](https://github.com/emqx/emqx/pull/14370) IoTDB データ統合の設定変更：

  - セルフディスクリビングテンプレートが削除されました。EMQX は設定されたデータテンプレートのみを処理し、メッセージペイロードからテンプレートを抽出しなくなりました。

  - 各 MQTT メッセージは単一の `payload` のみを持つことが可能で、ペイロードの配列はサポートされません。したがって、MQTT メッセージは単一またはバッチ挿入のいずれかの単一原子操作として IoTDB に処理されます。1つの MQTT メッセージから複数の IoTDB 操作を生成することはできなくなりました。

  - `data type` はテンプレート値ではなくプレーンな値として扱われます。

  - REST API ドライバーは IoTDB 1.3.x 以降のみをサポートします。

  - Thrift ドライバーは「バッチ」モードをサポートします。

    **重要**：バッチモードでタイムスタンプの重複を防ぐために、MQTT メッセージのタイムスタンプ（`${timestamp}`）を使用するか、ペイロード内に時間フィールド（例：`${payload.time}`）を含めることを推奨します。

## e5.8.3

- [#14305](https://github.com/emqx/emqx/pull/14305) 認証におけるハッシュアルゴリズム `MD4`、`MD5`、`RIPEMD-160` のサポートを削除しました。これらは [NIST Secure Hash Standard](https://www.nist.gov/publications/secure-hash-standard) に準拠していません。

## e5.8.2

- [#14004](https://github.com/emqx/emqx/pull/14004) クラスターリンクの `topics` 設定でトピックフィルターが重複している場合に、クロスクラスターのメッセージルーティングが不整合かつ不完全になる問題を修正しました。各トピックフィルターは個別に処理されるため、`topics` 設定内で冗長なトピックフィルター（例：`t/1` と `t/+`）は無効とみなされます。このような設定が検出されるとリンクは起動に失敗します。

- [#14015](https://github.com/emqx/emqx/pull/14015) 動的トピック（プレースホルダーを含むトピック）を使用する Kafka/Confluent/Azure Event Hub のプロデューサーはディスクバッファリングをサポートしなくなりました。現在はメモリおよびハイブリッドモードのみサポートされています。

- [#14106](https://github.com/emqx/emqx/pull/14106) 単一の Kafka コンシューマコネクターに同じ Kafka トピックを持つソースが複数含まれることを禁止するバリデーションを追加しました。トピックを繰り返したい場合は、新しいコネクターとソースを作成してください。

## e5.8.1

- [#13792](https://github.com/emqx/emqx/pull/13792) `until` 値なしで作成された禁止アイテムのデフォルト有効期限を無期限（以前は最大1年）に変更しました。

- [#13742](https://github.com/emqx/emqx/pull/13742) クライアントが `#` または `+` トピックをサブスクライブした際に、`$` で始まるトピックの保持メッセージを受信してしまう問題を修正しました。

  この修正は [MQTT-4.7.2-1](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901246) の要件を満たします。

## e5.8.0

- [#13080](https://github.com/emqx/emqx/pull/13080) `mqtt.retry_interval` 設定のデフォルト値を 30秒から無限大に変更しました。

  これまでは EMQX がデフォルトで30秒ごとにメッセージ配信を自動リトライしていましたが、新しいデフォルトでは自動リトライを行いません。この変更は MQTT 仕様に準拠したもので、セッション内でのメッセージ配信リトライは一般的に推奨されていません。

  リトライ機能に依存しているユーザーのために、特定のリトライ間隔を設定するオプションは引き続き利用可能です。

- [#13190](https://github.com/emqx/emqx/pull/13190) CentOS 7 および Ubuntu 18 向けリリースのサポートを終了しました。これらの OS のサポート終了に伴い、EMQX はこれらのビルドを提供しません。

- [#13248](https://github.com/emqx/emqx/pull/13248) `builtin` 耐久ストレージバックエンドを廃止し、柔軟性とスケーラビリティを向上させるために以下の新しいバックエンドに置き換えました：

  - **`builtin_local`**：レプリケーションをサポートしない耐久ストレージバックエンドで、シングルノード展開に適しています。オープンソース版およびエンタープライズ版で利用可能ですが、マルチノードクラスターには対応していません。

  - **`builtin_raft`**：Raft コンセンサスアルゴリズムを用いた耐久ストレージバックエンドで、複数ノード間でのデータレプリケーションを提供します。エンタープライズ版限定で利用可能で、データ耐久性とフォールトトレランスを強化します。

  また、以下の Prometheus メトリクス名が機能をより正確に反映するように変更されました：

  - `emqx_ds_egress_batches` → `emqx_ds_buffer_batches`
  - `emqx_ds_egress_batches_retry` → `emqx_ds_buffer_batches_retry`
  - `emqx_ds_egress_batches_failed` → `emqx_ds_buffer_batches_failed`
  - `emqx_ds_egress_messages` → `emqx_ds_buffer_messages`
  - `emqx_ds_egress_bytes` → `emqx_ds_buffer_bytes`
  - `emqx_ds_egress_flush_time` → `emqx_ds_buffer_flush_time`

- [#13526](https://github.com/emqx/emqx/pull/13526) オープンソース版から Core-replicant 機能を削除しました。5.8 以降、オープンソース版の全ノードは Core 役割で動作します。この変更はエンタープライズ版には影響せず、Core-replicant 機能は引き続き利用可能です。また、不要となった `cluster.core_nodes` 設定パラメータも削除されました。

- [#13372](https://github.com/emqx/emqx/pull/13372) ゲートウェイが受け入れる接続数はライセンス条件に基づいて制御されるようになり、許可された接続数の制限に準拠します。
